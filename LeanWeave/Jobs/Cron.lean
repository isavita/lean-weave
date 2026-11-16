import LeanWeave.Jobs.Types
import Lean

namespace LeanWeave.Jobs.Cron

structure Field where
  values : Array Nat
  wildcard : Bool := false
  deriving Repr, Inhabited

structure Schedule where
  minute : Field
  hour : Field
  day : Field
  month : Field
  weekday : Field
  deriving Repr, Inhabited

private def field (source : String) (low high : Nat) (sunday : Bool := false) : Except String Field := do
  if source.isEmpty || source.utf8ByteSize > 256 then throw "invalid cron field"
  let mut values : Array Nat := #[]
  for entry in source.splitOn "," do
    let stepped := entry.splitOn "/"
    if stepped.length > 2 then throw "invalid cron step"
    let step ← if stepped.length == 1 then pure 1 else
      match stepped[1]!.toNat? with
      | some n => if n == 0 || n > high + 1 then throw "invalid cron step" else pure n
      | none => throw "invalid cron step"
    let span := stepped[0]!
    let (first, last) ← if span == "*" then pure (low, high) else
      let range := span.splitOn "-"
      match range with
      | [one] =>
        let some value := one.toNat? | throw "invalid cron value"
        pure (value, if stepped.length > 1 then high else value)
      | [a, b] =>
        let some a := a.toNat? | throw "invalid cron range"
        let some b := b.toNat? | throw "invalid cron range"
        pure (a, b)
      | _ => throw "invalid cron range"
    if first < low || last > high || first > last then throw "cron value outside field range"
    for value in [first:last + 1] do
      if (value - first) % step == 0 then
        let value := if sunday && value == 7 then 0 else value
        if !values.contains value then values := values.push value
  if values.isEmpty then throw "empty cron field"
  return { values := values.qsort (· < ·), wildcard := source.startsWith "*" }

/-- Five-field UTC cron: minute hour day-of-month month day-of-week. Lists,
ranges and steps are checked; Sunday accepts 0 or 7. Restricted DOM/DOW use OR. -/
def parse (source : String) : Except String Schedule := do
  if source.utf8ByteSize > 1024 then throw "cron expression too large"
  let parts := (source.splitOn " ").filter (· != "")
  let [minute, hour, day, month, weekday] := parts | throw "cron requires exactly five fields"
  return {
    minute := ← field minute 0 59
    hour := ← field hour 0 23
    day := ← field day 1 31
    month := ← field month 1 12
    weekday := ← field weekday 0 7 true }

def Schedule.validate (schedule : Schedule) : Except String Unit := do
  for (component, low, high) in #[(schedule.minute, 0, 59), (schedule.hour, 0, 23),
      (schedule.day, 1, 31), (schedule.month, 1, 12), (schedule.weekday, 0, 6)] do
    if component.values.isEmpty || component.values.size > high - low + 1 ||
        component.values.any (fun value => value < low || value > high) then
      throw "invalid cron schedule field"

/-- Calendar matching uses UTC wall time; worker deadlines still use monotonic time. -/
def Schedule.matches (schedule : Schedule) (unixMs : Nat) : Bool :=
  let timestamp := Std.Time.Timestamp.ofMillisecondsSinceUnixEpoch ⟨Int.ofNat unixMs⟩
  let date := Std.Time.DateTime.ofTimestampWithZone timestamp Std.Time.TimeZone.UTC
  let dom := schedule.day.values.contains date.day.val.toNat
  let dow := schedule.weekday.values.contains (date.weekday.toOrdinal.val.toNat % 7)
  let dayMatches := if schedule.day.wildcard then dow else if schedule.weekday.wildcard then dom else dom || dow
  schedule.minute.values.contains date.minute.val.toNat && schedule.hour.values.contains date.hour.val.toNat &&
    schedule.month.values.contains date.month.val.toNat && dayMatches

structure Spec where
  name : String
  schedule : Schedule
  kind : String
  payload : Lean.Json
  metadata : Metadata := {}
  retry : RetryPolicy := {}
  timeoutMs : Nat := 30000
  deriving Inhabited

structure Limits where
  maxSchedules : Nat := 128
  maxEmissionsPerTick : Nat := 128
  /-- Older missed occurrences are skipped explicitly rather than replayed without bound. -/
  catchUpMinutes : Nat := 1

structure Tick where
  submitted : Nat := 0
  skipped : Nat := 0
  failed : Nat := 0
  backlogged : Bool := false
  deriving Repr

/-- Owned by one timer/job worker. Submit must be bounded and nonblocking when
installed on a reactor; a dedicated scheduler may drive its own PG AppM operations. -/
structure Scheduler where
  specs : Array Spec
  limits : Limits
  private lastMinute : IO.Ref (Array (Option Nat))
  private running : IO.Ref Bool
  private backlog : IO.Ref Bool
  private stopped : IO.Ref Bool
  submit : Envelope → IO (Except AppError String)

def Scheduler.create (specs : Array Spec) (registry : Registry)
    (submit : Envelope → IO (Except AppError String)) (limits : Limits := {}) : IO Scheduler := do
  if specs.size > limits.maxSchedules || limits.maxSchedules == 0 || limits.maxSchedules > 1024 ||
      limits.maxEmissionsPerTick == 0 || limits.maxEmissionsPerTick > 4096 ||
      limits.catchUpMinutes == 0 || limits.catchUpMinutes > 1440 then
    throw (IO.userError "invalid bounded scheduler limits")
  let mut names : Array String := #[]
  for spec in specs do
    if spec.name.isEmpty || spec.name.utf8ByteSize > 128 || names.contains spec.name then
      throw (IO.userError "cron schedules require unique bounded names")
    names := names.push spec.name
    match spec.schedule.validate with | .error e => throw (IO.userError e) | .ok () => pure ()
    let check : Envelope := {
      id := String.ofList (List.replicate 64 '0')
      kind := spec.kind
      payload := spec.payload
      metadata := spec.metadata
      retry := spec.retry
      timeoutMs := spec.timeoutMs }
    match check.validate registry with | .error e => throw (IO.userError e.message) | .ok _ => pure ()
  return ⟨specs, limits, ← IO.mkRef (Array.replicate specs.size none), ← IO.mkRef false, ← IO.mkRef false, ← IO.mkRef false, submit⟩

/-- Stop admitting cron occurrences on the timer owner during application drain. -/
def Scheduler.stop (scheduler : Scheduler) : IO Unit := scheduler.stopped.set true

/-- One bounded timer-wheel callback. Idempotency keys name UTC occurrences,
so distributed schedulers can safely compete through the durable queue. Failed
admission leaves that occurrence pending for a later tick. -/
def Scheduler.tick (scheduler : Scheduler) (unixMs : Nat) : IO Tick := do
  if ← scheduler.stopped.get then return {}
  if ← scheduler.running.get then return { backlogged := true }
  scheduler.running.set true
  try
    let current := unixMs / 60000
    let mut previous ← scheduler.lastMinute.get
    let mut result : Tick := {}
    let mut consumed := 0
    for index in [:scheduler.specs.size] do
      let spec := scheduler.specs[index]!
      let next := (previous[index]!.map (· + 1)).getD current
      let earliest := current + 1 - scheduler.limits.catchUpMinutes
      let start := max next earliest
      result := { result with skipped := result.skipped + (start - next) }
      let mut blocked := false
      for minute in [start:current + 1] do
        if !blocked then
          if consumed >= scheduler.limits.maxEmissionsPerTick then
            blocked := true
            result := { result with backlogged := true }
          else if spec.schedule.matches (minute * 60000) then
            consumed := consumed + 1
            let job : Envelope := {
              id := ← newId
              kind := spec.kind
              payload := spec.payload
              metadata := spec.metadata
              retry := spec.retry
              timeoutMs := spec.timeoutMs
              availableAtMs := minute * 60000
              idempotencyKey := s!"cron:{spec.name}:{minute}" }
            match ← scheduler.submit job with
            | .ok _ =>
              previous := previous.set! index (some minute)
              result := { result with submitted := result.submitted + 1 }
            | .error _ =>
              blocked := true
              result := { result with failed := result.failed + 1, backlogged := true }
          else previous := previous.set! index (some minute)
    scheduler.lastMinute.set previous
    scheduler.backlog.set result.backlogged
    return result
  finally scheduler.running.set false

/-- Next UTC minute boundary at which the timer wheel should call tick. If a
previous bounded tick still has backlog, retry at the next configured short pulse. -/
def Scheduler.nextDue (scheduler : Scheduler) (unixMs : Nat) : IO Nat := do
  return if ← scheduler.backlog.get then unixMs + 100 else (unixMs / 60000 + 1) * 60000

end LeanWeave.Jobs.Cron

open Lean Elab Term in
/-- Compile-time checked UTC cron schedule. -/
elab "cron!" source:str : term => do
  let schedule ← match LeanWeave.Jobs.Cron.parse source.getString with
    | .ok value => pure value | .error message => throwErrorAt source "{message}"
  let field := fun (value : LeanWeave.Jobs.Cron.Field) =>
    `(({ values := $(quote value.values), wildcard := $(quote value.wildcard) } : LeanWeave.Jobs.Cron.Field))
  let minute ← field schedule.minute
  let hour ← field schedule.hour
  let day ← field schedule.day
  let month ← field schedule.month
  let weekday ← field schedule.weekday
  elabTerm (← `(({ minute := $minute, hour := $hour, day := $day, month := $month, weekday := $weekday } : LeanWeave.Jobs.Cron.Schedule))) none
