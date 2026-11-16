import LeanWeave.Jobs.Types

namespace LeanWeave.Jobs

structure Record where
  job : Envelope
  state : State := .queued
  attempt : Nat := 0
  owner : String := ""
  token : String := ""
  leaseUntilMs : Nat := 0
  updatedAtMs : Nat := 0
  errorStatus : Option Nat := none
  errorCode : Option String := none
  deriving Inhabited

private def terminal (state : State) : Bool :=
  [.succeeded, .dead, .cancelled].contains state

private structure LocalState where
  records : Array Record := #[]
  bytes : Nat := 0
  claims : Nat := 0

/-- Bounded process-local queue. Idempotency is retained for at most maxHistory
terminal records and idempotencyMs; durable idempotency belongs to the DB backend. -/
structure Queue where
  private mk ::
  limits : Limits
  registry : Registry
  admission : Admission
  leaseMs : Nat
  private state : Std.Mutex LocalState
  hooks : Hooks

private def recount (records : Array Record) : Nat := records.foldl (fun n r => n + r.job.bytes) 0

private def prune (state : LocalState) (limits : Limits) (now : Nat) : LocalState := Id.run do
  let live := state.records.filter (fun r => !terminal r.state || now - r.updatedAtMs < limits.idempotencyMs)
  let history := live.filter (fun r => terminal r.state)
  let remove := history.size - limits.maxHistory
  let expiredIds := (history.qsort (fun a b => a.updatedAtMs < b.updatedAtMs)).extract 0 remove |>.map (·.job.id)
  let records := live.filter (fun r => !expiredIds.contains r.job.id)
  return { state with records, bytes := recount records }

private def atomic (queue : Queue) (f : LocalState → Except AppError (α × LocalState)) : IO (Except AppError α) := do
  let operation : IO (Option (Except AppError α)) := queue.state.tryAtomically do
    let state ← get
    match f state with
    | .error error => return .error error
    | .ok (value, next) => set next; return .ok value
  return (← operation).getD (.error { status := 503, message := "job queue busy" })

private def lift (result : Except AppError α) : AppM α :=
  match result with | .ok value => pure value | .error error => throw error

private def emit (hooks : Hooks) (value : Event) : IO Unit :=
  try hooks.emit value catch _ => pure ()

def Queue.create (registry : Registry) (limits : Limits := {}) (hooks : Hooks := {})
    (leaseMs : Nat := 30000) (admission : Option Admission := none) : IO Queue := do
  if limits.maxEntries == 0 || limits.maxEntries > 100000 || limits.maxHistory > limits.maxEntries ||
      limits.maxBytes < 1024 || limits.maxPayloadBytes == 0 || limits.maxPayloadBytes > limits.maxBytes ||
      limits.idempotencyMs == 0 || leaseMs < 100 || leaseMs > 3600000 then
    throw (IO.userError "invalid local queue limits")
  let admission ← match admission with | some value => pure value | none => Admission.new
  return ⟨limits, registry, admission, leaseMs, ← Std.Mutex.new {}, hooks⟩

/-- Admission and mutex acquisition are fail-fast. Canonical typed payloads and
idempotency key conflicts are checked before retaining bytes. -/
private def enqueueInternal (queue : Queue) (job : Envelope) (continuation : Bool) : IO (Except AppError String) := do
  unless continuation || (← queue.admission.isOpen) do return .error { status := 503, message := "job admission stopped" }
  let job ← match job.validate queue.registry queue.limits.maxPayloadBytes with
    | .ok job => pure job | .error error => return .error error
  let now ← wallMs
  let result ← atomic queue fun original => do
    let mut state := prune original queue.limits now
    if let some existing := state.records.find? (fun r => r.job.id == job.id ||
        (!job.idempotencyKey.isEmpty && r.job.metadata.tenant == job.metadata.tenant && r.job.idempotencyKey == job.idempotencyKey)) then
      if existing.job.kind != job.kind || existing.job.payload != job.payload || existing.job.metadata.tenant != job.metadata.tenant then
        throw { status := 409, message := "idempotency key reused with a different payload" }
      return (existing.job.id, state)
    for _ in [:state.records.size] do
      if state.records.size >= queue.limits.maxEntries || state.bytes + job.bytes > queue.limits.maxBytes then
        let oldest := (state.records.filter (fun r => terminal r.state)).qsort (fun a b => a.updatedAtMs < b.updatedAtMs)
        if let some record := oldest[0]? then
          let records := state.records.filter (fun r => r.job.id != record.job.id)
          state := { state with records,bytes := recount records }
    if state.records.size >= queue.limits.maxEntries || state.bytes + job.bytes > queue.limits.maxBytes then
      throw { status := 503, message := "job queue capacity reached" }
    let record : Record := { job, updatedAtMs := now }
    return (job.id, { state with records := state.records.push record, bytes := state.bytes + job.bytes })
  if let .ok id := result then
    if id == job.id then emit queue.hooks (event job .queued)
  return result

/-- New external work obeys the node admission gate. -/
def Queue.enqueue (queue : Queue) (job : Envelope) : IO (Except AppError String) := enqueueInternal queue job false

/-- Continue an already admitted workflow while its node drains. Only workflow
occurrence keys are accepted; this is an internal platform producer capability. -/
def Queue.continueWorkflow (queue : Queue) (job : Envelope) : IO (Except AppError String) := do
  unless job.idempotencyKey.startsWith "workflow:" do
    return .error { status := 422, message := "workflow continuation key required" }
  enqueueInternal queue job true

/-- Queued jobs become cancelled immediately; running jobs receive a cooperative
cancellation request checked by the worker and by every lease renewal. -/
def Queue.cancel (queue : Queue) (id : String) : IO (Except AppError Bool) := do
  let now ← wallMs
  atomic queue fun state => do
    let some record := state.records.find? (·.job.id == id) | return (false, state)
    if terminal record.state then return (false, state)
    let nextState := if record.state == .running || record.state == .cancelRequested then State.cancelRequested else State.cancelled
    let records := state.records.map fun r => if r.job.id == id then { r with state := nextState, updatedAtMs := now } else r
    return (true, { state with records })

def Queue.get (queue : Queue) (id : String) : IO (Except AppError (Option Record)) :=
  atomic queue fun state => .ok (state.records.find? (·.job.id == id), state)

def Queue.snapshot (queue : Queue) : IO (Except AppError Snapshot) := do
  let admitting ← queue.admission.isOpen
  atomic queue fun state =>
    let count := fun p => (state.records.filter p).size
    .ok (({
      queued := count (fun r => r.state == .queued || r.state == .retrying)
      active := count (fun r => r.state == .running || r.state == .cancelRequested)
      succeeded := count (fun r => r.state == .succeeded)
      dead := count (fun r => r.state == .dead)
      cancelled := count (fun r => r.state == .cancelled)
      bytes := state.bytes
      admitting } : Snapshot), state)

/-- Explicit dead-letter replay preserves the stable ID but resets its retry budget. -/
def Queue.retryDead (queue : Queue) (id : String) : IO (Except AppError Bool) := do
  unless ← queue.admission.isOpen do return .error { status := 503, message := "job admission stopped" }
  let now ← wallMs
  atomic queue fun state => do
    let some record := state.records.find? (fun r => r.job.id == id && r.state == .dead) | return (false, state)
    let replacement := { record with
      state := State.queued
      attempt := 0
      updatedAtMs := now
      errorStatus := none
      errorCode := none
      job := { record.job with availableAtMs := now } }
    return (true, { state with records := state.records.map fun r => if r.job.id == id then replacement else r })

/-- Export bounded owned pending records for an explicit durable shutdown handoff.
The caller must not report a clean in-memory drain until these are persisted. -/
def Queue.pending (queue : Queue) : IO (Except AppError (Array Envelope)) :=
  atomic queue fun state => .ok ((state.records.filter (fun r => !terminal r.state)).map (·.job), state)

private def claim (queue : Queue) (owner : String) : AppM (Option Lease) := do
  let now ← wallMs
  lift (← atomic queue fun state => do
    let records := state.records.map fun r =>
      if (r.state == .running || r.state == .cancelRequested) && r.leaseUntilMs <= now then
        if r.state == .cancelRequested then { r with state := .cancelled, updatedAtMs := now }
        else if r.attempt >= r.job.retry.maxAttempts then { r with state := .dead, updatedAtMs := now, errorCode := some "lease_expired" }
        else { r with state := .retrying, owner := "", token := "", leaseUntilMs := 0 }
      else r
    let available := (records.filter fun r => (r.state == .queued || r.state == .retrying) &&
      r.job.availableAtMs <= now).qsort (fun a b => a.job.availableAtMs < b.job.availableAtMs)
    let some record := available[0]? | return (none, { state with records })
    let attempt := record.attempt + 1
    let generation := state.claims + 1
    let token := s!"{owner}:{generation}"
    let records := records.map fun r => if r.job.id == record.job.id then
      { r with state := .running, attempt, owner, token, leaseUntilMs := now + queue.leaseMs, updatedAtMs := now } else r
    return (some { job := record.job, attempt, owner, token }, { state with records, claims := generation }))

private def owns (record : Record) (lease : Lease) : Bool :=
  record.job.id == lease.job.id && record.owner == lease.owner && record.token == lease.token &&
    (record.state == .running || record.state == .cancelRequested)

private def renew (queue : Queue) (lease : Lease) : AppM LeaseStatus := do
  let now ← wallMs
  lift (← atomic queue fun state => do
    let some record := state.records.find? (fun r => owns r lease) | return (.lost, state)
    if record.leaseUntilMs <= now then return (.lost, state)
    if record.state == .cancelRequested then return (.cancelled, state)
    let records := state.records.map (fun r => if owns r lease then { r with leaseUntilMs := now + queue.leaseMs } else r)
    return (.active, { state with records }))

/-- Atomic settlement returns its committed record even when terminal history is
immediately evicted. Workflow transitions must not race a later history lookup. -/
def Queue.settle (queue : Queue) (lease : Lease) (outcome : Outcome) : AppM (Option Record) := do
  let now ← wallMs
  let result ← lift (← atomic queue fun state => do
    let some record := state.records.find? (fun r => owns r lease) | return (none, state)
    if record.leaseUntilMs <= now then return (none, state)
    let (nextState, nextDue, status, code) : State × Nat × Option Nat × Option String := if record.state == .cancelRequested then
        (State.cancelled, record.job.availableAtMs, none, none)
      else match outcome with
      | .success => (.succeeded, record.job.availableAtMs, none, none)
      | .cancelled => (.cancelled, record.job.availableAtMs, none, none)
      | .leaseLost => (.retrying, now, none, some "lease_lost")
      | .failed status code retryable =>
        if retryable && record.attempt < record.job.retry.maxAttempts then
          (.retrying, now + record.job.retry.delay record.attempt record.job.id, some status, some code)
        else (.dead, record.job.availableAtMs, some status, some code)
    let replacement := { record with
      state := nextState
      owner := ""
      token := ""
      leaseUntilMs := 0
      updatedAtMs := now
      errorStatus := status
      errorCode := code
      attempt := if outcome == .leaseLost then record.attempt - 1 else record.attempt
      job := { record.job with availableAtMs := nextDue } }
    let records := state.records.map fun r => if r.job.id == record.job.id then replacement else r
    return (some replacement, prune { state with records } queue.limits now))
  if let some record := result then
    emit queue.hooks { event record.job record.state record.attempt with errorStatus := record.errorStatus, errorCode := record.errorCode }
    return some record
  return none

/-- The adapter shares only the queue's bounded mutex; application executions and
any DB connections they create remain on the dedicated worker. -/
def Queue.backend (queue : Queue) : Backend := {
  leaseMs := queue.leaseMs
  maxPayloadBytes := queue.limits.maxPayloadBytes
  claim := claim queue
  renew := renew queue
  settle := fun lease outcome => do return (← queue.settle lease outcome).isSome
  busy := do
    let snapshot ← lift (← queue.snapshot)
    return snapshot.queued > 0 || snapshot.active > 0 }

end LeanWeave.Jobs
