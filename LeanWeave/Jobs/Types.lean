import LeanWeave.Core
import LeanWeave.Session.Store
import Std.Sync.Mutex

namespace LeanWeave.Jobs

/-- Only owned correlation data crosses into a background worker. Never capture a
request Context, native socket, borrowed buffer or reactor-owned database pool. -/
structure Metadata where
  tenant : String := ""
  requestId : String := ""
  traceparent : String := ""
  deriving Repr, BEq, Inhabited

structure RetryPolicy where
  maxAttempts : Nat := 5
  initialDelayMs : Nat := 1000
  maxDelayMs : Nat := 60000
  /-- Stable bounded jitter avoids synchronized retry waves and is reproducible. -/
  jitter : Bool := true
  deriving Repr, BEq, Inhabited

def RetryPolicy.validate (policy : RetryPolicy) : Except String Unit := do
  if policy.maxAttempts == 0 || policy.maxAttempts > 32 || policy.initialDelayMs == 0 ||
      policy.maxDelayMs < policy.initialDelayMs || policy.maxDelayMs > 86400000 then
    throw "invalid bounded job retry policy"

def RetryPolicy.delay (policy : RetryPolicy) (attempt : Nat) (jobId : String) : Nat :=
  let base := min policy.maxDelayMs (policy.initialDelayMs * 2 ^ (min 31 (attempt - 1)))
  if !policy.jitter then base else
    let hash := jobId.toUTF8.foldl (fun (h : UInt64) b => h * 1099511628211 + b.toUInt64) 14695981039346656037
    base / 2 + hash.toNat % (base - base / 2 + 1)

/-- Due times are Unix milliseconds, allowing the same envelope to be persisted
and delivered across processes. Deadlines use a monotonic clock during execution. -/
structure Envelope where
  id : String
  kind : String
  payload : Lean.Json
  metadata : Metadata := {}
  idempotencyKey : String := ""
  availableAtMs : Nat := 0
  timeoutMs : Nat := 30000
  retry : RetryPolicy := {}
  deriving Inhabited

def wallMs : IO Nat := do
  let now ← Std.Time.Timestamp.now
  return now.toMillisecondsSinceUnixEpoch.val.toNat

def newId : IO String := Session.randomToken

inductive State where
  | queued | running | retrying | succeeded | dead | cancelRequested | cancelled | leaseLost
  deriving Repr, BEq, Inhabited

inductive Outcome where
  | success
  | failed (status : Nat) (code : String) (retryable : Bool)
  | cancelled
  | leaseLost
  deriving Repr, BEq, Inhabited

/-- Events never contain payloads, SQL parameters, credentials or raw exception messages. -/
structure Event where
  jobId : String
  kind : String
  tenant : String
  requestId : String
  traceparent : String
  state : State
  attempt : Nat := 0
  durationMs : Nat := 0
  errorStatus : Option Nat := none
  errorCode : Option String := none
  deriving Repr, Inhabited

structure Span where
  /-- Child traceparent returned at start; handlers propagate it to further work. -/
  traceparent : String
  finish : Event → IO Unit := fun _ => pure ()

structure Hooks where
  emit : Event → IO Unit := fun _ => pure ()
  /-- Invoked before execution, not synthesized after completion. All callbacks
  run on the job owner and must be bounded, nonblocking and exception-safe. -/
  startSpan : Event → IO Span := fun event => pure { traceparent := event.traceparent }

def event (job : Envelope) (state : State) (attempt : Nat := 0) : Event := {
  jobId := job.id, kind := job.kind, tenant := job.metadata.tenant,
  requestId := job.metadata.requestId, traceparent := job.metadata.traceparent, state, attempt }

structure ExecutionContext where
  jobId : String
  kind : String
  metadata : Metadata
  attempt : Nat
  cancelled : IO Bool

abbrev Handler := ExecutionContext → Lean.Json → AppM Unit

structure Definition where
  kind : String
  validate : Lean.Json → Except String Lean.Json
  run : Handler

/-- Decode, validate and canonicalize a typed payload at enqueue and again at
execution. Validation failures are permanent failures rather than retry loops. -/
def Definition.typed [Lean.FromJson α] [Lean.ToJson α] (kind : String)
    (validate : α → Except String α) (run : ExecutionContext → α → AppM Unit) : Definition := {
  kind
  validate := fun json => do return Lean.toJson (← validate (← Lean.fromJson? json))
  run := fun context json => do
    match Lean.fromJson? json >>= validate with
    | .ok payload => run context payload
    | .error _ => throw { status := 422, message := "invalid job payload" } }

structure Registry where
  definitions : Array Definition

def Registry.create (definitions : Array Definition) : Except String Registry := do
  if definitions.isEmpty || definitions.size > 256 then throw "job registry must contain 1 to 256 kinds"
  let mut names : Array String := #[]
  for definition in definitions do
    if definition.kind.isEmpty || definition.kind.utf8ByteSize > 128 ||
        !definition.kind.toUTF8.data.all (fun b => (b >= 65 && b <= 90) || (b >= 97 && b <= 122) ||
          (b >= 48 && b <= 57) || [45, 46, 95].contains b.toNat) then throw "invalid job kind"
    if names.contains definition.kind then throw "duplicate job kind"
    names := names.push definition.kind
  return { definitions }

def Registry.find (registry : Registry) (kind : String) : Option Definition :=
  registry.definitions.find? (·.kind == kind)

structure Limits where
  maxEntries : Nat := 1024
  maxBytes : Nat := 8 * 1024 * 1024
  maxPayloadBytes : Nat := 65536
  maxHistory : Nat := 128
  idempotencyMs : Nat := 86400000
  deriving Repr

def Envelope.bytes (job : Envelope) : Nat :=
  job.payload.compress.utf8ByteSize + job.id.utf8ByteSize + job.kind.utf8ByteSize +
    job.metadata.tenant.utf8ByteSize + job.metadata.requestId.utf8ByteSize +
    job.metadata.traceparent.utf8ByteSize + job.idempotencyKey.utf8ByteSize + 256

def Envelope.validate (job : Envelope) (registry : Registry) (maxPayloadBytes : Nat := 65536) : Except AppError Envelope := do
  let fail := fun message => AppError.mk 422 message
  job.retry.validate.mapError fail
  unless Session.validToken job.id do throw (fail "invalid job ID")
  if job.timeoutMs == 0 || job.timeoutMs > 86400000 || job.availableAtMs > 253402300799000 then
    throw (fail "invalid job deadline or scheduled timestamp")
  if job.metadata.tenant.utf8ByteSize > 256 || job.metadata.requestId.utf8ByteSize > 256 ||
      job.metadata.traceparent.utf8ByteSize > 256 || job.idempotencyKey.utf8ByteSize > 256 then
    throw (fail "job metadata exceeds its bound")
  for value in #[job.id, job.kind, job.metadata.tenant, job.metadata.requestId, job.metadata.traceparent, job.idempotencyKey] do
    if value.toUTF8.data.contains 0 then throw (fail "job metadata contains NUL")
  if job.payload.compress.utf8ByteSize > maxPayloadBytes then throw { status := 413, message := "job payload too large" }
  let some definition := registry.find job.kind | throw (fail "unknown job kind")
  let payload ← (definition.validate job.payload).mapError (fun _ => fail "invalid typed job payload")
  if payload.compress.utf8ByteSize > maxPayloadBytes then throw { status := 413, message := "canonical job payload too large" }
  return { job with payload }

structure Lease where
  job : Envelope
  attempt : Nat
  owner : String
  token : String
  deriving Inhabited

inductive LeaseStatus where
  | active | cancelled | lost
  deriving BEq, Repr

/-- Backend objects belong to one dedicated job worker. Their database handles
must be created and polled there. Shared queue state uses a bounded mutex only. -/
structure Backend where
  leaseMs : Nat := 30000
  maxPayloadBytes : Nat := 65536
  claim : String → AppM (Option Lease)
  renew : Lease → AppM LeaseStatus
  settle : Lease → Outcome → AppM Bool
  /-- True for runnable or currently leased work; scheduled future jobs remain
  durable rather than extending process shutdown until their due date. -/
  busy : AppM Bool
  /-- Bounded nonblocking owner maintenance, e.g. a span exporter pulse. -/
  tick : IO Unit := pure ()
  close : IO Unit := pure ()

structure WorkerResources where
  backend : Backend
  registry : Registry
  hooks : Hooks := {}

structure Snapshot where
  queued : Nat := 0
  active : Nat := 0
  succeeded : Nat := 0
  dead : Nat := 0
  cancelled : Nat := 0
  bytes : Nat := 0
  admitting : Bool := true
  deriving Repr, Inhabited

/-- Enqueue admission can be shared by request and job producers. Closing this
node's admission does not stop other distributed nodes. Reads never block a reactor. -/
structure Admission where
  state : Std.Mutex Bool
  private stopped : IO.Promise Unit

def Admission.new : IO Admission := return ⟨← Std.Mutex.new true, ← IO.Promise.new⟩

def Admission.isOpen (admission : Admission) : IO Bool := do
  if ← admission.stopped.isResolved then return false
  return (← admission.state.tryAtomically get).getD false

def Admission.stop (admission : Admission) : IO Unit := do
  admission.stopped.resolve ()
  let _ ← admission.state.tryAtomically (set false)
  pure ()

end LeanWeave.Jobs
