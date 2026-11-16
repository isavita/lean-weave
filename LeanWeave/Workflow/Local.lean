import LeanWeave.Workflow.Types
import LeanWeave.Jobs.Local

namespace LeanWeave.Workflow

structure Limits where
  maxInstances : Nat := 128
  maxBytes : Nat := 8 * 1024 * 1024
  maxActionsPerTick : Nat := 128
  deriving Repr

private structure StoreState where
  flows : Array Instance := #[]
  bytes : Nat := 0

structure Engine where
  private mk ::
  registry : Jobs.Registry
  limits : Limits
  private state : Std.Mutex StoreState
  admission : Jobs.Admission

private def bytes (flow : Instance) : Nat := (Lean.toJson flow).compress.utf8ByteSize + 256
private def terminal (status : Status) : Bool := [.succeeded, .failed, .cancelled, .compensationFailed].contains status

private def atomic (engine : Engine) (f : StoreState → Except AppError (α × StoreState)) : IO (Except AppError α) := do
  let operation : IO (Option (Except AppError α)) := engine.state.tryAtomically do
    let state ← get
    match f state with
    | .error error => return .error error
    | .ok (value, next) => set next; return .ok value
  return (← operation).getD (.error { status := 503, message := "workflow store busy" })

private def lift (result : Except AppError α) : AppM α :=
  match result with | .ok value => pure value | .error error => throw error

private def replace (state : StoreState) (flow : Instance) : StoreState :=
  let flows := state.flows.map fun old => if old.id == flow.id then flow else old
  { flows, bytes := flows.foldl (fun n flow => n + bytes flow) 0 }

def Engine.create (registry : Jobs.Registry) (limits : Limits := {})
    (admission : Option Jobs.Admission := none) : IO Engine := do
  if limits.maxInstances == 0 || limits.maxInstances > 10000 || limits.maxBytes < 1024 ||
      limits.maxActionsPerTick == 0 || limits.maxActionsPerTick > 4096 then
    throw (IO.userError "invalid workflow engine bounds")
  return ⟨registry, limits, ← Std.Mutex.new {}, ← match admission with | some a => pure a | none => Jobs.Admission.new⟩

/-- Persist before publishing. Ready actions remain explicit until admitted to
the bounded job queue, so transient queue saturation cannot drop a workflow step. -/
def Engine.start (engine : Engine) (plan : Plan) (metadata : Jobs.Metadata := {}) : IO (Except AppError String) := do
  unless ← engine.admission.isOpen do return .error { status := 503, message := "workflow admission stopped" }
  let flow ← match ← Workflow.create plan engine.registry metadata with
    | .ok flow => pure flow | .error error => return .error { status := 422, message := error }
  let flow := flow.advance
  atomic engine fun state => do
    if state.flows.size >= engine.limits.maxInstances || state.bytes + bytes flow > engine.limits.maxBytes then
      throw { status := 503, message := "workflow capacity reached" }
    return (flow.id, { flows := state.flows.push flow, bytes := state.bytes + bytes flow })

def Engine.get (engine : Engine) (tenant id : String) : IO (Except AppError (Option Instance)) :=
  atomic engine fun state => .ok (state.flows.find? (fun flow => flow.id == id && flow.tenant == tenant), state)

/-- Cancellation enters the compensation state machine rather than deleting rows. -/
def Engine.cancel (engine : Engine) (tenant id : String) : IO (Except AppError Bool) :=
  atomic engine fun state => do
    let some flow := state.flows.find? (fun flow => flow.id == id && flow.tenant == tenant) | return (false, state)
    if terminal flow.status then return (false, state)
    return (true, replace state flow.cancel)

/-- Explicit terminal-history removal is tenant scoped. Pending work is never evicted. -/
def Engine.remove (engine : Engine) (tenant id : String) : IO (Except AppError Bool) :=
  atomic engine fun state =>
    let flows := state.flows.filter (fun flow => !(flow.id == id && flow.tenant == tenant && terminal flow.status))
    .ok (flows.size != state.flows.size, { flows, bytes := flows.foldl (fun n f => n + bytes f) 0 })

/-- Retry a failed compensation explicitly, preserving its idempotency key and
stable job ID. The queue keeps the dead letter until this operator action. -/
def Engine.retryCompensation (engine : Engine) (queue : Jobs.Queue) (tenant id : String) : IO (Except AppError Bool) := do
  -- Use the same engine→queue lock order as publication. A finishing worker must
  -- not acknowledge the retried job before its workflow stage is committed.
  let operation : IO (Option (Except AppError Bool)) := engine.state.tryAtomically do
    let state ← getThe StoreState
    let some flow := state.flows.find? (fun f => f.id == id && f.tenant == tenant) | return .ok false
    if flow.status != .compensationFailed then return .ok false
    let failed := flow.steps.filter (fun s => s.stage == .compensationFailed)
    for step in failed do
      match ← queue.retryDead step.compensationId with
      | .error e => return .error e
      | .ok false => return .ok false
      | .ok true => pure ()
    let flow := { flow with
      status := Status.compensating
      revision := flow.revision + 1
      steps := flow.steps.map fun step => if step.stage == .compensationFailed then { step with stage := .compensateQueued } else step }
    set (replace state flow)
    return .ok true
  return (← operation).getD (.error { status := 503,message := "workflow store busy" })

private def complete (engine : Engine) (jobId : String) (outcome : Jobs.State) : IO Unit :=
  engine.state.atomically do
    let state ← get
    if let some flow := state.flows.find? (fun flow => flow.steps.any (fun s => s.jobId == jobId || s.compensationId == jobId)) then
      if [.succeeded, .dead, .cancelled].contains outcome then
        set (replace state (flow.finish jobId (outcome == .succeeded) (outcome == .cancelled)))

/-- Bounded idempotent reconciliation can be driven by native onTick or by the
job backend. Each action has a preallocated stable job ID and idempotency key. -/
def Engine.tick (engine : Engine) (queue : Jobs.Queue) : IO (Except AppError Nat) := do
  let flows ← match ← atomic engine (fun state => .ok (state.flows, state)) with
    | .ok flows => pure flows | .error error => return .error error
  let mut count := 0
  for flow in flows do
    for id in flow.jobsToCancel do
      let result ← queue.cancel id
      if let .error error := result then return .error error
      if let .ok (some record) ← queue.get id then complete engine id record.state
    for (_, job) in flow.actions do
      if count < engine.limits.maxActionsPerTick then
        let result ← engine.state.atomically do
          let state ← getThe StoreState
          let some current := state.flows.find? (·.id == flow.id) | return Except.ok ()
          unless current.actions.any (fun (_, action) => action.id == job.id) do return Except.ok ()
          match ← queue.continueWorkflow job with
          | .error error => return .error error
          | .ok _ => set (replace state (current.submitted job.id)); return .ok ()
        if let .error error := result then return .error error
        count := count + 1
  return .ok count

/-- Workflow completion is recorded synchronously by the job owner after a fenced
queue acknowledgement. Ready publications are retried before each next claim. -/
def Engine.backend (engine : Engine) (queue : Jobs.Queue) : Jobs.Backend := {
  queue.backend with
  claim := fun owner => do
    if let .error error ← engine.tick queue then
      unless error.status == 503 do throw error
    queue.backend.claim owner
  settle := fun lease outcome => do
    let committed ← queue.settle lease outcome
    if let some record := committed then complete engine lease.job.id record.state
    return committed.isSome
  busy := do
    if let .error error ← engine.tick queue then
      unless error.status == 503 do throw error
    let flows ← lift (← atomic engine fun state => .ok (state.flows, state))
    return flows.any (fun flow => !terminal flow.status) || (← queue.backend.busy) }

end LeanWeave.Workflow
