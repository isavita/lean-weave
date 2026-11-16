import LeanWeave.Jobs.Types

namespace LeanWeave.Workflow

structure Action where
  kind : String
  payload : Lean.Json
  deriving Inhabited, Lean.ToJson, Lean.FromJson

structure Step where
  name : String
  action : Action
  dependsOn : Array String := #[]
  compensation : Option Action := none
  timeoutMs : Nat := 30000
  maxAttempts : Nat := 5
  deriving Inhabited, Lean.ToJson, Lean.FromJson

structure Plan where
  name : String
  steps : Array Step
  deriving Inhabited, Lean.ToJson, Lean.FromJson

inductive Stage where
  | waiting | ready | queued | succeeded | failed | cancelled
  | compensateReady | compensateQueued | compensated | compensationFailed
  deriving Repr, BEq, Inhabited, Lean.ToJson, Lean.FromJson

inductive Status where
  | running | compensating | succeeded | failed | cancelled | compensationFailed
  deriving Repr, BEq, Inhabited, Lean.ToJson, Lean.FromJson

structure StepState where
  step : Step
  jobId : String
  compensationId : String
  stage : Stage := .waiting
  /-- A stable topological index determines reverse compensation order. -/
  order : Nat
  deriving Inhabited, Lean.ToJson, Lean.FromJson

structure Instance where
  id : String
  name : String
  tenant : String
  requestId : String := ""
  traceparent : String := ""
  steps : Array StepState
  status : Status := .running
  cancelRequested : Bool := false
  revision : Nat := 1
  deriving Inhabited, Lean.ToJson, Lean.FromJson

private def validName (value : String) : Bool :=
  !value.isEmpty && value.utf8ByteSize <= 128 && value.toUTF8.data.all (fun b =>
    (b >= 65 && b <= 90) || (b >= 97 && b <= 122) || (b >= 48 && b <= 57) || [45,46,95].contains b.toNat)

/-- Kahn sorting rejects cycles, missing dependencies and duplicate step names.
A plan has at most 128 steps, 4096 edges and one MiB of serialized definitions. -/
def Plan.validate (plan : Plan) (registry : Jobs.Registry) : Except String (Array Step) := do
  if !validName plan.name || plan.steps.isEmpty || plan.steps.size > 128 ||
      (Lean.toJson plan).compress.utf8ByteSize > 1024 * 1024 then throw "invalid bounded workflow plan"
  let mut names : Array String := #[]
  let mut edges := 0
  for step in plan.steps do
    if !validName step.name || names.contains step.name || step.timeoutMs == 0 || step.timeoutMs > 86400000 ||
        step.maxAttempts == 0 || step.maxAttempts > 32 then throw "invalid or duplicate workflow step"
    names := names.push step.name
    edges := edges + step.dependsOn.size
    if edges > 4096 || step.dependsOn.contains step.name || step.dependsOn.toList.eraseDups.length != step.dependsOn.size then
      throw "invalid workflow dependency list"
    for action in #[some step.action, step.compensation] do
      if let some action := action then
        let some definition := registry.find action.kind | throw "unknown workflow job kind"
        let _ ← definition.validate action.payload
  for step in plan.steps do
    unless step.dependsOn.all names.contains do throw "missing workflow dependency"
  let mut ordered : Array Step := #[]
  let mut completed : Array String := #[]
  for _ in [:plan.steps.size] do
    if let some step := plan.steps.find? (fun step => !completed.contains step.name && step.dependsOn.all completed.contains) then
      ordered := ordered.push step
      completed := completed.push step.name
    else throw "workflow dependency cycle"
  return ordered

/-- Allocate stable forward/compensation job IDs before persisting the flow.
The same IDs and occurrence keys survive retries, reconciliation and process crashes. -/
def create (plan : Plan) (registry : Jobs.Registry) (metadata : Jobs.Metadata := {}) : IO (Except String Instance) := do
  let steps ← match plan.validate registry with
    | .ok steps => pure steps | .error error => return .error error
  let mut states := #[]
  for i in [:steps.size] do
    states := states.push { step := steps[i]!, jobId := ← Jobs.newId, compensationId := ← Jobs.newId, order := i }
  return .ok {
    id := ← Jobs.newId
    name := plan.name
    tenant := metadata.tenant
    requestId := metadata.requestId
    traceparent := metadata.traceparent
    steps := states }

private def completeForward (state : StepState) : Bool :=
  [.succeeded, .failed, .cancelled, .compensateReady, .compensateQueued, .compensated, .compensationFailed].contains state.stage

/-- Pure state transition. Compensation waits for all forward jobs to settle,
then runs one step at a time in reverse topological order. Failed steps must undo
partial effects themselves; declared compensations undo acknowledged successful steps. -/
def Instance.advance (flow : Instance) : Instance := Id.run do
  let mut flow := flow
  if flow.status == .running then
    let successful := (flow.steps.filter (fun s => s.stage == .succeeded)).map (·.step.name)
    let steps := flow.steps.map fun s =>
      if s.stage == .waiting && s.step.dependsOn.all successful.contains then { s with stage := .ready } else s
    flow := { flow with steps }
    if steps.all (fun s => s.stage == .succeeded) then return { flow with status := .succeeded }
  else if flow.status == .compensating then
    let steps := flow.steps.map fun s => if s.stage == .waiting || s.stage == .ready then { s with stage := .cancelled } else s
    flow := { flow with steps }
    if steps.any (fun s => s.stage == .compensationFailed) then return { flow with status := .compensationFailed }
    if steps.all completeForward && !steps.any (fun s => s.stage == .compensateReady || s.stage == .compensateQueued) then
      let remaining := (steps.filter (fun s => s.stage == .succeeded && s.step.compensation.isSome)).qsort (fun a b => a.order > b.order)
      if let some next := remaining[0]? then
        flow := { flow with steps := steps.map fun s => if s.step.name == next.step.name then { s with stage := .compensateReady } else s }
      else return { flow with status := if flow.cancelRequested then .cancelled else .failed }
  return flow

/-- Pending publications are explicit persisted states, not transient callbacks. -/
def Instance.actions (flow : Instance) : Array (String × Jobs.Envelope) :=
  flow.steps.filterMap fun state =>
    let action := if state.stage == .ready then some (state.jobId, state.step.action, false)
      else if state.stage == .compensateReady then state.step.compensation.map fun action => (state.compensationId, action, true)
      else none
    action.map fun (id, action, compensation) =>
      (state.step.name, {
        id
        kind := action.kind
        payload := action.payload
        metadata := { tenant := flow.tenant, requestId := flow.requestId, traceparent := flow.traceparent }
        timeoutMs := state.step.timeoutMs
        retry := { maxAttempts := state.step.maxAttempts }
        idempotencyKey := s!"workflow:{flow.id}:{state.step.name}:{compensation}" })

def Instance.submitted (flow : Instance) (jobId : String) : Instance :=
  { flow with steps := flow.steps.map fun s =>
    if s.jobId == jobId && s.stage == .ready then { s with stage := .queued }
    else if s.compensationId == jobId && s.stage == .compensateReady then { s with stage := .compensateQueued }
    else s }

/-- Only terminal queue outcomes affect workflow transitions. A retry keeps the
step queued; duplicate delivery/ack is idempotent. -/
def Instance.finish (flow : Instance) (jobId : String) (success : Bool) (cancelled : Bool := false) : Instance := Id.run do
  let mut failed := false
  let steps := flow.steps.map fun s =>
    if s.jobId == jobId && (s.stage == .ready || s.stage == .queued) then
      if success then { s with stage := .succeeded }
      else { s with stage := if cancelled then .cancelled else .failed }
    else if s.compensationId == jobId && (s.stage == .compensateReady || s.stage == .compensateQueued) then
      { s with stage := if success then .compensated else .compensationFailed }
    else s
  for s in steps do
    if s.stage == .failed || (s.stage == .cancelled && flow.status == .running) then failed := true
  let status := if flow.status == .running && failed then Status.compensating else flow.status
  return ({ flow with steps, status, revision := flow.revision + 1 }).advance

def Instance.cancel (flow : Instance) : Instance :=
  if flow.status != .running then flow else
    ({ flow with status := .compensating, cancelRequested := true, revision := flow.revision + 1 }).advance

def Instance.jobsToCancel (flow : Instance) : Array String :=
  if flow.status != .compensating then #[] else
    flow.steps.filterMap fun s => if s.stage == .queued then some s.jobId else none

end LeanWeave.Workflow
