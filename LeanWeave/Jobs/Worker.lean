import LeanWeave.Jobs.Types

namespace LeanWeave.Jobs

structure WorkerConfig where
  workers : Nat := 1
  pollMs : Nat := 10
  heartbeatMs : Nat := 1000
  operationTimeoutMs : Nat := 30000
  maxRetainedBytes : Nat := 4 * 1024 * 1024
  deriving Repr

structure WorkerSnapshot where
  workers : Nat := 0
  finished : Nat := 0
  ready : Nat := 0
  workerErrors : Nat := 0
  active : Nat := 0
  completed : Nat := 0
  failures : Nat := 0
  admitting : Bool := true
  draining : Bool := false
  forced : Bool := false
  deriving Repr, Inhabited

private structure Signals where
  draining : IO.Promise Nat
  force : IO.Promise Unit

private def Signals.create : IO Signals := return ⟨← IO.Promise.new, ← IO.Promise.new⟩

private structure Control where
  snapshot : WorkerSnapshot
  signals : Signals
  force : Bool := false

structure Workers where
  private mk ::
  admission : Admission
  private control : Std.Mutex Control
  private signals : Signals
  private tasks : Array (Task (Except IO.Error Unit))
  config : WorkerConfig

/-- A lifecycle result never claims that unfinished in-memory jobs were flushed.
Durable jobs retain their rows and release/recover unacknowledged leases. -/
structure DrainResult where
  drained : Bool
  snapshot : WorkerSnapshot
  deriving Repr

private def effective (state : Control) : IO Control := do
  let draining ← state.signals.draining.isResolved
  let deadline := if draining then state.signals.draining.result?.get.getD 0 else 0
  let force := state.force || (← state.signals.force.isResolved) || (draining && (← IO.monoMsNow) >= deadline)
  return { state with force, snapshot := { state.snapshot with draining, forced := force } }

private def readControl (state : Std.Mutex Control) : IO Control := do effective (← state.atomically get)
private def changeControl (state : Std.Mutex Control) (f : Control → Control) : IO Unit := state.atomically (modify f)

private def observe (hooks : Hooks) (value : Event) : IO Unit :=
  try hooks.emit value catch _ => pure ()

private def startSpan (hooks : Hooks) (value : Event) : IO Span :=
  try hooks.startSpan value catch _ => pure { traceparent := value.traceparent }

private partial def driveLoop (execution : AppM.Execution α) (cutoff : Nat)
    (control : Option (Std.Mutex Control)) : IO (Except AppError α) := do
  let forced ← match control with
    | none => pure false
    | some state => do pure (← readControl state).force
  if forced then
    execution.cancel .shutdown
    return .error { status := 503, message := "job worker stopping" }
  if (← IO.monoMsNow) >= cutoff then
    execution.cancel .timeout
    return .error { status := 504, message := "job operation timed out" }
  match ← execution.poll with
  | some result => return result
  | none => IO.sleep 1; driveLoop execution cutoff control

/-- Drive cooperative application effects on a dedicated job owner, without a
reactor or moving native database handles between threads. -/
def run (action : AppM α) (timeoutMs : Nat := 30000)
    (maxRetainedBytes : Nat := 4 * 1024 * 1024) : IO (Except AppError α) := do
  let execution ← AppM.execute action maxRetainedBytes
  driveLoop execution ((← IO.monoMsNow) + timeoutMs) none

private def operation (action : AppM α) (config : WorkerConfig) (control : Std.Mutex Control)
    (ignoreStop : Bool := false) : IO (Except AppError α) := do
  try
    let execution ← AppM.execute action config.maxRetainedBytes
    driveLoop execution ((← IO.monoMsNow) + config.operationTimeoutMs) (if ignoreStop then none else some control)
  catch _ => return .error { status := 503, message := "job backend operation failed" }

/-- Retry transient backend contention without abandoning a still-owned lease.
Each retry and the entire operation window remain bounded. -/
private partial def retryOperation (action : AppM α) (cutoff : Nat) (config : WorkerConfig)
    (control : Std.Mutex Control) (ignoreStop : Bool := false) : IO (Except AppError α) := do
  let now ← IO.monoMsNow
  if now >= cutoff then return .error { status := 504, message := "job backend retry window expired" }
  let result ← operation action { config with operationTimeoutMs := max 1 (min config.operationTimeoutMs (cutoff - now)) } control ignoreStop
  match result with
  | .error error =>
    if [429, 503].contains error.status && (← IO.monoMsNow) < cutoff then
      IO.sleep 1
      retryOperation action cutoff config control ignoreStop
    else return result
  | _ => return result

private def outcomeEvent (lease : Lease) (outcome : Outcome) (duration : Nat) : Event :=
  let value := event lease.job (.succeeded) lease.attempt
  match outcome with
  | .success => { value with durationMs := duration }
  | .cancelled => { value with state := .cancelled, durationMs := duration }
  | .leaseLost => { value with state := .leaseLost, durationMs := duration, errorCode := some "lease_lost" }
  | .failed status code retryable => { value with
      state := if retryable && lease.attempt < lease.job.retry.maxAttempts then .retrying else .dead
      durationMs := duration
      errorStatus := some status
      errorCode := some code }

private partial def executeLoop (backend : Backend) (lease : Lease) (execution : AppM.Execution Unit)
    (started : Nat) (lastHeartbeat : Nat) (config : WorkerConfig) (control : Std.Mutex Control) : IO Outcome := do
  try backend.tick catch _ => pure ()
  let now ← IO.monoMsNow
  if (← readControl control).force then
    execution.cancel .shutdown
    return .leaseLost
  if now - started >= lease.job.timeoutMs then
    execution.cancel .timeout
    return .failed 504 "execution_timeout" true
  let heartbeat := min config.heartbeatMs (max 1 (backend.leaseMs / 3))
  let mut beat := lastHeartbeat
  if now - lastHeartbeat >= heartbeat then
    match ← retryOperation (backend.renew lease) (now + heartbeat) config control with
    | .ok .active => beat := ← IO.monoMsNow
    | .ok .cancelled => execution.cancel .shutdown; return .cancelled
    | _ => execution.cancel .failed; return .leaseLost
  try
    match ← execution.poll with
    | some (.ok ()) => return .success
    | some (.error error) => return .failed error.status "application_failure" (error.status >= 500 || [408, 429].contains error.status)
    | none => IO.sleep config.pollMs.toUInt32; executeLoop backend lease execution started beat config control
  catch _ =>
    execution.cancel .failed
    return .failed 500 "handler_exception" true

private def executeOne (backend : Backend) (registry : Registry) (lease : Lease)
    (hooks : Hooks) (config : WorkerConfig) (control : Std.Mutex Control) : IO Outcome := do
  changeControl control fun s => { s with snapshot := { s.snapshot with active := s.snapshot.active + 1 } }
  try
    let started ← IO.monoMsNow
    let span ← startSpan hooks (event lease.job .running lease.attempt)
    let lease := { lease with job := { lease.job with metadata := { lease.job.metadata with traceparent := span.traceparent } } }
    observe hooks (event lease.job .running lease.attempt)
    let outcome ← match lease.job.validate registry backend.maxPayloadBytes with
      | .error _ => pure (.failed 422 "invalid_payload" false)
      | .ok job =>
        match registry.find job.kind with
        | none => pure (.failed 422 "unknown_kind" false)
        | some definition => do
          let context : ExecutionContext := {
            jobId := job.id, kind := job.kind, metadata := job.metadata, attempt := lease.attempt
            cancelled := do return (← readControl control).force }
          try
            let execution ← AppM.execute (definition.run context job.payload) config.maxRetainedBytes
            executeLoop backend lease execution started started config control
          catch _ => pure (.failed 500 "handler_exception" true)
    let settled ← retryOperation (backend.settle lease outcome)
      ((← IO.monoMsNow) + min 1000 (max 1 (backend.leaseMs / 3))) config control true
    let outcome := match settled with | .ok true => outcome | _ => .leaseLost
    let value := outcomeEvent lease outcome ((← IO.monoMsNow) - started)
    try span.finish value catch _ => pure ()
    observe hooks value
    changeControl control fun s => { s with snapshot := { s.snapshot with
      completed := s.snapshot.completed + (if outcome == .success then 1 else 0)
      failures := s.snapshot.failures + (if outcome == .success then 0 else 1) } }
    return outcome
  finally
    changeControl control fun s => { s with snapshot := { s.snapshot with active := s.snapshot.active - 1 } }

private partial def workerLoop (backend : Backend) (registry : Registry) (owner : String)
    (hooks : Hooks) (config : WorkerConfig) (control : Std.Mutex Control) : IO Unit := do
  let state ← readControl control
  if state.force then return
  try backend.tick catch _ => pure ()
  match ← operation (backend.claim owner) config control with
  | .ok (some lease) => let _ ← executeOne backend registry lease hooks config control; pure ()
  | _ =>
    if state.snapshot.draining then
      match ← operation backend.busy config control with
      | .ok false => return
      | _ => pure ()
    IO.sleep config.pollMs.toUInt32
  workerLoop backend registry owner hooks config control

/-- Factory runs inside each dedicated thread. Construct native adapter pools here,
not on the caller/reactor thread. Backend.close runs on that same owner. -/
private def launchOwned (factory : Nat → IO WorkerResources) (config : WorkerConfig := {})
    (admission : Option Admission := none) : IO Workers := do
  if config.workers == 0 || config.workers > 64 || config.pollMs == 0 || config.pollMs > 1000 ||
      config.heartbeatMs == 0 || config.operationTimeoutMs == 0 || config.operationTimeoutMs > 3600000 ||
      config.maxRetainedBytes < 1024 || config.maxRetainedBytes > 256 * 1024 * 1024 then
    throw (IO.userError "invalid background worker bounds")
  let admission ← match admission with | some value => pure value | none => Admission.new
  let signals ← Signals.create
  let control ← Std.Mutex.new ({ snapshot := { workers := config.workers }, signals } : Control)
  let workerPrefix ← newId
  let tasks ← (Array.range config.workers).mapM fun worker =>
    IO.asTask (do
      try
        let resources ← factory worker
        let backend := resources.backend
        let registry := resources.registry
        let hooks := resources.hooks
        changeControl control fun s => { s with snapshot := { s.snapshot with ready := s.snapshot.ready + 1 } }
        try workerLoop backend registry s!"{workerPrefix}-{worker}" hooks config control finally backend.close
      catch _ => changeControl control fun s => { s with snapshot := { s.snapshot with workerErrors := s.snapshot.workerErrors + 1 } }
      finally changeControl control fun s => { s with snapshot := { s.snapshot with finished := s.snapshot.finished + 1 } })
      Task.Priority.dedicated
  return ⟨admission, control, signals, tasks, config⟩

/-- Registry, native pools, tracing sink and hooks are constructed together on
their dedicated owner. No worker-local tracer is shared across threads. -/
def Workers.startOwned (factory : Nat → IO WorkerResources) (config : WorkerConfig := {})
    (admission : Option Admission := none) : IO Workers := launchOwned factory config admission

/-- Compatibility API for immutable/thread-safe hooks. A hook factory, when
supplied, executes once on each owner instead of sharing the provided hooks. -/
def Workers.start (factory : Nat → IO (Backend × Registry)) (config : WorkerConfig := {})
    (hooks : Hooks := {}) (admission : Option Admission := none)
    (hooksFactory : Option (Nat → IO Hooks) := none) : IO Workers :=
  launchOwned (fun worker => do
    let (backend, registry) ← factory worker
    try
      let ownHooks ← match hooksFactory with | some make => make worker | none => pure hooks
      return { backend, registry, hooks := ownHooks }
    catch error => do backend.close; throw error) config admission

def Workers.snapshot (workers : Workers) : IO WorkerSnapshot := do
  let state ← workers.control.tryAtomically get
  let current : WorkerSnapshot ← match state with
    | some state => return (← effective state).snapshot
    | none => pure { workers := workers.config.workers, active := workers.config.workers, draining := ← workers.signals.draining.isResolved, forced := ← workers.signals.force.isResolved }
  return { current with admitting := ← workers.admission.isOpen }

def Workers.stopAdmission (workers : Workers) : IO Unit := workers.admission.stop

/-- Stops admission and permits queued work to finish. At the deadline running
executions are cancelled and their unacknowledged leases are released/recovered.
The result reports unfinished workers rather than waiting without a bound. -/
def Workers.drain (workers : Workers) (timeoutMs : Nat := 30000) : IO DrainResult := do
  workers.stopAdmission
  let cutoff := (← IO.monoMsNow) + timeoutMs
  workers.signals.draining.resolve cutoff
  repeat
    let snapshot ← workers.snapshot
    if snapshot.finished == snapshot.workers then return { drained := !snapshot.forced && snapshot.workerErrors == 0, snapshot }
    if (← IO.monoMsNow) >= cutoff then
      workers.signals.force.resolve ()
      return { drained := false, snapshot := ← workers.snapshot }
    IO.sleep 1

/-- Bounded close can be called repeatedly after a timed-out drain. No arbitrary
Lean task is killed; handlers must respect cooperative await/cancellation. -/
def Workers.close (workers : Workers) (timeoutMs : Nat := 1000) : IO Bool := do
  workers.stopAdmission
  workers.signals.force.resolve ()
  let cutoff := (← IO.monoMsNow) + timeoutMs
  repeat
    let snapshot ← workers.snapshot
    if snapshot.finished == snapshot.workers then return true
    if (← IO.monoMsNow) >= cutoff then return false
    IO.sleep 1


/-- Nonblocking lifecycle request suitable for a reactor timer/shutdown callback.
The first request fixes the deadline; workers observe it through runtime promises. -/
def Workers.beginDrainAt (workers : Workers) (deadlineMs : Nat) : IO Unit := do
  workers.stopAdmission
  workers.signals.draining.resolve deadlineMs

def Workers.beginDrain (workers : Workers) (timeoutMs : Nat := 30000) : IO Unit := do
  workers.beginDrainAt ((← IO.monoMsNow) + timeoutMs)

def Workers.requestClose (workers : Workers) : IO Unit := do
  workers.stopAdmission
  workers.signals.force.resolve ()

/-- Nonblocking drain observation. False includes future in-memory jobs awaiting
an explicit persistence handoff; a durable backend may safely retain scheduled rows. -/
def Workers.pollDrain (workers : Workers) : IO DrainResult := do
  let snapshot ← workers.snapshot
  return { drained := snapshot.finished == snapshot.workers && !snapshot.forced && snapshot.workerErrors == 0, snapshot }

/-- Always close dedicated workers when producer/schema/handler setup fails.
Factories and handlers must remain cooperative; arbitrary blocking user IO cannot
be forcibly terminated safely by Lean. Cleanup has an explicit bounded deadline. -/
def withWorkers (factory : Nat → IO (Backend × Registry)) (use : Workers → IO α)
    (config : WorkerConfig := {}) (hooks : Hooks := {}) (admission : Option Admission := none)
    (closeTimeoutMs : Nat := 30000) : IO α := do
  let workers ← Workers.start factory config hooks admission
  try use workers
  finally
    unless ← workers.close closeTimeoutMs do
      throw (IO.userError "job workers did not close within the cooperative cleanup deadline")

/-- Bracket owner-local pools and instrumentation together. The factory and all
backend/hooks callbacks execute on their dedicated owner; cleanup also runs when
the producer or HTTP application factory throws. -/
def withOwnedWorkers (factory : Nat → IO WorkerResources) (use : Workers → IO α)
    (config : WorkerConfig := {}) (admission : Option Admission := none)
    (closeTimeoutMs : Nat := 30000) : IO α := do
  let workers ← Workers.startOwned factory config admission
  try use workers
  finally
    unless ← workers.close closeTimeoutMs do
      throw (IO.userError "job workers did not close within the cooperative cleanup deadline")

/-- Synchronous execution on the calling owner. Use from a CLI/dedicated worker;
this does not make blocking work appropriate for an HTTP reactor callback. -/
def performSync (registry : Registry) (job : Envelope) (hooks : Hooks := {})
    (config : WorkerConfig := {}) : IO Outcome := do
  let signals ← Signals.create
  let control ← Std.Mutex.new ({ snapshot := { workers := 1, ready := 1 }, signals } : Control)
  let backend : Backend := {
    claim := fun _ => pure none
    renew := fun _ => pure .active
    settle := fun _ _ => pure true
    busy := pure false }
  executeOne backend registry { job, owner := "sync", token := job.id, attempt := 1 } hooks config control

end LeanWeave.Jobs
