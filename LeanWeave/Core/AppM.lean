import LeanNet.HTTP.Deferred

namespace LeanWeave

structure AppError where
  status : Nat := 500
  message : String := "Internal Server Error"
  deriving Repr, BEq, Inhabited

/-- One bounded, nonblocking operation owned by the worker. `poll` invokes its
    continuation when complete, and returns true exactly on completion. -/
structure Suspension where
  poll : IO Bool
  cancel : LeanNet.HTTP.Deferred.CancelReason → IO Unit
  retainedBytes : Nat

/-- Scheduling capability supplied by the reactor adapter, never a socket. -/
structure AppRuntime where
  suspend : Suspension → IO Unit

/-- Continuation-based cooperative effects. No continuation is moved to a task.
    Immediate IO, poll, and cancel callbacks must never block a reactor worker. -/
structure AppM (α : Type) where
  start : AppRuntime → (Except AppError α → IO Unit) → IO Unit

namespace AppM

instance : Monad AppM where
  pure a := ⟨fun _ done => done (.ok a)⟩
  bind a f := ⟨fun runtime done => a.start runtime fun result =>
    match result with
    | .ok value => (f value).start runtime done
    | .error error => done (.error error)⟩

instance : MonadLift IO AppM where
  monadLift io := ⟨fun _ done => do done (.ok (← io))⟩

instance : MonadExceptOf AppError AppM where
  throw e := ⟨fun _ done => done (.error e)⟩
  tryCatch a f := ⟨fun runtime done => a.start runtime fun result =>
    match result with
    | .ok value => done (.ok value)
    | .error error => (f error).start runtime done⟩

/-- Await bounded asynchronous I/O. Cancellation is terminal and adapter-owned
    producers must discard late results. Timeouts cover each individual await. -/
def await (poll : IO (Option (Except AppError α)))
    (cancel : LeanNet.HTTP.Deferred.CancelReason → IO Unit)
    (timeoutMs : Nat := 30000) (retainedBytes : Nat := 0) : AppM α :=
  ⟨fun runtime done => do
    let started ← IO.monoMsNow
    let cancelled ← IO.mkRef false
    let cancelOnce := fun reason => do
      unless ← cancelled.get do
        cancelled.set true
        cancel reason
    runtime.suspend {
      retainedBytes, cancel := cancelOnce,
      poll := do
        if (← IO.monoMsNow) - started >= timeoutMs then
          cancelOnce .timeout
          done (.error { status := 504, message := "application operation timed out" })
          return true
        match ← poll with
        | none => return false
        | some result => done result; return true }⟩

/-- Release resources on completion, error, or cancellation; cleanup runs once. -/
def ensuring (action : AppM α) (cleanup : IO Unit) : AppM α :=
  ⟨fun runtime done => do
    let cleaned ← IO.mkRef false
    let finish := do
      unless ← cleaned.get do
        cleaned.set true
        cleanup
    let wrap := fun (pending : Suspension) =>
      runtime.suspend { pending with cancel := fun reason => try pending.cancel reason finally finish }
    let wrapped := AppRuntime.mk wrap
    try action.start wrapped (fun result => do finish; done result)
    catch e => do finish; throw e⟩

/-- Reactor-owned completion state with a fixed reservation for adapter buffers. -/
structure Execution (α : Type) where
  result : IO.Ref (Option (Except AppError α))
  pending : IO.Ref (Option Suspension)
  maxRetainedBytes : Nat
  /-- Terminal cancellation prevents a retained producer continuation from
      publishing a late result or scheduling another operation. -/
  cancelled : IO.Ref (Option LeanNet.HTTP.Deferred.CancelReason)

def Execution.cancel (execution : Execution α) (reason : LeanNet.HTTP.Deferred.CancelReason) : IO Unit := do
  if (← execution.cancelled.get).isSome then return
  execution.cancelled.set (some reason)
  let pending ← execution.pending.get
  execution.pending.set none
  if let some operation := pending then operation.cancel reason

def Execution.poll (execution : Execution α) : IO (Option (Except AppError α)) := do
  if (← execution.cancelled.get).isSome then return none
  if let some result ← execution.result.get then return some result
  if let some operation ← execution.pending.get then
    execution.pending.set none
    let ready ← try operation.poll catch error => do
      try operation.cancel .failed finally execution.cancel .failed
      throw error
    if !ready && (← execution.cancelled.get).isNone && (← execution.result.get).isNone then
      if (← execution.pending.get).isNone then execution.pending.set (some operation)
      else
        -- A producer must finish its current await before scheduling another.
        try operation.cancel .failed finally execution.cancel .failed
        throw (IO.userError "AppM producer scheduled overlapping suspensions")
  return ← execution.result.get

def execute (action : AppM α) (maxRetainedBytes : Nat := 1024 * 1024) : IO (Execution α) := do
  let result ← IO.mkRef none
  let pending ← IO.mkRef none
  let cancelled ← IO.mkRef none
  let execution : Execution α := ⟨result, pending, maxRetainedBytes, cancelled⟩
  let runtime : AppRuntime := { suspend := fun operation => do
    let stopped ← IO.mkRef false
    let stop := fun reason => do
      unless ← stopped.get do
        stopped.set true
        operation.cancel reason
    let operation := { operation with cancel := stop }
    if let some reason ← cancelled.get then operation.cancel reason
    else if (← result.get).isSome then operation.cancel .failed
    else if operation.retainedBytes > maxRetainedBytes then
      try operation.cancel .overloaded finally
        execution.result.set (some (.error { status := 503, message := "application buffer budget exceeded" }))
    else if (← pending.get).isSome then
      try operation.cancel .failed finally execution.cancel .failed
      throw (IO.userError "AppM supports one outstanding suspension per execution")
    else execution.pending.set (some operation) }
  try action.start runtime (fun value => do
    if (← cancelled.get).isNone && (← result.get).isNone then execution.result.set (some value))
  catch error => do
    execution.cancel .failed
    throw error
  return execution

/-- Immediate test runner. Deferred tests drive `Execution.poll` explicitly. -/
def run (action : AppM α) : IO (Except AppError α) := do
  let execution ← execute action
  if let some result ← execution.result.get then return result
  execution.cancel .failed
  return .error { message := "asynchronous AppM requires reactor execution" }

end AppM
end LeanWeave
