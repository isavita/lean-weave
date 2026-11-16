import LeanWeave.Jobs.Local
import LeanWeave.Jobs.Worker

namespace LeanWeave.Jobs.Test

private def check (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw (IO.userError s!"jobs: {message}")

private def ok (result : Except AppError α) : IO α :=
  match result with | .ok value => pure value | .error e => throw (IO.userError e.message)

private def registry (run : ExecutionContext → Nat → AppM Unit) : IO Registry :=
  match Registry.create #[Definition.typed "count" (fun (n : Nat) =>
      if n > 1000 then .error "too large" else .ok n) run] with
  | .ok value => pure value | .error message => throw (IO.userError message)

private structure Counts where
  active : Nat := 0
  peak : Nat := 0
  complete : Nat := 0
  seen : Array String := #[]
  starts : Nat := 0
  ends : Nat := 0
  wrongTrace : Bool := false
  loggedSecret : Bool := false

private def update (state : Std.Mutex Counts) (f : Counts → Counts) : IO Unit := state.atomically (modify f)
private def read (state : Std.Mutex Counts) : IO Counts := state.atomically get

private def waitUntil (predicate : IO Bool) (message : String) (timeoutMs : Nat := 2000) : IO Unit := do
  let cutoff := (← IO.monoMsNow) + timeoutMs
  repeat
    if ← predicate then return
    if (← IO.monoMsNow) >= cutoff then throw (IO.userError ("jobs wait: " ++ message))
    IO.sleep 1

private def envelope (value : Nat) : IO Envelope := return {
  id := ← newId, kind := "count", payload := Lean.toJson value,
  metadata := { tenant := "tenant-a", requestId := "request-1", traceparent := "parent" },
  retry := { maxAttempts := 3, initialDelayMs := 5, maxDelayMs := 20, jitter := false } }

/-- Native dedicated-worker concurrency, retry, fencing, typed payload and drain tests. -/
def run : IO Unit := do
  let counts ← Std.Mutex.new ({} : Counts)
  let definition ← registry fun context value => do
    update counts fun s => { s with
      active := s.active + 1
      peak := max s.peak (s.active + 1)
      wrongTrace := s.wrongTrace || context.metadata.traceparent != "child-" ++ context.jobId }
    let action : AppM Unit := do
      let start ← IO.monoMsNow
      AppM.await (do
        if (← IO.monoMsNow) - start < 20 then return none
        return some (.ok ())) (fun _ => pure ()) 1000 32
      if value % 5 == 0 && context.attempt == 1 then
        throw { status := 503, message := "do-not-log-secret" }
      update counts fun s => if s.seen.contains context.jobId then s else
        { s with complete := s.complete + 1, seen := s.seen.push context.jobId }
    action.ensuring (update counts fun s => { s with active := s.active - 1 })
  let hooks : Hooks := {
    startSpan := fun event => do
      update counts fun s => { s with starts := s.starts + 1 }
      pure { traceparent := "child-" ++ event.jobId, finish := fun _ => update counts fun s => { s with ends := s.ends + 1 } }
    emit := fun event => update counts fun s => { s with loggedSecret := s.loggedSecret ||
      ((event.errorCode.getD "").splitOn "do-not-log-secret").length > 1 } }
  let queue ← Queue.create definition { maxEntries := 64, maxHistory := 64 } hooks 300
  IO.println "jobs: concurrent queue/retry/drain"
  withWorkers (fun _ => pure (queue.backend, definition)) (fun workers => do
    let mut ids := #[]
    for i in [:20] do
      let job ← envelope i
      let cutoff := (← IO.monoMsNow) + 1000
      let mut accepted := false
      repeat
        match ← queue.enqueue job with
        | .ok id => ids := ids.push id; accepted := true
        | .error error =>
          unless error.status == 503 && (← IO.monoMsNow) < cutoff do throw (IO.userError error.message)
        if accepted then break
        IO.sleep 1
    let drained ← workers.drain 5000
    check drained.drained "bounded workers drained all runnable jobs"
    let result ← read counts
    IO.println s!"jobs counts: peak={result.peak}, active={result.active}, complete={result.complete}, starts={result.starts}, ends={result.ends}"
    IO.println s!"jobs queue: {repr (← ok (← queue.snapshot))}"
    check (result.peak > 1 && result.peak <= 4 && result.active == 0 && result.complete == 20) "parallelism is real and bounded"
    check (result.starts == result.ends && result.starts >= 24) "execution spans start before work and finish across retries"
    check (!result.wrongTrace && !result.loggedSecret) "child trace context and secret-free hooks"
    let state ← ok (← queue.snapshot)
    check (state.succeeded == 20 && state.active == 0 && state.dead == 0 && state.queued == 0) "successful queue terminal state"
    check (!(← queue.enqueue (← envelope 99)).isOk) "drain stops admission"
    check (← workers.close 1000) "workers close on their owners"
  ) { workers := 4, pollMs := 2, heartbeatMs := 25 } hooks (some queue.admission)

  let simple ← registry (fun _ _ => pure ())
  let bounded ← Queue.create simple { maxEntries := 1, maxHistory := 1 }
  let job := { (← envelope 1) with idempotencyKey := "same-key" }
  let id ← ok (← bounded.enqueue job)
  let same := { job with id := ← newId }
  check ((← bounded.enqueue same).toOption == some id) "idempotency replay"
  check (!(← bounded.enqueue { same with payload := Lean.toJson (2 : Nat) }).isOk) "idempotency conflicts rejected"
  check (!(← bounded.enqueue (← envelope 2)).isOk) "queue entry budget"
  check (!(← bounded.enqueue { same with payload := Lean.Json.str "invalid" }).isOk) "typed payload validated before admission"
  check (← ok (← bounded.cancel id)) "queued cancellation"
  check (((← ok (← bounded.get id)).map (·.state)) == some .cancelled) "cancelled state retained"

  IO.println "jobs: lease expiry/fencing"
  let retryQueue ← Queue.create simple {} {} 100
  let retryJob ← envelope 2
  let _ ← ok (← retryQueue.enqueue retryJob)
  let some lease ← ok (← Jobs.run (retryQueue.backend.claim "owner-a")) | throw (IO.userError "missing lease")
  IO.sleep 110
  let some reclaimed ← ok (← Jobs.run (retryQueue.backend.claim "owner-b")) | throw (IO.userError "expired lease not recovered")
  check (reclaimed.job.id == lease.job.id && reclaimed.token != lease.token) "expired lease reclaimed with new fence"
  check (!(← ok (← Jobs.run (retryQueue.backend.settle lease .success)))) "stale ack rejected"
  check (← ok (← Jobs.run (retryQueue.backend.settle reclaimed .success))) "current ack accepted"

  let future ← Queue.create simple
  let pending := { (← envelope 1) with availableAtMs := (← wallMs) + 60000 }
  let _ ← ok (← future.enqueue pending)
  IO.println "jobs: scheduled pending handoff"
  withWorkers (fun _ => pure (future.backend, simple)) (fun futureWorkers => do
    let incomplete ← futureWorkers.drain 20
    check (!incomplete.drained && incomplete.snapshot.forced) "future in-memory work is not falsely reported flushed"
    check ((← ok (← future.pending)).size == 1) "future work exportable for durable handoff"
    check (← futureWorkers.close 1000) "forced idle workers close"
  ) {} {} (some future.admission)

  let synced ← performSync simple (← envelope 3)
  check (synced == .success) "synchronous typed job execution"

  IO.println "jobs: producer exception and owner-local cleanup"
  let closed ← Std.Mutex.new (#[] : Array Nat)
  let producerFailed ← try
    withOwnedWorkers (fun owner => do
      let ownerLocal ← IO.mkRef owner
      let backend := { bounded.backend with close := do
        let own ← ownerLocal.get
        closed.atomically (modify (·.push own)) }
      return { backend, registry := simple, hooks := {} }) (fun workers => (do
        waitUntil (do return (← workers.snapshot).ready == 2) "owner factories ready"
        throw (IO.userError "producer failed") : IO Unit))
      { workers := 2,pollMs := 2 } none 1000
    pure false
  catch _ => pure true
  check producerFailed "producer error propagated"
  check ((← closed.atomically get).toList.mergeSort (· <= ·) == [0,1]) "bracket closes every owner after producer error"

  let failed ← Workers.start (fun _ => throw (IO.userError "factory failed")) { workers := 2 }
  try
    waitUntil (do return (← failed.snapshot).finished == 2) "failed factories finish"
    failed.beginDrain 100
    let result ← failed.pollDrain
    check (!result.drained && result.snapshot.workerErrors == 2) "failed factory never reports a clean drain"
  finally let _ ← failed.close 1000; pure ()

  IO.println "jobs: absolute deadline cancels running execution and retains job"
  let cancellation ← Std.Mutex.new (0 : Nat)
  let longDefinition ← registry fun _ _ =>
    (AppM.await (pure none) (fun _ => cancellation.atomically (modify (· + 1))) 10000 32 : AppM Unit)
  let longQueue ← Queue.create longDefinition {} {} 300
  let longJob ← envelope 9
  let _ ← ok (← longQueue.enqueue longJob)
  withOwnedWorkers (fun _ => pure { backend := longQueue.backend,registry := longDefinition }) (fun workers => do
    waitUntil (do return (← workers.snapshot).active == 1) "running cancellable job"
    let cutoff := (← IO.monoMsNow) + 30
    workers.beginDrainAt cutoff
    check (!(← longQueue.admission.isOpen)) "absolute drain immediately stops admission"
    waitUntil (do return (← workers.snapshot).finished == 1) "forced owner finishes"
    let result ← workers.pollDrain
    check (!result.drained && result.snapshot.forced && result.snapshot.active == 0) "forced drain is observable"
    check ((← cancellation.atomically get) == 1) "pending job receives cancellation exactly once"
    let some record ← ok (← longQueue.get longJob.id) | throw (IO.userError "forced job lost")
    check (record.state == .retrying && record.attempt == 0) "unacknowledged work retained without spending retry"
  ) { pollMs := 2,heartbeatMs := 25 } (some longQueue.admission) 1000
  IO.println "jobs concurrency/retry/lease/idempotency/cancellation/drain checks passed"

end LeanWeave.Jobs.Test
