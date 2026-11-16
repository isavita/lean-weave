import LeanWeavePostgres.Workflows
import LeanWeave.Core.Server
import LeanWeave.Jobs.Lifecycle
import LeanWeave.Jobs.Local

open LeanWeave

namespace JobAcceptance

structure Payload where
  value : Nat
  delayMs : Nat := 0
  failUntil : Nat := 0
  label : String := "effect"
  deriving Lean.ToJson, Lean.FromJson

private def check (value : Bool) (message : String) : IO Unit :=
  unless value do throw (IO.userError ("PG jobs: " ++ message))

private def result (action : AppM α) (config : Postgres.Config) : IO α := do
  match ← Jobs.run action 30000 config.retainedBytes with
  | .ok value => pure value | .error error => throw (IO.userError error.message)

private def registry (config : Postgres.Config) (queue : String) : IO Jobs.Registry := do
  let definition := Jobs.Definition.typed "effect"
    (fun (p : Payload) => if p.value > 1000 || p.delayMs > 10000 || p.failUntil > 32 || p.label.utf8ByteSize > 128 then .error "payload bounds" else .ok p)
    (fun context payload => do
      if payload.delayMs > 0 then
        let start ← IO.monoMsNow
        AppM.await (do
          if (← IO.monoMsNow) - start < payload.delayMs then return none
          return some (.ok ())) (fun _ => pure ()) 11000 32
      if context.attempt <= payload.failUntil then throw { status := 503,message := "retry" }
      let pool ← Postgres.Pool.create { config with maxConnections := 1 }
      let action : AppM Unit := pool.transaction fun connection => do
        if payload.label == "undo-controlled" then
          let enabled ← connection.query "SELECT enabled FROM public.weave_job_controls WHERE name=$1" #[some queue]
          unless enabled.rows == #[#[some "t"]] do throw { status := 422,message := "compensation requires operator intervention" }
        let _ ← connection.query "INSERT INTO public.weave_job_effects(job_id,queue_name,value,label) VALUES($1,$2,$3::int,$4) ON CONFLICT(job_id) DO NOTHING"
          #[some context.jobId,some queue,some (toString payload.value),some payload.label]
      action.ensuring pool.close)
  match Jobs.Registry.create #[definition] with | .ok r => pure r | .error e => throw (IO.userError e)

private def settings (queue : String) : Postgres.Jobs.Config := {
  queue,leaseMs := 300,maxJobs := if queue == "bounded-flow" then 5 else 1000,maxBytes := 16 * 1024 * 1024,maxWorkflows := 64,maxWorkflowBytes := 4 * 1024 * 1024 }

private def envelope (value delay fail : Nat) : IO Jobs.Envelope := return {
  id := ← Jobs.newId,kind := "effect",payload := Lean.toJson ({ value,delayMs := delay,failUntil := fail } : Payload),
  metadata := { tenant := "tenant-a",requestId := "producer",traceparent := "" },
  retry := { maxAttempts := 3,initialDelayMs := 10,maxDelayMs := 50,jitter := false } }

private def bootstrap (pool : Postgres.Pool) (store : Postgres.Jobs.Store) (config : Postgres.Config) : IO Unit := do
  result (Postgres.Workflows.install store) config
  let _ ← result (pool.query "CREATE TABLE IF NOT EXISTS public.weave_job_effects (sequence bigserial PRIMARY KEY,job_id text UNIQUE NOT NULL,queue_name text NOT NULL,value integer NOT NULL,label text NOT NULL)") config
  let _ ← result (pool.query "CREATE TABLE IF NOT EXISTS public.weave_job_business (id text PRIMARY KEY)") config
  let _ ← result (pool.query "CREATE TABLE IF NOT EXISTS public.weave_job_controls (name text PRIMARY KEY,enabled boolean NOT NULL)") config
  pure ()

private def contracts (config : Postgres.Config) (pool : Postgres.Pool) (store : Postgres.Jobs.Store) : IO Unit := do
  let rolled ← envelope 1 0 0
  let rollback := pool.transaction fun c => do
    let _ ← c.query "INSERT INTO public.weave_job_business(id) VALUES($1)" #[some rolled.id]
    let _ ← store.enqueueOn c rolled
    throw ({ status := 409,message := "rollback" } : AppError)
  check (!(← Jobs.run (rollback : AppM Unit) 5000 config.retainedBytes).isOk) "rollback expected"
  check ((← result (store.state "tenant-a" rolled.id) config).isNone) "outbox rollback invisible"
  let business ← result (pool.query "SELECT count(*)::text FROM public.weave_job_business WHERE id=$1" #[some rolled.id]) config
  check (business.rows == #[#[some "0"]]) "business rollback atomic"
  let committed := { (← envelope 2 0 0) with idempotencyKey := "committed" }
  let _ ← result (pool.transaction fun c => do
    let _ ← c.query "INSERT INTO public.weave_job_business(id) VALUES($1)" #[some committed.id]
    store.enqueueOn c committed) config
  let replay := { committed with id := ← Jobs.newId }
  check ((← result (store.enqueue replay) config) == committed.id) "durable idempotency"
  check (!(← Jobs.run (store.enqueue { replay with payload := Lean.toJson ({ value := 3 } : Payload) }) 5000 config.retainedBytes).isOk) "idempotency conflict"
  check ((← result (store.state "tenant-b" committed.id) config).isNone) "tenant read isolated"
  check (!(← result (store.cancel "tenant-b" committed.id) config)) "tenant cancellation isolated"
  let some first ← result (store.claim "owner-a") config | throw (IO.userError "missing first claim")
  IO.sleep 350
  let some second ← result (store.claim "owner-b") config | throw (IO.userError "lease not recovered")
  check (first.job.id == second.job.id && first.token != second.token && second.attempt == 2) "lease recovery/fencing"
  check (!(← result (store.settle first .success) config)) "stale ACK rejected"
  check (← result (store.settle second .success) config) "current ACK accepted"
  let retried ← envelope 4 0 0
  let _ ← result (store.enqueue retried) config
  let some first ← result (store.claim "retry") config | throw (IO.userError "missing retry claim")
  check (← result (store.settle first (.failed 503 "transient" true)) config) "retry persisted"
  check ((← result (store.state "tenant-a" retried.id) config) == some "retrying") "backoff state"
  IO.sleep 30
  let some second ← result (store.claim "retry") config | throw (IO.userError "retry not due")
  check (second.attempt == 2) "attempt increased"
  check (← result (store.settle second (.failed 422 "permanent" false)) config) "dead letter accepted"
  check ((← result (store.state "tenant-a" retried.id) config) == some "dead") "dead letter retained"
  let cancelled := { (← envelope 5 0 0) with availableAtMs := (← Jobs.wallMs) + 60000 }
  let _ ← result (store.enqueue cancelled) config
  check (← result (store.cancel "tenant-a" cancelled.id) config) "scheduled cancellation"
  check ((← result (store.state "tenant-a" cancelled.id) config) == some "cancelled") "cancel persisted"
  check ((← result (store.prune ((← Jobs.wallMs) + 1000)) config) >= 3) "terminal prune releases admission"
  IO.println "PG job transactions/idempotency/tenant/lease/retry/deadletter/cancel checks passed"

private def flow (registry : Jobs.Registry) (variant : String) (delay : Nat := 0) : IO Workflow.Instance := do
  let action (value : Nat) (label : String) (delayMs : Nat := 0) (failUntil : Nat := 0) : Workflow.Action :=
    { kind := "effect",payload := Lean.toJson ({ value,label,delayMs,failUntil } : Payload) }
  let plan : Workflow.Plan := { name := "acceptance",steps := #[
    { name := "a",action := action 1 "a" delay,compensation := some (action 11 "undo-a"),maxAttempts := 1 },
    { name := "b",action := action 2 "b",dependsOn := #["a"],
      compensation := some (action 12 (if variant == "comp-fail" then "undo-controlled" else "undo-b")),maxAttempts := 1 },
    { name := "c",action := action 3 "c" 0 (if variant == "success" then 0 else 1),dependsOn := #["b"],maxAttempts := 1 }] }
  match ← Workflow.create plan registry { tenant := "tenant-a",requestId := "workflow-producer" } with
  | .ok flow => pure flow | .error error => throw (IO.userError error)

private def workflowContracts (config : Postgres.Config) (registry : Jobs.Registry)
    (pool : Postgres.Pool) (store : Postgres.Jobs.Store) : IO Unit := do
  let rolled ← flow registry "success"
  let rollback : AppM Unit := pool.transaction fun c => do
    let _ ← c.query "INSERT INTO public.weave_job_business(id) VALUES($1)" #[some rolled.id]
    let _ ← Postgres.Workflows.enqueueOn store c rolled
    throw { status := 409,message := "rollback workflow" }
  check (!(← Jobs.run rollback 5000 config.retainedBytes).isOk) "workflow transaction rollback"
  check ((← result (Postgres.Workflows.get store "tenant-a" rolled.id) config).isNone) "workflow rollback invisible"
  let committed ← flow registry "success"
  let first ← result (pool.transaction fun c => do
    let _ ← c.query "INSERT INTO public.weave_job_business(id) VALUES($1)" #[some committed.id]
    Postgres.Workflows.enqueueOn store c committed) config
  check ((← result (Postgres.Workflows.enqueue store committed) config) == first) "workflow idempotent definition"
  check ((← result (Postgres.Workflows.get store "tenant-b" first) config).isNone) "workflow tenant read isolated"
  check (!(← result (Postgres.Workflows.cancel store "tenant-b" first) config)) "workflow tenant cancellation isolated"
  check (← result (Postgres.Workflows.cancel store "tenant-a" first) config) "workflow cancellation persisted"
  let _ ← result (Postgres.Workflows.reconcile store) config
  let some cancelled ← result (Postgres.Workflows.get store "tenant-a" first) config | throw (IO.userError "missing workflow")
  check (cancelled.status == .cancelled) "queued workflow cancelled through durable event"
  check (← result (Postgres.Workflows.remove store "tenant-a" first) config) "workflow terminal retention removal"
  let counters ← result (pool.query "SELECT jobs::text FROM public.leanweave_job_queues WHERE name=$1" #[some store.config.queue]) config
  check (counters.rows == #[#[some "0"]]) "workflow job budget cleanup atomic"
  IO.println "PG workflow transactional outbox/idempotency/tenant/cancel/retention checks passed"

/-- Real native listener and dedicated PostgreSQL workers, sharing one absolute
SIGTERM cutoff. The native libpq owner guard checks every query and close. -/
private def lifecycle (config : Postgres.Config) (registry : Jobs.Registry)
    (queue : String) (port drainMs : Nat) : IO Unit := do
  let admission ← Jobs.Admission.new
  Server.serveApplications (fun _ use => do
    let pool ← Postgres.Pool.create config
    try
      let store ← Postgres.Jobs.Store.ofPool pool registry (settings queue) (some admission)
      bootstrap pool store config
      let pulses ← Std.Mutex.new (0 : Nat)
      let pulseRegistry ← match Jobs.Registry.create #[Jobs.Definition.typed "pulse"
          (fun (_ : Unit) => .ok ()) (fun _ _ => do (pulses.atomically (modify (· + 1)) : IO Unit))] with
        | .ok r => pure r | .error e => throw (IO.userError e)
      let pulseQueue ← Jobs.Queue.create pulseRegistry { maxEntries := 8,maxHistory := 1 } {} 300 (some admission)
      let scheduler ← Jobs.Cron.Scheduler.create #[{
        name := "native-timer",schedule := cron! "* * * * *",kind := "pulse",payload := Lean.toJson ()
      }] pulseRegistry pulseQueue.enqueue
      Jobs.withWorkers (fun _ => pure (pulseQueue.backend,pulseRegistry)) (fun pulseWorkers => do
      Jobs.withOwnedWorkers (fun owner => do
        let (backend, ownRegistry) ← Postgres.Workflows.workerFactory config registry (settings queue) (some admission) owner
        return { backend := { backend with close := do
          backend.close
          IO.println s!"OWNER_CLOSED:{owner}"
          (← IO.getStdout).flush }, registry := ownRegistry }) (fun workers => do
        let handler : LeanWeave.Handler := fun context => do
          if context.path == #["ready"] then
            let s ← workers.snapshot
            return Response.text "ready" (if s.ready == s.workers && s.workerErrors == 0 then 200 else 503)
          if context.path == #["enqueue"] && context.request.method == "POST" then
            let value := (context.queryValue "value" >>= String.toNat?).getD 0
            let delay := (context.queryValue "delay" >>= String.toNat?).getD 0
            return Response.text (← store.enqueue (← envelope value delay 0)) 202
          if context.path == #["snapshot"] then
            let s ← workers.snapshot
            return Response.json (Lean.Json.mkObj [
              ("ready",Lean.toJson s.ready),("active",Lean.toJson s.active),
              ("completed",Lean.toJson s.completed),("admitting",Lean.toJson s.admitting)])
          if context.path == #["cron"] then
            let count ← (pulses.atomically get : IO Nat)
            return Response.text (toString count)
          return Response.text "Not Found" 404
        let app ← scheduler.application (pulseWorkers.application {
          handler, tickIntervalMs := 2
          onDrain := fun deadline => do
            workers.beginDrainAt deadline
            unless !(← admission.isOpen) do throw (IO.userError "job admission remained open")
            IO.println s!"ADMISSION_STOPPED:{deadline}"
            (← IO.getStdout).flush
          isDrained := do return (← workers.snapshot).finished == 1
          onShutdown := do
            workers.requestClose
            IO.println "SERVER_SHUTDOWN"
            (← IO.getStdout).flush })
        use app
      ) { workers := 1,pollMs := 2,heartbeatMs := 25,operationTimeoutMs := 500,maxRetainedBytes := config.retainedBytes }
        (some admission) 1500
      ) { pollMs := 2,heartbeatMs := 25 } {} (some admission) 1500
    finally pool.close
  ) { port := port.toUInt16,appRetainedBytes := config.retainedBytes,
      net := { workers := 1,shutdownDrainMs := drainMs,deferred := { pollIntervalMs := 2 } } }

end JobAcceptance

def main (args : List String) : IO Unit := do
  let [mode,queue,a,b] := args | throw (IO.userError "usage: weave-pg-jobs-test MODE QUEUE ARG1 ARG2")
  let config ← match ← Postgres.Config.fromEnv {
      sslMode := "disable",maxConnections := 2,maxResultBytes := 2 * 1024 * 1024,maxParameterBytes := 1024 * 1024 } with
    | .ok c => pure c | .error e => throw (IO.userError e)
  let registry ← JobAcceptance.registry config queue
  if mode == "lifecycle" then
    let some port := a.toNat? | throw (IO.userError "bad port")
    let some drainMs := b.toNat? | throw (IO.userError "bad drain")
    JobAcceptance.lifecycle config registry queue port drainMs
    return
  let pool ← Postgres.Pool.create config
  try
    let store ← Postgres.Jobs.Store.ofPool pool registry (JobAcceptance.settings queue)
    JobAcceptance.bootstrap pool store config
    if mode == "contracts" then JobAcceptance.contracts config pool store
    else if mode == "workflow-contracts" then JobAcceptance.workflowContracts config registry pool store
    else if mode == "workflow-seed" then
      let some delay := b.toNat? | throw (IO.userError "bad delay")
      let flow ← JobAcceptance.flow registry a delay
      IO.println (← JobAcceptance.result (Postgres.Workflows.enqueue store flow) config)
    else if mode == "workflow-cancel" then
      IO.println (← JobAcceptance.result (Postgres.Workflows.cancel store "tenant-a" a) config)
    else if mode == "workflow-remove" then
      IO.println (← JobAcceptance.result (Postgres.Workflows.remove store "tenant-a" a) config)
    else if mode == "workflow-retry" then
      IO.println (← JobAcceptance.result (Postgres.Workflows.retryCompensation store "tenant-a" a) config)
    else if mode == "ack-stop" then
      let some lease ← JobAcceptance.result (store.claim "ack-then-crash") config | throw (IO.userError "no workflow job")
      let executed ← Jobs.performSync registry lease.job {} { maxRetainedBytes := config.retainedBytes }
      JobAcceptance.check (executed == .success) "manual workflow root effect"
      JobAcceptance.check (← JobAcceptance.result (store.settle lease .success) config) "manual durable ACK"
      IO.println "ACK"
      (← IO.getStdout).flush
      IO.sleep 60000
    else if mode == "seed" then
      let some count := a.toNat? | throw (IO.userError "bad count")
      let some delay := b.toNat? | throw (IO.userError "bad delay")
      let mut ids : Array String := #[]
      for i in [:count] do ids := ids.push (← JobAcceptance.result (store.enqueue (← JobAcceptance.envelope i delay 0)) config)
      IO.println (Lean.toJson ids).compress
    else if mode == "worker" then
      let some duration := a.toNat? | throw (IO.userError "bad duration")
      let some count := b.toNat? | throw (IO.userError "bad worker count")
      Jobs.withWorkers (Postgres.Workflows.workerFactory config registry (JobAcceptance.settings queue)) (fun workers => do
        IO.sleep duration.toUInt32
        let result ← workers.drain 5000
        JobAcceptance.check result.drained "worker drain"
      ) { workers := count,pollMs := 2,heartbeatMs := 50,maxRetainedBytes := config.retainedBytes }
    else throw (IO.userError "unknown mode")
  finally pool.close
