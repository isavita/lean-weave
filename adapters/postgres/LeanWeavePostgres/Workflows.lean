import LeanWeavePostgres.Jobs

namespace LeanWeave.Postgres.Workflows

abbrev Store := Jobs.Store

private def field (row : Row) (n : Nat) : AppM String :=
  match row[n]? with | some (some s) => pure s | _ => throw { status := 503,message := "invalid workflow database row" }

private def statusName : LeanWeave.Workflow.Status → String
  | .running => "running" | .compensating => "compensating" | .succeeded => "succeeded"
  | .failed => "failed" | .cancelled => "cancelled" | .compensationFailed => "compensation_failed"

private def decode (text : String) : AppM LeanWeave.Workflow.Instance := do
  -- Leave bounded room for PostgreSQL's jsonb formatting; the persisted state
  -- retains the one MiB compact limit enforced when the workflow is admitted.
  let json ← match Body.json text.toUTF8 { maxBytes := 2 * 1024 * 1024, maxDepth := 40, maxFields := 1024 } with
    | .ok value => pure value
    | .error _ => throw { status := 503,message := "invalid persisted workflow state" }
  if json.compress.utf8ByteSize > 1024 * 1024 then
    throw { status := 503,message := "invalid persisted workflow state" }
  match Lean.fromJson? json with
  | .ok flow => pure flow
  | .error _ => throw { status := 503,message := "invalid persisted workflow state" }

/-- Job terminal transitions and workflow events commit together in PostgreSQL.
The event is not acknowledged until state changes and dependent publications commit.
A crash at any point leaves either the job lease or its outbox event recoverable. -/
def install (store : Store) : AppM Unit := do
  store.initialize
  store.pool.transaction fun c => do
    let _ ← c.query "SELECT pg_advisory_xact_lock(549589302)"
    let _ ← c.query "ALTER TABLE public.leanweave_workflows ADD COLUMN IF NOT EXISTS definition jsonb"
    let _ ← c.query "ALTER TABLE public.leanweave_workflow_jobs ADD COLUMN IF NOT EXISTS retained_bytes bigint NOT NULL DEFAULT 0"
    let _ ← c.query "CREATE TABLE IF NOT EXISTS public.leanweave_workflow_events (job_id text PRIMARY KEY,workflow_id text NOT NULL REFERENCES public.leanweave_workflows(id) ON DELETE CASCADE,state text NOT NULL CHECK(state IN ('succeeded','dead','cancelled')),created_at timestamptz NOT NULL DEFAULT clock_timestamp())"
    let _ ← c.query "CREATE OR REPLACE FUNCTION public.leanweave_workflow_job_event() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN INSERT INTO public.leanweave_workflow_events(job_id,workflow_id,state) SELECT NEW.id,m.workflow_id,NEW.state FROM public.leanweave_workflow_jobs m WHERE m.job_id=NEW.id ON CONFLICT(job_id) DO UPDATE SET state=EXCLUDED.state; RETURN NEW; END $$"
    let _ ← c.query "DO $$ BEGIN IF NOT EXISTS(SELECT 1 FROM pg_trigger WHERE tgname='leanweave_workflow_job_event' AND tgrelid='public.leanweave_jobs'::regclass) THEN CREATE TRIGGER leanweave_workflow_job_event AFTER UPDATE OF state ON public.leanweave_jobs FOR EACH ROW WHEN (NEW.state IN ('succeeded','dead','cancelled') AND OLD.state IS DISTINCT FROM NEW.state) EXECUTE FUNCTION public.leanweave_workflow_job_event(); END IF; END $$"
    pure ()

private def persist (store : Store) (c : Connection) (flow : LeanWeave.Workflow.Instance) : AppM Unit := do
  let _ ← c.query "UPDATE public.leanweave_workflows SET status=$3,snapshot=$4::jsonb,updated_at=clock_timestamp() WHERE id=$1 AND queue_name=$2"
    #[some flow.id,some store.config.queue,some (statusName flow.status),some (Lean.toJson flow).compress]
  pure ()

private def publish (store : Store) (c : Connection) (original : LeanWeave.Workflow.Instance) : AppM LeanWeave.Workflow.Instance := do
  let mut flow := original.advance
  for jobId in flow.jobsToCancel do
    let _ ← c.query "UPDATE public.leanweave_jobs SET cancel_requested=true,state=CASE WHEN state IN ('queued','retrying') THEN 'cancelled' ELSE state END,updated_at=clock_timestamp() WHERE id=$1 AND queue_name=$2 AND tenant=$3 AND state IN ('queued','retrying','running')"
      #[some jobId,some store.config.queue,some flow.tenant]
  for (_, job) in flow.actions do
    let _ ← store.enqueueContinuationOn c job
    flow := flow.submitted job.id
  persist store c flow
  return flow

private def validateNew (store : Store) (flow : LeanWeave.Workflow.Instance) : Except String Unit := do
  if !LeanWeave.Session.validToken flow.id || flow.revision != 1 || flow.status != .running ||
      !flow.steps.all (fun s => s.stage == .waiting && LeanWeave.Session.validToken s.jobId && LeanWeave.Session.validToken s.compensationId) then
    throw "new workflows require fresh IDs and waiting steps"
  let plan : LeanWeave.Workflow.Plan := { name := flow.name,steps := flow.steps.map (·.step) }
  let order ← plan.validate store.registry
  let mut ids := #[flow.id]
  for i in [:flow.steps.size] do
    let step := flow.steps[i]!
    if step.order != i || step.step.name != order[i]!.name || ids.contains step.jobId || ids.contains step.compensationId || step.jobId == step.compensationId then
      throw "invalid workflow topological order or duplicate job IDs"
    ids := ids.push step.jobId |>.push step.compensationId
    for action in #[some step.step.action,step.step.compensation] do
      if let some action := action then
        let job : LeanWeave.Jobs.Envelope := {
          id := step.jobId,kind := action.kind,payload := action.payload,
          metadata := { tenant := flow.tenant,requestId := flow.requestId,traceparent := flow.traceparent },
          retry := { maxAttempts := step.step.maxAttempts },timeoutMs := step.step.timeoutMs }
        let _ ← (job.validate store.registry store.config.maxPayloadBytes).mapError (·.message)
  if (Lean.toJson flow).compress.utf8ByteSize > 1024 * 1024 then throw "workflow state exceeds one MiB"

/-- Business writes and initial workflow/job publications share this connection.
The queue/flow counters, definition, mappings and initial jobs commit atomically. -/
def enqueueOn (store : Store) (c : Connection) (flow : LeanWeave.Workflow.Instance) : AppM String := do
  unless ← store.admission.isOpen do throw { status := 503,message := "workflow admission stopped" }
  match validateNew store flow with | .error e => throw { status := 422,message := e } | .ok () => pure ()
  let definition := (Lean.toJson flow).compress
  let _ ← c.query "SELECT queue_name FROM public.leanweave_workflow_budgets WHERE queue_name=$1 FOR UPDATE" #[some store.config.queue]
  let existing ← c.query "SELECT tenant=$2 AND queue_name=$3 AND definition=$4::jsonb FROM public.leanweave_workflows WHERE id=$1"
    #[some flow.id,some flow.tenant,some store.config.queue,some definition]
  if let some row := existing.rows[0]? then
    unless (← field row 0) == "t" do throw { status := 409,message := "workflow ID reused with a different definition" }
    return flow.id
  let chargedBytes := definition.utf8ByteSize + flow.steps.size * 512 + 512
  let admitted ← c.query "UPDATE public.leanweave_workflow_budgets SET flows=flows+1,bytes=bytes+$2::bigint WHERE queue_name=$1 AND flows<max_flows AND bytes+$2::bigint<=max_bytes RETURNING queue_name"
    #[some store.config.queue,some (toString chargedBytes)]
  if admitted.rows.isEmpty then throw { status := 503,message := "durable workflow capacity reached" }
  let _ ← c.query "INSERT INTO public.leanweave_workflows(id,queue_name,tenant,status,snapshot,definition,retained_bytes) VALUES($1,$2,$3,'running',$4::jsonb,$4::jsonb,$5::bigint)"
    #[some flow.id,some store.config.queue,some flow.tenant,some definition,some (toString chargedBytes)]
  let mut mappings : Array Lean.Json := #[]
  let mut reservationBytes := 0
  for step in flow.steps do
    for (id,action,compensation) in #[(step.jobId,some step.step.action,false),(step.compensationId,step.step.compensation,true)] do
      if let some action := action then
        let job : LeanWeave.Jobs.Envelope := {
          id,kind := action.kind,payload := action.payload
          metadata := { tenant := flow.tenant,requestId := flow.requestId,traceparent := flow.traceparent }
          timeoutMs := step.step.timeoutMs,retry := { maxAttempts := step.step.maxAttempts }
          idempotencyKey := s!"workflow:{flow.id}:{step.step.name}:{compensation}" }
        let canonical ← match job.validate store.registry store.config.maxPayloadBytes with | .ok j => pure j | .error e => throw e
        reservationBytes := reservationBytes + canonical.bytes
        mappings := mappings.push (Lean.Json.mkObj [("id",Lean.toJson id),("compensation",Lean.toJson compensation),("bytes",Lean.toJson canonical.bytes)])
  let reserved ← c.query "UPDATE public.leanweave_job_queues SET jobs=jobs+$2::bigint,bytes=bytes+$3::bigint WHERE name=$1 AND jobs+$2::bigint<=max_jobs AND bytes+$3::bigint<=max_bytes RETURNING name"
    #[some store.config.queue,some (toString mappings.size),some (toString reservationBytes)]
  if reserved.rows.isEmpty then throw { status := 503,message := "whole workflow job reservation exceeds queue capacity" }
  let _ ← c.query "INSERT INTO public.leanweave_workflow_jobs(job_id,workflow_id,compensation,retained_bytes) SELECT value->>'id',$1,(value->>'compensation')::boolean,(value->>'bytes')::bigint FROM jsonb_array_elements($2::jsonb) AS value"
    #[some flow.id,some (Lean.toJson mappings).compress]
  let _ ← publish store c flow
  return flow.id

def enqueue (store : Store) (flow : LeanWeave.Workflow.Instance) : AppM String :=
  store.pool.transaction fun c => enqueueOn store c flow

def get (store : Store) (tenant id : String) : AppM (Option LeanWeave.Workflow.Instance) := do
  let result ← store.pool.query "SELECT snapshot::text FROM public.leanweave_workflows WHERE queue_name=$1 AND tenant=$2 AND id=$3"
    #[some store.config.queue,some tenant,some id]
  match result.rows[0]? with | none => pure none | some row => return some (← decode (← field row 0))

def cancel (store : Store) (tenant id : String) : AppM Bool := store.pool.transaction fun c => do
  let result ← c.query "SELECT snapshot::text FROM public.leanweave_workflows WHERE queue_name=$1 AND tenant=$2 AND id=$3 FOR NO KEY UPDATE"
    #[some store.config.queue,some tenant,some id]
  let some row := result.rows[0]? | return false
  let flow ← decode (← field row 0)
  if flow.status != .running then return false
  let _ ← publish store c flow.cancel
  return true

/-- Lock the workflow before its event, avoiding cross-worker lock inversion.
SKIP LOCKED permits unrelated workflows to transition concurrently. -/
private def reconcileOne (store : Store) : AppM Bool := store.pool.transaction fun c => do
  let selected ← c.query "SELECT w.id,w.snapshot::text,e.job_id FROM public.leanweave_workflow_events e JOIN public.leanweave_workflows w ON w.id=e.workflow_id WHERE w.queue_name=$1 ORDER BY e.created_at,e.job_id FOR NO KEY UPDATE OF w SKIP LOCKED LIMIT 1"
    #[some store.config.queue]
  let some row := selected.rows[0]? | return false
  let flow ← decode (← field row 1)
  let jobId ← field row 2
  let outcome ← c.query "SELECT state FROM public.leanweave_workflow_events WHERE job_id=$1 FOR UPDATE" #[some jobId]
  let some event := outcome.rows[0]? | return false
  let state ← field event 0
  let flow := flow.finish jobId (state == "succeeded") (state == "cancelled")
  let _ ← publish store c flow
  let _ ← c.query "DELETE FROM public.leanweave_workflow_events WHERE job_id=$1" #[some jobId]
  return true

/-- Bounded outbox replay; failures roll back both state and publications so the
event remains durable for a later worker. -/
def reconcile (store : Store) (limit : Nat := 16) : AppM Nat := do
  if limit == 0 || limit > 128 then throw { status := 422,message := "invalid workflow replay batch" }
  let mut count := 0
  let mut finished := false
  for _ in [:limit] do
    unless finished do
      if ← reconcileOne store then count := count + 1 else finished := true
  return count

/-- Explicit compensation retry resets its durable dead letter and state in the
same transaction. Successful forward steps are never replayed implicitly. -/
def retryCompensation (store : Store) (tenant id : String) : AppM Bool := store.pool.transaction fun c => do
  unless ← store.admission.isOpen do throw { status := 503,message := "workflow admission stopped" }
  let result ← c.query "SELECT snapshot::text FROM public.leanweave_workflows WHERE queue_name=$1 AND tenant=$2 AND id=$3 FOR NO KEY UPDATE"
    #[some store.config.queue,some tenant,some id]
  let some row := result.rows[0]? | return false
  let flow ← decode (← field row 0)
  if flow.status != .compensationFailed then return false
  let some step := flow.steps.find? (·.stage == .compensationFailed) | return false
  let retried ← c.query "UPDATE public.leanweave_jobs SET state='queued',attempt=0,cancel_requested=false,last_code=NULL,last_status=NULL,available_at=clock_timestamp(),updated_at=clock_timestamp() WHERE id=$1 AND state='dead' RETURNING id" #[some step.compensationId]
  if retried.rows.isEmpty then return false
  let flow := { flow with status := LeanWeave.Workflow.Status.compensating,steps := flow.steps.map fun s =>
    if s.compensationId == step.compensationId then { s with stage := .compensateQueued } else s }
  persist store c flow
  return true

/-- Terminal history is removed deliberately; its workflow/job counters and
idempotency reservations are released together. -/
def remove (store : Store) (tenant id : String) : AppM Bool := store.pool.transaction fun c => do
  let _ ← c.query "SELECT queue_name FROM public.leanweave_workflow_budgets WHERE queue_name=$1 FOR UPDATE" #[some store.config.queue]
  let result ← c.query "SELECT retained_bytes::text,status FROM public.leanweave_workflows WHERE id=$1 AND queue_name=$2 AND tenant=$3 FOR UPDATE"
    #[some id,some store.config.queue,some tenant]
  let some row := result.rows[0]? | return false
  unless ["succeeded","failed","cancelled","compensation_failed"].contains (← field row 1) do return false
  let live ← c.query "SELECT EXISTS(SELECT 1 FROM public.leanweave_jobs j JOIN public.leanweave_workflow_jobs m ON m.job_id=j.id WHERE m.workflow_id=$1 AND j.state IN ('queued','retrying','running'))" #[some id]
  unless live.rows == #[#[some "f"]] do return false
  let _ ← c.query "SELECT name FROM public.leanweave_job_queues WHERE name=$1 FOR UPDATE" #[some store.config.queue]
  let _ ← c.query "WITH charge AS (SELECT count(*) FILTER(WHERE m.retained_bytes>0) + count(j.id) FILTER(WHERE m.retained_bytes=0) AS jobs,coalesce(sum(CASE WHEN m.retained_bytes>0 THEN m.retained_bytes ELSE coalesce(j.retained_bytes,0) END),0) AS bytes FROM public.leanweave_workflow_jobs m LEFT JOIN public.leanweave_jobs j ON j.id=m.job_id WHERE m.workflow_id=$2) UPDATE public.leanweave_job_queues AS q SET jobs=q.jobs-charge.jobs,bytes=q.bytes-charge.bytes FROM charge WHERE q.name=$1" #[some store.config.queue,some id]
  let _ ← c.query "DELETE FROM public.leanweave_jobs WHERE id IN (SELECT job_id FROM public.leanweave_workflow_jobs WHERE workflow_id=$1)" #[some id]
  let _ ← c.query "DELETE FROM public.leanweave_workflows WHERE id=$1" #[some id]
  let _ ← c.query "UPDATE public.leanweave_workflow_budgets SET flows=flows-1,bytes=bytes-$2::bigint WHERE queue_name=$1" #[some store.config.queue,some (← field row 0)]
  return true

/-- Job owners also replay the durable transition outbox. Scheduled future rows
remain durable across a clean process drain; pending workflow events keep it busy. -/
def backend (store : Store) : LeanWeave.Jobs.Backend := {
  store.backend with
  claim := fun owner => do let _ ← reconcile store; store.claim owner
  settle := fun lease outcome => do
    let result ← store.settle lease outcome
    if result then let _ ← reconcile store; pure ()
    return result
  busy := do
    let _ ← reconcile store
    let result ← store.pool.query "SELECT EXISTS(SELECT 1 FROM public.leanweave_workflow_events e JOIN public.leanweave_workflows w ON w.id=e.workflow_id WHERE w.queue_name=$1)" #[some store.config.queue]
    return result.rows == #[#[some "t"]] || (← store.backend.busy) }

def workerFactory (database : Postgres.Config) (registry : LeanWeave.Jobs.Registry) (config : Jobs.Config := {})
    (admission : Option LeanWeave.Jobs.Admission := none) : Nat → IO (LeanWeave.Jobs.Backend × LeanWeave.Jobs.Registry) := fun _ => do
  let pool ← Pool.create { database with maxConnections := 1,maxWaiters := 1 }
  try
    let store ← Jobs.Store.ofPool pool registry config admission
    match ← LeanWeave.Jobs.run (install store) 30000 database.retainedBytes with
    | .error e => throw (IO.userError e.message)
    | .ok () => return (backend store,registry)
  catch error => pool.close; throw error

end LeanWeave.Postgres.Workflows
