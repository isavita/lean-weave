import LeanWeavePostgres.Pool
import LeanWeave.Jobs.Worker
import LeanWeave.Workflow.Types

namespace LeanWeave.Postgres.Jobs

structure Config where
  queue : String := "default"
  leaseMs : Nat := 30000
  maxJobs : Nat := 100000
  maxBytes : Nat := 512 * 1024 * 1024
  maxPayloadBytes : Nat := 65536
  maxWorkflows : Nat := 1024
  maxWorkflowBytes : Nat := 16 * 1024 * 1024

structure Store where
  pool : Pool
  registry : LeanWeave.Jobs.Registry
  config : Config
  admission : LeanWeave.Jobs.Admission

private def validate (c : Config) : Except String Unit := do
  if c.queue.isEmpty || c.queue.utf8ByteSize > 128 || !c.queue.toUTF8.data.all (fun b =>
      (b >= 65 && b <= 90) || (b >= 97 && b <= 122) || (b >= 48 && b <= 57) || [45,46,95].contains b.toNat) then
    throw "invalid durable queue name"
  if c.leaseMs < 100 || c.leaseMs > 3600000 || c.maxJobs == 0 || c.maxJobs > 10000000 ||
      c.maxBytes < 1024 || c.maxBytes > 1099511627776 || c.maxPayloadBytes == 0 || c.maxPayloadBytes > 1024*1024 ||
      c.maxWorkflows == 0 || c.maxWorkflows > 1000000 || c.maxWorkflowBytes < 1024 then throw "invalid durable queue bounds"

def Store.ofPool (pool : Pool) (registry : LeanWeave.Jobs.Registry) (config : Config := {})
    (admission : Option LeanWeave.Jobs.Admission := none) : IO Store := do
  match validate config with | .error e => throw (IO.userError e) | .ok () => pure ()
  return { pool, registry, config, admission := ← match admission with | some a => pure a | none => LeanWeave.Jobs.Admission.new }

/-- Shared counters and rows change in one transaction; nodes must agree on bounds. -/
def Store.initialize (store : Store) : AppM Unit := store.pool.transaction fun c => do
  let _ ← c.query "SELECT pg_advisory_xact_lock(549589301)"
  for sql in #[
    "CREATE TABLE IF NOT EXISTS public.leanweave_job_queues (name text PRIMARY KEY,max_jobs bigint NOT NULL,max_bytes bigint NOT NULL,jobs bigint NOT NULL DEFAULT 0 CHECK(jobs>=0),bytes bigint NOT NULL DEFAULT 0 CHECK(bytes>=0))",
    "CREATE TABLE IF NOT EXISTS public.leanweave_jobs (id text PRIMARY KEY,queue_name text NOT NULL REFERENCES public.leanweave_job_queues(name),tenant text NOT NULL,kind text NOT NULL,payload jsonb NOT NULL,request_id text NOT NULL,traceparent text NOT NULL,idempotency_key text NOT NULL,state text NOT NULL DEFAULT 'queued' CHECK(state IN ('queued','running','retrying','succeeded','dead','cancelled')),attempt integer NOT NULL DEFAULT 0,max_attempts integer NOT NULL CHECK(max_attempts BETWEEN 1 AND 32),initial_delay_ms bigint NOT NULL,max_delay_ms bigint NOT NULL,jitter boolean NOT NULL,timeout_ms bigint NOT NULL,available_at timestamptz NOT NULL,lease_owner text,lease_token text,leased_until timestamptz,cancel_requested boolean NOT NULL DEFAULT false,last_status integer,last_code text,retained_bytes bigint NOT NULL CHECK(retained_bytes>=0),created_at timestamptz NOT NULL DEFAULT clock_timestamp(),updated_at timestamptz NOT NULL DEFAULT clock_timestamp())",
    "CREATE UNIQUE INDEX IF NOT EXISTS leanweave_jobs_idempotency ON public.leanweave_jobs(queue_name,tenant,idempotency_key) WHERE idempotency_key<>''",
    "CREATE INDEX IF NOT EXISTS leanweave_jobs_ready ON public.leanweave_jobs(queue_name,available_at,id) WHERE state IN ('queued','retrying','running')",
    "CREATE TABLE IF NOT EXISTS public.leanweave_workflow_budgets (queue_name text PRIMARY KEY REFERENCES public.leanweave_job_queues(name),max_flows bigint NOT NULL,max_bytes bigint NOT NULL,flows bigint NOT NULL DEFAULT 0 CHECK(flows>=0),bytes bigint NOT NULL DEFAULT 0 CHECK(bytes>=0))",
    "CREATE TABLE IF NOT EXISTS public.leanweave_workflows (id text PRIMARY KEY,queue_name text NOT NULL REFERENCES public.leanweave_job_queues(name),tenant text NOT NULL,status text NOT NULL,snapshot jsonb NOT NULL,retained_bytes bigint NOT NULL,updated_at timestamptz NOT NULL DEFAULT clock_timestamp())",
    "CREATE TABLE IF NOT EXISTS public.leanweave_workflow_jobs (job_id text PRIMARY KEY,workflow_id text NOT NULL REFERENCES public.leanweave_workflows(id) ON DELETE CASCADE,compensation boolean NOT NULL,retained_bytes bigint NOT NULL DEFAULT 0)" ] do
    let _ ← c.query sql
  let params := #[some store.config.queue,some (toString store.config.maxJobs),some (toString store.config.maxBytes)]
  let _ ← c.query "INSERT INTO public.leanweave_job_queues(name,max_jobs,max_bytes) VALUES($1,$2::bigint,$3::bigint) ON CONFLICT(name) DO NOTHING" params
  unless (← c.query "SELECT max_jobs=$2::bigint AND max_bytes=$3::bigint FROM public.leanweave_job_queues WHERE name=$1" params).rows == #[#[some "t"]] do
    throw { status := 409, message := "durable queue bounds differ between nodes" }
  let params := #[some store.config.queue,some (toString store.config.maxWorkflows),some (toString store.config.maxWorkflowBytes)]
  let _ ← c.query "INSERT INTO public.leanweave_workflow_budgets(queue_name,max_flows,max_bytes) VALUES($1,$2::bigint,$3::bigint) ON CONFLICT(queue_name) DO NOTHING" params
  unless (← c.query "SELECT max_flows=$2::bigint AND max_bytes=$3::bigint FROM public.leanweave_workflow_budgets WHERE queue_name=$1" params).rows == #[#[some "t"]] do
    throw { status := 409, message := "workflow bounds differ between nodes" }

private def col (row : Row) (i : Nat) : AppM String :=
  match row[i]? with | some (some v) => pure v | _ => throw { status := 503,message := "invalid durable job row" }
private def number (row : Row) (i : Nat) : AppM Nat := do
  let some n := (← col row i).toNat? | throw { status := 503,message := "invalid durable number" }
  pure n

private def insertJob (store : Store) (c : Connection) (job : LeanWeave.Jobs.Envelope) (continuation : Bool := false) : AppM String := do
  let job ← match job.validate store.registry store.config.maxPayloadBytes with | .ok j => pure j | .error e => throw e
  let _ ← c.query "SELECT name FROM public.leanweave_job_queues WHERE name=$1 FOR UPDATE" #[some store.config.queue]
  let existing ← c.query "SELECT id,(queue_name=$1 AND tenant=$2 AND kind=$5 AND payload=$6::jsonb) FROM public.leanweave_jobs WHERE id=$4 OR (queue_name=$1 AND tenant=$2 AND idempotency_key=$3 AND $3<>'')"
    #[some store.config.queue,some job.metadata.tenant,some job.idempotencyKey,some job.id,some job.kind,some job.payload.compress]
  if let some row := existing.rows[0]? then
    unless (← col row 1) == "t" do throw { status := 409,message := "idempotency key reused with different job data" }
    return ← col row 0
  let reserved ← c.query "SELECT m.retained_bytes::text FROM public.leanweave_workflow_jobs m JOIN public.leanweave_workflows w ON w.id=m.workflow_id WHERE m.job_id=$1 AND w.queue_name=$2 AND w.tenant=$3"
    #[some job.id,some store.config.queue,some job.metadata.tenant]
  if !reserved.rows.isEmpty && !continuation then throw { status := 422,message := "workflow jobs require internal publication" }
  let reservation ← match reserved.rows[0]? with | none => pure 0 | some row => number row 0
  if reservation > 0 then
    if job.bytes > reservation then throw { status := 413,message := "workflow action exceeds its reserved budget" }
  else
    let admitted ← c.query "UPDATE public.leanweave_job_queues SET jobs=jobs+1,bytes=bytes+$2::bigint WHERE name=$1 AND jobs<max_jobs AND bytes+$2::bigint<=max_bytes RETURNING name"
      #[some store.config.queue,some (toString job.bytes)]
    if admitted.rows.isEmpty then throw { status := 503,message := "durable queue capacity reached" }
  let _ ← c.query "INSERT INTO public.leanweave_jobs(id,queue_name,tenant,kind,payload,request_id,traceparent,idempotency_key,max_attempts,initial_delay_ms,max_delay_ms,jitter,timeout_ms,available_at,retained_bytes) VALUES($1,$2,$3,$4,$5::jsonb,$6,$7,$8,$9::int,$10::bigint,$11::bigint,$12::boolean,$13::bigint,to_timestamp($14::numeric/1000),$15::bigint)"
    #[some job.id,some store.config.queue,some job.metadata.tenant,some job.kind,some job.payload.compress,
      some job.metadata.requestId,some job.metadata.traceparent,some job.idempotencyKey,some (toString job.retry.maxAttempts),
      some (toString job.retry.initialDelayMs),some (toString job.retry.maxDelayMs),some (if job.retry.jitter then "true" else "false"),
      some (toString job.timeoutMs),some (toString job.availableAtMs),some (toString job.bytes)]
  return job.id

/-- Internal workflow publications remain allowed while external admission is
closed, but require a persisted workflow/job mapping for this queue and tenant. -/
def Store.enqueueContinuationOn (store : Store) (c : Connection) (job : LeanWeave.Jobs.Envelope) : AppM String := do
  unless job.idempotencyKey.startsWith "workflow:" do throw { status := 422,message := "workflow occurrence required" }
  let mapped ← c.query "SELECT EXISTS(SELECT 1 FROM public.leanweave_workflow_jobs m JOIN public.leanweave_workflows w ON w.id=m.workflow_id WHERE m.job_id=$1 AND w.queue_name=$2 AND w.tenant=$3)"
    #[some job.id,some store.config.queue,some job.metadata.tenant]
  unless mapped.rows == #[#[some "t"]] do throw { status := 422,message := "persisted workflow mapping required" }
  insertJob store c job true

/-- The outbox enqueue uses the SAME connection/transaction as business writes;
workers cannot observe its row until the transaction commits. -/
def Store.enqueueOn (store : Store) (c : Connection) (job : LeanWeave.Jobs.Envelope) : AppM String := do
  unless ← store.admission.isOpen do throw { status := 503,message := "job admission stopped" }
  insertJob store c job

def Store.enqueue (store : Store) (job : LeanWeave.Jobs.Envelope) : AppM String :=
  store.pool.transaction fun c => store.enqueueOn c job

private def columns : String := "job.id,kind,payload::text,tenant,request_id,traceparent,idempotency_key,(extract(epoch FROM available_at)*1000)::bigint::text,timeout_ms::text,max_attempts::text,initial_delay_ms::text,max_delay_ms::text,jitter::text,attempt::text,lease_owner,lease_token"

private def decodeLease (row : Row) : AppM LeanWeave.Jobs.Lease := do
  let payload ← match Lean.Json.parse (← col row 2) with | .ok p => pure p | .error _ => throw { status := 503,message := "invalid stored job payload" }
  let job : LeanWeave.Jobs.Envelope := {
    id := ← col row 0
    kind := ← col row 1
    payload
    metadata := { tenant := ← col row 3,requestId := ← col row 4,traceparent := ← col row 5 }
    idempotencyKey := ← col row 6
    availableAtMs := ← number row 7
    timeoutMs := ← number row 8
    retry := {
      maxAttempts := ← number row 9
      initialDelayMs := ← number row 10
      maxDelayMs := ← number row 11
      jitter := (← col row 12) == "true" } }
  return { job,attempt := ← number row 13,owner := ← col row 14,token := ← col row 15 }

/-- Independent distributed claimers use SKIP LOCKED. Lease expiry reclaims the
row with a fresh fence and bounded attempts; exhausted claims become dead letters. -/
def Store.claim (store : Store) (owner : String) : AppM (Option LeanWeave.Jobs.Lease) := do
  let token ← LeanWeave.Jobs.newId
  store.pool.transaction fun c => do
    let _ ← c.query "WITH expired AS (SELECT id FROM public.leanweave_jobs WHERE queue_name=$1 AND state='running' AND leased_until<=clock_timestamp() AND (cancel_requested OR attempt>=max_attempts) FOR UPDATE SKIP LOCKED LIMIT 64) UPDATE public.leanweave_jobs AS job SET state=CASE WHEN job.cancel_requested THEN 'cancelled' ELSE 'dead' END,last_code='lease_expired',lease_owner=NULL,lease_token=NULL,leased_until=NULL,updated_at=clock_timestamp() FROM expired WHERE job.id=expired.id" #[some store.config.queue]
    let result ← c.query ("WITH picked AS (SELECT id FROM public.leanweave_jobs WHERE queue_name=$1 AND ((state IN ('queued','retrying') AND available_at<=clock_timestamp()) OR (state='running' AND leased_until<=clock_timestamp())) AND NOT cancel_requested ORDER BY available_at,id FOR UPDATE SKIP LOCKED LIMIT 1) UPDATE public.leanweave_jobs AS job SET state='running',attempt=job.attempt+1,lease_owner=$2,lease_token=$3,leased_until=clock_timestamp()+$4::bigint*interval '1 millisecond',updated_at=clock_timestamp() FROM picked WHERE job.id=picked.id RETURNING " ++ columns)
      #[some store.config.queue,some owner,some token,some (toString store.config.leaseMs)]
    match result.rows[0]? with | none => pure none | some row => return some (← decodeLease row)

def Store.renew (store : Store) (claim : LeanWeave.Jobs.Lease) : AppM LeanWeave.Jobs.LeaseStatus := do
  let result ← store.pool.query "UPDATE public.leanweave_jobs SET leased_until=clock_timestamp()+$5::bigint*interval '1 millisecond' WHERE id=$1 AND queue_name=$2 AND state='running' AND lease_owner=$3 AND lease_token=$4 AND leased_until>clock_timestamp() RETURNING cancel_requested"
    #[some claim.job.id,some store.config.queue,some claim.owner,some claim.token,some (toString store.config.leaseMs)]
  match result.rows[0]? with
  | none => pure .lost
  | some row => return if (← col row 0) == "t" then .cancelled else .active

private def settleOn (store : Store) (c : Connection) (claim : LeanWeave.Jobs.Lease) (outcome : LeanWeave.Jobs.Outcome) : AppM Bool := do
  let (state,delay,status,code,decrement) : String × Nat × Option Nat × Option String × Bool := match outcome with
    | .success => ("succeeded",0,none,none,false)
    | .cancelled => ("cancelled",0,none,none,false)
    | .leaseLost => ("retrying",0,none,some "lease_released",true)
    | .failed status code retryable =>
      if retryable && claim.attempt < claim.job.retry.maxAttempts then
        ("retrying",claim.job.retry.delay claim.attempt claim.job.id,some status,some code,false)
      else ("dead",0,some status,some code,false)
  let result ← c.query "UPDATE public.leanweave_jobs SET state=CASE WHEN cancel_requested THEN 'cancelled' ELSE $5 END,available_at=clock_timestamp()+$6::bigint*interval '1 millisecond',attempt=CASE WHEN $9::boolean THEN greatest(attempt-1,0) ELSE attempt END,last_status=$7::int,last_code=$8,lease_owner=NULL,lease_token=NULL,leased_until=NULL,updated_at=clock_timestamp() WHERE id=$1 AND queue_name=$2 AND state='running' AND lease_owner=$3 AND lease_token=$4 AND leased_until>clock_timestamp() RETURNING id"
    #[some claim.job.id,some store.config.queue,some claim.owner,some claim.token,some state,some (toString delay),status.map toString,code,some (if decrement then "true" else "false")]
  return !result.rows.isEmpty

def Store.settle (store : Store) (claim : LeanWeave.Jobs.Lease) (outcome : LeanWeave.Jobs.Outcome) : AppM Bool :=
  store.pool.transaction fun c => settleOn store c claim outcome

/-- Public mutation/read APIs are tenant scoped; worker claims are privileged. -/
def Store.cancel (store : Store) (tenant id : String) : AppM Bool := do
  let result ← store.pool.query "UPDATE public.leanweave_jobs SET cancel_requested=true,state=CASE WHEN state IN ('queued','retrying') THEN 'cancelled' ELSE state END,updated_at=clock_timestamp() WHERE queue_name=$1 AND tenant=$2 AND id=$3 AND state IN ('queued','retrying','running') RETURNING id"
    #[some store.config.queue,some tenant,some id]
  return !result.rows.isEmpty

def Store.state (store : Store) (tenant id : String) : AppM (Option String) := do
  let result ← store.pool.query "SELECT state FROM public.leanweave_jobs WHERE queue_name=$1 AND tenant=$2 AND id=$3" #[some store.config.queue,some tenant,some id]
  match result.rows[0]? with | none => pure none | some row => return some (← col row 0)

/-- Explicit dead-letter replay for standalone jobs. Workflow compensations
must be retried through the workflow API so their state transition stays atomic. -/
def Store.retryDead (store : Store) (tenant id : String) : AppM Bool := do
  unless ← store.admission.isOpen do throw { status := 503,message := "job admission stopped" }
  let result ← store.pool.query "UPDATE public.leanweave_jobs SET state='queued',attempt=0,cancel_requested=false,last_status=NULL,last_code=NULL,available_at=clock_timestamp(),updated_at=clock_timestamp() WHERE queue_name=$1 AND tenant=$2 AND id=$3 AND state='dead' AND NOT EXISTS(SELECT 1 FROM public.leanweave_workflow_jobs m WHERE m.job_id=leanweave_jobs.id) RETURNING id"
    #[some store.config.queue,some tenant,some id]
  return !result.rows.isEmpty

/-- Explicit cleanup releases count/byte capacity and its idempotency reservation. -/
def Store.prune (store : Store) (olderThanMs : Nat) (limit : Nat := 1000) : AppM Nat := do
  if limit == 0 || limit > 10000 then throw { status := 422,message := "invalid prune batch bound" }
  store.pool.transaction fun c => do
    let _ ← c.query "SELECT name FROM public.leanweave_job_queues WHERE name=$1 FOR UPDATE" #[some store.config.queue]
    let result ← c.query "WITH picked AS (SELECT id FROM public.leanweave_jobs WHERE queue_name=$1 AND state IN ('succeeded','dead','cancelled') AND updated_at<to_timestamp($2::numeric/1000) AND NOT EXISTS(SELECT 1 FROM public.leanweave_workflow_jobs w WHERE w.job_id=leanweave_jobs.id) ORDER BY updated_at,id FOR UPDATE SKIP LOCKED LIMIT $3::int),removed AS (DELETE FROM public.leanweave_jobs WHERE id IN (SELECT id FROM picked) RETURNING retained_bytes) UPDATE public.leanweave_job_queues SET jobs=jobs-(SELECT count(*) FROM removed),bytes=bytes-coalesce((SELECT sum(retained_bytes) FROM removed),0) WHERE name=$1 RETURNING (SELECT count(*) FROM removed)::text"
      #[some store.config.queue,some (toString olderThanMs),some (toString limit)]
    match result.rows[0]? with | none => pure 0 | some row => number row 0

/-- Heartbeats use this dedicated control pool, separate from handler DB pools. -/
def Store.backend (store : Store) : LeanWeave.Jobs.Backend := {
  leaseMs := store.config.leaseMs
  maxPayloadBytes := store.config.maxPayloadBytes
  claim := store.claim
  renew := store.renew
  settle := store.settle
  busy := do
    let result ← store.pool.query "SELECT EXISTS(SELECT 1 FROM public.leanweave_jobs WHERE queue_name=$1 AND (state='running' OR (state IN ('queued','retrying') AND available_at<=clock_timestamp())))" #[some store.config.queue]
    match result.rows[0]? with | none => pure false | some row => return (← col row 0) == "t"
  close := store.pool.close }

/-- Native pools initialize on the dedicated worker and close there after drain. -/
def workerFactory (database : Postgres.Config) (registry : LeanWeave.Jobs.Registry) (config : Config := {})
    (admission : Option LeanWeave.Jobs.Admission := none) : Nat → IO (LeanWeave.Jobs.Backend × LeanWeave.Jobs.Registry) := fun _ => do
  let pool ← Pool.create { database with maxConnections := 1,maxWaiters := 1 }
  try
    let store ← Store.ofPool pool registry config admission
    match ← LeanWeave.Jobs.run store.initialize 30000 database.retainedBytes with
    | .error e => throw (IO.userError e.message)
    | .ok () => return (store.backend,registry)
  catch error => pool.close; throw error

end LeanWeave.Postgres.Jobs
