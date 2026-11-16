# Jobs, cron, workflows and transactional outboxes

Import `LeanWeave.Jobs` and `LeanWeave.Workflow`. PostgreSQL persistence is optional:
add the `adapters/postgres` package and import `LeanWeavePostgres.Jobs` and
`LeanWeavePostgres.Workflows`.

## Typed execution and ownership

`Jobs.Definition.typed` decodes, validates and canonicalizes a `FromJson`/`ToJson`
payload both before admission and before execution. A registry has at most 256
unique kinds. Envelopes own JSON and bounded tenant/request/trace identifiers;
they never contain a request `Context`, native socket or borrowed receive buffer.

```lean
structure Resize where
  imageId : Nat
  deriving Lean.FromJson, Lean.ToJson

def resize : Jobs.Definition := Jobs.Definition.typed "image.resize"
  (fun (p : Resize) => if p.imageId == 0 then .error "image required" else .ok p)
  (fun context payload => do
    -- Use context.jobId as the external effect's idempotency key.
    pure ())
```

`Jobs.performSync` runs on its caller and is intended for CLI/dedicated owners.
`Jobs.Queue` is a bounded, thread-safe process-local store. Admission and queue
locks fail fast with 503, allowing callers to apply their own bounded backpressure.
Count, bytes, payload bytes, terminal history and history TTL have explicit limits.
Capacity pressure evicts terminal history oldest first; pending work is never
silently evicted. Idempotency lasts only while that bounded history is retained.
Local queue get/cancel/retry are privileged administrative operations, so an
application must enforce its tenant access before exposing them.

`Jobs.withOwnedWorkers` creates a fixed number of dedicated workers (1–64), calls
each `Nat → IO WorkerResources` factory on its owner, and closes its backend on
that same owner, including setup/producer failures. Construct PostgreSQL pools,
tracers, logger state and hook closures inside this factory. `Workers.start` also
accepts an optional per-owner `hooksFactory`; its compatibility shared Hooks value
must be thread-safe. Queue-level producer hooks must likewise be thread-safe if
multiple owners enqueue. No task is created per job.

Handlers use cooperative `AppM.await`; timeouts, cancellation and lease heartbeats
are checked between polls. Arbitrary blocking user IO cannot be safely killed by
Lean, and therefore cannot offer a bounded shutdown guarantee. Backend operation
and retry windows are bounded, with capped exponential backoff and jitter. Dead
letters retain status/code and allow explicit operator retry.

## PostgreSQL durability

`Postgres.Jobs.Store.ofPool` attaches an owner-local pool to a registry, queue
limits and an optional shared admission gate. Call `store.initialize` once before
serving. All processes using a queue must agree on its configured bounds.

`Postgres.Workflows.install` installs jobs, workflow state and the transition
outbox. The provided job/workflow `workerFactory` creates and closes native pools
inside dedicated workers. Applications should perform schema installation before
readiness; production roles need the corresponding DDL privilege during that step.

Claims use `FOR UPDATE SKIP LOCKED`; PostgreSQL time determines due/expiry times.
Every claim gets a new fencing token. Renewals and acknowledgements require its
owner, token and unexpired lease. A crash leaves a row for expiry recovery. A
bounded SKIP LOCKED cleanup pass handles exhausted/cancelled expired leases.
Shared transactional counters enforce retained row/byte bounds across processes.
Terminal rows remain charged until explicit `store.prune`; no live row is pruned.
Tenant-scoped get/cancel/retry operations require the tenant string explicitly.

Delivery is **at least once**. Fencing prevents stale acknowledgements; it cannot
undo an external effect performed before a crash. Use `context.jobId` at an
idempotent external service, or commit database effects and an application dedupe
row in one transaction. This is not a claim of exactly-once arbitrary external IO.
Idempotency keys are scoped by queue and tenant; reusing one with a different
kind/payload is a conflict. Durable dedupe lasts until explicit retention removal.

Business writes and publishing use the **same connection**:

```lean
pool.transaction fun connection => do
  let _ ← connection.query "INSERT INTO orders(id) VALUES($1)" #[some orderId]
  store.enqueueOn connection envelope
```

Rollback removes both effects; commit makes both visible. `enqueueOn` must be
inside a caller-owned transaction; `store.enqueue` opens its own transaction when
there is no business write to combine. Workflow admission has the equivalent
`Postgres.Workflows.enqueueOn store connection flow` entry point.

## Cron on native timer pulses

`cron! "*/15 0-6 * * 1-5"` is checked during elaboration. Five fields use UTC:
minute, hour, day of month, month, weekday. Lists, ranges and steps are supported;
Sunday is 0 or 7. Restricted day-of-month/day-of-week fields use OR. DST/local
time zones are deliberately not inferred.

Create a `Jobs.Cron.Scheduler` from bounded `Spec`s and an enqueue callback.
`Scheduler.application scheduler app` mounts it on lean-net's native `onTick`
timer pulse and composes existing application callbacks. The callback submits at
most `maxEmissionsPerTick`; failed admissions remain pending. `nextDue` gives the
next UTC minute or a 100ms retry for backlog. `catchUpMinutes` caps missed replay;
older missed occurrences are counted as skipped. Drain stops new occurrences.

A reactor scheduler's submit callback must be nonblocking, such as
`Queue.enqueue`. A PostgreSQL scheduler must own a dedicated scheduler thread and
connection rather than synchronously driving database IO in a reactor callback.
Duplicate distributed schedulers use the durable queue's stable
`cron:<schedule-name>:<UTC-minute>` idempotency key. Use distinct schedule names
where different jobs must have distinct occurrences.

## Workflow state and compensation

`Workflow.Plan` describes a bounded DAG (128 steps, 4096 dependencies, one MiB
definition). Validation rejects duplicate/missing/self dependencies and cycles.
Each step has a typed registry action, timeout/attempt policy and optional
compensation. Stable forward and compensation job IDs are allocated before
publication. Pure state transitions are serializable through Lean JSON.

Successful dependencies release waiting steps. Failure cancels remaining forward
work, waits for running jobs to settle, then compensates successful steps one at a
time in reverse topological order. A failed step must clean up its own partial
effects; compensation undoes acknowledged successful steps. Failed compensation
is an explicit terminal state requiring operator `retryCompensation`, and preserves
its job ID. Cancellation enters this same state machine rather than deleting work.

The local `Workflow.Engine.backend queue` acknowledges queue completion and records
the resulting transition atomically with respect to retained job history, then
retries persisted ready actions under queue backpressure. Keep the engine and
queue admission gate shared. Use `Engine.cancel`, rather than independently
deleting or retrying a workflow-owned job.

PostgreSQL uses a terminal-job trigger to append a workflow transition event in
the **same transaction as the acknowledgement**. Reconciliation locks each flow,
updates its state, publishes/cancels dependent jobs and removes the consumed event
in one transaction. A process killed after ACK is recovered by any worker replaying
that durable transition outbox. Initial workflow admission reserves row/byte
capacity for **all forward and defined compensation jobs**, so later dependencies
can publish even when the public queue is full. Already-admitted continuations
can finish after public admission stops. Explicit terminal `Workflows.remove`
releases all reservations, including actions that were never published.

## Lifecycle and observations

`Workers.stopAdmission`, `beginDrainAt absoluteMonotonicDeadline`, `requestClose`,
`snapshot` and `pollDrain` are nonblocking lifecycle operations. `Workers.application`
composes them with `Server.Application`, using the native server's exact cutoff.
Bracket `serveApplications` in `withOwnedWorkers` so the final join occurs after
the reactor returns. `drain` and `close` are bounded blocking helpers for CLI/final
cleanup, and report incomplete work rather than claiming success. Local future
jobs keep drain incomplete and can be exported with `Queue.pending`; durable
future rows safely remain in PostgreSQL across a successful process drain.

`WorkerSnapshot` reports readiness, active/completed/errors, admission and forced
shutdown. `Hooks.startSpan` starts before execution and supplies the child
traceparent passed to the handler; `Span.finish` runs after the outcome. Events
carry job ID, kind, tenant, request ID, traceparent, attempt, duration, state and
optional error status/code. Automatic hooks never include payloads, credentials
or raw exception messages. `Backend.tick` runs on the job owner for bounded
exporter polling; backend close also runs there.

## Reproducible acceptance

```sh
lake build weave-jobs-test
lake -d adapters/postgres build weave-pg-jobs-test
python3 adapters/postgres/test/jobs_cluster.py --native --output docs/platform/jobs-result.json
```

PostgreSQL development tools (`pg_config`, `initdb`, `pg_ctl`, `psql`) are required.
The test creates a disposable localhost cluster and removes it afterward; it does
not start a persistent service. On this macOS environment, native build commands
also use `PKG_CONFIG_PATH=/opt/homebrew/opt/zlib/lib/pkgconfig:/opt/homebrew/opt/openssl@3/lib/pkgconfig`.

The source-bound JSON records the tested platform/PostgreSQL version and SHA256
of relevant modules. Checks execute real dedicated workers, bounded contention,
typed rejection, retry/dead letters, stale fencing, producer failure cleanup,
workflow compensation/retry, native cron timer dispatch, two processes/four PG
workers, transactional rollback, killed-worker lease recovery, and a process
killed after ACK before workflow transition. Real SIGTERM tests cover both
queued/running completion and forced cancellation at a shared 150ms cutoff,
native owner-guarded PostgreSQL close and clean process exit. These are local
native acceptance results; they do not assert unrun operating-system results.
