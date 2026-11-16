# Tenants, flags, observability and operations

LeanWeave 0.2 keeps application continuations on their reactor owner. Construct
owner-local tracers, loggers, caches, SQL/HTTP pools, secret managers and plugin
managers inside `Server.serveApplications` (or `TLS.serveApplications`). Use
`serveStaticApplications` to retain native compressed file serving alongside
application hooks. Shared tenant/feature configuration uses bounded try-lock
snapshots; it never runs application callbacks while holding the lock.

## Tenants and feature configuration

`Tenant.middleware resolver authorize` requires an explicit access policy.
Subdomain, decoded path, header and verified JWT sources identify a tenant;
identification alone does not authorize access. `Tenant.agree` rejects conflicting
sources. `Tenant.fromPath` consumes already decoded segments without decoding a
second time. `Tenant.fromJwt` verifies an HS256 JWT before reading `tenant_id`;
keys need at least 32 bytes. Issuer, audience, lifetime and clock bounds are
mandatory verification inputs. JWT objects reject duplicate top-level keys,
including escaped spellings, before JSON canonicalization.

JSON decoders preflight numeric literals before Lean's arbitrary-precision
parser. Defaults bound mantissas and canonical decimal forms to 512 digits and
explicit/effective denominator exponents to 1024; configurable limits cannot
exceed 4096. This includes finite IEEE754 boundary values while rejecting tiny
inputs such as `1e1000000000` before expansion. Accepted numbers survive bounded
render/parse round trips; numeric zero is normalized before rendering. The same
guard applies to JWT, audit replay, stored jobs/workflows, plugin headers,
GraphQL numeric literals and OTLP responses. Numeric-looking strings stay strings.

`Tenant.Store.set` replaces a validated snapshot and advances its revision.
Compose `Tenant.configured`, `Tenant.RateLimiter.middleware` and
`Feature.middleware` once. Configuration changes affect subsequent requests.
A request retains its own immutable snapshot. Brand text is HTML-escaped; CSS
colors accept only validated `#RRGGBB` values.

Feature cohorts use SHA-256 over length-delimited flag name, salt and stable
identity. Authenticated principals or verified session identities supply the
subject. Anonymous callers without a verified subject share the tenant cohort.
Global disable wins over all overrides; role, tenant and principal conditions
also apply before tenant overrides. Feature gates do not replace authorization.
`auth_policy! all [authenticated, any [role "admin", permission "report.read"], tenant "alpha"]`
compiles a checked authorization policy; attach `Policy.middleware`. Empty groups,
invalid role/permission names, invalid tenant IDs and oversized/deep policies fail
at compile time. Role/permission tests imply authentication. Boolean negation has
its usual meaning, so combine negative conditions with `authenticated` when needed.
Only trusted middleware should mint role and permission attributes.

Every `Feature.Store.define flag actor` records actor, timestamp and full bounded
before/after snapshots atomically with the change. The default actor is `system`
for startup/internal changes. Audit count/byte exhaustion rejects the mutation.
Export `Store.audit` and acknowledge only a persisted, verified checkpoint.
Tenant configuration set/remove operations audit their full snapshots too, including
flag overrides. Both stores are shared within a process: a global flag disable
affects every subject on that store immediately for subsequent requests. Cross-node
configuration propagation is an application responsibility; no distributed flag
configuration backend or deployment-wide instantaneous delivery is implied.

Local and Redis response/fragment cache keys include tenant configuration
revision, a canonical configuration fingerprint, the original raw request target
and feature variant/version digest. Keeping raw bytes alongside decoded route/query
dimensions avoids collisions for protocols with different query-form conventions.
The fingerprint prevents stale reuse after a process restart; a monotone revision
watermark prevents remove/recreate reuse within a store. `tenant.cache.enabled=false` or
TTL zero bypasses reads and writes; tenant TTL can tighten a route's maximum.
`Cache.requestFragment` applies these request dimensions to local fragments.
The low-level `Cache.fragment` API still accepts an explicit application key.
Request contexts containing a CSP nonce and responses containing CSP/report-only
nonce sources bypass caching in either middleware order. Request-aware fragments
also bypass nonce-bearing contexts. Existing public-response, authentication/session, Vary and Set-Cookie exclusions
continue to apply. Worker-local cache invalidation remains local; Redis is the
shared authoritative store without an untracked L1.

## SQL isolation and lifetime

The separate `lean-weave-postgres` package exports `Postgres.Tenant.withTenant`.
A `Scoped tenant` accepts only `Tenant.Sql.Command tenant`. The checked builders
inject tenant parameters into reads, inserts, updates and deletes, reject tenant
column overrides and validate identifiers. Values remain SQL parameters.
Scoped callers cannot obtain the raw connection through the public API.

Run `Postgres.Tenant.migrationStatements table` with the schema-owner role before
starting application pools. It enables and forces RLS, installing a restrictive
tenant boundary that existing permissive policies cannot widen. Application
roles must be non-superusers without BYPASSRLS. Each scoped transaction uses
transaction-local `leanweave.tenant_id`, which resets before pool reuse.

Connections and scopes are valid only during their acquisition. Per-acquisition
epochs prevent an escaped old capability from becoming valid when the physical
connection is borrowed again. The type index protects ordinary application code;
this is not a sandbox against hostile code linked into the same process or a
substitute for least-privilege database roles and correctly applied RLS migrations.

## Tracing and structured logs

Create an `Observability.Tracer` on each owner and compose its middleware.
It validates W3C traceparent, respects remote sampling and applies reloadable
root sampling to locally generated contexts. Native trace provenance preserves
the native access-log trace ID without treating its synthetic unsampled header
as a remote sampling decision. Context propagation uses immutable request values.
Stream spans remain active until stream completion, HEAD suppression or terminal
cancellation; span finish is idempotent.

Pass the tracer to `Postgres.Pool.create`, then use `pool.withTraceContext ctx`
per request. Pass it to `Http.Client.create` and supply `some ctx` when issuing
HTTP calls. `Observability.jobsHooks` starts actual consumer spans before job
execution. Construct these hooks in `Jobs.Workers.startOwned`; use `Backend.tick`
to pulse that owner's exporter and close it in the same backend's finalizer.
Never share one owner-local tracer/logger among independent job threads.
SQL text/parameters, request bodies and credentials are not exported.

`Observability.Exporter` retains one bounded batch and one cooperative operation.
The optional HTTP adapter provides `Http.otlpTransport`. Exporter calls suppress client instrumentation to avoid recursive spans,
even when a caller supplies an instrumented HTTP client. Pulses run from `onTick`; the
participant drains within the runtime's absolute cutoff. Retryable failures use
bounded backoff/attempts. Retries are at-least-once and can duplicate accepted
spans after an ambiguous network failure. OTLP partial success is terminal.

`Observability.Logger` emits bounded JSON lines with request, tenant, authenticated
principal and trace context. Sampling/minimum level can change without rebuilding
middleware. Error events survive sampling; producer capacity still applies.
Use stable event codes rather than exception strings or secret-bearing messages.
Known sensitive field names are redacted. `FileWriter` owns one dedicated append
thread; `Logger.flushTo` and its runtime participant use nonblocking admission.
Queue entries and in-flight records both count against byte/record limits.
File receipts mean successful buffered write plus OS flush, not filesystem
power-loss/fsync durability. Abort is observed between writes; it cannot interrupt
a blocked kernel filesystem write/flush. A stalled filesystem can therefore keep
the dedicated task alive after a timed-out join. The drain budget is cooperative
under responsive I/O, rather than an unconditional hard process-exit guarantee. Join the writer after the reactor has returned;
request callbacks never join threads or write files.

## Audit journal

`Security.Audit` produces canonical JSON records with sequence, predecessor hash,
metadata and SHA-256 digest. The bounded ledger never evicts unacknowledged
records. `Audit.Journal.open` locks a sidecar for exclusive chain ownership,
verifies the bounded existing journal before startup, resumes its sequence and
uses a dedicated append writer. `Journal.record` can be awaited before performing
a security-sensitive action. Disconnect or timeout stops the caller's wait but
does not erase an admitted event. Writer failure causes subsequent operations
to fail closed. File and memory limits require deliberate archival/rotation.

Store the last checkpoint in an independently trusted, versioned location and
pass it when reopening/verifying. A bare hash chain detects changes, reordering
and deletion within a retained chain; it cannot detect a truncated tail or
complete replacement without that external anchor. Metadata must exclude secrets
and payloads. The filesystem remains writable by its owner; tampering is detected
by verification, not prevented by file permissions or the hash function.

## Secrets and native outbound HTTP

The optional `lean-weave-http` package uses libcurl's native multi interface with
numeric connection addresses. `hostName` supplies the TLS/SNI identity while
`hostAddr` supplies a startup-resolved numeric IP. Requests do not follow
redirects, consult proxy environment variables or perform DNS on the reactor.
Request/response/header sizes, active operations and timeouts are bounded.
Set `Server.Config.appRetainedBytes` to at least `Http.Config.retainedBytes`
when awaiting these operations; the default generic handler reservation is smaller
than the default HTTP adapter reservation. Keep the worker deferred byte budget
large enough for the configured number of concurrent reservations.

For strict reactor TLS, supply a PEM `caBundle`: the client preloads bounded CA
bytes at startup and each transfer uses an in-memory CA blob. Use a curl TLS
backend supporting CA blobs (the acceptance configuration uses OpenSSL).
Unsupported backends fail closed. `Endpoint.systemTrust=true` is an explicit
opt-in to the TLS backend's system trust behavior, which can perform filesystem
or system-store work during connection setup. Peer and hostname verification
remain enabled in either mode. On macOS select Homebrew curl through pkg-config
rather than the system SecureTransport build when using strict CA blobs.

`Http.Vault.backend` implements KV v2 with certificate-verified TLS. Bind named
sources to a mount/path/field and give `Secrets.Manager` its backend. Timer pulses
load and rotate bounded values. The bootstrap token callback is re-read for every
refresh, allowing credential rotation without restart. Old values remain usable
only until their original lease expiry; failed refresh never extends a lease.
KV v2 has no native dynamic-secret lease, so each binding explicitly supplies the
application's refresh/expiry interval. This adapter does not claim Vault dynamic
lease renewal or AWS support.

Secret values and tokens have no Repr/JSON instance and are held only in memory;
there is no plaintext staging file. `Secret.use` checks expiry before exposing
bytes to a deliberate callback. Callbacks must not log/persist those bytes. Lean
immutable storage does not promise physical zeroization of every collected copy.

## Health and shutdown

`Operations.Health.middleware` mounts `/health/live`, `/health/ready` and
`/health/startup`. Startup is explicit, readiness also requires healthy required
dependencies and no drain, and liveness stays successful during startup/drain.
Dependency owners publish connectivity snapshots; probes never perform network
checks on a request worker. Keep `Server.Config.net.healthPaths=false` (the default)
when mounting these dependency-aware probes, so native transport-only probes do
not shadow the application routes. Mount these probes outside authentication.

`Operations.Runtime.application` composes bounded maintenance, drain and cleanup
participants with an application. Native SIGTERM stops listener admission and
supplies one absolute `IO.monoMsNow` deadline to every participant. Jobs stop new
admission, finish admitted work while budget remains, then cancel and release
leases for recovery. `Jobs.Workers.application` and `Cron.Scheduler.application`
connect those APIs directly to native hooks. Finalizers execute once in reverse
participant order, even when another finalizer fails. Bracket pools/workers and
join dedicated threads after the server returns. Callbacks must remain
cooperative; arbitrary blocking user code cannot be forcibly preempted by Lean.

## Reproduce acceptance and allocations

Use Python 3.14 with the hash-locked `scripts/requirements-test.txt` HTTP/2 codec.
Install the native packages used by the relevant adapter and set pkg-config to
the matching codec/OpenSSL/curl installation. The following creates only temporary
service processes and data directories; it does not start a persistent service:

```sh
python test/platform/fetch_tools.py
export WEAVE_VAULT_BINARY="$HOME/.cache/leanweave-test-tools/vault-1.20.0/vault"
export WEAVE_OTELCOL_BINARY="$HOME/.cache/leanweave-test-tools/otelcol-0.131.0/otelcol"
export WEAVE_PG_BIN="$(pg_config --bindir)"
python test/platform/run.py --postgres --http --external
python scripts/benchmark.py --platform --iterations 5000 --output benchmarks/platform-results.json
```

The fetcher pins test-only vendor versions and verifies their SHA-256 manifests.
`platform-result.json` records source hashes and commands; separate reports cover
119 independent Python crypto vectors, actual non-superuser PostgreSQL TLS/RLS,
real Vault secret/token rotation and an official Collector receiving six real
request/SQL/HTTP/job spans. Browser CSP enforcement is in realtime evidence.
Local evidence names its operating system; CI artifacts establish other runs.
Benchmarks report native wall time and thread-local small-allocation deltas,
including loop/IO scaffolding. They exclude native malloc, large buffers, sockets,
TLS and RSS and are not an end-to-end throughput or production latency claim.
