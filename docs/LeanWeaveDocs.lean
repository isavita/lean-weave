import LeanWeave
import LeanWeave.Test
import LeanWeavePostgres
import LeanWeaveRedis
import LeanWeaveTLS
import LeanWeaveHttp
import LeanWeavePlugins

/-!
# LeanWeave

A typed, cooperative Lean web application platform over lean-net. Start with
`LeanWeave.Server.Config` and `LeanWeave.Server.serve`. Build named routing tables
with `weave_routes`, checked HTML with `html!`, and compile ordered middleware
once using `LeanWeave.pipeline`.

Application continuations, cache stores and adapters remain on their owner
worker. `LeanWeave.AppM.await` suspends without creating per-request threads.
The optional adapters are documented here too: `LeanWeave.Postgres.Pool` provides
bounded PostgreSQL queries, transactions and migrations; `LeanWeave.Redis.Client`
and its cache helpers provide deferred Redis operations; `LeanWeave.TLS.serve`
and `LeanWeave.TLS.serveStatic` preserve native TLS and compressed static output.
`LeanWeave.Server.serveApplications` builds realtime streams and lifecycle hooks
on their owner. The platform includes Channels, CRDT Presence, Live components,
typed jobs/cron/workflows, tenant SQL scopes, dynamic feature settings, tracing,
audit journals, health probes and typed security policies. Schema/protobuf DSLs
generate checked values and API documentation; gRPC supports all four method shapes.
`LeanWeave.Http.Client` adds bounded native outbound HTTP, OTLP and rotating Vault
KV v2 values. `LeanWeavePlugins` provides a development native loader with generation
leases. Applications only link native adapters when their packages are required.
See the [getting started guide](https://github.com/isavita/lean-weave/blob/main/docs/getting-started.md)
for a minimal Lake project and the [runnable examples](https://github.com/isavita/lean-weave/tree/main/Examples)
for pages, forms, CRUD and caching.
-/
