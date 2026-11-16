# LeanWeave

Typed web applications and application services in Lean 4, built over [lean-net](https://github.com/isavita/lean-net).
Package: `lean-weave`. Namespace: `LeanWeave`. Lean: `v4.35.0`. License: Apache-2.0.

[API reference](https://isavita.github.io/lean-weave/) · [Application platform](docs/platform/operations.md) · [Execution and security](docs/contracts.md) · [Cache consistency](docs/caching.md) · [Examples](Examples/README.md) · [Acceptance and benchmarks](docs/acceptance.md)

```lean
import LeanWeave

open LeanWeave
open scoped LeanWeave.Router

private def greeting : String → Html.Html := html! "<h1>Hello {{name}}</h1>"
private def hello (name : String) : Handler := fun _ =>
  pure (Html.toResponse (Html.page "Welcome" (greeting name)))

weave_routes routes where {
  route "GET" "/hello/:name<text>" as hello => hello
}

def main : IO Unit :=
  Server.serve (fun _ => pure routes.handler)
```

`routes.helloUrl "Lean <4>"` generates an encoded URL. Route names, capture types,
unsafe HTML attributes, template markup and route ambiguities are checked during
compilation. User text is escaped; URL attributes require `Html.SafeUrl`.

## Build and run

Install Lean with elan, plus a C compiler, `pkg-config`, zlib and Brotli development
libraries. On Linux: `apt install zlib1g-dev libbrotli-dev pkg-config`. On macOS:

```sh
brew install zlib brotli pkgconf
export PKG_CONFIG_PATH="$(brew --prefix zlib)/lib/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"
lake build
lake exe weave-test
lake build weave-pages weave-forms weave-crud weave-dashboard
lake exe weave-pages
```

The dependency is pinned to lean-net's streaming/lifecycle commit. A sibling checkout
is unnecessary. Windows build steps are in `.github/workflows/ci.yml`.

```sh
lake exe weave new /tmp/my-app
cd /tmp/my-app
/path/to/lean-weave/.lake/build/bin/weave build
/path/to/lean-weave/.lake/build/bin/weave routes
/path/to/lean-weave/.lake/build/bin/weave test
/path/to/lean-weave/.lake/build/bin/weave dev
```

Scaffolds use the selected local LeanWeave checkout (`--path` or `WEAVE_PATH`).
`dev` watches a bounded source set, rebuilds, preserves the running app after a
failed build and gracefully restarts after a successful build.

Startup variables: `WEAVE_HOST`, `WEAVE_PORT`, `WEAVE_WORKERS`,
`WEAVE_APP_TIMEOUT_MS`, `WEAVE_APP_RETAINED_BYTES`. Invalid values fail startup.
Default listener: `127.0.0.1:8080`; shutdown uses lean-net's bounded drain.

## Application model

`Handler := Context → AppM Response`; `Middleware := Handler → Handler`.
`Context` contains owned request data, decoded path/query, typed route captures,
tenant/locale, request identity and session attributes. Proofs remain in `Prop`.

AppM is a cooperative continuation monad. `AppM.await` suspends until a bounded,
nonblocking adapter poll completes; it never creates a thread per request. Callbacks,
continuations, worker caches and native sockets stay on their owner worker.
Immediate IO must be bounded and nonblocking. Arbitrary blocking IO does not become
asynchronous merely by lifting it into AppM.

Compose `pipeline #[...] handler` once. Use route groups for browser/API/admin
pipelines. Supplied middleware covers errors, IDs, proxy trust, buffered logging,
metrics, authentication, authorization, session loading, CSRF, CORS and security
headers. `Server.Config.headerChecks` and `onHeaders` run before body intake on
HTTP/1 and HTTP/2. Decode and validate mutation bodies afterward.

`Body.form`, `Body.json`, `Body.multipart` enforce byte/field/depth/part bounds.
`Validation` constructs typed results and accumulates field errors. Sessions use
OS-random 256-bit opaque IDs, expiry, rotation and HttpOnly/Secure/SameSite cookies.
Session storage is shared across workers with fail-fast try-lock admission.

## Routing and pages

The DSL supports typed `:id<nat>`, `:id<int>`, `:slug<text>` and final `*rest`
captures, host scopes, nested groups, named URL helpers and resource controllers.
Precedence: exact host, longest host suffix, any host; then literal segment,
natural, integer, text, final rest. Ambiguous equal-precedence overlaps are build
errors. Paths decode once; encoded slash/backslash, dot segments, malformed UTF-8
and malformed percent escapes fail closed. Trailing slash is significant.
404/405/Allow, GET-backed HEAD, per-resource OPTIONS and `OPTIONS *` are built in.

HTML components, layouts and partials render into ByteArray builders. `html!`
compiles literal markup, precomputes static bytes and infers String or SafeUrl
slots. `Html.rawUnsafe` is the explicit raw HTML escape hatch. Form/error/flash,
negotiation, redirects and fingerprinted asset URL helpers are included.

The routing, ordered pipeline and caching designs draw on
[Phoenix](https://phoenix.hexdocs.pm/routing.html),
[Plug](https://plug.hexdocs.pm/Plug.html) and
[Rails](https://guides.rubyonrails.org/caching_with_rails.html).

## Optional adapters and transport

- [Redis](adapters/redis/README.md): deferred, bounded RESP operations and distributed fill leases.
- [PostgreSQL](adapters/postgres/README.md): nonblocking libpq pools, parameterized queries, transactions and migrations.
- [TLS](adapters/tls/README.md): optional wrappers use lean-net's native TLS worker, ALPN and certificate reload. OpenSSL 1.1.1 or newer is required; 3.x and 4.x are tested.
- Compressed static files: `Server.serveStatic` mounts a sealed catalog with gzip/Brotli and file regions; [composition](docs/transport.md).
- Admission/rate limits, access logs and Prometheus metrics use `Server.Config.net`.

Background jobs, realtime and plugins are explicit application runtimes: importing
LeanWeave does not start workers, listeners or services. PostgreSQL, Redis, outbound
HTTP/Vault/OTLP, TLS and native plugin loading remain separate Lake packages.
Mail can be supplied by an application adapter.

## Application platform

- [Realtime](LeanWeave/Realtime/README.md): bounded channels, local/Redis PubSub, causal CRDT presence, replaying SSE, typed Live components and WebSocket/SSE transport.
- [Jobs and workflows](docs/platform/jobs-workflows.md): typed payloads, dedicated owner workers, retry/dead letters, cron timers, durable PostgreSQL outbox and compensating workflows.
- [Tenancy and operations](docs/platform/operations.md): verified tenant resolution, typed SQL/RLS scopes, dynamic tenant settings, sticky flags and audited mutations, tracing/OTLP, sampled logs, probes, drain, rotating Vault secrets and typed CSP/authorization policies.
- [Protocols and API documentation](docs/platform/protocols.md): checked schemas, generated OpenAPI/Swagger, native protobuf and gRPC/gRPC-Web unary/streaming methods.
- [Native plugins](adapters/plugins/README.md): versioned C ABI, per-generation request/stream leases and development reload without restarting the listener.

`Server.Application` builds streaming, WebSocket and lifecycle callbacks on their
owner. `Server.serveApplications`, `TLS.serveApplications` and their static variants
bracket the complete runtime. `Operations.Runtime` shares one native drain deadline
among participants. Bounded stores reject overload instead of silently growing.

```sh
lake build weave-platform-test weave-jobs-test weave-stream-test
lake exe weave-platform-test
lake exe weave-jobs-test
lake exe weave-stream-test
python scripts/benchmark.py --platform --iterations 5000 --output benchmarks/platform-results.json
```

[The 27-item acceptance register](docs/platform/acceptance.json) links each requested
platform feature to its implementation and evidence. Reports state their actual
platform and source hashes; passing local evidence does not assert unrun CI or
cluster failover guarantees. Live state is owner-local and needs one worker or
sticky routing; distributed PubSub and presence have separate consistency rules.
