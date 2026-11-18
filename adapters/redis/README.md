# lean-weave-redis

Optional Redis adapter for LeanWeave. Import `LeanWeaveRedis`; declarations live
in `LeanWeave.Redis`. The core package does not load or require this adapter.

For an application using a local checkout, require `lean-weave-redis` with
`path = "../lean-weave/adapters/redis"` in its `lakefile.toml`. The example below
connects to Redis at its default `127.0.0.1:6379` endpoint and uses a separate key
namespace for the app.

For distributed channels, construct a `PubSub.Adapter` on its native owner and
use `adapter.channel authorize`. The resulting `Realtime.Channel` uses the same
typed `broadcast` API as a local channel, with WS/SSE subscriptions fed by Redis
messages. A successful `Publication` has `recipients := none`: it means an
outgoing publication was queued, not that Redis or a remote application received
it. PubSub is at-most-once during disconnect; presence uses periodic full CRDT
snapshots for anti-entropy. Call `adapter.tick` from native maintenance and
`adapter.close` from shutdown. The two-process test exercises `Channel.broadcast`
over an isolated real Redis and observes delivery on the other node's WS/SSE.

Create one `Client` in each reactor worker's application factory. Commands use
numeric IPv4/IPv6 addresses, nonblocking connect/send/receive, and bounded RESP2.
No DNS resolution, background tasks, subprocesses or borrowed reactor buffers
are used by requests. Each unfinished command owns one nonblocking connection
on its application worker. Completion, cancellation, timeout, protocol failure
and reservation rejection close it. The default is 16 concurrent commands per
client, with a 1-second operation deadline. There is no hidden retry queue.

```lean
import LeanWeaveRedis
import LeanWeave.Core.Server
import LeanWeave.Cache.Response

open LeanWeave

def app (_worker : Nat) : IO Handler := do
  let redis ← Redis.Client.new { namespacePrefix := "myapp:production:" }
  let origin : Handler := fun _ =>
    pure ((Response.text "dashboard").withHeader "cache-control" "public, max-age=5")
  let policy : Cache.Policy := {
    admit := some (fun ctx => pure (.cache ctx "public:dashboard")) }
  return Redis.middleware redis policy origin

def main : IO Unit :=
  Server.serve app { appRetainedBytes := 131072 }
```

`Client.retainedBytes` declares the AppM reservation for query storage,
response assembly, parsed bulk data and value-node overhead. Its default is
81,920 bytes; configure `Server.Config.appRetainedBytes` to cover it. The native
server separately charges the HTTP request snapshot. Exceeding either worker
queue or application reservation produces 503 and closes the command socket.
Each send/receive poll processes at most 16 KiB. RESP parsing bounds total bytes,
bulk length, line length, nesting depth and total value nodes. Defaults are
16 KiB for request/response/bulk, 1 KiB metadata lines, depth 16 and 256 nodes.
The parser supports simple strings, errors, integers, nullable bulk values and
non-null arrays. The command transport uses RESP2 and accepts exactly one reply.

`get`, `set` with mandatory finite `PX` TTL, and `delete` provide a byte cache.
Value and lease key spaces are disjoint under the configured application prefix.
`command` exposes binary-safe parameterized RESP command arguments, including
single-command atomic Lua evaluations. Each command uses a fresh connection:
connection-affine MULTI/EXEC transactions are not supported across calls. It
does not interpolate argument bytes into a command string. Error replies remain typed `Value.error`;
cache helper methods convert them into AppError 503.

`Redis.middleware client policy` caches explicitly public GET responses using
the core host/tenant/route/path/query/locale/Vary key and strict eligibility
checks. The `policy.admit` callback runs on every request, including hits; its
verified scope enters the key. The default policy bypasses caching. Complete
tenant resolution, authentication, authorization and feature checks before
returning `.cache`; use a verified tenant scope for tenant routes.
Authenticated/session/cookie traffic, private/no-store responses,
Set-Cookie, Vary:* and Vary dimensions outside `policy.vary` bypass storage.
The Redis adapter does not maintain a dynamic Vary variant index. HEAD reuses
GET metadata without filling on a miss. ETags and If-None-Match reuse the core
conditional response logic. Oversized fills bypass storage. `Redis.fragment`
provides the same public request isolation for named component bytes.

## Fill leases and consistency

`acquire key owner leaseMs` uses atomic `SET NX PX`, returning immediately with a
Boolean; its TTL must be 1–60,000 ms. Use a fresh cryptographically random owner
token of at least 32 bytes for each fill attempt. `release key owner` compares
the token and deletes the lease in one Lua evaluation. An expired producer
cannot release another producer's lease. Contention never blocks a reactor:
serve a known stale value or retry through bounded AppM operations according to
application policy. `publish` checks the owner token, writes the value with TTL and releases the
lease in one atomic evaluation. `delete` invalidates both the value and any
outstanding lease atomically. Consequently an expired, replaced or invalidated
producer cannot repopulate the key through `publish`. The response/fragment
middleware uses this protocol. For response invalidation, pass
`Redis.responseKey checkedContext verifiedScope policy` to `client.delete`.
Plain `set` is an unconditional low-level write
and does not provide that fill guarantee. Cancellation closes command sockets;
a fill lease remaining after cancellation expires within its bounded TTL.

All cache commands address one configured Redis primary. Each command is atomic
at that primary; separate concurrent command connections have no caller-defined
ordering. A successful awaited SET/DEL is visible to subsequent commands against
the same primary. Reads do not use replicas. Shared response envelopes use Unix epoch timestamps
for Age accounting; hosts should keep their clocks synchronized. Redis TTL is
the primary freshness limit. Failover, Redis persistence and
network partitions can lose acknowledged cache data; this is a disposable cache,
not an authoritative database. Timeout/disconnect after writing a command has an
unknown commit outcome. The adapter never retries mutations automatically.

Worker-local caches are independent. Redis writes and deletes do not magically
invalidate local L1 entries. Use Redis directly, version keys on updates, or
explicitly invalidate every worker and bound any remaining staleness with TTLs.
Include the full LeanWeave cache isolation dimensions in each key and preserve
public GET eligibility checks before storing or serving complete HTTP responses.
The adapter does not make an authenticated/private response cacheable.

Transport is plain Redis TCP. Use a private network or a locally terminating TLS
proxy and Redis ACLs as appropriate; no Redis TLS, AUTH/SELECT handshake, cluster
redirect or connection pooling is implicit in this adapter. Numeric addresses
are intentional: resolve names during startup outside reactor callbacks.

## Build and acceptance

Install the same native prerequisites as LeanWeave, plus `redis-server` for
integration acceptance. The test runner starts an isolated loopback instance in
a temporary directory, with persistence disabled, and terminates it afterward.
It does not install or start a background service.

```sh
lake -d adapters/redis update
lake -d adapters/redis build weave-redis-test
python3 adapters/redis/test/run.py
# Portable codec/native linkage checks when a Redis server is unavailable:
adapters/redis/.lake/build/bin/weave-redis-test --protocol-only
```

The native acceptance covers binary round-trips, namespace isolation, finite
TTL, invalidation, competing fill owners, atomic token release/publication, lease expiry,
public response/fragment caching, conditional GET/HEAD, private response bypass,
cache key isolation and invalidation during a suspended fill,
command capacity, AppM buffer reservation, timeout/disconnect cleanup, refused
connections, rejection of DNS hosts, malformed/fragmented RESP and 300-command
connection churn. Run on Linux/macOS with a local Redis binary; the socket shim
also includes a Winsock implementation, which still requires Windows execution
for platform acceptance.
