# Cache consistency

Create each response `Cache.ResponseStore` in its reactor worker's handler
factory. It uses a worker-local `IO.Ref`, hash indexes, and segmented LRU with
hard entry-count and accounted-byte limits. Accounted bytes include bodies,
headers, keys and fixed metadata, but exclude allocator and hash-table overhead.
TTL expiry is lazy. Each base key
has a bounded variant index. Response bodies stay as owned `ByteArray` values;
hits reuse that array without encoding or copying it in LeanWeave. lean-net
still copies or frames bytes for socket writes, HTTP/2, and TLS.
Key hashing, SLRU bookkeeping and response-header updates still allocate;
`weave-bench` reports Lean small-allocation counts for direct lookup and full
middleware hits.

Concurrent misses for the same base key share one worker-local fill. Followers
suspend without blocking the reactor, check the result against their own Vary
values, and start a bounded new fill when the variant differs. Flight and waiter
counts, wait time, and retained result bytes
are bounded; overload receives a retryable 503. Cancellation and errors wake
waiters. Invalidating or clearing a store advances a generation so an older
in-flight fill cannot repopulate it. Separate workers can fetch the same key
independently. `Cache.Store` remains the bounded fragment cache; its miss lease
does not coalesce followers.

Every cache policy must provide `admit`. The callback runs on **every** request,
including hits, before the cache key is computed. It must finish the route's
tenant resolution, authentication, authorization, tenant configuration and
feature checks, then return `.cache context scope`. Return `.bypass context` to
run the handler without caching, or throw `AppError` to deny access. The default
policy has no admission and therefore performs no lookup or
fill. Use a stable global scope only for a declared tenant-free public route;
tenant routes must use a verified tenant scope. Keep the callback after trusted
proxy normalization and route selection. A response marked `public` alone does
not grant cache admission. The scope is included in the stored key. Custom
authentication must also mark `Context.authenticated` so authenticated traffic
bypasses caching, including API-key and transport identities.
The admission callback is trusted application code: returning `.cache` before
an inner tenant or authorization middleware runs can still expose that inner
handler's response. Put all such checks in the callback or before the cache.

Response caching also requires an explicitly `public` 200 GET response. HEAD
reads the stored GET representation and keeps its full
length for the serializer; a HEAD miss does not populate GET storage. Route TTL
is capped by `s-maxage` or `max-age` after corrected Age from `Date`, `Age` and
fill delay. `Expires` applies when explicit freshness is absent. Invalid or
repeated freshness metadata bypasses storage. Corrected Age increases on hits.
ETags and If-None-Match support 304 without a body. Error responses remain errors.
The built-in weak validator uses SHA-256 of the body. An upstream ETag is
preserved.

Keys hash length-prefixed admission scope, host, tenant, route name, raw target,
decoded path, ordered query pairs, locale, version, tenant/feature revisions,
and Vary request dimensions. The response's `Vary` fields are parsed strictly;
each named request header, including opaque values and absent versus empty
fields, forms a variant key. Configured `Policy.vary` names add proactive key
dimensions. The original response `Vary` field is preserved. Header names are
case-insensitive. Cookies, Authorization, authenticated contexts, sessions,
GET bodies, Range and unsupported preconditions bypass caching. Request
`no-cache`, `no-store`, `max-age`, `min-fresh`, and `Pragma: no-cache` also
bypass. `private`, `no-store`, `no-cache`, malformed `Cache-Control` or `Vary`,
`Vary: *`, and Set-Cookie responses bypass it.

Local invalidation is immediate on the owner worker. Separate workers may retain
their own value until TTL expiry; local stores provide TTL-bounded eventual
consistency, not cross-worker linearizability. Applications requiring immediate
multiworker invalidation must broadcast it through bounded worker-local queues,
or use the Redis adapter without an L1 response store. Include a deployment/data
version in keys when invalidation must survive restarts.

`Cache.requestFragment` requires the same per-request admission. Low-level
`Cache.fragment` remains an explicit-key primitive: include every isolation
dimension with `Cache.key`. Never share personalized fragments using only a
component name.

The separate Redis adapter provides shared server-side GET/SET PX/DEL and
expiring fill leases. See its README for timeout, failure, invalidation and
consistency rules. Distributed Redis storage is not silently layered below the
worker-local response cache.
