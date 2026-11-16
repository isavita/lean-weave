# Cache consistency

Create each `Cache.Store` on its reactor worker. Stores are bounded by entry
count, key+value bytes, entry bytes and in-flight key count. TTL expiry is lazy,
eviction is least-recently-used. Leases prevent duplicate generation on that
worker; contenders receive a retryable 503 rather than joining an unbounded queue.
Cancellation and errors release leases. Invalidating/clearing a store advances a
generation; an older in-flight fill cannot repopulate it.

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
is capped by `s-maxage` or `max-age`, after Age. A stored Age increases on hits.
ETags and If-None-Match support 304 without a body. Error responses remain errors.
The built-in validator is a weak, noncryptographic content hash; applications may
supply their own validation policy when stronger content digests are needed.

Keys use length-prefixed admission scope, host, tenant, route name, decoded path, ordered query
pairs, locale, version and configured Vary request dimensions. Header names are
case-insensitive. Cookies, Authorization, an authenticated context or a session
bypass caching. `private`, `no-store`, `no-cache`, `Vary: *`, unknown Vary
dimensions and Set-Cookie responses bypass it. Duplicate cache-control/Vary
fields are considered together; duplicate configured request Vary fields bypass.

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
