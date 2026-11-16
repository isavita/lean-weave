# Cache consistency

Create each `Cache.Store` on its reactor worker. Stores are bounded by entry
count, key+value bytes, entry bytes and in-flight key count. TTL expiry is lazy,
eviction is least-recently-used. Leases prevent duplicate generation on that
worker; contenders receive a retryable 503 rather than joining an unbounded queue.
Cancellation and errors release leases. Invalidating/clearing a store advances a
generation; an older in-flight fill cannot repopulate it.

Place cache middleware **after** trusted-proxy, tenant/locale, authentication and
authorization resolvers and after route selection. Outer middleware cannot see
Context changes made inside an inner resolver. Custom authentication must mark
Context.authenticated before lookup; this includes API-key and transport identities.

Response caching is opt-in middleware and also requires an explicitly `public`
200 GET response. HEAD reads the stored GET representation and keeps its full
length for the serializer; a HEAD miss does not populate GET storage. Route TTL
is capped by `s-maxage` or `max-age`, after Age. A stored Age increases on hits.
ETags and If-None-Match support 304 without a body. Error responses remain errors.
The built-in validator is a weak, noncryptographic content hash; applications may
supply their own validation policy when stronger content digests are needed.

Keys use length-prefixed host, tenant, route name, decoded path, ordered query
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

Fragment caching uses the same bounded store/lease rules. All isolation dimensions
must be present in application fragment keys: use `Cache.key` to construct them.
Never share personalized fragments using only a component name.

The separate Redis adapter provides shared server-side GET/SET PX/DEL and
expiring fill leases. See its README for timeout, failure, invalidation and
consistency rules. Distributed Redis storage is not silently layered below the
worker-local response cache.
