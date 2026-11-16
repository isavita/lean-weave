import LeanWeaveRedis.Client
import LeanWeave.Cache.Response
import LeanWeave.Session.Store
import Std.Time.DateTime.Timestamp

namespace LeanWeave.Redis

private def epochMs : IO Nat := do
  return (← Std.Time.Timestamp.now).toMillisecondsSinceUnixEpoch.val.toNat

private def cached (ctx : Context) (policy : Cache.Policy) (bytes : ByteArray) : IO (Option Response) := do
  let some (response, insertedAt) := Cache.decodeResponse bytes | return none
  if !Cache.eligibleResponse policy response then return none
  let age := ((response.getHeader "age").bind String.toNat?).getD 0
  let age := age + ((← epochMs) - insertedAt) / 1000
  return some (Cache.conditional ctx (response.withHeader "age" (toString age)))

private def releaseQuietly (client : Client) (key owner : String) : AppM Unit := do
  try let _ ← client.release key owner; pure () catch _ => pure ()

/-- Public response caching with the core isolation and eligibility policy.
Redis is the shared authoritative cache layer; no local L1 is retained. Fill
publication atomically checks lease ownership, so invalidation and lease expiry
prevent a late fill from repopulating the key. -/
def middleware (client : Client) (policy : Cache.Policy := {}) : Middleware := fun next ctx => do
  if !Cache.eligibleRequest ctx || policy.vary.any (fun name =>
      (ctx.request.headers.filter (fun h => h.name.toLower == name.toLower)).size > 1) then
    return ← next ctx
  let key := Cache.responseKey ctx policy
  if let some bytes ← client.get key then
    if let some response ← cached ctx policy bytes then return response
  if ctx.request.method == "HEAD" then return Cache.conditional ctx (← next ctx)
  let owner ← Session.randomToken
  unless ← client.acquire key owner do
    return (Response.text "cache generation busy" 503).withHeader "retry-after" "1"
  try
    -- Another worker may have filled between our initial GET and lease grant.
    if let some bytes ← client.get key then
      if let some response ← cached ctx policy bytes then
        releaseQuietly client key owner
        return response
    let response ← next ctx
    let ttl := Cache.freshnessMs policy response
    if Cache.eligibleResponse policy response && ttl > 0 && response.headers.size <= 1000 then
      let response := response.withHeader "etag" (Cache.etag response.body)
      let bytes := Cache.encodeResponse response (← epochMs)
      if client.canPublish key owner bytes ttl then
        let _ ← client.publish key owner bytes ttl
        return Cache.conditional ctx response
    releaseQuietly client key owner
    return Cache.conditional ctx response
  catch error =>
    releaseQuietly client key owner
    throw error

/-- Cache explicitly public request fragments using the same host/tenant,
route/query/locale/Vary isolation as response caching. HEAD misses do not fill.
On cancellation the bounded fill lease expires; no untracked cleanup task runs. -/
def fragment (client : Client) (ctx : Context) (name : String) (build : AppM ByteArray)
    (policy : Cache.Policy := {}) : AppM ByteArray := do
  if !Cache.eligibleRequest ctx || policy.vary.any (fun name =>
      (ctx.request.headers.filter (fun h => h.name.toLower == name.toLower)).size > 1) then
    return ← build
  let key := Cache.key #["fragment", name, Cache.responseKey ctx policy]
  if let some value ← client.get key then return value
  if ctx.request.method == "HEAD" then return ← build
  let owner ← Session.randomToken
  unless ← client.acquire key owner do
    throw { status := 503, message := "fragment generation busy" }
  try
    if let some value ← client.get key then
      releaseQuietly client key owner
      return value
    let value ← build
    if policy.ttlMs > 0 && client.canPublish key owner value policy.ttlMs then
      let _ ← client.publish key owner value policy.ttlMs
      return value
    releaseQuietly client key owner
    return value
  catch error =>
    releaseQuietly client key owner
    throw error

end LeanWeave.Redis
