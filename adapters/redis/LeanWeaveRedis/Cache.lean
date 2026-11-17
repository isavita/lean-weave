import LeanWeaveRedis.Client
import LeanWeave.Cache.Response
import LeanWeave.Session.Store
import Std.Time.DateTime.Timestamp

namespace LeanWeave.Redis

private def epochMs : IO Nat := do
  return (← Std.Time.Timestamp.now).toMillisecondsSinceUnixEpoch.val.toNat

/-- Recheck route-owned access before every Redis lookup. The response adapter
    does not retain state across workers, so the verified scope enters the key. -/
private def admitRequest (ctx : Context) (policy : Cache.Policy) : AppM Cache.AdmissionResult := do
  let some admit := policy.admit | return .bypass ctx
  let result ← admit ctx
  let .cache verified scope := result | return result
  if scope.isEmpty || scope.utf8ByteSize > 256 then return .bypass verified
  return .cache verified scope

private def responseKey (ctx : Context) (scope : String) (policy : Cache.Policy) : String :=
  Cache.key #["admitted:redis:v1", scope, Cache.responseKey ctx policy]

/-- Redis keys currently include only configured Vary dimensions. Until the
    adapter has a variant index, bypass any response that names another field. -/
private def eligibleResponse (policy : Cache.Policy) (response : Response) : Bool :=
  Cache.eligibleResponse policy response &&
    match Cache.parseVary response.headers policy.maxVaryNames with
    | none => false
    | some names => names.all (fun name => policy.vary.any (·.toLower == name))

private def cached (ctx : Context) (policy : Cache.Policy) (bytes : ByteArray) : IO (Option Response) := do
  let some (response, insertedAt) := Cache.decodeResponse bytes | return none
  if !eligibleResponse policy response then return none
  let now ← epochMs
  if now < insertedAt then return none
  let some (ttl, ageMs) := Cache.freshnessAt? policy response (Int.ofNat now) (now - insertedAt)
    | return none
  if ttl == 0 then return none
  return some (Cache.conditional ctx (response.withHeader "age" (toString (ageMs / 1000))))

private def releaseQuietly (client : Client) (key owner : String) : AppM Unit := do
  try let _ ← client.release key owner; pure () catch _ => pure ()

/-- Public response caching with the core isolation and eligibility policy.
Redis is the shared authoritative cache layer; no local L1 is retained. Fill
publication atomically checks lease ownership, so invalidation and lease expiry
prevent a late fill from repopulating the key. -/
def middleware (client : Client) (policy : Cache.Policy := {}) : Middleware := fun next ctx => do
  let incomingEligible := Cache.eligibleRequest ctx
  let decision ← admitRequest ctx policy
  let .cache ctx scope := decision | return ← next (match decision with
    | .bypass ctx => ctx
    | .cache ctx _ => ctx)
  let policy := Cache.effectivePolicy ctx policy
  if policy.ttlMs == 0 || !incomingEligible || !Cache.eligibleRequest ctx || policy.vary.any (fun name =>
      (ctx.request.headers.filter (fun h => h.name.toLower == name.toLower)).size > 1) then
    return ← next ctx
  let key := responseKey ctx scope policy
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
    let started ← IO.monoMsNow
    let response ← next ctx
    if eligibleResponse policy response && response.headers.size <= 1000 then
      let now ← epochMs
      let elapsed := (← IO.monoMsNow) - started
      if let some (ttl, _) := Cache.freshnessAt? policy response (Int.ofNat now) elapsed then
        if ttl > 0 then
          let response := if (response.getHeader "etag").isSome then response
            else response.withHeader "etag" (Cache.etag response.body)
          let bytes := Cache.encodeResponse response now
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
  let incomingEligible := Cache.eligibleRequest ctx
  let decision ← admitRequest ctx policy
  let .cache ctx scope := decision | return ← build
  let policy := Cache.effectivePolicy ctx policy
  if policy.ttlMs == 0 || !incomingEligible || !Cache.eligibleRequest ctx || policy.vary.any (fun name =>
      (ctx.request.headers.filter (fun h => h.name.toLower == name.toLower)).size > 1) then
    return ← build
  let key := Cache.key #["fragment", name, responseKey ctx scope policy]
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
