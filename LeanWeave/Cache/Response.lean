import LeanWeave.Cache.Store

namespace LeanWeave.Cache

/-- Only explicitly public responses in explicitly configured routes are cached. -/
structure Policy where
  ttlMs : Nat := 10000
  vary : Array String := #["accept", "accept-language", "accept-encoding"]
  version : String := "1"

private def tokens (s : String) : Array String :=
  (s.toLower.splitOn ",").toArray.map (fun p => p.trimAscii.toString)

private def directive (s name : String) : Bool :=
  (tokens s).any (fun p => (p.splitOn "=").head! == name)

private def values (headers : Array LeanNet.HTTP1.Header) (name : String) : String :=
  String.intercalate "," ((headers.filter (fun h => h.name.toLower == name)).toList.map fun h => (String.fromUTF8? h.value).getD "no-store")

def eligibleRequest (ctx : Context) : Bool :=
  (ctx.request.method == "GET" || ctx.request.method == "HEAD") && ctx.attribute "tenant.cache.enabled" != some "false" && !ctx.authenticated &&
  ctx.sessionId.isNone && ctx.request.clientCertificate.isNone && !(ctx.request.headers.any (fun h => ["authorization", "cookie"].contains h.name.toLower)) &&
  !(directive (values ctx.request.headers "cache-control") "no-cache") &&
  !(directive (values ctx.request.headers "cache-control") "no-store")

def eligibleResponse (policy : Policy) (response : Response) : Bool :=
  let cc := values response.headers "cache-control"
  let vary := tokens (values response.headers "vary")
  response.stream.isNone && response.status == 200 && directive cc "public" && !directive cc "private" &&
    !directive cc "no-store" && !directive cc "no-cache" &&
    !(response.headers.any (fun h => h.name.toLower == "set-cookie")) && !vary.contains "*" &&
    vary.all (fun name => name.isEmpty || policy.vary.any (·.toLower == name))

private def rawHeaderKey (ctx : Context) (name : String) : String :=
  let headers := ctx.request.headers.filter (fun h => h.name.toLower == name.toLower)
  key (headers.map fun h => key (h.value.data.map (fun b => toString b.toNat)))

def responseKey (ctx : Context) (policy : Policy) : String :=
  key (#["response", policy.version, ctx.host, ctx.tenant, ctx.routeName,
    key ctx.path, key (ctx.query.map fun (k,v) => key #[k,v]), ctx.locale,
    (ctx.attribute "tenant.config.revision").getD "", (ctx.attribute "feature.variant").getD ""] ++
    policy.vary.map (fun name => key #[name.toLower, rawHeaderKey ctx name]))

/-- Dynamic tenant settings can tighten a route's TTL; the tenant revision and
feature variant are also part of every response/fragment key. -/
def effectivePolicy (ctx : Context) (policy : Policy) : Policy :=
  match (ctx.attribute "tenant.cache.ttlMs").bind String.toNat? with
  | some ttl => { policy with ttlMs := min ttl policy.ttlMs }
  | none => policy

/-- Route TTL is capped by response freshness; s-maxage overrides max-age.
    Invalid or repeated freshness directives make the response uncacheable. -/
def freshnessMs (policy : Policy) (response : Response) : Nat := Id.run do
  let cc := tokens (values response.headers "cache-control")
  let directives := cc.filter (fun t => (t.splitOn "=").head! == "s-maxage")
  let directives := if directives.isEmpty then cc.filter (fun t => (t.splitOn "=").head! == "max-age") else directives
  if directives.isEmpty then return policy.ttlMs
  if directives.size != 1 then return 0
  let pair := directives[0]!.splitOn "="
  if pair.length != 2 then return 0
  let raw := pair[1]!.trimAscii.toString
  let some seconds := raw.toNat? | return 0
  let age := ((response.getHeader "age").bind String.toNat?).getD 0
  return min policy.ttlMs ((seconds - age) * 1000)

/-- Weak representation validator; hash is not used for cache-key isolation or secrets. -/
def etag (bytes : ByteArray) : String :=
  let hash := bytes.data.foldl (fun (h : UInt64) b => (h ^^^ b.toUInt64) * 1099511628211) 14695981039346656037
  s!"W/\"{hash}-{bytes.size}\""

private def stripWeak (s : String) : String :=
  let text := s.trimAscii.toString
  if text.startsWith "W/" then (text.drop 2).toString else text

def conditional (ctx : Context) (response : Response) : Response :=
  if response.status != 200 then response else
  let tag := (response.getHeader "etag").getD (etag response.body)
  let response := response.withHeader "etag" tag
  let tagMatches := ((ctx.header "if-none-match").getD "").splitOn "," |>.any
    (fun t => t.trimAscii.toString == "*" || stripWeak t == stripWeak tag)
  if (ctx.request.method == "GET" || ctx.request.method == "HEAD") && tagMatches then
    { response with status := 304, body := ByteArray.empty }
  else response

private def u32 (n : Nat) : ByteArray :=
  ByteArray.mk #[(n / 16777216).toUInt8, (n / 65536).toUInt8, (n / 256).toUInt8, n.toUInt8]

private def read32 (bytes : ByteArray) (offset : Nat) : Option Nat :=
  if offset + 4 > bytes.size then none else
    some ((bytes[offset]!.toNat * 256 + bytes[offset+1]!.toNat) * 65536 +
      bytes[offset+2]!.toNat * 256 + bytes[offset+3]!.toNat)

def encodeResponse (response : Response) (now : Nat) : ByteArray := Id.run do
  let mut bytes := u32 (now / 4294967296) ++ u32 now ++ u32 response.headers.size
  for header in response.headers do
    let name := header.name.toUTF8
    bytes := bytes ++ u32 name.size ++ u32 header.value.size ++ name ++ header.value
  return bytes ++ response.body

def decodeResponse (bytes : ByteArray) : Option (Response × Nat) := do
  let high ← read32 bytes 0
  let low ← read32 bytes 4
  let count ← read32 bytes 8
  if count > 1000 then none else do
    let mut cursor := 12
    let mut headers : Array LeanNet.HTTP1.Header := #[]
    for _ in [:count] do
      let nameSize ← read32 bytes cursor
      let valueSize ← read32 bytes (cursor + 4)
      cursor := cursor + 8
      if cursor + nameSize + valueSize > bytes.size then none else do
        let name ← String.fromUTF8? (bytes.extract cursor (cursor + nameSize))
        let value := bytes.extract (cursor + nameSize) (cursor + nameSize + valueSize)
        headers := headers.push { name, value }
        cursor := cursor + nameSize + valueSize
    return ({ headers, body := bytes.extract cursor bytes.size }, high * 4294967296 + low)

/-- Create once per worker. HEAD reuses the complete GET representation metadata.
    Miss contenders fail fast; they never form an unbounded waiter queue. -/
def middleware (store : Store) (policy : Policy := {}) : Middleware := fun next ctx => do
  let policy := effectivePolicy ctx policy
  if policy.ttlMs == 0 || !eligibleRequest ctx || policy.vary.any (fun name => (ctx.request.headers.filter (fun h => h.name.toLower == name.toLower)).size > 1) then return ← next ctx
  let cacheKey := responseKey ctx policy
  if let some bytes ← store.get cacheKey (← IO.monoMsNow) then
    if let some (response, insertedAt) := decodeResponse bytes then
      let age := ((response.getHeader "age").bind String.toNat?).getD 0
      let response := response.withHeader "age" (toString (age + ((← IO.monoMsNow) - insertedAt) / 1000))
      return conditional ctx response
  -- A HEAD miss must not populate a GET cache using a HEAD-specific handler.
  if ctx.request.method == "HEAD" then return conditional ctx (← next ctx)
  unless ← store.acquire cacheKey do
    return (Response.text "cache generation busy" 503).withHeader "retry-after" "1"
  let generation := (← store.state.get).generation
  (do
    let response ← next ctx
    if eligibleResponse policy response && freshnessMs policy response > 0 && response.headers.size <= 1000 then
      let response := response.withHeader "etag" (etag response.body)
      if (← store.state.get).generation == generation then
        let now ← IO.monoMsNow
        let _ ← store.put cacheKey (encodeResponse response now) (freshnessMs policy response) now
        pure ()
      return conditional ctx response
    return response).ensuring (store.release cacheKey)

/-- Context-aware fragment caching applies tenant rules and all response isolation
keys. Low-level `fragment` remains available for explicit non-request keys. -/
def requestFragment (store : Store) (ctx : Context) (name : String) (generate : AppM ByteArray)
    (policy : Policy := {}) : AppM ByteArray := do
  let policy := effectivePolicy ctx policy
  if !eligibleRequest ctx || policy.ttlMs == 0 then return ← generate
  let cacheKey := key #["fragment", name, responseKey ctx policy]
  if ctx.request.method == "HEAD" then
    if let some bytes ← store.get cacheKey (← IO.monoMsNow) then return bytes
    return ← generate
  fragment store cacheKey policy.ttlMs generate

end LeanWeave.Cache
