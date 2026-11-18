import LeanWeave.Cache.Store
import LeanWeave.Cache.ResponseStore
import LeanNet.HTTP.Static.Date

namespace LeanWeave.Cache

/-- An access decision is distinct from a cache decision. Throw `AppError` to
    reject a request; `bypass` still runs the handler with the checked context. -/
inductive AdmissionResult where
  | bypass (context : Context)
  | cache (context : Context) (scope : String)

/-- Runs on every request, including cache hits. It must complete every access
    check relevant to this route before allowing lookup. A tenant-free public
    route may explicitly use a global scope; tenant routes should include their
    verified tenant in the scope. -/
abbrev Admission := Context → AppM AdmissionResult

/-- Only explicitly public responses in explicitly configured routes are cached. -/
structure Policy where
  ttlMs : Nat := 10000
  /-- Additional request dimensions even when a response omits Vary. -/
  vary : Array String := #["accept", "accept-language", "accept-encoding"]
  maxVaryNames : Nat := 16
  waitTimeoutMs : Nat := 5000
  version : String := "1"
  /-- No cache lookup is allowed until an explicit per-request admission runs. -/
  admit : Option Admission := none

private def tokens (s : String) : Array String :=
  (s.toLower.splitOn ",").toArray.map (fun p => p.trimAscii.toString)

private def directive (s name : String) : Bool :=
  (tokens s).any (fun p => ((p.splitOn "=").head!).trimAscii.toString == name)

private def values (headers : Array LeanNet.HTTP1.Header) (name : String) : String :=
  String.intercalate "," ((headers.filter (fun h => h.name.toLower == name)).toList.map fun h => (String.fromUTF8? h.value).getD "no-store")

private def isFieldNameByte (b : UInt8) : Bool :=
  let n := b.toNat
  (n >= 48 && n <= 57) || (n >= 65 && n <= 90) || (n >= 97 && n <= 122) ||
    [33,35,36,37,38,39,42,43,45,46,94,95,96,124,126].contains n

private def isOWS (b : UInt8) : Bool := b == 32 || b == 9

/-- Strict Vary field-name list. Invalid syntax, opaque bytes, or `*` bypass
    storage. The original response header fields are never rewritten. -/
def parseVary (headers : Array LeanNet.HTTP1.Header) (maxNames : Nat := 16) : Option (Array String) := Id.run do
  let fields := headers.filter (fun h => h.name.toLower == "vary")
  let mut names : Array String := #[]
  for field in fields do
    let bytes := field.value
    let mut begin := 0
    for i in [:bytes.size + 1] do
      if i == bytes.size || bytes[i]! == 44 then
        let mut first := begin
        let mut last := i
        while first < last && isOWS bytes[first]! do first := first + 1
        while last > first && isOWS bytes[last - 1]! do last := last - 1
        let nameBytes := bytes.extract first last
        if nameBytes.isEmpty || !nameBytes.data.all isFieldNameByte then return none
        let name := (String.fromUTF8! nameBytes).toLower
        if name == "*" then return none
        if !names.contains name then
          if names.size >= maxNames then return none
          names := names.push name
        begin := i + 1
  return some (names.qsort (· <= ·))

private def effectiveVary (policy : Policy) (response : Response) : Option (Array String) := do
  let responseNames ← parseVary response.headers policy.maxVaryNames
  let mut names := responseNames
  for configured in policy.vary do
    let bytes := configured.toUTF8
    if bytes.isEmpty || !bytes.data.all isFieldNameByte || configured == "*" then none else do
      let name := configured.toLower
      if !names.contains name then
        if names.size >= policy.maxVaryNames then none else do
          names := names.push name
  return names.qsort (· <= ·)

private structure CacheDirectives where
  publicResponse : Bool := false
  sharedForbidden : Bool := false
  maxAge : Option Nat := none
  sharedMaxAge : Option Nat := none

private def deltaSeconds (value : String) : Option Nat :=
  if value.startsWith "\"" && value.endsWith "\"" && value.utf8ByteSize >= 2 then
    ((value.drop 1).toString.dropEnd 1).toString.toNat?
  else value.toNat?

/-- Scan each field once. Numeric directives accept a quoted decimal argument.
    Other quoted arguments conservatively bypass this cache. -/
private def cacheDirectives (headers : Array LeanNet.HTTP1.Header) : Option CacheDirectives := Id.run do
  let mut result : CacheDirectives := {}
  for field in headers do
    if field.name.toLower != "cache-control" then continue
    let some raw := String.fromUTF8? field.value | return none
    if field.value.data.any (fun b => (b < 32 && b != 9) || b == 127) then return none
    for part in raw.splitOn "," do
      let pieces := part.trimAscii.toString.splitOn "="
      if pieces.length > 2 then return none
      let name := pieces.head!.trimAscii.toString.toLower
      if name.isEmpty || !name.toUTF8.data.all isFieldNameByte then return none
      let argument := pieces[1]?.map (·.trimAscii.toString)
      match name with
      | "public" =>
          if argument.isSome then return none
          result := { result with publicResponse := true }
      | "max-age" | "s-maxage" =>
          let some seconds := argument.bind deltaSeconds | return none
          if name == "max-age" then
            if result.maxAge.isSome then return none
            result := { result with maxAge := some seconds }
          else
            if result.sharedMaxAge.isSome then return none
            result := { result with sharedMaxAge := some seconds }
      | _ =>
          if argument.any (fun value => value.isEmpty || !value.toUTF8.data.all isFieldNameByte) then return none
          if name == "private" || name == "no-store" || name == "no-cache" then
            result := { result with sharedForbidden := true }
  return some result

private def asksForRevalidation (ctx : Context) : Bool :=
  let directives := tokens (values ctx.request.headers "cache-control")
  directives.any (fun part =>
    let pieces := part.splitOn "="
    let name := pieces.head!.trimAscii.toString
    name == "no-cache" || name == "no-store" ||
      name == "max-age" || name == "min-fresh") ||
    directive (values ctx.request.headers "pragma") "no-cache"

def eligibleRequest (ctx : Context) : Bool :=
  (ctx.request.method == "GET" || ctx.request.method == "HEAD") && ctx.request.body.isEmpty &&
  ctx.attribute "tenant.cache.enabled" != some "false" && (ctx.attribute "csp.nonce").isNone && !ctx.authenticated &&
  ctx.sessionId.isNone && ctx.request.clientCertificate.isNone && !(ctx.request.headers.any (fun h => ["authorization", "cookie"].contains h.name.toLower)) &&
  !(ctx.request.headers.any (fun h => ["range", "if-range", "if-match", "if-unmodified-since",
    "if-modified-since"].contains h.name.toLower)) &&
  !asksForRevalidation ctx

private def admittedVary? (policy : Policy) (response : Response) : Option (Array String) := do
  let cc ← cacheDirectives response.headers
  let noncePolicy := response.headers.any fun header =>
    if !["content-security-policy", "content-security-policy-report-only"].contains header.name.toLower then false
    else match String.fromUTF8? header.value with
      | none => true
      | some value => (value.toLower.splitOn "nonce-").length > 1
  if noncePolicy || response.stream.isSome || response.status != 200 ||
      !cc.publicResponse || cc.sharedForbidden ||
      response.headers.any (fun h => ["set-cookie", "connection", "keep-alive",
        "proxy-connection", "transfer-encoding", "upgrade", "te", "trailer",
        "content-range"].contains h.name.toLower) then none
  else effectiveVary policy response

def eligibleResponse (policy : Policy) (response : Response) : Bool :=
  (admittedVary? policy response).isSome

private def rawHeaderKey (ctx : Context) (name : String) : String :=
  let headers := ctx.request.headers.filter (fun h => h.name.toLower == name.toLower)
  (CacheKey.ofByteParts (#["request-header:v1".toUTF8,
    (toString headers.size).toUTF8] ++ headers.map (·.value))).digest

def responseKey (ctx : Context) (policy : Policy) : String :=
  key (#["response:v2", policy.version, ctx.host, ctx.tenant, ctx.routeName,
    (CacheKey.ofByteParts #[ctx.request.target]).digest,
    key ctx.path, key (ctx.query.map fun (k,v) => key #[k,v]), ctx.locale,
    (ctx.attribute "tenant.config.revision").getD "", (ctx.attribute "tenant.config.variant").getD "",
    (ctx.attribute "feature.variant").getD ""] ++
    policy.vary.map (fun name => key #[name.toLower, rawHeaderKey ctx name]))

private def admittedKey (scope : String) (ctx : Context) (policy : Policy) : String :=
  key #["admitted:v1", scope, responseKey ctx policy]

/-- Compute the key used by `ResponseStore.invalidateBase`. Supply the checked
    request context and scope returned by admission, and the route's policy.
    This selects all dynamic Vary variants of that key on the current worker. -/
def responseBaseKey (scope : String) (ctx : Context) (policy : Policy := {}) : CacheKey := Id.run do
  let mut parts : Array ByteArray := #["response-base:v4".toUTF8, scope.toUTF8,
    policy.version.toUTF8, ctx.host.toUTF8, ctx.tenant.toUTF8,
    ctx.routeName.toUTF8, ctx.request.target, ctx.locale.toUTF8,
    ((ctx.attribute "tenant.config.revision").getD "").toUTF8,
    ((ctx.attribute "tenant.config.variant").getD "").toUTF8,
    ((ctx.attribute "feature.variant").getD "").toUTF8,
    (toString ctx.path.size).toUTF8]
  for segment in ctx.path do parts := parts.push segment.toUTF8
  parts := parts.push (toString ctx.query.size).toUTF8
  for (name, value) in ctx.query do
    parts := parts.push name.toUTF8
    parts := parts.push value.toUTF8
  parts := parts.push (toString policy.vary.size).toUTF8
  for name in policy.vary do
    let fields := ctx.request.headers.filter (fun h => h.name.toLower == name.toLower)
    parts := parts.push name.toLower.toUTF8
    parts := parts.push (toString fields.size).toUTF8
    for field in fields do parts := parts.push field.value
  return CacheKey.ofByteParts parts

private def noAdditionalVary : CacheKey := CacheKey.ofParts #["vary:v2"]

/-- Request values nominated by Vary, preserving absent versus present-empty
    fields and opaque bytes. Exact byte matching is deliberately conservative. -/
private def varyFingerprint (ctx : Context) (policy : Policy) (names : Array String) : CacheKey := Id.run do
  let mut parts : Array ByteArray := #[]
  for name in names do
    -- Configured dimensions are already compared by the base key.
    if policy.vary.any (fun configured => configured == name ||
        (configured.utf8ByteSize == name.utf8ByteSize &&
          configured.toSlice.eqIgnoreAsciiCase name.toSlice)) then continue
    let fields := ctx.request.headers.filter (fun h => h.name.toLower == name)
    parts := parts.push name.toUTF8
    parts := parts.push (toString fields.size).toUTF8
    for field in fields do parts := parts.push field.value
  if parts.isEmpty then return noAdditionalVary
  return CacheKey.ofByteParts (#["vary:v2".toUTF8] ++ parts)

private def variantKey (base fingerprint : CacheKey) : CacheKey :=
  CacheKey.ofParts #["variant:v1", base.digest, fingerprint.digest]

private def admitRequest (ctx : Context) (policy : Policy) : AppM AdmissionResult := do
  let some admit := policy.admit | return .bypass ctx
  let result ← admit ctx
  let .cache ctx scope := result | return result
  -- Admission scopes are application-owned, but still bounded before they
  -- enter a worker-local lease table or a cache key.
  if scope.isEmpty || scope.utf8ByteSize > 256 then return .bypass ctx
  return .cache ctx scope

/-- Dynamic tenant settings can tighten a route's TTL; the tenant revision and
feature variant are also part of every response/fragment key. -/
def effectivePolicy (ctx : Context) (policy : Policy) : Policy :=
  match (ctx.attribute "tenant.cache.ttlMs").bind String.toNat? with
  | some ttl => { policy with ttlMs := min ttl policy.ttlMs }
  | none => policy

private def CacheDirectives.lifetime (cc : CacheDirectives) : Option Nat :=
  (cc.sharedMaxAge.or cc.maxAge).map (· * 1000)

private def ageValueMs? (response : Response) : Option Nat := do
  let fields := response.headers.filter (fun h => h.name.toLower == "age")
  if fields.size > 1 then none else do
    if fields.isEmpty then return 0
    let raw ← String.fromUTF8? fields[0]!.value
    let seconds ← raw.trimAscii.toString.toNat?
    return seconds * 1000

private def apparentAgeMs? (response : Response) (nowUnixMs : Int) : Option Nat := do
  let fields := response.headers.filter (fun h => h.name.toLower == "date")
  if fields.size > 1 then none else do
    if fields.isEmpty then return 0
    let date ← LeanNet.HTTP.Static.parseHttpDate? fields[0]!.value (nowUnixMs / 1000)
    return (max 0 (nowUnixMs - date * 1000)).toNat

/-- RFC 9111 corrected initial age. Invalid or repeated Date/Age and freshness
    directives bypass storage. The fill delay is included conservatively. -/
def freshnessAt? (policy : Policy) (response : Response) (nowUnixMs : Int)
    (fillDelayMs : Nat) : Option (Nat × Nat) := do
  let directives ← cacheDirectives response.headers
  let explicit := directives.lifetime
  let mut lifetime := min policy.ttlMs (explicit.getD policy.ttlMs)
  let expires := response.headers.filter (fun h => h.name.toLower == "expires")
  if expires.size > 1 then none else do
    if explicit.isNone && !expires.isEmpty then
      let expiry ← LeanNet.HTTP.Static.parseHttpDate? expires[0]!.value (nowUnixMs / 1000)
      let dates := response.headers.filter (fun h => h.name.toLower == "date")
      if dates.size > 1 then none else do
        let origin ← if dates.isEmpty then some (nowUnixMs / 1000)
          else LeanNet.HTTP.Static.parseHttpDate? dates[0]!.value (nowUnixMs / 1000)
        lifetime := min lifetime ((max 0 (expiry - origin)) * 1000).toNat
  let age ← ageValueMs? response
  let apparent ← apparentAgeMs? response nowUnixMs
  let current := max apparent (age + fillDelayMs)
  return (lifetime - current, current)

/-- Age-only freshness estimate for callers without a wall clock. A Date header
    requires `freshnessAt?` with the current Unix time. -/
def freshnessMs (policy : Policy) (response : Response) : Nat :=
  if response.headers.any (fun h => h.name.toLower == "date" || h.name.toLower == "expires") then 0 else
  match cacheDirectives response.headers, ageValueMs? response with
  | some directives, some age => min policy.ttlMs (directives.lifetime.getD policy.ttlMs) - age
  | _, _ => 0

/-- Weak representation validator derived from SHA-256 of the owned body. -/
def etag (bytes : ByteArray) : String :=
  "W/\"" ++ Security.Crypto.hex (Security.Crypto.sha256 bytes) ++ "\""

private def skipOWS (bytes : ByteArray) (start : Nat) : Nat := Id.run do
  let mut offset := start
  while offset < bytes.size && isOWS bytes[offset]! do
    offset := offset + 1
  return offset

-- Return the quoted portion without the weak prefix and the next position.
private def entityTagAt (bytes : ByteArray) (start : Nat) : Option (Nat × Nat) := Id.run do
  let weak := start + 1 < bytes.size && bytes[start]! == 87 && bytes[start + 1]! == 47
  let first := start + if weak then 2 else 0
  if first >= bytes.size || bytes[first]! != 34 then return none
  for offset in [first + 1:bytes.size] do
    let byte := bytes[offset]!
    if byte == 34 then return some (first, offset + 1)
    if byte < 33 || byte == 127 then return none
  return none

-- If-None-Match is a list of quoted tags. Commas can occur inside a tag,
-- and `*` is only valid as the whole field value.
private def matchesIfNoneMatch (headers : Array LeanNet.HTTP1.Header) (current : ByteArray) : Bool := Id.run do
  let mut fields := 0
  let mut wildcard := false
  let mut found := false
  let currentTag := (entityTagAt current 0).bind fun (first, stop) =>
    if stop == current.size then some (current.extract first stop) else none
  for header in headers do
    if header.name.toLower != "if-none-match" then continue
    fields := fields + 1
    let bytes := header.value
    let start := skipOWS bytes 0
    if start < bytes.size && bytes[start]! == 42 && skipOWS bytes (start + 1) == bytes.size then
      wildcard := true
      continue
    let mut cursor := start
    while cursor < bytes.size do
      if bytes[cursor]! == 44 then
        cursor := skipOWS bytes (cursor + 1)
        continue
      let some (first, stop) := entityTagAt bytes cursor | return false
      if currentTag.any (· == bytes.extract first stop) then found := true
      cursor := skipOWS bytes stop
      if cursor == bytes.size then break
      if bytes[cursor]! != 44 then return false
      cursor := skipOWS bytes (cursor + 1)
  return if wildcard then fields == 1 else found

private def entityTag (response : Response) : Option ByteArray :=
  (response.headers.find? (fun h => h.name.toLower == "etag")).map (·.value)

def conditional (ctx : Context) (response : Response) : Response :=
  if response.status != 200 || response.stream.isSome then response else
  let (tag, response) := match entityTag response with
    | some upstream => (upstream, response)
    | none =>
        let tag := etag response.body
        (tag.toUTF8, response.withHeader "etag" tag)
  if ctx.request.method != "GET" && ctx.request.method != "HEAD" then response else
  if ctx.request.headers.any (fun h => h.name.toLower == "if-none-match") &&
      matchesIfNoneMatch ctx.request.headers tag then
    { response with status := 304, body := ByteArray.empty }
  else response

-- Redis's optional adapter persists complete response envelopes. The worker-
-- local ResponseStore does not call these functions or copy body bytes.
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

private def lookupVariant (store : ResponseStore) (ctx : Context) (policy : Policy)
    (base : CacheKey) : IO (Option CacheEntry) :=
  store.lookupMatching base (fun entry =>
    entry.varyFingerprint == varyFingerprint ctx policy entry.varyNames)

private def cachedResponse? (ctx : Context) (policy : Policy) (base : CacheKey)
    (entry : CacheEntry) : IO (Option Response) := do
  let now ← IO.monoMsNow
  let elapsed := now - entry.storedAt
  if now < entry.storedAt || elapsed >= entry.ttlMs || entry.initialAgeMs + elapsed >= policy.ttlMs ||
      entry.baseKey != base then
    return none
  let response : Response := { status := entry.status, headers := entry.headers, body := entry.body }
  if admittedVary? policy response != some entry.varyNames then
    return none
  let response := response.withHeader "age" (toString ((entry.initialAgeMs + elapsed) / 1000))
  return some (conditional ctx response)

/-- A bounded second claim lets a newly discovered Vary variant start its own
    fill. Other followers for that variant join it on this worker. -/
private def fill (store : ResponseStore) (next : Handler) (ctx : Context)
    (base : CacheKey) (policy : Policy) : Nat → AppM Response
  | 0 => next ctx
  | fuel + 1 => do
    match ← store.claim base with
    | .overloaded =>
        return (Response.text "cache generation busy" 503).withHeader "retry-after" "1"
    | .follower flight =>
        let shared ← store.wait flight (max 1 (min policy.waitTimeoutMs 30000))
        if let some entry := shared then
          if entry.varyFingerprint == varyFingerprint ctx policy entry.varyNames then
            if let some response ← cachedResponse? ctx policy base entry then return response
        if let some entry ← lookupVariant store ctx policy base then
          if let some response ← cachedResponse? ctx policy base entry then return response
        return ← fill store next ctx base policy fuel
    | .leader flight =>
        AppM.ensuring (do
          let started ← IO.monoMsNow
          let response ← next ctx
          if response.headers.size > 1000 || response.body.size > store.limits.maxEntryBytes then
            return response
          let some names := admittedVary? policy response | return response
          let now ← IO.monoMsNow
          let nowUnixMs := (← Std.Time.Timestamp.now).toMillisecondsSinceUnixEpoch.val
          let some (ttl, initialAge) := freshnessAt? policy response nowUnixMs (now - started)
            | return response
          if ttl == 0 || (← store.generation) != flight.generation then return response
          let response := if (entityTag response).isSome then response
            else response.withHeader "etag" (etag response.body)
          let response := if response.headers.any (fun h => h.name.toLower == "date") then response
            else
              let date : LeanNet.HTTP1.Header := ⟨"date", LeanNet.HTTP.Static.formatHttpDate (nowUnixMs / 1000)⟩
              { response with headers := response.headers.push date }
          let fingerprint := varyFingerprint ctx policy names
          let entry : CacheEntry := {
            status := response.status
            headers := response.headers
            body := response.body
            ttlMs := ttl
            storedAt := now
            initialAgeMs := initialAge
            baseKey := base
            varyNames := names
            varyFingerprint := fingerprint
          }
          if ← store.storeChecked (variantKey base fingerprint) entry then
            let _ ← store.finish flight (some entry)
            pure ()
          return conditional ctx response) (store.abort flight)

/-- Create once per reactor worker. A cache hit reuses the owned body ByteArray;
    lean-net remains responsible for framing and transport writes. -/
def middleware (store : ResponseStore) (policy : Policy := {}) : Middleware := fun next ctx => do
  let incomingEligible := eligibleRequest ctx
  let decision ← admitRequest ctx policy
  let .cache ctx scope := decision | return ← next (match decision with | .bypass ctx => ctx | .cache ctx _ => ctx)
  let policy := effectivePolicy ctx policy
  if policy.ttlMs == 0 || !incomingEligible || !eligibleRequest ctx then return ← next ctx
  let base := responseBaseKey scope ctx policy
  if let some entry ← lookupVariant store ctx policy base then
    if let some response ← cachedResponse? ctx policy base entry then return response
  -- A HEAD miss must not populate GET storage with a HEAD-specific handler.
  if ctx.request.method == "HEAD" then return ← next ctx
  fill store next ctx base policy (store.limits.maxVariantsPerBase + 1)

/-- Context-aware fragment caching applies tenant rules and all response isolation
keys. Low-level `fragment` remains available for explicit non-request keys. -/
def requestFragment (store : Store) (ctx : Context) (name : String) (generate : Context → AppM ByteArray)
    (policy : Policy := {}) : AppM ByteArray := do
  let incomingEligible := eligibleRequest ctx
  let decision ← admitRequest ctx policy
  let .cache ctx scope := decision | return ← generate (match decision with | .bypass ctx => ctx | .cache ctx _ => ctx)
  let policy := effectivePolicy ctx policy
  if !incomingEligible || !eligibleRequest ctx || policy.ttlMs == 0 then return ← generate ctx
  let cacheKey := key #["fragment", name, admittedKey scope ctx policy]
  if ctx.request.method == "HEAD" then
    if let some bytes ← store.get cacheKey (← IO.monoMsNow) then return bytes
    return ← generate ctx
  fragment store cacheKey policy.ttlMs (generate ctx)

end LeanWeave.Cache
