import LeanWeave.Core
import LeanWeave.Core.Body
import LeanWeave.Session

namespace LeanWeave.Middleware

/-- Convert application errors without disclosing IO exception details. -/
def errors : Middleware := fun next ctx =>
  try next ctx catch error => pure (Response.ofError error)

/-- Request IDs are generated locally, not accepted from untrusted clients. -/
def requestId : Middleware := fun next ctx => do
  let id ← Session.randomToken
  let response ← next { ctx with requestId := id }
  return response.withHeader "x-request-id" id

def securityHeaders : Middleware := fun next ctx => do
  let response ← next ctx
  return response.withHeader "x-content-type-options" "nosniff"
    |>.withHeader "referrer-policy" "strict-origin-when-cross-origin"
    |>.withHeader "x-frame-options" "DENY"
    |>.withHeader "content-security-policy" "default-src 'self'; object-src 'none'; base-uri 'self'; frame-ancestors 'none'"

/-- Trust is a transport capability supplied by the application, never an incoming header.
    Default-deny proxy policy; exactly one forwarded authority/proto is supported. -/
def trustedProxies (trustedPeer : Context → Bool := fun _ => false) : Middleware := fun next ctx => do
  if !trustedPeer ctx then return ← next ctx
  for name in ["x-forwarded-host", "x-forwarded-proto"] do
    let raw := ctx.request.headers.filter (fun h => h.name.toLower == name)
    if raw.size > 1 || raw.any (fun h => (String.fromUTF8? h.value).isNone) then
      throw { status := 400, message := "invalid or repeated forwarded header" }
  let host ← match ctx.header "x-forwarded-host" with
    | none => pure ctx.host
    | some value => match normalizeHost value with
      | .ok host => pure host
      | .error message => throw { status := 400, message }
  let scheme := (ctx.header "x-forwarded-proto").getD (if ctx.secure then "https" else "http")
  unless scheme == "http" || scheme == "https" do throw { status := 400, message := "invalid forwarded protocol" }
  next { ctx with host, attributes := (ctx.attributes.filter (fun p => p.1 != "scheme")).push ("scheme", scheme) }

/-- Trust only explicitly listed numeric peer IPs minted by the native transport. -/
def trustedProxyAddresses (peers : Array ByteArray) : Middleware :=
  trustedProxies fun ctx => ctx.peer.any (fun peer =>
    (peer.size == 4 || peer.size == 16) && peers.contains peer)

def authenticate (resolve : Context → AppM (Option String)) : Middleware := fun next ctx => do
  match ← resolve ctx with
  | none => next ctx
  | some principal => next { ctx with authenticated := true, attributes := (ctx.attributes.filter (fun p => p.1 != "principal")).push ("principal", principal) }

def requireAuth : Middleware := fun next ctx => do
  unless ctx.authenticated do throw { status := 401, message := "authentication required" }
  next ctx

def authorize (allowed : Context → Bool) : Middleware := fun next ctx => do
  unless allowed ctx do throw { status := 403, message := "forbidden" }
  next ctx

/-- Load existing sessions only. Creating a session is an explicit application action. -/
def session (store : Session.Store) (cookie : Session.CookieConfig := {}) : Middleware := fun next ctx => do
  let some id := Session.cookieId ctx cookie.name | return ← next ctx
  let data ← match ← store.get id (← IO.monoMsNow) with
    | .ok data => pure data
    | .error error => throw error
  let some data := data | return ← next ctx
  let attrs := data.values.map fun (k,v) => ("session." ++ k,v)
  let ctx := { ctx with sessionId := some data.id, attributes := ctx.attributes ++ attrs ++ #[("csrf", data.csrf)] }
  next ctx

def sessionAuth : Middleware := authenticate (fun ctx => pure (ctx.attribute "session.userId"))

private def unsafeMethod (method : String) : Bool :=
  !["GET", "HEAD", "OPTIONS"].contains method

/-- Keep an absent field distinct from one with repeated or undecodable values. -/
private def singletonHeader [Monad m] [MonadExceptOf AppError m]
    (headers : Array LeanNet.HTTP1.Header) (name : String) : m (Option String) := do
  let mut value := none
  for header in headers do
    if header.name.toLower != name then continue
    if value.isSome then throw { status := 400, message := s!"repeated {name} header" }
    let some decoded := String.fromUTF8? header.value
      | throw { status := 400, message := s!"invalid {name} header" }
    value := some decoded
  return value

/-- Validate the token after decoding the body. Header checks alone cannot verify form CSRF. -/
def csrf (limits : Body.Limits := {}) : Middleware := fun next ctx => do
  if !unsafeMethod ctx.request.method then return ← next ctx
  let some expected := ctx.attribute "csrf" | throw { status := 403, message := "CSRF session required" }
  let headerToken ← singletonHeader ctx.request.headers "x-csrf-token"
  let token ← match headerToken with
    | some token => pure token
    | none =>
      let contentType := (← singletonHeader ctx.request.headers "content-type").getD ""
      let media := (contentType.splitOn ";").head!.trimAscii.toString.toLower
      let values ← if media == "application/x-www-form-urlencoded" then do
        let fields ← match Body.form ctx.request.body limits with
          | .ok fields => pure fields
          | .error error => throw error
        pure (fields.filter (·.1 == "_csrf"))
      else if media == "multipart/form-data" then do
        let parameters := (contentType.splitOn ";").tail!.filterMap fun p =>
          let pair := p.trimAscii.toString.splitOn "="
          if pair.head!.trimAscii.toString.toLower == "boundary" then
            some (String.intercalate "=" pair.tail!).trimAscii.toString else none
        if parameters.length != 1 then throw { status := 400, message := "one multipart boundary required" }
        let raw := parameters.head!
        let boundary := if raw.startsWith "\"" && raw.endsWith "\"" then
            ((raw.drop 1).toString.dropEnd 1).toString else raw
        let parts ← match Body.multipart ctx.request.body boundary limits with
          | .ok parts => pure parts
          | .error error => throw error
        let mut tokens := #[]
        for part in parts do
          if part.name == "_csrf" then
            if part.filename.isSome then throw { status := 403, message := "CSRF token cannot be an upload" }
            let some value := String.fromUTF8? part.bytes | throw { status := 403, message := "invalid CSRF token encoding" }
            tokens := tokens.push ("_csrf", value)
        pure tokens
      else throw { status := 403, message := "CSRF token required" }
      if values.size != 1 then throw { status := 403, message := "exactly one CSRF token required" }
      pure values[0]!.2
  unless Session.tokenEquals expected token do throw { status := 403, message := "invalid CSRF token" }
  next ctx

structure Cors where
  origins : Array String
  methods : Array String := #["GET", "HEAD", "POST", "PUT", "PATCH", "DELETE", "OPTIONS"]
  headers : Array String := #["content-type", "x-csrf-token"]
  credentials : Bool := false

private def varyOnOrigin (response : Response) : Response := Id.run do
  let mut names := #[]
  let mut hasOrigin := false
  for field in response.headers do
    if field.name.toLower != "vary" then continue
    let some value := String.fromUTF8? field.value | return response.withHeader "vary" "*"
    for token in value.splitOn "," do
      let name := token.trimAscii.toString
      if name == "*" then return response.withHeader "vary" "*"
      if name.isEmpty then continue
      if name.toLower == "origin" then
        if hasOrigin then continue
        hasOrigin := true
      names := names.push name
  unless hasOrigin do names := names.push "Origin"
  return response.withHeader "vary" (String.intercalate ", " names.toList)

/-- Exact-origin CORS. Successful responses vary on Origin, including when the
    request omits it. Preflight responses also vary on the requested method and headers. -/
def cors (policy : Cors) : Middleware := fun next ctx => do
  let some origin ← singletonHeader ctx.request.headers "origin" | return varyOnOrigin (← next ctx)
  unless policy.origins.contains origin && origin != "*" do
    throw { status := 403, message := "origin not allowed" }
  let requestedMethod ← if ctx.request.method == "OPTIONS" then
    singletonHeader ctx.request.headers "access-control-request-method" else pure none
  let response ← match requestedMethod with
    | none => next ctx
    | some requested => do
      unless policy.methods.contains requested do throw { status := 403, message := "CORS method not allowed" }
      for field in ctx.request.headers do
        if field.name.toLower != "access-control-request-headers" then continue
        let some value := String.fromUTF8? field.value
          | throw { status := 400, message := "invalid access-control-request-headers header" }
        for item in value.split ',' do
          let name := item.trimAscii.toString.toLower
          unless name.isEmpty || policy.headers.contains name do
            throw { status := 403, message := "CORS header not allowed" }
      pure (({ status := 204 } : Response).withHeader "access-control-allow-methods" (String.intercalate ", " policy.methods.toList)
        |>.withHeader "access-control-allow-headers" (String.intercalate ", " policy.headers.toList)
        |>.withHeader "vary" "Access-Control-Request-Method, Access-Control-Request-Headers")
  let response := (varyOnOrigin response).withHeader "access-control-allow-origin" origin
  return if policy.credentials then response.withHeader "access-control-allow-credentials" "true" else response

/-- Header policies run before a decoded body exists. Scope them using head method/target. -/
abbrev HeaderCheck := LeanNet.HTTP1.RequestHead → Except AppError Unit

def sameOrigin (origin : String) (scope : LeanNet.HTTP1.RequestHead → Bool := fun _ => true)
    (allowMissing : Bool := false) : HeaderCheck := fun head => do
  if !scope head || !unsafeMethod head.method then return
  let some value ← singletonHeader head.headers "origin" |
    if allowMissing then return else throw { status := 403, message := "Origin required" }
  unless value == origin do throw { status := 403, message := "cross-origin request rejected" }

def allowedHosts (hosts : Array String) : HeaderCheck := fun head => do
  let values := head.headers.filter (fun h => h.name.toLower == "host")
  if values.size != 1 then throw { status := 400, message := "exactly one Host required" }
  let host ← (normalizeHost ((header? head.headers "host").getD "")).mapError fun message => { status := 400, message }
  unless hosts.contains host do throw { status := 400, message := "host not allowed" }

def checkHeaders (checks : Array HeaderCheck) (head : LeanNet.HTTP1.RequestHead) : Except AppError Unit :=
  checks.forM (fun check => check head)

structure Metrics where
  requests : Nat := 0
  errors : Nat := 0
  totalMs : Nat := 0
  active : Nat := 0
  cancelled : Nat := 0
  deriving Repr

/-- Create per worker. Gauges use cancellation finalizers, including disconnects. -/
def metrics (state : IO.Ref Metrics) : Middleware := fun next ctx => do
  let started ← IO.monoMsNow
  state.modify fun s => { s with requests := s.requests + 1, active := s.active + 1 }
  let completed ← IO.mkRef false
  (do
    try
      let response ← next ctx
      completed.set true
      state.modify fun s => { s with errors := s.errors + (if response.status >= 500 then 1 else 0) }
      return response
    catch error =>
      completed.set true
      state.modify fun s => { s with errors := s.errors + (if error.status >= 500 then 1 else 0) }
      throw error).ensuring (do
      let elapsed := (← IO.monoMsNow) - started
      let done ← completed.get
      state.modify fun s => { s with active := s.active - 1, totalMs := s.totalMs + elapsed, cancelled := s.cancelled + (if done then 0 else 1) })

structure LogEvent where
  requestId : String
  route : String
  method : String
  status : Nat
  elapsedMs : Nat
  deriving Repr

structure LogBuffer where
  maxEntries : Nat := 1024
  entries : IO.Ref (Array LogEvent)
  dropped : IO.Ref Nat

def LogBuffer.new (maxEntries : Nat := 1024) : IO LogBuffer := do
  if maxEntries == 0 then throw (IO.userError "log capacity must be positive")
  return { maxEntries, entries := ← IO.mkRef #[], dropped := ← IO.mkRef 0 }

/-- Bounded in-memory logging. Drain outside reactor callbacks, or use lean-net's
    process-backed access logger. No console/disk write occurs in this middleware. -/
def logging (buffer : LogBuffer) : Middleware := fun next ctx => do
  let started ← IO.monoMsNow
  let response ← next ctx
  let entries ← buffer.entries.get
  if entries.size < buffer.maxEntries then
    buffer.entries.set (entries.push { requestId := ctx.requestId, route := ctx.routeName, method := ctx.request.method, status := response.status, elapsedMs := (← IO.monoMsNow) - started })
  else buffer.dropped.modify (· + 1)
  return response

end LeanWeave.Middleware
