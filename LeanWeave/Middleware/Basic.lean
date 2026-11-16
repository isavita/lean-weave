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
  let scheme := (ctx.header "x-forwarded-proto").getD "https"
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

/-- Validate the token after decoding the body. Header checks alone cannot verify form CSRF. -/
def csrf (limits : Body.Limits := {}) : Middleware := fun next ctx => do
  if !unsafeMethod ctx.request.method then return ← next ctx
  let some expected := ctx.attribute "csrf" | throw { status := 403, message := "CSRF session required" }
  let token ← match ctx.header "x-csrf-token" with
    | some token => pure token
    | none =>
      let contentType := (ctx.header "content-type").getD ""
      unless (contentType.splitOn ";").head!.trimAscii.toString.toLower == "application/x-www-form-urlencoded" do
        throw { status := 403, message := "CSRF token required" }
      let fields ← match Body.form ctx.request.body limits with
        | .ok fields => pure fields
        | .error error => throw error
      let values := fields.filter (·.1 == "_csrf")
      if values.size != 1 then throw { status := 403, message := "exactly one CSRF token required" }
      pure values[0]!.2
  unless Session.tokenEquals expected token do throw { status := 403, message := "invalid CSRF token" }
  next ctx

structure Cors where
  origins : Array String
  methods : Array String := #["GET", "HEAD", "POST", "PUT", "PATCH", "DELETE", "OPTIONS"]
  headers : Array String := #["content-type", "x-csrf-token"]
  credentials : Bool := false

/-- Exact-origin CORS; wildcard credentialed origins are rejected by construction at use. -/
def cors (policy : Cors) : Middleware := fun next ctx => do
  let rawOrigin := ctx.request.headers.filter (fun h => h.name.toLower == "origin")
  if rawOrigin.size > 1 || rawOrigin.any (fun h => (String.fromUTF8? h.value).isNone) then
    throw { status := 400, message := "invalid or repeated Origin" }
  let some origin := ctx.header "origin" | return ← next ctx
  unless policy.origins.contains origin && origin != "*" do
    throw { status := 403, message := "origin not allowed" }
  let response ← if ctx.request.method == "OPTIONS" && (ctx.header "access-control-request-method").isSome then do
    let requested := (ctx.header "access-control-request-method").getD ""
    unless policy.methods.contains requested do throw { status := 403, message := "CORS method not allowed" }
    for name in ((ctx.header "access-control-request-headers").getD "").splitOn "," do
      unless name.trimAscii.toString.isEmpty || policy.headers.contains name.trimAscii.toString.toLower do
        throw { status := 403, message := "CORS header not allowed" }
    pure (({ status := 204 } : Response).withHeader "access-control-allow-methods" (String.intercalate ", " policy.methods.toList)
      |>.withHeader "access-control-allow-headers" (String.intercalate ", " policy.headers.toList))
    else next ctx
  let vary := String.intercalate ", " ((response.headers.filter (fun h => h.name.toLower == "vary")).toList.map fun h => (String.fromUTF8? h.value).getD "*")
  let response := response.withHeader "access-control-allow-origin" origin
    |>.withHeader "vary" (if vary.isEmpty then "Origin" else vary ++ ", Origin")
  return if policy.credentials then response.withHeader "access-control-allow-credentials" "true" else response

/-- Header policies run before a decoded body exists. Scope them using head method/target. -/
abbrev HeaderCheck := LeanNet.HTTP1.RequestHead → Except AppError Unit

def sameOrigin (origin : String) (scope : LeanNet.HTTP1.RequestHead → Bool := fun _ => true)
    (allowMissing : Bool := false) : HeaderCheck := fun head => do
  if !scope head || !unsafeMethod head.method then return
  if (head.headers.filter (fun h => h.name.toLower == "origin")).size > 1 then
    throw { status := 400, message := "multiple Origin headers" }
  match header? head.headers "origin" with
  | some value => unless value == origin do throw { status := 403, message := "cross-origin request rejected" }
  | none => unless allowMissing do throw { status := 403, message := "Origin required" }

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
