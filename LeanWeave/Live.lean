import LeanWeave.Html.Basic
import LeanWeave.Core.Body
import LeanWeave.SSE
import LeanWeave.Live.Client
import LeanNet.WebSocket.Server

namespace LeanWeave.Live

structure Region where
  key : String
  html : Html.Html

/-- Typed events and state remain on the owning server worker. `stateBytes`
is the application's conservative retained-state reservation. -/
structure Component (State Event : Type) where
  mount : Context → IO State
  decode : Lean.Json → Except String Event
  update : State → Event → IO State
  render : State → Array Region
  stateBytes : State → Nat

structure Config where
  /-- Mandatory exact browser Origin, including scheme and nondefault port. -/
  origin : String
  basePath : String := "/live"
  secureCookie : Bool := true
  cookieName : String := "__Host-weave-live"
  maxSessions : Nat := 128
  maxStateBytes : Nat := 1048576
  maxHtmlBytes : Nat := 262144
  maxMessageBytes : Nat := 65536
  maxQueuedMessages : Nat := 32
  maxQueuedBytes : Nat := 262144
  ttlMs : Nat := 1800000
  keepAliveMs : Nat := 15000

private structure Session (State : Type) where
  id : String
  csrf : String
  state : State
  rendered : Array (String × String)
  version : Nat := 0
  generation : Nat := 0
  outbox : Array ByteArray := #[]
  queuedBytes : Nat := 0
  expires : Nat

structure Runtime (State Event : Type) where
  private sessions : IO.Ref (Array (Session State))
  component : Component State Event
  config : Config

private def validKey (key : String) : Bool :=
  !key.isEmpty && key.utf8ByteSize <= 64 && key.toUTF8.data.all (fun b =>
    (b >= 48 && b <= 57) || (b >= 65 && b <= 90) || (b >= 97 && b <= 122) || b == 45 || b == 95)

private def randomToken : IO String := do
  return String.intercalate "" ((← IO.getRandomBytes 32).toList.map fun b =>
    let chars := "0123456789abcdef".toList.toArray
    String.ofList [chars[b.toNat / 16]!, chars[b.toNat % 16]!])

def Runtime.new (component : Component State Event) (config : Config) : IO (Runtime State Event) := do
  if config.origin.isEmpty || !(config.origin.startsWith "https://" || config.origin.startsWith "http://") ||
      config.origin.toUTF8.data.any (fun b => b <= 32 || b == 127) ||
      !config.basePath.startsWith "/" || config.basePath.startsWith "//" ||
      config.basePath.toList.any (fun c => c == '?' || c == '#' || c == '\\' || c.toNat <= 32) ||
      !validKey config.cookieName || (config.cookieName.startsWith "__Host-" && !config.secureCookie) ||
      config.maxSessions == 0 || config.maxSessions > 4096 || config.maxStateBytes == 0 ||
      config.maxHtmlBytes == 0 || config.maxMessageBytes == 0 || config.maxMessageBytes > 1048576 ||
      config.maxQueuedMessages == 0 || config.maxQueuedBytes < config.maxMessageBytes ||
      config.ttlMs < 1000 || config.keepAliveMs == 0 then
    throw (IO.userError "invalid bounded Live runtime configuration")
  return ⟨← IO.mkRef #[], component, config⟩

private def cookieId (headers : Array LeanNet.HTTP1.Header) (name : String) : Option String := do
  let cookie ← header? headers "cookie"
  let part ← (cookie.splitOn ";").find? fun part => part.trimAscii.toString.startsWith (name ++ "=")
  let value := String.intercalate "=" ((part.trimAscii.toString.splitOn "=").drop 1)
  if value.utf8ByteSize != 64 || !value.toUTF8.data.all (fun b =>
      (b >= 48 && b <= 57) || (b >= 97 && b <= 102)) then none else some value

private def Runtime.rendered (runtime : Runtime State Event) (state : State) : Except String (Array (String × String)) := do
  if runtime.component.stateBytes state > runtime.config.maxStateBytes then throw "Live state exceeds byte reservation"
  let regions := runtime.component.render state
  if regions.size > 128 then throw "Live region count exceeds bound"
  let mut keys : Array String := #[]
  let mut rendered := #[]
  let mut bytes := 0
  for region in regions do
    if !validKey region.key || keys.contains region.key then throw "invalid/duplicate Live region key"
    keys := keys.push region.key
    bytes := bytes + region.html.bytes.size
    if bytes > runtime.config.maxHtmlBytes then throw "Live HTML exceeds byte bound"
    rendered := rendered.push (region.key, String.fromUTF8! region.html.bytes)
  return rendered

private def patchJson (key : String) (html : Option String) : Lean.Json :=
  Lean.Json.mkObj [("key", .str key), ("html", html.map Lean.Json.str |>.getD .null)]

private def packet (kind : String) (version : Nat) (patches : Array Lean.Json) : ByteArray :=
  (Lean.Json.mkObj [("type", .str kind), ("version", Lean.toJson version), ("patches", .arr patches)]).compress.toUTF8

private def snapshotPacket (session : Session State) : ByteArray :=
  packet "snapshot" session.version (session.rendered.map fun (key, html) => patchJson key (some html))

private def Runtime.lookup (runtime : Runtime State Event) (headers : Array LeanNet.HTTP1.Header) : IO (Option (Session State)) := do
  let now ← IO.monoMsNow
  runtime.sessions.modify (·.filter (fun session => now < session.expires))
  let some id := cookieId headers runtime.config.cookieName | return none
  return (← runtime.sessions.get).find? (·.id == id)

private def Runtime.put (runtime : Runtime State Event) (session : Session State) : IO Unit :=
  runtime.sessions.modify fun sessions => (sessions.filter (·.id != session.id)).push session

/-- Mount an opaque cookie-bound server session. Caller controls page access
through its normal authenticated browser pipeline. All dynamic HTML is escaped. -/
def Runtime.mount (runtime : Runtime State Event) (ctx : Context) : IO Response := do
  if runtime.config.secureCookie && !ctx.secure then return Response.text "Live requires HTTPS" 400
  let now ← IO.monoMsNow
  runtime.sessions.modify (·.filter (fun session => now < session.expires))
  let previous := cookieId ctx.request.headers runtime.config.cookieName
  let retained := (← runtime.sessions.get).filter (fun session => previous != some session.id)
  if retained.size >= runtime.config.maxSessions then return Response.text "Live session capacity exhausted" 503
  let state ← runtime.component.mount ctx
  let .ok rendered := runtime.rendered state | return Response.text "Live rendering exceeds bounds" 503
  let id ← randomToken
  let csrf ← randomToken
  let session : Session State := { id, csrf, state, rendered, expires := now + runtime.config.ttlMs }
  if (snapshotPacket session).size > runtime.config.maxMessageBytes then return Response.text "Live snapshot exceeds message bound" 503
  runtime.sessions.set retained
  runtime.put session
  let metadataHtml := Html.fragment #[
    Html.void .meta_ #[.text .name "weave-base", .text .content runtime.config.basePath],
    Html.void .meta_ #[.text .name "weave-csrf", .text .content csrf]]
  let regions := rendered.map fun (key, html) => Html.element .div #[.text .id ("weave-" ++ key)] #[Html.rawUnsafe html.toUTF8]
  let page := Html.fragment #[Html.rawUnsafe "<!doctype html><html><head><meta charset=\"utf-8\">".toUTF8,
    metadataHtml, Html.rawUnsafe "</head><body>".toUTF8,
    Html.element .main #[.text .id "weave-live"] regions,
    Html.rawUnsafe (("<script defer src=\"".toUTF8 |> fun b => Html.escapeInto b (runtime.config.basePath ++ "/client.js")) ++ "\"></script></body></html>".toUTF8)]
  let cookie := s!"{runtime.config.cookieName}={id}; Path=/; HttpOnly; SameSite=Strict; Max-Age={runtime.config.ttlMs / 1000}" ++
    (if runtime.config.secureCookie then "; Secure" else "")
  return (Html.toResponse page).withHeader "set-cookie" cookie |>.withHeader "cache-control" "no-store"
    |>.withHeader "referrer-policy" "no-referrer" |>.withHeader "content-security-policy" "default-src 'self'; script-src 'self'; object-src 'none'; base-uri 'none'; frame-ancestors 'none'"

/-- Revoke the cookie-bound Live state on logout/permission revocation. Existing
connections terminate on their next owner-worker poll. -/
def Runtime.invalidate (runtime : Runtime State Event) (ctx : Context) : IO Unit := do
  if let some id := cookieId ctx.request.headers runtime.config.cookieName then
    runtime.sessions.modify (·.filter (·.id != id))

def Runtime.close (runtime : Runtime State Event) : IO Unit := runtime.sessions.set #[]

private def equalToken (left right : String) : Bool := Id.run do
  let a := left.toUTF8
  let b := right.toUTF8
  if a.size != b.size then return false
  let mut difference : UInt8 := 0
  for i in [:a.size] do difference := difference ||| (a[i]! ^^^ b[i]!)
  return difference == 0

private def Runtime.dispatch (runtime : Runtime State Event) (id : String) (bytes : ByteArray) : IO (Except String Unit) := do
  if bytes.size > runtime.config.maxMessageBytes then return .error "Live command exceeds byte bound"
  let .ok json := Body.json bytes { maxBytes := runtime.config.maxMessageBytes } | return .error "Live command JSON bounds/encoding rejected"
  let sessions ← runtime.sessions.get
  let some session := sessions.find? (·.id == id) | return .error "Live session expired"
  if (← IO.monoMsNow) >= session.expires then return .error "Live session expired"
  let .ok csrf := json.getObjValAs? String "csrf" | return .error "Live CSRF missing"
  if !equalToken csrf session.csrf then return .error "Live CSRF rejected"
  let .ok event := json.getObjVal? "event" | return .error "Live event missing"
  let .ok event := runtime.component.decode event | return .error "Live event invalid"
  let state ← runtime.component.update session.state event
  let .ok rendered := runtime.rendered state | return .error "Live rendering exceeds bounds"
  let mut patches := rendered.filterMap fun (key, html) =>
    if session.rendered.any (fun (oldKey, oldHtml) => key == oldKey && html == oldHtml) then none else some (patchJson key (some html))
  for (key, _) in session.rendered do
    if !rendered.any (·.1 == key) then patches := patches.push (patchJson key none)
  let next := { session with state, rendered, version := session.version + 1 }
  let bytes := packet "patch" next.version patches
  let full := snapshotPacket next
  if max bytes.size full.size > runtime.config.maxMessageBytes then return .error "Live diff exceeds message bound"
  let next := if next.outbox.size >= runtime.config.maxQueuedMessages || bytes.size > runtime.config.maxQueuedBytes - next.queuedBytes then
      { next with outbox := #[full], queuedBytes := full.size }
    else { next with outbox := next.outbox.push bytes, queuedBytes := next.queuedBytes + bytes.size }
  runtime.put next
  return .ok ()

private def Runtime.open (runtime : Runtime State Event) (headers : Array LeanNet.HTTP1.Header) : IO (Except String (String × Nat)) := do
  let some session ← runtime.lookup headers | return .error "Live session missing"
  let next := { session with generation := session.generation + 1, outbox := #[snapshotPacket session], queuedBytes := (snapshotPacket session).size }
  runtime.put next
  return .ok (next.id, next.generation)

private def Runtime.poll (runtime : Runtime State Event) (id : String) (generation : Nat) : IO (Option ByteArray) := do
  let some session := (← runtime.sessions.get).find? (·.id == id) | throw (IO.userError "Live session expired")
  if generation != session.generation || (← IO.monoMsNow) >= session.expires then throw (IO.userError "Live connection superseded/expired")
  let some bytes := session.outbox[0]? | return none
  runtime.put { session with outbox := session.outbox.extract 1 session.outbox.size, queuedBytes := session.queuedBytes - bytes.size }
  return some bytes

/-- Exact-Origin and cookie-bound commands for the SSE fallback. -/
def Runtime.post (runtime : Runtime State Event) (ctx : Context) : IO Response := do
  if ctx.request.method != "POST" then return (Response.text "Method not allowed" 405).withHeader "allow" "POST"
  if ctx.header "origin" != some runtime.config.origin then return Response.text "Live Origin rejected" 403
  let some session ← runtime.lookup ctx.request.headers | return Response.text "Live session missing" 401
  match ← runtime.dispatch session.id ctx.request.body with
  | .ok () => return { status := 204 }
  | .error _ => return Response.text "Live command rejected" 403

def Runtime.sse (runtime : Runtime State Event) (ctx : Context) : IO Response := do
  if (ctx.header "origin").any (· != runtime.config.origin) then return Response.text "Live Origin rejected" 403
  let .ok (id, generation) ← runtime.open ctx.request.headers | return Response.text "Live session missing" 401
  let pending ← IO.mkRef ByteArray.empty
  let emitted ← IO.mkRef (← IO.monoMsNow)
  let body : Stream.Body := {
    retainedBytes := runtime.config.maxMessageBytes + 1024
    idleTimeoutMs := runtime.config.keepAliveMs * 3
    poll := fun budget => do
      if (← pending.get).isEmpty then
        if let some bytes ← runtime.poll id generation then
          let .ok event := (SSE.Event.mk (String.fromUTF8! bytes) none (some "patch") none).encode
            | throw (IO.userError "invalid Live SSE packet")
          pending.set event
        else
          if (← IO.monoMsNow) - (← emitted.get) < runtime.config.keepAliveMs then return none
          pending.set ": keep-alive\n\n".toUTF8
      if budget == 0 then return none
      let bytes ← pending.get
      let count := min budget bytes.size
      pending.set (bytes.extract count bytes.size)
      emitted.set (← IO.monoMsNow)
      return some (.data (bytes.extract 0 count))
    close := fun _ => pending.set ByteArray.empty }
  return (Response.streaming body).withHeader "content-type" "text/event-stream" |>.withHeader "cache-control" "no-store"

private def Runtime.callbacks (runtime : Runtime State Event) (headers : Array LeanNet.HTTP1.Header) : IO (Option LeanNet.WebSocket.Callbacks) := do
  let .ok (id, generation) ← runtime.open headers | return none
  let emitted ← IO.mkRef (← IO.monoMsNow)
  return some {
    onMessage := fun opcode bytes => do
      if opcode != .text then throw (IO.userError "Live requires text messages")
      let .ok () ← runtime.dispatch id bytes | throw (IO.userError "Live command rejected")
      return none
    onPoll := do
      if let some bytes ← runtime.poll id generation then
        emitted.set (← IO.monoMsNow)
        return some { opcode := .text, payload := bytes }
      let now ← IO.monoMsNow
      if now - (← emitted.get) < runtime.config.keepAliveMs then return none
      emitted.set now
      return some { opcode := .ping, payload := ByteArray.empty } }

def Runtime.upgradeHTTP1 (runtime : Runtime State Event) (head : LeanNet.HTTP1.RequestHead) : IO LeanNet.HTTP1.HeaderResult := do
  let policy : LeanNet.WebSocket.Handshake.Policy := { originAllowed := fun value => value == some runtime.config.origin.toUTF8 }
  if let .error rejection := LeanNet.WebSocket.Handshake.acceptHTTP1 head policy then
    return .reject { response := { status := rejection.status } }
  let some callbacks ← runtime.callbacks head.headers | return .reject { response := { status := 401 } }
  LeanNet.WebSocket.Server.acceptHTTP1 head policy callbacks

def Runtime.upgradeHTTP2 (runtime : Runtime State Event) (head : LeanNet.HTTP2.RequestHead) : IO (Option (LeanNet.WebSocket.Handshake.Policy × LeanNet.WebSocket.Callbacks)) := do
  let policy : LeanNet.WebSocket.Handshake.Policy := { originAllowed := fun value => value == some runtime.config.origin.toUTF8 }
  if let .error _ := LeanNet.WebSocket.Handshake.acceptHTTP2 head head.protocol policy then return none
  let some callbacks ← runtime.callbacks head.headers | return none
  return some (policy, callbacks)

def clientResponse : Response :=
  (Response.text clientScript).withHeader "content-type" "text/javascript; charset=utf-8" |>.withHeader "cache-control" "no-cache"

end LeanWeave.Live
