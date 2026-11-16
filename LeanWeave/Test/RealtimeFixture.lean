import LeanWeave.Core.Server
import LeanWeave.SSE
import LeanWeave.Stream
import LeanWeave.Live
import LeanWeave.Security.Csp

open LeanWeave LeanWeave.Security.Csp

private inductive CounterEvent where
  | increment | decrement

private def counter : Live.Component Int CounterEvent := {
  mount := fun _ => pure 0
  decode := fun json => do
    match ← json.getStr? with
    | "inc" => return .increment
    | "dec" => return .decrement
    | _ => throw "unknown counter event"
  update := fun state event => pure (match event with | .increment => state + 1 | .decrement => state - 1)
  render := fun state => #[
    ⟨"counter", Html.element .section #[] #[
      Html.element .h1 #[] #[Html.text "LeanWeave Live counter"],
      Html.element .p #[.text .id "count"] #[Html.text (toString state)],
      Html.element .button #[.text .name "weave-event", .text .value "inc"] #[Html.text "Increment"],
      Html.element .button #[.text .name "weave-event", .text .value "dec"] #[Html.text "Decrement"]]⟩,
    ⟨"unchanged", Html.element .p #[.text .id "static"] #[Html.text "This region stays unchanged"]⟩]
  stateBytes := fun _ => 64 }

private def application (port : Nat) : IO Server.Application := do
  let origin := s!"http://127.0.0.1:{port}"
  let live ← Live.Runtime.new counter { origin, secureCookie := false, cookieName := "weave-live", keepAliveMs := 80 }
  let store ← SSE.Store.new { maxEvents := 3, maxBytes := 4096, maxEventBytes := 1024, keepAliveMs := 30, retryMs := 50 }
  let cspPage := Security.Csp.middleware (csp! "default-src 'self'; script-src 'nonce'; object-src 'none'; base-uri 'self'; frame-ancestors 'none'") (fun ctx => do
    let nonce := (Security.Csp.nonce ctx).getD ""
    let html := s!"<!doctype html><html><body><h1>CSP browser acceptance</h1><script nonce=\"{nonce}\">window.allowed=true</script><script nonce=\"wrong\">window.wrongNonce=true</script><script>window.missingNonce=true</script></body></html>"
    return Html.toResponse (Html.rawUnsafe html.toUTF8))
  let handler : Handler := fun ctx => do
    match ctx.path.toList with
    | [] => return ← live.mount ctx
    | ["csp"] => cspPage ctx
    | ["live", "client.js"] => return Live.clientResponse
    | ["live", "event"] => return ← live.post ctx
    | ["live", "events"] => return ← live.sse ctx
    | ["live", "invalidate"] =>
      if ctx.request.method != "POST" || ctx.header "origin" != some origin then return Response.text "Forbidden" 403
      live.invalidate ctx
      return Response.text "" 204
    | ["events"] => return ← store.response ctx
    | ["publish"] =>
      match ← store.publish { data := (ctx.queryValue "data").getD "event" } with
      | .ok id => return Response.text id
      | .error message => throw { status := 400, message }
    | ["multipart"] =>
      let one ← Stream.chunks #["one".toUTF8]
      let two ← Stream.chunks #["two".toUTF8]
      let body ← Stream.multipart "weave" #[⟨#[], one⟩, ⟨#[], two⟩]
      return (Response.streaming body).withHeader "content-type" "multipart/mixed; boundary=weave"
    | _ => return Response.text "unknown" 404
  return {
    handler
    tickIntervalMs := 2
    onUpgrade := fun head _ => do
      let ctx := Context.fromRequest { method := head.method, version := head.version, target := head.target, headers := head.headers, body := ByteArray.empty }
      if ctx.toOption.any (·.path == #["live", "socket"]) && head.version != "HTTP/2" then return some (← live.upgradeHTTP1 head)
      return none
    onWebSocket := fun head => do
      let ctx := Context.fromRequest { method := head.method, version := "HTTP/2", target := head.target, headers := head.headers, body := ByteArray.empty }
      if ctx.toOption.any (·.path == #["live", "socket"]) then live.upgradeHTTP2 head else pure none }

def main (args : List String) : IO Unit := do
  let some port := args[0]?.bind String.toNat? | throw (IO.userError "usage: weave-realtime-fixture PORT")
  Server.serveApplications (fun _ use => do use (← application port)) {
    port := port.toUInt16
    net := { workers := 1, shutdownDrainMs := 100, server := { idleTimeoutMs := 300 }, http2 := { enableConnectProtocol := true }, deferred := { pollIntervalMs := 2 } } }
