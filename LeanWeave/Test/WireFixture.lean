import LeanWeave

open LeanWeave
open scoped LeanWeave.Router

private def greeting : String → Html.Html := html! "<p>Hello {{name}}</p>"

private def delay (cancelled : IO.Ref Nat) (ms : Nat) : Handler := fun _ => do
  let start ← IO.monoMsNow
  AppM.await
    (do
      if (← IO.monoMsNow) - start >= ms then return some (.ok (Response.text "slow"))
      return none)
    (fun _ => cancelled.modify (· + 1)) 1000 32

private def app (worker : Nat) : IO Handler := do
  let cancellations ← IO.mkRef 0
  let calls ← IO.mkRef 0
  let cache ← Cache.Store.new
  let cached := Cache.middleware cache { admit := some (fun ctx => pure (.cache ctx "public:cache")) } (fun ctx => do
    calls.modify (· + 1)
    let body := Lean.Json.mkObj [("host", Lean.toJson ctx.host), ("query", Lean.toJson ctx.query),
      ("locale", Lean.toJson ctx.locale), ("calls", Lean.toJson (← calls.get))]
    pure ((Response.json body).withHeader "cache-control" "public, max-age=10"))
  let slow := delay cancellations 300
  let guarded := pipeline #[Middleware.csrf {}] (fun _ => pure (Response.text "protected"))
  let csrfRoute : Handler := fun ctx => guarded { ctx with attributes := #[("csrf", String.ofList (List.replicate 64 'a'))] }
  let hello : String → Handler := fun name _ => pure (Html.toResponse (greeting name))
  let stats : Handler := fun _ => do pure (Response.text s!"worker={worker};cancelled={← cancellations.get}")
  let fast : Handler := fun _ => pure (Response.text "fast")
  let makeRoute := fun (method path name : String) (handler : Handler) => do
    let pattern ← Router.parsePattern path
    pure ({ method, path, name, handler, pattern } : Router.Route)
  let table := do
    Router.compile (← #[
      makeRoute "GET" "/hello/:name<text>" "hello" (fun c => do hello (← Router.captureText c "name") c),
      makeRoute "GET" "/fast" "fast" fast,
      makeRoute "GET" "/slow" "slow" slow,
      makeRoute "GET" "/cache" "cache" cached,
      makeRoute "GET" "/stats" "stats" stats,
      makeRoute "POST" "/protected" "protected" csrfRoute].mapM id)
  let routes ← match table with
    | .ok routes => pure routes
    | .error error => throw (IO.userError error)
  return pipeline #[Middleware.securityHeaders, Middleware.requestId, Middleware.errors] routes.handler

def main (args : List String) : IO Unit := do
  let port := (args.head?.bind String.toNat?).getD 8080
  if port == 0 || port > 65535 then throw (IO.userError "invalid port")
  Server.serve app {
    port := port.toUInt16
    appTimeoutMs := 1000
    headerChecks := #[Middleware.sameOrigin "http://localhost" (fun head => head.method == "POST") false]
    net := { workers := 1, tracing := false, shutdownDrainMs := 500, server := { maxClients := 32 }, machine := { limits := { maxBodyBytes := 4096 } } } }
