import LeanWeave.Core.Server
import LeanWeave.Router
import LeanWeave.Html
import LeanWeave.Middleware

namespace LeanWeave.Examples

abbrev Factory := Nat → IO Router

/-- Validate dynamically closed handler tables once at worker startup. The Pages
example shows the compile-time DSL; these tables bind worker-local state. -/
def router (rows : Array (String × String × String × Handler)) : IO Router := do
  let routes ← rows.mapM fun (method, path, name, handler) => do
    let pattern ← match Router.parsePattern path with
      | .ok pattern => pure pattern
      | .error message => throw (IO.userError message)
    return ({ name, method, path, pattern, handler } : Router.Route)
  match Router.compile routes with
  | .ok routes => return routes
  | .error message => throw (IO.userError message)

def localUrl (path : String) : Html.SafeUrl :=
  (Html.SafeUrl.parse path).toOption.getD Html.SafeUrl.root

def anchor (path label : String) : Html.Html :=
  Html.element .a #[.url .href (localUrl path)] #[Html.text label]

def liftResult (result : Except AppError α) : AppM α :=
  match result with | .ok value => pure value | .error error => throw error

def noStore (response : Response) : Response := response.withHeader "cache-control" "no-store"

/-- All example servers share typed startup settings and graceful lean-net shutdown. -/
def run (factory : Factory) (args : List String)
    (protectForms : Bool := false) : IO Unit := do
  if args == ["--routes"] then
    IO.println (← factory 0).listing
    return
  unless args.isEmpty do throw (IO.userError "usage: example [--routes] (WEAVE_PORT selects port)")
  let defaults : Server.Config := {
    net := {
      workers := 1
      healthPaths := true
      server := { maxClients := 128 }
      shutdownDrainMs := 2000 } }
  let config ← match ← Server.Config.fromEnv defaults with
    | .ok config => pure config
    | .error message => throw (IO.userError message)
  let origin := s!"http://{config.host}:{config.port}"
  let config := if protectForms then { config with headerChecks := #[Middleware.sameOrigin origin] } else config
  Server.serve (fun worker => do
    let routes ← factory worker
    return pipeline #[Middleware.securityHeaders, Middleware.requestId, Middleware.errors] routes.handler) config

end LeanWeave.Examples

namespace LeanWeave.Examples

/-- Development-only cookie for the loopback HTTP examples. Library defaults are Secure. -/
def devCookie : Session.CookieConfig := { name := "weave_dev", secure := false }

def ensureSession (store : Session.Store) (ctx : Context) : AppM Session.Data := do
  if let some id := ctx.sessionId then
    if let some data ← liftResult (← store.get id (← IO.monoMsNow)) then return data
  liftResult (← store.create (← IO.monoMsNow))

def withSessionCookie (response : Response) (id : String) : AppM Response := do
  let cookie ← match Session.cookieValue devCookie id with
    | .ok cookie => pure cookie
    | .error message => throw { message }
  pure <| noStore (response.withHeader "set-cookie" cookie)

def expiredCookie (response : Response) : Response :=
  noStore <| response.withHeader "set-cookie" "weave_dev=; Path=/; Max-Age=0; HttpOnly; SameSite=Lax"

/-- A configured demo secret has no built-in default and never appears in responses. -/
def demoPassword (args : List String) : IO String := do
  if args == ["--routes"] then return "route-listing-disabled"
  let some secret ← IO.getEnv "WEAVE_DEMO_PASSWORD"
    | throw (IO.userError "Set WEAVE_DEMO_PASSWORD (at least 12 characters) for the login example")
  if secret.length < 12 then throw (IO.userError "WEAVE_DEMO_PASSWORD must contain at least 12 characters")
  return secret

end LeanWeave.Examples
