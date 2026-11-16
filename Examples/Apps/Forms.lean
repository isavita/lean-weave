import Examples.Common
import LeanWeave.Core.Validation

namespace LeanWeave.Examples.Forms

private def loginBody (token : String) (errors : Array String := #[]) : Html.Html :=
  Html.page "Login" <| Html.fragment #[
    (html! "<h1>Forms and sessions</h1><p>Sign in as demo using WEAVE_DEMO_PASSWORD.</p>"),
    Html.form (localUrl "/login") .post #[
      Html.textField { name := "username", label := "Username", value := "demo", errors },
      Html.textField { name := "password", label := "Password" } "password",
      Html.element .button #[.text .type_ "submit"] #[Html.text "Sign in"]] token]

private def loginPage (store : Session.Store) : Handler := fun ctx => do
  let session ← ensureSession store ctx
  withSessionCookie (Html.toResponse (loginBody session.csrf)) session.id

private def login (store : Session.Store) (secret : String) : Handler := fun ctx => do
  let fields ← liftResult (Body.form ctx.request.body { maxBytes := 4096, maxFields := 8, maxFieldBytes := 512 })
  let validated := Validation.map2 Prod.mk
    (Validation.field fields "username" (Validation.bounded "username" 128))
    (Validation.field fields "password" (Validation.bounded "password" 512))
  let (username, password) ← match validated with
    | .ok credentials => pure credentials
    | .error errors =>
      return noStore (Html.toResponse (loginBody ((ctx.attribute "csrf").getD "") (errors.map (·.message))) 422)
  if username != "demo" || password != secret then
    return noStore (Html.toResponse (loginBody ((ctx.attribute "csrf").getD "") #["Invalid credentials"]) 401)
  let some id := ctx.sessionId | throw { status := 403, message := "login session required" }
  let session ← liftResult (← store.rotate id (← IO.monoMsNow))
  liftResult (← store.update session.id #[("userId", "demo")] #[("success", "Signed in; session ID rotated.")] (← IO.monoMsNow))
  withSessionCookie (Html.redirect (localUrl "/account")) session.id

private def account (store : Session.Store) : Handler := fun ctx => do
  let session ← ensureSession store ctx
  let messages ← liftResult (← store.takeFlash session.id (← IO.monoMsNow))
  let flashes := messages.map fun (_, message) => Html.flash { kind := .success, message }
  pure <| noStore <| Html.toResponse <| Html.page "Account" <| Html.fragment <|
    #[(html! "<h1>Welcome {{user}}</h1>") ((ctx.attribute "principal").getD "demo")] ++ flashes ++ #[
      Html.form (localUrl "/logout") .post #[Html.element .button #[.text .type_ "submit"] #[Html.text "Sign out"]] session.csrf]

private def logout (store : Session.Store) : Handler := fun ctx => do
  if let some id := ctx.sessionId then liftResult (← store.destroy id)
  pure <| expiredCookie (Html.redirect (localUrl "/login"))

/-- Sessions are shared by bounded try-lock storage; each worker composes its own pipeline once. -/
def make (secret : String) : IO Factory := do
  let store ← Session.Store.new { maxSessions := 256, maxValueBytes := 2048, lifetimeMs := 30 * 60 * 1000 }
  return fun _ => do
    let browser := fun handler => pipeline #[Middleware.session store devCookie,
      Middleware.sessionAuth, Middleware.csrf] handler
    router #[
      ("GET", "/", "home", fun _ => pure (Html.redirect (localUrl "/login"))),
      ("GET", "/login", "login.new", browser (loginPage store)),
      ("POST", "/login", "login.create", browser (login store secret)),
      ("GET", "/account", "account", browser (Middleware.requireAuth (account store))),
      ("POST", "/logout", "logout", browser (Middleware.requireAuth (logout store)))]

end LeanWeave.Examples.Forms
