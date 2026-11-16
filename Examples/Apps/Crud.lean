import Examples.Common
import LeanWeave.Core.Validation
import Std.Sync.Mutex

namespace LeanWeave.Examples.Crud

structure Post where
  id : Nat
  title : String
  revision : Nat := 1
  deriving Repr, Inhabited

private structure State where
  posts : Array Post := #[]
  nextId : Nat := 1

private abbrev Store := Std.Mutex State

/-- Bounded nonblocking in-memory demo store; contention fails fast with 503. -/
private def atomic (store : Store) (f : State → Except AppError (α × State)) : AppM α := do
  let operation : IO (Option (Except AppError α)) := store.tryAtomically do
    let state ← get
    match f state with
    | .error error => return .error error
    | .ok (value, next) => set next; return .ok value
  let result ← operation
  liftResult (result.getD (.error { status := 503, message := "posts busy; retry" }))

private def postJson (post : Post) : Lean.Json := Lean.Json.mkObj [
  ("id", Lean.toJson post.id), ("title", Lean.toJson post.title), ("revision", Lean.toJson post.revision)]

private def fields (ctx : Context) : AppM (Array (String × String)) := do
  let contentType := ((ctx.header "content-type").getD "").splitOn ";" |>.head! |>.trimAscii.toString.toLower
  if contentType == "application/json" then
    let json ← liftResult (Body.json ctx.request.body { maxBytes := 4096, maxDepth := 4, maxFields := 8 })
    let title := (json.getObjValAs? String "title").toOption.getD ""
    let revision := (json.getObjValAs? Nat "revision").toOption.map toString |>.getD ""
    return #[("title", title), ("revision", revision)]
  unless contentType == "application/x-www-form-urlencoded" do
    throw { status := 415, message := "expected form or JSON" }
  liftResult (Body.form ctx.request.body { maxBytes := 4096, maxFields := 8, maxFieldBytes := 512 })

private def title (values : Array (String × String)) : AppM String :=
  match Validation.field values "title" (Validation.bounded "title" 256) with
  | .ok value => pure value
  | .error errors => throw { status := 422, message := String.intercalate "; " (errors.toList.map fun e => e.field ++ " " ++ e.message) }

private def find (store : Store) (id : Nat) : AppM Post := atomic store fun state => do
  let some post := state.posts.find? (·.id == id) | throw { status := 404, message := "Post not found" }
  return (post, state)

private def postForm (post : Post) (csrf : String) : Html.Html :=
  Html.fragment #[
    Html.form (localUrl s!"/posts/{post.id}/save") .post #[
      Html.hidden "revision" (toString post.revision),
      Html.textField { name := "title", label := "Title", value := post.title },
      Html.element .button #[.text .type_ "submit"] #[Html.text "Save"]] csrf,
    Html.form (localUrl s!"/posts/{post.id}/delete") .post #[
      Html.element .button #[.text .type_ "submit"] #[Html.text "Delete"]] csrf]

private def index (store : Store) (sessions : Session.Store) : Handler := fun ctx => do
  let session ← ensureSession sessions ctx
  let posts ← atomic store fun state => .ok (state.posts, state)
  let body := Html.page "Posts" <| Html.fragment #[
    (html! "<h1>Posts</h1><p>A shared public demo board; at most 256 posts, 256 bytes per title.</p>"),
    Html.element .ul #[] (posts.map fun post => Html.element .li #[] #[anchor s!"/posts/{post.id}" post.title]),
    Html.form (localUrl "/posts") .post #[
      Html.textField { name := "title", label := "Title" },
      Html.element .button #[.text .type_ "submit"] #[Html.text "Create post"]] session.csrf]
  withSessionCookie (Html.negotiated ctx body (Lean.toJson (posts.map postJson))) session.id

private def showPost (store : Store) (sessions : Session.Store) (id : Nat) : Handler := fun ctx => do
  let post ← find store id
  let session ← ensureSession sessions ctx
  let body := Html.page post.title <| Html.fragment #[
    anchor "/posts" "All posts", (html! "<h1>{{title}}</h1><p>Revision {{revision}}</p>") post.title (toString post.revision),
    postForm post session.csrf]
  withSessionCookie (Html.negotiated ctx body (postJson post)) session.id

private def create (store : Store) : Handler := fun ctx => do
  let title ← title (← fields ctx)
  let post ← atomic store fun state => do
    if state.posts.size >= 256 then throw { status := 503, message := "post capacity reached" }
    let post : Post := { id := state.nextId, title }
    return (post, { posts := state.posts.push post, nextId := state.nextId + 1 })
  let response := match Html.negotiate (ctx.header "accept") with
    | some .json => Response.json (postJson post) 201
    | _ => Html.redirect (localUrl s!"/posts/{post.id}")
  pure <| noStore (response.withHeader "location" s!"/posts/{post.id}")

private def update (store : Store) (id : Nat) : Handler := fun ctx => do
  let values ← fields ctx
  let title ← title values
  let revision ← match Validation.field values "revision" (Validation.natural "revision") with
    | .ok revision => pure revision
    | .error _ => throw { status := 422, message := "revision must be a natural number" }
  let updated ← atomic store fun state => do
    let some post := state.posts.find? (·.id == id) | throw { status := 404, message := "Post not found" }
    if post.revision != revision then throw { status := 409, message := "Post changed; reload before saving" }
    let post := { post with title, revision := post.revision + 1 }
    return (post, { state with posts := state.posts.map fun p => if p.id == id then post else p })
  pure <| noStore <| match Html.negotiate (ctx.header "accept") with
    | some .json => Response.json (postJson updated)
    | _ => Html.redirect (localUrl s!"/posts/{id}")

private def delete (store : Store) (id : Nat) : Handler := fun ctx => do
  atomic store fun state => do
    unless state.posts.any (·.id == id) do throw { status := 404, message := "Post not found" }
    return ((), { state with posts := state.posts.filter (·.id != id) })
  pure <| noStore <| if Html.negotiate (ctx.header "accept") == some .json then
    Response.text "" 204 else Html.redirect (localUrl "/posts")

private def csrf (sessions : Session.Store) : Handler := fun ctx => do
  let session ← ensureSession sessions ctx
  withSessionCookie (Response.json (Lean.Json.mkObj [("csrf", Lean.toJson session.csrf)])) session.id

/-- Mutations use session CSRF plus an early exact Origin check in the server wrapper.
All data is ephemeral and public; authentication is demonstrated by Forms instead. -/
def make : IO Factory := do
  let store ← Std.Mutex.new ({} : State)
  let sessions ← Session.Store.new { maxSessions := 256 }
  return fun _ => do
    let browser := fun handler => pipeline #[Middleware.session sessions devCookie, Middleware.csrf] handler
    let captured := fun handler => fun ctx => do handler (← Router.captureNat ctx "id") ctx
    router #[
      ("GET", "/", "home", fun _ => pure (Html.redirect (localUrl "/posts"))),
      ("GET", "/csrf", "csrf", browser (csrf sessions)),
      ("GET", "/posts", "posts.index", browser (index store sessions)),
      ("GET", "/posts/new", "posts.new", browser (index store sessions)),
      ("POST", "/posts", "posts.create", browser (create store)),
      ("GET", "/posts/:id<nat>", "posts.show", browser (captured (showPost store sessions))),
      ("GET", "/posts/:id<nat>/edit", "posts.edit", browser (captured (showPost store sessions))),
      ("POST", "/posts/:id<nat>/save", "posts.save", browser (captured (update store))),
      ("PATCH", "/posts/:id<nat>", "posts.update", browser (captured (update store))),
      ("PUT", "/posts/:id<nat>", "posts.replace", browser (captured (update store))),
      ("POST", "/posts/:id<nat>/delete", "posts.remove", browser (captured (delete store))),
      ("DELETE", "/posts/:id<nat>", "posts.delete", browser (captured (delete store)))]

end LeanWeave.Examples.Crud
