import LeanWeave.OpenApi

open LeanWeave
open scoped LeanWeave.Router LeanWeave.OpenApi

namespace LeanWeave.Test.MacroHygiene

private def response (outer : Nat) (saved : Context) (capture : Nat) (incoming : Context) : AppM Response :=
  pure (Response.text s!"{outer}/{saved.requestId}/{capture}/{incoming.requestId}")

section
variable (__weave_capture_0 : Nat) (__weave_context : Context)
weave_routes collisionRoutes where {
  route "GET" "/router/:id<nat>" as item => response __weave_capture_0 __weave_context,
  route "GET" "/query/:query<nat>" as queryItem => response __weave_capture_0 __weave_context,
  route "GET" "/name/:collisionRoutes<nat>" as routerName => response __weave_capture_0 __weave_context
}
end

private def number (outer capture : Nat) (_ : Context) : AppM Nat := pure (outer + capture)
section
variable (__weave_api_capture_0 : Nat)
weave_api collisionApi where {
  route "GET" "/api/:id<nat>" as item returns Nat => number __weave_api_capture_0
}
end

section
variable {__weave_api_capture_0 : Nat}
weave_api implicitApi where {
  route "GET" "/implicit/:id<nat>" as item returns Nat => number __weave_api_capture_0
}
end

private def generic {α : Type} [ToString α] (outer : α) (capture : Nat) (_ : Context) : AppM String :=
  pure (toString outer ++ "/" ++ toString capture)
section
variable {α : Type} [ToString α] (__weave_api_capture_0 : α)
weave_api genericApi where {
  route "GET" "/generic/:id<nat>" as item returns String => generic __weave_api_capture_0
}
end

private def check (value : Bool) (message : String) : IO Unit :=
  unless value do throw (IO.userError ("route macro hygiene: " ++ message))
private def need (result : Except ε α) : IO α := match result with
  | .ok value => pure value
  | .error _ => throw (IO.userError "expected successful route macro result")
private def context (path : String) : IO Context := do
  let context ← need (Context.fromRequest {
    method := "GET"
    target := path.toUTF8
    headers := #[{ name := "host", value := "example.test".toUTF8 }], body := ByteArray.empty })
  return { context with requestId := "incoming" }

def run : IO Unit := do
  let saved := { (← context "/") with requestId := "outside" }
  let router : Nat → Context → Router := collisionRoutes
  for path in ["/router/7", "/query/7", "/name/7"] do
    let result ← need (← (Router.handler (router 40 saved) (← context path)).run)
    check (String.fromUTF8! result.body == "40/outside/7/incoming") "caller capture/context remain distinct from generated binders"
  check ((← need (collisionRoutes.itemUrl 7)) == "/router/7") "reverse helper independent of handler section parameters"
  check ((← need (collisionRoutes.queryItemUrl 7 #[("page", "1")])) == "/query/7?page=1") "path capture named query does not capture URL query array"
  check ((← need (collisionRoutes.routerNameUrl 7)) == "/name/7") "capture named after router cannot shadow router reference"
  let api : Nat → OpenApi.Api := collisionApi
  let result ← need (← (Router.handler (api 40).router (← context "/api/7")).run)
  check (String.fromUTF8! result.body == "47") "OpenAPI caller capture retains outer value"
  let result ← need (← (Router.handler (implicitApi ( __weave_api_capture_0 := 40)).router (← context "/implicit/7")).run)
  check (String.fromUTF8! result.body == "47") "OpenAPI implicit section parameter retained"
  let result ← need (← (Router.handler (genericApi (40 : Nat)).router (← context "/generic/7")).run)
  check (String.fromUTF8! result.body == "\"40/7\"") "OpenAPI generic section/typeclass parameters retained"
  check ((← need (collisionApi.routes.itemUrl 7)) == "/api/7") "typed API URL stays capture-typed"
  IO.println "PASS route/OpenAPI hygiene: caller sections, context, implicit/typeclass parameters and reverse URLs"

end LeanWeave.Test.MacroHygiene
