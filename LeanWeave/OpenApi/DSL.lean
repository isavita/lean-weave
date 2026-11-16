import Lean
import LeanWeave.OpenApi.Basic

open Lean Elab Command
open scoped LeanWeave.Router

namespace LeanWeave.OpenApi

declare_syntax_cat weave_api_route
scoped syntax "route " str str " as " ident " returns " term " => " term : weave_api_route
scoped syntax "route " str str " as " ident " returns " term " status " num " => " term : weave_api_route
scoped syntax "route " str str " as " ident " accepts " term " returns " term " => " term : weave_api_route
scoped syntax "route " str str " as " ident " accepts " term " returns " term " status " num " => " term : weave_api_route

/-- Compile typed HTTP routes and their OpenAPI metadata from the same declarations.
    A handler receives typed path captures, optional JSON input, then Context.
    Named reverse helpers live in `api.routes.<name>Url`.
```
weave_api api where {
  route "GET" "/users/:id<nat>" as users.show returns User => showUser,
  route "POST" "/users" as users.create accepts NewUser returns User status 201 => createUser
}
```
-/
scoped syntax (name := weaveApi) "weave_api " ident " where " "{" weave_api_route,* "}" : command

private structure Row where
  method : StrLit
  path : StrLit
  name : Ident
  input : Option Term
  output : Term
  code : Nat
  target : Term

private def parseRow (entry : TSyntax `weave_api_route) : CommandElabM Row := do
  let (method, path, name, input, output, code, target) ← match entry with
    | `(weave_api_route| route $method:str $path:str as $name:ident returns $output:term => $target:term) =>
      pure (method, path, name, none, output, 200, target)
    | `(weave_api_route| route $method:str $path:str as $name:ident returns $output:term status $code:num => $target:term) =>
      pure (method, path, name, none, output, code.getNat, target)
    | `(weave_api_route| route $method:str $path:str as $name:ident accepts $input:term returns $output:term => $target:term) =>
      pure (method, path, name, some input, output, 200, target)
    | `(weave_api_route| route $method:str $path:str as $name:ident accepts $input:term returns $output:term status $code:num => $target:term) =>
      pure (method, path, name, some input, output, code.getNat, target)
    | _ => throwErrorAt entry "invalid typed API route"
  if code < 200 || code > 299 then throwErrorAt entry "typed API success status must be from 200 to 299"
  return ⟨method, path, name, input, output, code, target⟩

private def wrapTarget (row : Row) : CommandElabM Term := do
  let segments ← match Router.parsePattern row.path.getString with
    | .ok segments => pure segments | .error message => throwErrorAt row.path "{message}"
  let captures := segments.filterMap fun segment => match segment with
    | .literal _ => none
    | .capture _ .nat => some (mkIdent ``Nat)
    | .capture _ .int => some (mkIdent ``Int)
    | .capture _ .text | .rest _ => some (mkIdent ``String)
  let arguments := captures.mapIdx fun index _ => mkIdent (Name.mkSimple s!"__weave_api_capture_{index}")
  let target ← `($(row.target) $arguments*)
  let mut wrapped ← match row.input with
    | none => `(Schema.outputHandler $(row.output) $target $(quote row.code))
    | some input => `(Schema.jsonHandler $input $(row.output) $target $(quote row.code))
  for i in (List.range captures.size).reverse do
    let argument := arguments[i]!
    let type := captures[i]!
    wrapped ← `(fun ($argument : $type) => $wrapped)
  return wrapped

@[command_elab weaveApi] def elabWeaveApi : CommandElab := fun stx => do
  let `(weave_api $name:ident where { $entries:weave_api_route,* }) := stx | throwUnsupportedSyntax
  let rows ← entries.getElems.mapM parseRow
  let routerName := mkIdent (name.getId ++ `routes)
  let mut routes : Array (TSyntax `weave_route) := #[]
  let mut operations : Array Term := #[]
  for row in rows do
    let wrapped ← wrapTarget row
    routes := routes.push (← `(weave_route| route $(row.method):str $(row.path):str as $(row.name):ident => $wrapped))
    let request ← match row.input with
      | none => `((none : Option Schema.Definition))
      | some input => `(some (Schema.definition $input))
    operations := operations.push (← `(({ name := $(quote row.name.getId.toString), request := $request, response := Schema.definition $(row.output), «status» := $(quote row.code) } : Operation)))
  elabCommand (← `(weave_routes $routerName where { $routes:weave_route,* }))
  elabCommand (← `(def $name : Api := { router := $routerName, operations := #[$operations,*] }))

end LeanWeave.OpenApi
