import Lean
import LeanWeave.Router.Runtime

open Lean.Parser.Term

open Lean Elab Command

namespace LeanWeave.Router

declare_syntax_cat weave_route
scoped syntax "route " str str " as " ident " => " term : weave_route
scoped syntax "resources " str " as " ident " => " term : weave_route
scoped syntax "group " str " {" weave_route,* "}" : weave_route
scoped syntax "group " str " host " str " {" weave_route,* "}" : weave_route
scoped syntax "group " str " using " "(" term ")" " {" weave_route,* "}" : weave_route
scoped syntax "group " str " host " str " using " "(" term ")" " {" weave_route,* "}" : weave_route

/-- Compile a named router and typed reverse helpers. Example:
```
weave_routes routes where {
  route "GET" "/users/:id<nat>" as users.show => showUser,
  group "/api" host "api.example.com" using (apiPipeline) {
    resources "/posts" as posts => postsController
  }
}
```
Handlers receive capture values in path order, followed by `Context`. -/
scoped syntax (name := weaveRoutes) "weave_routes " ident " where " "{" weave_route,* "}" : command

private structure Row where
  source : Syntax
  name : Name
  method : String
  path : String
  hostRule : Host
  segments : Array Segment
  target : TSyntax `term
  pipelines : Array (TSyntax `term)
  deriving Inhabited

private def joinPath (basePath path : String) : String :=
  if basePath.isEmpty || basePath == "/" then path
  else if path == "/" then basePath
  else if basePath.endsWith "/" && path.startsWith "/" then basePath ++ (path.drop 1).toString
  else basePath ++ path

private def mkRow (source : Syntax) (name : Name) (method path : String)
    (hostRule : Host) (target : TSyntax `term) (pipelines : Array (TSyntax `term)) : CommandElabM Row := do
  let segments ← match parsePattern path with
    | .ok value => pure value
    | .error message => throwErrorAt source "{message}"
  unless !method.isEmpty && method.toList.all (fun c => c >= 'A' && c <= 'Z') do
    throwErrorAt source "methods must be uppercase ASCII tokens"
  return { source, name, method, path, hostRule, segments, target, pipelines }

private partial def flatten (entries : Array (TSyntax `weave_route)) (basePath : String := "")
    (hostRule : Host := .any) (pipelines : Array (TSyntax `term) := #[]) : CommandElabM (Array Row) := do
  let mut rows := #[]
  for entry in entries do
    match entry with
    | `(weave_route| route $method:str $path:str as $name:ident => $target:term) =>
      rows := rows.push (← mkRow entry name.getId method.getString
        (joinPath basePath path.getString) hostRule target pipelines)
    | `(weave_route| resources $path:str as $name:ident => $controller:term) =>
      let path := joinPath basePath path.getString
      for (method, suffix, action) in #[
          ("GET", "", "index"), ("GET", "/new", "new"), ("POST", "", "create"),
          ("GET", "/:id<nat>", "show"), ("GET", "/:id<nat>/edit", "edit"),
          ("PATCH", "/:id<nat>", "update"), ("PUT", "/:id<nat>", "replace"),
          ("DELETE", "/:id<nat>", "delete")] do
        let field := mkIdent (Name.mkSimple (if action == "replace" then "update" else action))
        let target ← `($controller.$field)
        let actionPath := if suffix.isEmpty then path else joinPath path suffix
        rows := rows.push (← mkRow entry (name.getId ++ Name.mkSimple action) method
          actionPath hostRule target pipelines)
    | `(weave_route| group $path:str { $children:weave_route,* }) =>
      rows := rows ++ (← flatten children.getElems (joinPath basePath path.getString) hostRule pipelines)
    | `(weave_route| group $path:str host $hostname:str { $children:weave_route,* }) =>
      let hostRule ← match Host.parse hostname.getString with
        | .ok value => pure value
        | .error message => throwErrorAt hostname "{message}"
      rows := rows ++ (← flatten children.getElems (joinPath basePath path.getString) hostRule pipelines)
    | `(weave_route| group $path:str using ($step:term) { $children:weave_route,* }) =>
      rows := rows ++ (← flatten children.getElems (joinPath basePath path.getString) hostRule (pipelines.push step))
    | `(weave_route| group $path:str host $hostname:str using ($step:term) { $children:weave_route,* }) =>
      let hostRule ← match Host.parse hostname.getString with
        | .ok value => pure value
        | .error message => throwErrorAt hostname "{message}"
      rows := rows ++ (← flatten children.getElems (joinPath basePath path.getString) hostRule (pipelines.push step))
    | _ => throwErrorAt entry "unsupported LeanWeave route entry"
  return rows

private def segmentSyntax (segment : Segment) : CommandElabM (TSyntax `term) :=
  match segment with
  | .literal value => `(Segment.literal $(quote value))
  | .capture name .nat => `(Segment.capture $(quote name) CaptureType.nat)
  | .capture name .int => `(Segment.capture $(quote name) CaptureType.int)
  | .capture name .text => `(Segment.capture $(quote name) CaptureType.text)
  | .rest name => `(Segment.rest $(quote name))

private def hostSyntax : Host → CommandElabM (TSyntax `term)
  | .any => `(Host.any)
  | .exact value => `(Host.exact $(quote value))
  | .suffix value => `(Host.suffix $(quote value))

private def captures (row : Row) : Array (String × CaptureType) := row.segments.filterMap fun s =>
  match s with
  | .literal _ => none
  | .capture name type => some (name, type)
  | .rest name => some (name, .text)

private def routeSyntax (row : Row) : CommandElabM (TSyntax `term) := do
  let ctx := mkIdent (← liftCoreM (mkFreshUserName `__weave_context))
  let cs := captures row
  let args ← cs.mapIdxM fun i _ =>
    return mkIdent (← liftCoreM (mkFreshUserName (Name.mkSimple s!"__weave_capture_{i}")))
  let mut body ← `($(row.target) $args* $ctx)
  for i in (List.range cs.size).reverse do
    let (name, type) := cs[i]!
    let fn := mkIdent (match type with
      | .nat => ``captureNat | .int => ``captureInt | .text => ``captureText)
    let arg := args[i]!
    body ← `($fn $ctx $(quote name) >>= fun $arg => $body)
  let handler ← `(LeanWeave.pipeline #[$(row.pipelines),*] (fun $ctx => $body))
  let segments ← row.segments.mapM segmentSyntax
  let hostRule ← hostSyntax row.hostRule
  `(({ name := $(quote row.name.toString), method := $(quote row.method),
        path := $(quote row.path), «host» := $hostRule, pattern := #[$segments,*], handler := $handler } : Route))

private def reverseCommand (router : Ident) (row : Row) : CommandElabM (TSyntax `command) := do
  let helper := mkIdent (router.getId ++ row.name.appendAfter "Url")
  let cs := captures row
  let mut binders : Array (TSyntax ``Lean.Parser.Term.bracketedBinder) := #[]
  let mut pairs : Array (TSyntax `term) := #[]
  -- Reversal depends only on route metadata. In particular it must not require
  -- the section parameters captured by an application handler or middleware.
  let segments ← row.segments.mapM segmentSyntax
  let hostRule ← hostSyntax row.hostRule
  let metadata ← `(({ routes := #[({
      name := $(quote row.name.toString), method := $(quote row.method), path := $(quote row.path),
      «host» := $hostRule, pattern := #[$segments,*], handler := fun _ => pure {} } : Route)] } : LeanWeave.Router))
  for (name, type) in cs do
    let arg := mkIdent (← liftCoreM (mkFreshUserName (Name.mkSimple name)))
    let ty ← match type with | .nat => `(Nat) | .int => `(Int) | .text => `(String)
    binders := binders.push (← `(bracketedBinderF| ($arg : $ty)))
    let ctor := mkIdent (match type with
      | .nat => ``LeanWeave.CaptureValue.nat
      | .int => ``LeanWeave.CaptureValue.int
      | .text => ``LeanWeave.CaptureValue.text)
    pairs := pairs.push (← `(($(quote name), $ctor $arg)))
  `(def $helper $binders* (query : Array (String × String) := #[]) : Except String String :=
      LeanWeave.Router.url $metadata $(quote row.name.toString) #[$pairs,*] query)

@[command_elab weaveRoutes] def elabWeaveRoutes : CommandElab := fun stx => do
  let `(weave_routes $router:ident where { $entries:weave_route,* }) := stx
    | throwUnsupportedSyntax
  let rows ← flatten entries.getElems
  for i in [:rows.size] do
    let row := rows[i]!
    for j in [:i] do
      let other := rows[j]!
      if row.name == other.name then
        throwErrorAt row.source "duplicate route name '{row.name}'"
      if row.method == other.method && row.hostRule == other.hostRule && ambiguousPatterns row.segments other.segments then
        throwErrorAt row.source "ambiguous routes '{other.name}' and '{row.name}'"
  let routes ← rows.mapM routeSyntax
  elabCommand (← `(def $router : LeanWeave.Router :=
    { routes := (#[$routes,*] : Array Route).qsort Route.precedes }))
  for row in rows do elabCommand (← reverseCommand router row)

end LeanWeave.Router
