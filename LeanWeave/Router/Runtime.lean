import LeanWeave.Core
import LeanWeave.Router.Pattern

namespace LeanWeave

namespace Router

inductive Host where
  | any
  | exact (value : String)
  | suffix (value : String)
  deriving BEq, Repr, Inhabited

def Host.parse (value : String) : Except String Host := do
  if value == "*" then return .any
  if value.startsWith "*." then
    let suffix ← normalizeHost (value.drop 1 |>.toString)
    return .suffix suffix
  return .exact (← normalizeHost value)

def Host.matches : Host → String → Bool
  | .any, _ => true
  | .exact expected, actual => expected == actual
  | .suffix value, actual => actual.endsWith value && actual.length > value.length

def Host.rank : Host → Nat
  | .any => 0
  | .suffix value => value.length + 1
  | .exact _ => 1000000

/-- Fully compiled route; middleware is composed when constructing this value. -/
structure Route where
  name : String
  method : String
  path : String
  host : Host := .any
  pattern : Array Segment
  handler : Handler

instance : Inhabited Route := ⟨{ name := "", method := "", path := "", pattern := #[], handler := fun _ => pure {} }⟩

private def matchOne (segment : Segment) (value : String) : Option (Option (String × CaptureValue)) :=
  match segment with
  | .literal expected => if expected == value then some none else none
  | .capture name .text => if value.isEmpty then none else some (some (name, .text value))
  | .capture name .nat => do
    if !isNatText value then none else
      return some (name, .nat (← value.toNat?))
  | .capture name .int => do
    if !isIntText value then none else
      return some (name, .int (← value.toInt?))
  | .rest _ => none

def Route.matches (route : Route) (ctx : Context) : Option (Array (String × CaptureValue)) := do
  if !route.host.matches ctx.host then failure
  let mut captures := #[]
  for i in [:route.pattern.size] do
    let segment := route.pattern[i]!
    if let .rest name := segment then
      if i + 1 != route.pattern.size then failure
      else
        let tail := ctx.path.extract i ctx.path.size
        return captures.push (name, .text (String.intercalate "/" tail.toList))
    let value ← ctx.path[i]?
    if let some pair ← matchOne segment value then captures := captures.push pair
  if route.pattern.size == ctx.path.size then return captures
  else failure

def Route.precedes (a b : Route) : Bool :=
  if a.host.rank != b.host.rank then a.host.rank > b.host.rank
  else patternPrecedes a.pattern b.pattern

/-- Validation is also available for applications constructing route tables without the DSL. -/
def validate (routes : Array Route) : Except String Unit := do
  for i in [:routes.size] do
    let route := routes[i]!
    if route.name.isEmpty then throw "routes must be named"
    if route.method.isEmpty || !route.method.toList.all (fun c => c >= 'A' && c <= 'Z') then
      throw s!"invalid method '{route.method}'"
    for j in [:i] do
      let other := routes[j]!
      if route.name == other.name then throw s!"duplicate route name '{route.name}'"
      if route.method == other.method && route.host == other.host &&
          ambiguousPatterns route.pattern other.pattern then
        throw s!"ambiguous routes '{other.name}' and '{route.name}'"

def captureText (ctx : Context) (name : String) : AppM String :=
  match ctx.capture name with
  | some (.text value) => pure value
  | _ => throw { message := s!"missing text capture {name}" }

def captureNat (ctx : Context) (name : String) : AppM Nat :=
  match ctx.capture name with
  | some (.nat value) => pure value
  | _ => throw { message := s!"missing natural capture {name}" }

def captureInt (ctx : Context) (name : String) : AppM Int :=
  match ctx.capture name with
  | some (.int value) => pure value
  | _ => throw { message := s!"missing integer capture {name}" }

/-- Conventional seven-action resource. Updates accept PATCH and PUT. -/
structure Resource where
  index : Handler
  new : Handler
  create : Handler
  «show» : Nat → Handler
  edit : Nat → Handler
  update : Nat → Handler
  delete : Nat → Handler

end Router

/-- A table sorted by specificity; declaration order never resolves an ambiguity. -/
structure Router where
  routes : Array Router.Route

namespace Router

def compile (routes : Array Route) : Except String Router := do
  validate routes
  return { routes := routes.qsort Route.precedes }

private def withImplicitMethods (methods : Array String) : Array String := Id.run do
  let mut methods := methods
  if methods.contains "GET" && !methods.contains "HEAD" then methods := methods.push "HEAD"
  if !methods.contains "OPTIONS" then methods := methods.push "OPTIONS"
  return methods.qsort (· < ·)

private def validCaptureSegment (value : String) : Bool :=
  value != "." && value != ".." && !value.contains '/' && !value.contains '\\' &&
    !value.toUTF8.toList.contains 0

/-- HEAD invokes an explicit HEAD route or GET with the original request method.
    Keep the representation bytes: lean-net suppresses the wire body while retaining
    its length. Automatic OPTIONS and 405 share the deterministic Allow set. -/
def handler (router : Router) : Handler := fun ctx => do
  if ctx.request.method == "OPTIONS" && ctx.request.target == "*".toUTF8 then
    let mut methods := #[]
    for route in router.routes do
      if route.host.matches ctx.host && !methods.contains route.method then
        methods := methods.push route.method
    return (Response.text "" 204).withHeader "allow" (String.intercalate ", " (withImplicitMethods methods).toList)
  let method := ctx.request.method
  let mut methods := #[]
  let mut headFallback : Option (Route × Array (String × CaptureValue)) := none
  for route in router.routes do
    if let some captures := route.matches ctx then
      if route.method == method then return ← route.handler { ctx with captures, routeName := route.name }
      if method == "HEAD" && route.method == "GET" && headFallback.isNone then
        headFallback := some (route, captures)
      if !methods.contains route.method then methods := methods.push route.method
  if let some (route, captures) := headFallback then
    return ← route.handler { ctx with captures, routeName := route.name }
  if methods.isEmpty then return Response.text "Not Found" 404
  let allow := String.intercalate ", " (withImplicitMethods methods).toList
  if method == "OPTIONS" then
    return (Response.text "" 204).withHeader "allow" allow
  return (Response.text "Method Not Allowed" 405).withHeader "allow" allow

/-- Reverse generation checks the name and each capture's codec; extra values are
    rejected. Use the DSL's generated typed helpers when the name is known statically. -/
def url (router : Router) (name : String) (values : Array (String × CaptureValue) := #[])
    (query : Array (String × String) := #[]) : Except String String := do
  let some route := router.routes.find? (fun route => route.name == name)
    | throw s!"unknown route '{name}'"
  let mut parts : Array String := #[]
  let mut used : Array String := #[]
  for segment in route.pattern do
    match segment with
    | .literal value => parts := parts.push (encodeSegment value)
    | .capture key type =>
      let some (_, value) := values.find? (fun pair => pair.1 == key)
        | throw s!"missing capture '{key}'"
      let text ← match type, value with
        | .text, .text text =>
          if text.isEmpty || !validCaptureSegment text then throw "invalid text capture" else pure text
        | .nat, .nat n => pure (toString n)
        | .int, .int n => pure (toString n)
        | _, _ => throw s!"incorrect type for capture '{key}'"
      parts := parts.push (encodeSegment text)
      used := used.push key
    | .rest key =>
      let some (_, .text value) := values.find? (fun pair => pair.1 == key)
        | throw s!"missing text rest capture '{key}'"
      unless (value.splitOn "/").all validCaptureSegment do throw "invalid rest capture"
      parts := parts ++ ((value.splitOn "/").toArray.map encodeSegment)
      used := used.push key
  if values.size != used.size || values.any (fun pair => !used.contains pair.1) then
    throw "unexpected or duplicate reverse-route capture"
  let path := "/" ++ String.intercalate "/" parts.toList
  if query.isEmpty then return path
  return path ++ "?" ++ String.intercalate "&" (query.toList.map fun (key, value) =>
    percentEncode key ++ "=" ++ percentEncode value)

/-- Human-readable rows for `weave routes`; captures and host scopes remain visible. -/
def listing (router : Router) : String := String.intercalate "\n" <|
  router.routes.toList.map fun route =>
    let host := match route.host with
      | .any => "*" | .exact value => value | .suffix value => "*" ++ value
    s!"{route.method}\t{host}\t{route.path}\t{route.name}"

end Router
end LeanWeave
