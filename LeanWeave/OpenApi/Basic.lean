import LeanWeave.Router
import LeanWeave.Schema
import LeanWeave.Html

namespace LeanWeave.OpenApi

structure Operation where
  name : String
  request : Option Schema.Definition := none
  response : Schema.Definition
  status : Nat := 200
  summary : String := ""
  security : Array String := #[]

/-- A single checked route table drives both dispatch and API documentation. -/
structure Api where
  router : Router
  operations : Array Operation

structure Config where
  title : String
  version : String := "1.0.0"
  description : String := ""
  servers : Array String := #[]
  /-- Restrict host-scoped tables to one concrete authority, when necessary. -/
  host : Option String := none
  routeNames : Array String := #[]
  securitySchemes : Array (String × Lean.Json) := #[]

private def pointerToken (s : String) : String := s.replace "~" "~0" |>.replace "/" "~1"

private def digit (n : Nat) : Char := Char.ofNat (if n < 10 then 48+n else 65+n-10)

/-- OAS component keys use a restricted ASCII alphabet. Encoding is injective. -/
def componentName (name : String) : String := String.ofList <| name.toUTF8.toList.flatMap fun byte =>
  let n := byte.toNat
  if byte == 95 then ['_', '_']
  else if (n >= 48 && n <= 57) || (n >= 65 && n <= 90) || (n >= 97 && n <= 122) || byte == 46 || byte == 45 then
    [Char.ofNat n]
  else ['_', digit (n/16), digit (n%16)]

private def reference (definition : Schema.Definition) : Lean.Json :=
  Lean.Json.mkObj [("$ref", Lean.toJson ("#/components/schemas/" ++ pointerToken (componentName definition.name)))]

private def pathName (route : Router.Route) (canonical : Bool := false) : String :=
  "/" ++ String.intercalate "/" (route.pattern.toList.map fun segment => match segment with
    | .literal text => percentEncode text
    | .capture name _ | .rest name => "{" ++ (if canonical then "_" else name) ++ "}")

private def pathParameters (route : Router.Route) : Array Lean.Json := route.pattern.filterMap fun segment =>
  let value := match segment with
    | .literal _ => none
    | .capture name .nat => some (name, Schema.Shape.scalar .integer { minimum := some 0 }, false)
    | .capture name .int => some (name, Schema.Shape.scalar .integer, false)
    | .capture name .text => some (name, Schema.Shape.scalar .string { minLength := some 1 }, false)
    | .rest name => some (name, Schema.Shape.scalar .string, true)
  value.map fun (name, shape, rest) => Lean.Json.mkObj <|
    [("name", Lean.toJson name), ("in", Lean.toJson "path"), ("required", Lean.toJson true), ("schema", shape.toJson)] ++
    (if rest then [("x-leanweave-rest", Lean.toJson true)] else [])

private def plainError (description : String) : Lean.Json := Lean.Json.mkObj [
  ("description", Lean.toJson description),
  ("content", Lean.Json.mkObj [("text/plain", Lean.Json.mkObj [("schema", (Schema.Shape.scalar .string).toJson)])])]

private def operationJson (route : Router.Route) (operation : Operation) : Lean.Json :=
  let successful := Lean.Json.mkObj <| [("description", Lean.toJson "Success")] ++
    if operation.status == 204 || operation.status == 205 then [] else
      [("content", Lean.Json.mkObj [("application/json", Lean.Json.mkObj [("schema", reference operation.response)])])]
  let responses := [(toString operation.status, successful), ("500", plainError "Application error")] ++
    if operation.request.isSome then [("400", plainError "Malformed JSON"), ("415", plainError "Unsupported content type"),
      ("422", plainError "Schema validation failed")] else []
  let host := match route.host with | .any => "*" | .exact value => value | .suffix value => "*" ++ value
  Lean.Json.mkObj <| [
    ("operationId", Lean.toJson operation.name), ("summary", Lean.toJson operation.summary),
    ("parameters", Lean.Json.arr (pathParameters route)), ("responses", Lean.Json.mkObj responses),
    ("x-leanweave-host", Lean.toJson host),
    ("security", Lean.Json.arr (operation.security.map fun name => Lean.Json.mkObj [(name, Lean.Json.arr #[])]))] ++
    match operation.request with
    | none => []
    | some request => [("requestBody", Lean.Json.mkObj [("required", Lean.toJson true),
        ("content", Lean.Json.mkObj [("application/json", Lean.Json.mkObj [("schema", reference request)])])])]

/-- Emit an OpenAPI 3.1.1 document. Fail when an HTTP route cannot be represented
    unambiguously in OpenAPI (for example two typed captures in the same slot).
    `host`/`routeNames` can explicitly select a representable API surface. -/
def generate (api : Api) (config : Config) : Except String Lean.Json := do
  if config.title.isEmpty || config.version.isEmpty then throw "OpenAPI title and version are required"
  let mut operationNames : Array String := #[]
  for operation in api.operations do
    if operationNames.contains operation.name then throw "duplicate typed API operation metadata"
    unless api.router.routes.any (·.name == operation.name) do throw "typed API operation has no corresponding route"
    operationNames := operationNames.push operation.name
  for name in config.routeNames do
    unless api.router.routes.any (fun route => route.name == name) do throw s!"unknown API route '{name}'"
  let docHost ← config.host.mapM normalizeHost
  let mut paths : Array (String × Array (String × Lean.Json)) := #[]
  let mut canonical : Array (String × String × String) := #[]
  let mut templates : Array (String × String) := #[]
  let mut definitions : Array Schema.Definition := #[]
  for route in api.router.routes do
    if docHost.any (fun host => !route.host.matches host) then continue
    if !config.routeNames.isEmpty && !config.routeNames.contains route.name then continue
    let some operation := api.operations.find? (fun operation => operation.name == route.name)
      | throw s!"route '{route.name}' has no typed API operation"
    let method := route.method.toLower
    if operation.status < 200 || operation.status > 599 then throw "invalid API response status"
    unless ["get", "put", "post", "delete", "options", "head", "patch", "trace"].contains method do
      throw s!"OpenAPI cannot represent method '{route.method}'"
    for required in operation.security do
      unless config.securitySchemes.any (fun pair => pair.1 == required) do
        throw s!"undefined security scheme '{required}'"
    let key := pathName route
    let normalized := pathName route true
    if let some (_, previous) := templates.find? (fun item => item.1 == normalized) then
      if previous != key then
        throw "OpenAPI cannot use different capture names for the same path template"
    else templates := templates.push (normalized, key)
    if let some (_, _, previous) := canonical.find? (fun item => item.1 == normalized && item.2.1 == method) then
      throw s!"OpenAPI paths overlap for '{previous}' and '{route.name}'; select host/routes explicitly"
    canonical := canonical.push (normalized, method, route.name)
    let value := operationJson route operation
    if paths.any (fun pair => pair.1 == key) then
      paths := paths.map fun (path, values) => if path == key then (path, values.push (method, value)) else (path, values)
    else paths := paths.push (key, #[(method, value)])
    for definition in #[some operation.response, operation.request] do
      if let some definition := definition then
        if let some previous := definitions.find? (fun d => componentName d.name == componentName definition.name) then
          unless previous == definition do throw s!"schema component collision for '{definition.name}'"
        else definitions := definitions.push definition
  return Lean.Json.mkObj [
    ("openapi", Lean.toJson "3.1.1"),
    ("info", Lean.Json.mkObj [("title", Lean.toJson config.title), ("version", Lean.toJson config.version),
      ("description", Lean.toJson config.description)]),
    ("servers", Lean.Json.arr (config.servers.map fun url => Lean.Json.mkObj [("url", Lean.toJson url)])),
    ("paths", Lean.Json.mkObj (paths.toList.map fun (path, operations) => (path, Lean.Json.mkObj operations.toList))),
    ("components", Lean.Json.mkObj [("schemas", Lean.Json.mkObj (definitions.toList.map fun definition =>
      (componentName definition.name, definition.shape.toJson))), ("securitySchemes", Lean.Json.mkObj config.securitySchemes.toList)])]

/-- Precompute the complete spec once; the request callback only returns bytes. -/
def specHandler (api : Api) (config : Config) : Except String Handler := do
  let spec ← generate api config
  let response := Response.json spec |>.withHeader "cache-control" "no-cache"
  return fun _ => pure response

structure Swagger where
  specUrl : Html.SafeUrl
  bundleUrl : Html.SafeUrl
  stylesheetUrl : Html.SafeUrl
  bootstrapUrl : Html.SafeUrl

private def externalScript (url : Html.SafeUrl) : Html.Html :=
  Html.rawUnsafe ((Html.escapeInto "<script src=\"".toUTF8 url.value).append "\"></script>".toUTF8)

/-- Self-host Swagger UI assets with lean-net's static catalog. The generated page
    uses external scripts and needs no inline-script CSP exception. -/
def swaggerPage (config : Swagger) : Response := Html.toResponse <|
  Html.element .html #[] #[Html.element .head #[] #[
    Html.element .title #[] #[Html.text "API documentation"],
    Html.void .link #[.text .rel "stylesheet", .url .href config.stylesheetUrl]],
    Html.element .body #[] #[Html.element .div #[.text .id "swagger-ui"] #[],
      externalScript config.bundleUrl, externalScript config.bootstrapUrl]]

def swaggerBootstrap (config : Swagger) : Response :=
  let options := Lean.Json.mkObj [("url", Lean.toJson config.specUrl.value), ("dom_id", Lean.toJson "#swagger-ui")]
  (Response.text ("window.addEventListener('load',()=>SwaggerUIBundle(" ++ options.compress ++ "));"))
    |>.withHeader "content-type" "text/javascript; charset=utf-8"

end LeanWeave.OpenApi
