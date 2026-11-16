import LeanWeave.GraphQL.Execution

namespace LeanWeave.GraphQL

def Request.decode (json : Lean.Json) : Except String Request := do
  let query ← json.getObjValAs? String "query"
  let variables := (json.getObjVal? "variables").toOption.getD (.mkObj [])
  let variables := if variables == .null then Lean.Json.mkObj [] else variables
  let _ ← variables.getObj?
  let operationName ← match (json.getObjVal? "operationName").toOption with
    | none | some .null => pure none
    | some value => some <$> value.getStr?
  return ⟨query, variables, operationName⟩

private def failure (message : String) (status : Nat) : Response :=
  Response.json (Lean.Json.mkObj [("errors", Lean.toJson (#[Error.mk message #[] "GRAPHQL_VALIDATION_FAILED"]))]) status

/-- GraphQL-over-HTTP: GET/HEAD are query-only; POST uses bounded JSON decoding.
Configure Server.appRetainedBytes conservatively for query AST and response
retention when a resolver awaits. No resolver runs before prepare succeeds. -/
def handler (schema : ExecutableSchema) (limits : Limits := {}) : Handler := fun ctx => do
  let readMethod := ["GET", "HEAD"].contains ctx.request.method
  if !readMethod && ctx.request.method != "POST" then
    return (failure "GET, HEAD or POST required" 405).withHeader "allow" "GET, HEAD, POST"
  if ctx.request.method == "POST" then
    let media := (((ctx.header "content-type").getD "").splitOn ";").head!.trimAscii.toString.toLower
    if media != "application/json" then return failure "JSON Content-Type required" 415
  let request : Except String Request := if readMethod then do
      -- GraphQL GET transport uses URLSearchParams form encoding. Decode the
      -- raw query independently; do not reinterpret already-decoded '+' data.
      let some target := String.fromUTF8? ctx.request.target | throw "request target is not UTF-8"
      let parameters ← decodePairs (String.intercalate "?" (target.splitOn "?").tail!) 128 8192 true
      for name in #["query", "variables", "operationName", "extensions"] do
        if (parameters.filter (·.1 == name)).size > 1 then throw "duplicate GraphQL request parameter"
      let parameter := fun name => (parameters.find? (·.1 == name)).map (·.2)
      let some query := parameter "query" | throw "query is required"
      let variables ← match parameter "variables" with
        | none | some "" => pure (Lean.Json.mkObj [])
        | some text => (Body.json text.toUTF8 { maxBytes := limits.parse.maxBytes }).mapError AppError.message
      let variables := if variables == .null then Lean.Json.mkObj [] else variables
      let operationName := (parameter "operationName").filter (· != "")
      pure ({ query, variables, operationName } : Request)
    else do
      let media := (((ctx.header "content-type").getD "").splitOn ";").head!.trimAscii.toString.toLower
      if media != "application/json" then throw "JSON Content-Type required"
      Request.decode (← (Body.json ctx.request.body { maxBytes := limits.parse.maxBytes * 4 }).mapError AppError.message)
  let .ok request := request | return failure "Invalid GraphQL request envelope" 400
  let prepared := prepare schema request limits
  if let .error message := prepared then return failure message 400
  let .ok prepared := prepared | return failure "Invalid operation" 400
  if prepared.kind == .subscription then return failure "Subscriptions require graphql-transport-ws" 400
  if readMethod && prepared.kind != .query then
    return (failure "Mutations require POST" 405).withHeader "allow" "POST"
  let result ← execute prepared ctx limits
  return (Response.json (Lean.toJson result)).withHeader "cache-control" "no-store"

end LeanWeave.GraphQL
