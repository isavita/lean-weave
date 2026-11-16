import LeanWeave.Test.GraphQLSupport
import LeanWeave.GraphQL.ParserTest

open LeanWeave LeanWeave.GraphQL

private def check (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw (IO.userError message)

private def context : Context := {
  request := { method := "POST", target := "/graphql".toUTF8, headers := #[], body := ByteArray.empty } }

private def runQuery (schema : ExecutableSchema) (query : String)
    (variables : Lean.Json := Lean.Json.mkObj []) (operationName : Option String := none) : IO Lean.Json := do
  let .ok prepared := prepare schema ⟨query, variables, operationName⟩ | throw (IO.userError s!"query prepare failed: {query}")
  match ← AppM.run (execute prepared context) with
  | .ok result => return Lean.toJson result
  | .error error => throw (IO.userError error.message)

def main : IO Unit := do
  GraphQL.ParserTest.run
  let (schema, count, _) ← Test.GraphQLSupport.newSchema
  let result ← runQuery schema "{ counter { value label __typename } }"
  check (result.compress == "{\"data\":{\"counter\":{\"__typename\":\"Counter\",\"label\":\"counter\",\"value\":0}}}") "typed object projection"
  let result ← runQuery schema "{counter @skip(if:true){label} counter{value}}"
  check (result.compress == "{\"data\":{\"counter\":{\"value\":0}}}") "disabled duplicate field leaked children"
  let result ← runQuery schema "{...Hidden @include(if:false) counter{value}} fragment Hidden on Query {counter{label}}"
  check (result.compress == "{\"data\":{\"counter\":{\"value\":0}}}") "disabled fragment leaked duplicate children"
  let result ← runQuery schema "mutation Add($n:Int!){ changed:increment(amount:$n){value} }" (Lean.Json.mkObj [("n", Lean.toJson (2 : Nat))])
  check ((← count.get) == 2 && (result.compress.splitOn "changed").length == 2) "typed variable mutation"
  let result ← runQuery schema "query Selected($show:Boolean! = true) { counter { ...Fields label @skip(if:$show) } } fragment Fields on Counter { value }"
  check (!(result.compress.splitOn "label").length > 1) "fragment/directive projection"
  let result ← runQuery schema "query A { echo(text:\"a\") } query B { echo(text:\"b\") }" (.mkObj []) (some "B")
  check ((result.compress.splitOn "b").length > 1) "operation selection"
  let result ← runQuery schema "{ __schema { queryType { name fields { name } } } __type(name:\"Counter\") { name kind fields {name type {kind name ofType {name kind}}} } }"
  check ((result.compress.splitOn "Counter").length > 1) "schema introspection"
  let result ← runQuery schema "{color(color:RED)}"
  check (result.compress == "{\"data\":{\"color\":\"RED\"}}") "enum literal input"
  let result ← runQuery schema "query($c:ColorArgsInput_color!){color(color:$c)}" (Lean.Json.mkObj [("c", .str "BLUE")])
  check (result.compress == "{\"data\":{\"color\":\"BLUE\"}}") "typed enum variable"
  for query in #[
    "{__schema{types{possibleTypes{noSuchField}}}}", "{__type(name:\"Missing\"){noSuchField}}",
    "subscription{ticks @skip(if:true){value}}",
    "query($unused:Int){counter{value}}", "{counter{value}} fragment Unused on Counter {value}",
    "{color(color:\"RED\")}", "mutation($n:Int! = 1.0){increment(amount:$n){value}}",
    "{ unknown }", "{ counter { missing } }", "{ counter }", "{ echo }", "{ echo(nope:1) }",
    "mutation { increment(amount:1.0){value} }", "mutation { increment(amount:2147483648){value} }",
    "mutation { increment(amount:101){value} }", "mutation { increment(amount:$missing){value} }",
    "query($n:String!){echo(text:$n)}", "{ x:counter{value} x:echo(text:\"x\") }",
    "{counter{...Loop}} fragment Loop on Counter {...Loop}",
    "mutation { increment(amount:1){value} missing }" ] do
    check (!(prepare schema ⟨query, .mkObj [], none⟩).isOk) s!"invalid query accepted: {query}"
  check ((← count.get) == 2) "validation triggered a mutation"
  check (!(prepare schema ⟨"{counter{value label}}", .mkObj [], none⟩ { maxExpandedSelections := 2 }).isOk) "expansion bound"
  check (!(prepare { schema with mutation := #[] } ⟨"mutation{__typename}", .mkObj [], none⟩).isOk) "unavailable mutation root accepted"
  let custom ← match ExecutableSchema.make #[
      Field.typed "large" (fun (_ : Context) (_ : NoArgs) => pure (2147483648 : Int)),
      Field.typed "ok" (fun (_ : Context) (_ : NoArgs) => pure (7 : Nat)),
      Field.typed "denied" (fun (_ : Context) (_ : NoArgs) => (throw { status := 403, message := "denied" } : AppM (Option Int)))] with
    | .ok value => pure value
    | .error error => throw (IO.userError error)
  let result ← runQuery custom "{large}"
  check ((result.getObjVal? "data").toOption == some .null && (result.getObjVal? "errors").isOk) "out-of-range Int output was not a non-null error"
  let result ← runQuery custom "{denied ok}"
  check ((result.getObjVal? "data" >>= fun data => data.getObjValAs? Nat "ok").toOption == some 7) "nullable error erased sibling data"
  IO.println "GraphQL parser, typed execution, variables, validation and introspection tests passed"
