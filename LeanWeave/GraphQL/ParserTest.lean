import LeanWeave.GraphQL.Parser

namespace LeanWeave.GraphQL.ParserTest

private def check (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw (IO.userError message)

private def expectError (query : String) (fragment : String) (limits : ParseLimits := {}) : IO Unit :=
  match parse query limits with
  | .error message => check ((message.splitOn fragment).length > 1) ("unexpected parse error: " ++ message)
  | .ok _ => throw (IO.userError ("invalid GraphQL accepted: " ++ query))

private def accepted (query : String) : IO Document := match parse query with
  | .ok document => pure document
  | .error message => throw (IO.userError message)

private def firstArgument (query : String) : IO Value := do
  let document ← accepted query
  let .field _ _ arguments _ _ := document.operations[0]!.selections[0]! | throw (IO.userError "expected field")
  return arguments[0]!.2

def run : IO Unit := do
  let document ← accepted "query Read($id: ID!, $limit: Int = 4) @skip(if: false) { alias: user(id: $id) {name ...Details ... on User @include(if: true) {id}} } fragment Details on User { age } mutation Change {update(id: 1)} subscription Watch {updates}"
  check (document.operations.size == 3 && document.fragments.size == 1) "operation/fragment count"
  check (document.operations[0]!.variables[0]!.type == .nonNull (.named "ID")) "variable type"
  check (document.operations[0]!.variables[1]!.defaultValue == some (.literal (Lean.toJson (4 : Nat)))) "variable default"
  check (document.operations[1]!.kind == .mutation && document.operations[2]!.kind == .subscription) "operation kinds"
  check ((← firstArgument "{f(value: ENUM)}") == .enumValue "ENUM") "enum AST"
  check ((← firstArgument "{f(value: \"ENUM\")}") == .literal (.str "ENUM")) "quoted enum remains string"
  check ((← firstArgument "{f(value: \"}\")}") == .literal (.str "}")) "string punctuation is a value"
  check ((← firstArgument "{f(value: \"\\u03BB\\uD83D\\uDE80\\u{1F680}\")}") == .literal (.str "λ🚀🚀")) "Unicode escapes"
  check ((← firstArgument "{f(value: \"\\u{000000041}\")}") == .literal (.str "A")) "Unicode leading zeroes"
  check ((← firstArgument "{f(value: \"\"\"\n  hello\n    world\n\"\"\")}") == .literal (.str "hello\n  world")) "block string indentation"
  check ((← firstArgument "{f(value: \"\"\"quote \\\"\"\" text\"\"\")}") == .literal (.str "quote \"\"\" text")) "escaped block delimiter"
  check ((← firstArgument "{f(value: {x: [1, -2.5e+3, true, null, $item]})}") == .object #[("x", .list #[
    .literal (Lean.toJson (1 : Nat)), .floatValue (-2500), .literal (.bool true), .literal .null, .variable "item"])]) "nested object/list values"
  let _ ← accepted (String.singleton (Char.ofNat 65279) ++ " # comment\n{, name, ... @include(if:true) {id},}")
  let _ ← accepted "\"Description\" query Q(\"Variable\" $x: Int = 1) {f(x:$x)} \"Fragment\" fragment A on User {id}"
  for (query, error) in #[
    ("", "empty"), ("{}", "empty"), ("{f()}", "empty"), ("query Q() {f}", "empty"),
    ("{f(a:1,a:2)}", "duplicate GraphQL argument"),
    ("query Q($a:Int,$a:Int){f}", "duplicate GraphQL variable"),
    ("fragment A on T {f} fragment A on T {g}", "duplicate GraphQL fragment"),
    ("query Q {f} query Q {g}", "duplicate GraphQL operation"),
    ("{f} query Q {g}", "anonymous"),
    ("{f(a:{x:1,x:2})}", "duplicate GraphQL input"),
    ("query Q($a:Int=$b){f}", "constant"),
    ("query Q($a:Int @foo){f}", "variable-definition directives"),
    ("{f(a:01)}", "leading zero"), ("{f(a:1.)}", "fraction"), ("{f(a:1e)}", "exponent"),
    ("{f(a:1abc)}", "after GraphQL number"), ("{f(a:+1)}", "character"),
    ("{f(a:\"\\uD800\")}", "surrogate"), ("{f(a:\"\\uDEAD\")}", "scalar"),
    ("{f(a:\"\\u{110000}\")}", "scalar"), ("{f(a:\"\\z\")}", "escape"),
    ("{f(a:\"unterminated)}", "unterminated"), ("{f", "end"), ("{ ...on }", "name"),
    ("type User {id:ID}", "expected GraphQL operation"), ("\"desc\" {f}", "shorthand")
  ] do expectError query error
  expectError "{f}" "byte limit" {maxBytes := 2}
  expectError "{f}" "token limit" {maxTokens := 2}
  expectError "{f{g{h}}}" "depth limit" {maxDepth := 2}
  expectError "{a b c}" "selection count" {maxSelections := 2}
  IO.println "PASS GraphQL parser: AST, strings, Unicode, fragments, variables, grammar, duplicate and resource bounds"

end LeanWeave.GraphQL.ParserTest
