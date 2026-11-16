import LeanWeave.Schema.Basic
import LeanWeave.Core.Stream

namespace LeanWeave.GraphQL

structure Error where
  message : String
  path : Array Lean.Json := #[]
  code : String := "GRAPHQL_ERROR"
  deriving Inhabited

instance : Lean.ToJson Error where
  toJson error := Lean.Json.mkObj <|
    [("message", .str error.message), ("extensions", Lean.Json.mkObj [("code", .str error.code)])] ++
      if error.path.isEmpty then [] else [("path", .arr error.path)]

/-- Cooperative owner-worker subscription. None is pending, ok none is EOF;
cleanup is called once by the protocol owner on every terminal path. -/
structure Source (α : Type) where
  poll : IO (Option (Except Error (Option α)))
  close : Option Stream.CancelReason → IO Unit := fun _ => pure ()
  retainedBytes : Nat := 65536

def Source.map (f : α → β) (source : Source α) : Source β := {
  poll := do return (← source.poll).map (fun result => result.map (Option.map f))
  close := source.close
  retainedBytes := source.retainedBytes }

structure NoArgs where
  deriving Lean.ToJson, Lean.FromJson, Inhabited

instance : Schema.HasSchema NoArgs := ⟨"NoArgs", .object #[]⟩

structure Field where
  name : String
  description : String := ""
  arguments : Schema.Definition
  output : Schema.Definition
  resolve : Context → Lean.Json → AppM Lean.Json
  subscribe : Option (Context → Lean.Json → AppM (Source Lean.Json)) := none

/-- Typed query/mutation resolver: the same HasSchema definition validates
GraphQL input and output, keeping runtime docs and Lean signatures aligned. -/
def Field.typed (name : String) (resolve : Context → α → AppM β)
    [Schema.HasSchema α] [Lean.FromJson α] [Schema.HasSchema β] [Lean.ToJson β] : Field := {
  name
  arguments := Schema.definition α
  output := Schema.definition β
  resolve := fun ctx json => do
    let input ← match Schema.decode α json with
      | .ok input => pure input
      | .error message => throw { status := 400, message }
    let output ← resolve ctx input
    let json := Lean.toJson output
    if !((Schema.HasSchema.shape (α := β)).validate json).isOk then
      throw { status := 500, message := "GraphQL resolver violated output schema" }
    return json }

def Field.subscription (name : String) (subscribe : Context → α → AppM (Source β))
    [Schema.HasSchema α] [Lean.FromJson α] [Schema.HasSchema β] [Lean.ToJson β] : Field := {
  name
  arguments := Schema.definition α
  output := Schema.definition β
  resolve := fun _ _ => throw { status := 400, message := "subscription requires a streaming transport" }
  subscribe := some fun ctx json => do
    let input ← match Schema.decode α json with
      | .ok input => pure input
      | .error message => throw { status := 400, message }
    return (← subscribe ctx input).map Lean.toJson }

structure TypeDefinition where
  name : String
  shape : Schema.Shape
  input : Bool
  deriving Inhabited

structure ExecutableSchema where
  query : Array Field
  mutation : Array Field := #[]
  subscription : Array Field := #[]
  types : Array TypeDefinition := #[]

/-- GraphQL identifiers have ASCII syntax; double-underscore names belong to
introspection and cannot be installed as application fields/types. -/
def validName (name : String) : Bool := Id.run do
  let bytes := name.toUTF8
  if bytes.isEmpty then return false
  for i in [:bytes.size] do
    let b := bytes[i]!
    if !((b >= 65 && b <= 90) || (b >= 97 && b <= 122) || b == 95 ||
        (i > 0 && b >= 48 && b <= 57)) then return false
  return true

private def scalarName : Schema.Scalar → String
  | .string => "String" | .integer => "Int" | .number => "Float" | .boolean => "Boolean" | .null => "JSON"

/-- GraphQL named type for a schema tree; object children are stable generated
names, while HasSchema supplies each resolver's public root type name. -/
def namedType (shape : Schema.Shape) (name : String) : String :=
  match shape with
  | .scalar kind _ => scalarName kind
  | .nullable child | .array child _ => namedType child name
  | _ => name

private partial def collectType (shape : Schema.Shape) (name : String) (input : Bool)
    (types : Array TypeDefinition) (fuel : Nat) : Except String (Array TypeDefinition) := do
  if fuel == 0 || types.size > 4096 then throw "GraphQL schema type depth/count exceeds bound"
  match shape with
  | .nullable child | .array child _ => collectType child name input types (fuel - 1)
  | .scalar kind _ =>
    let name := scalarName kind
    if types.any (·.name == name) then return types
    return types.push ⟨name, .scalar kind, false⟩
  | .enumeration values =>
    if !validName name || name.startsWith "__" || ["Query", "Mutation", "Subscription"].contains name then throw "invalid GraphQL enum type name"
    if values.isEmpty || values.size > 256 || values.toList.eraseDups.length != values.size then throw "GraphQL enum values must be nonempty, unique and bounded"
    for value in values do
      let value ← value.getStr?
      if !validName value || value.startsWith "__" || ["true", "false", "null"].contains value then throw "invalid GraphQL enum value"
    if let some previous := types.find? (·.name == name) then
      if previous.shape != shape then throw s!"conflicting GraphQL type {name}"
      return types
    return types.push ⟨name, shape, input⟩
  | .object fields additional =>
    if additional || fields.isEmpty || !validName name || name.startsWith "__" || ["Query", "Mutation", "Subscription"].contains name || fields.size > 256 then
      throw "GraphQL object types must have bounded named fields"
    if let some previous := types.find? (·.name == name) then
      if previous.shape != shape || previous.input != input then throw s!"conflicting GraphQL type {name}"
      return types
    let mut types := types.push ⟨name, shape, input⟩
    let mut names : Array String := #[]
    for (key, child, _) in fields do
      if !validName key || key.startsWith "__" || names.contains key then throw "invalid/duplicate GraphQL object field"
      names := names.push key
      types ← collectType child (name ++ "_" ++ key) input types (fuel - 1)
    return types

/-- Validate once before serving. Resolver argument definitions are finite
objects (use an empty object for no arguments), never untyped arbitrary JSON. -/
def ExecutableSchema.make (query : Array Field) (mutation subscription : Array Field := #[]) : Except String ExecutableSchema := do
  if query.isEmpty || query.size + mutation.size + subscription.size > 768 then
    throw "GraphQL requires a bounded nonempty query root"
  let mut types : Array TypeDefinition := #[
    ⟨"String", .scalar .string, false⟩, ⟨"Boolean", .scalar .boolean, false⟩]
  for fields in #[query, mutation, subscription] do
    let mut names : Array String := #[]
    for field in fields do
      if !validName field.name || field.name.startsWith "__" || names.contains field.name then throw "invalid/duplicate GraphQL root field"
      names := names.push field.name
      match field.arguments.shape with
      | .object _ false => pure ()
      | _ => throw "GraphQL resolver arguments require a closed object schema"
      match field.arguments.shape with
      | .object fields _ =>
        if !fields.isEmpty then types ← collectType field.arguments.shape (field.arguments.name ++ "Input") true types 32
      | _ => pure ()
      types ← collectType field.output.shape field.output.name false types 32
  if subscription.any (·.subscribe.isNone) then throw "GraphQL subscription field requires a source"
  return { query, mutation, subscription, types }

end LeanWeave.GraphQL
