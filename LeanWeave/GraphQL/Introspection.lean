import LeanWeave.GraphQL.Schema

namespace LeanWeave.GraphQL

private def object (fields : List (String × Lean.Json)) : Lean.Json := Lean.Json.mkObj fields
private def string := Lean.Json.str
private def null := Lean.Json.null

private def scalarKind (shape : Schema.Shape) (input : Bool) : String :=
  match shape with
  | .object .. => if input then "INPUT_OBJECT" else "OBJECT"
  | .enumeration .. => "ENUM"
  | _ => "SCALAR"

private partial def referenceCore (shape : Schema.Shape) (name : String) (input : Bool) : Lean.Json :=
  match shape with
  | .nullable child => referenceCore child name input
  | .array child _ => object [("kind", string "LIST"), ("name", null), ("ofType", reference child name input)]
  | _ => object [("kind", string (scalarKind shape input)), ("name", string (namedType shape name)), ("ofType", null)]
where
  reference (shape : Schema.Shape) (name : String) (input : Bool) : Lean.Json :=
    match shape with
    | .nullable child => referenceCore child name input
    | _ => object [("kind", string "NON_NULL"), ("name", null), ("ofType", referenceCore shape name input)]

def typeReference (shape : Schema.Shape) (name : String) (input : Bool := false) (required : Bool := true) : Lean.Json :=
  if !required then referenceCore shape name input else referenceCore.reference shape name input

private def inputValue (name : String) (shape : Schema.Shape) (typeName : String) (required : Bool) : Lean.Json :=
  object [("name", string name), ("description", null), ("type", typeReference shape typeName true required),
    ("defaultValue", null), ("isDeprecated", .bool false), ("deprecationReason", null)]

private def fieldValue (name : String) (shape : Schema.Shape) (typeName : String)
    (arguments : Array Lean.Json := #[]) (description : Lean.Json := .null) : Lean.Json :=
  object [("name", string name), ("description", description), ("args", .arr arguments),
    ("type", typeReference shape typeName), ("isDeprecated", .bool false), ("deprecationReason", null)]

private def definitionValue (definition : TypeDefinition) : Lean.Json :=
  let (fields, inputs, enums) := match definition.shape with
    | .object fields _ =>
      if definition.input then (null, Lean.Json.arr (fields.map fun (name, shape, required) =>
        inputValue name shape (definition.name ++ "_" ++ name) required), null)
      else (Lean.Json.arr (fields.map fun (name, shape, _) =>
        fieldValue name shape (definition.name ++ "_" ++ name)), null, null)
    | .enumeration values => (null, null, Lean.Json.arr (values.map fun value =>
        object [("name", value), ("description", null), ("isDeprecated", .bool false), ("deprecationReason", null)]))
    | _ => (null, null, null)
  object [("kind", string (scalarKind definition.shape definition.input)), ("name", string definition.name),
    ("description", null), ("specifiedByURL", null), ("fields", fields), ("inputFields", inputs),
    ("interfaces", if definition.input then null else .arr #[]), ("possibleTypes", null),
    ("enumValues", enums), ("ofType", null), ("isOneOf", .bool false)]

private def rootValue (name : String) (fields : Array Field) : Lean.Json :=
  let fields := fields.map fun field =>
    let arguments := match field.arguments.shape with
      | .object arguments _ => arguments.map fun (name, shape, required) =>
          inputValue name shape (field.arguments.name ++ "Input_" ++ name) required
      | _ => #[]
    fieldValue field.name field.output.shape field.output.name arguments
      (if field.description.isEmpty then null else string field.description)
  object [("kind", string "OBJECT"), ("name", string name), ("description", null), ("specifiedByURL", null),
    ("fields", .arr fields), ("inputFields", null), ("interfaces", .arr #[]), ("possibleTypes", null),
    ("enumValues", null), ("ofType", null), ("isOneOf", .bool false)]

def ExecutableSchema.introspectionTypes (schema : ExecutableSchema) : Array Lean.Json := Id.run do
  let mut types := schema.types.map definitionValue
  types := types.push (rootValue "Query" schema.query)
  if !schema.mutation.isEmpty then types := types.push (rootValue "Mutation" schema.mutation)
  if !schema.subscription.isEmpty then types := types.push (rootValue "Subscription" schema.subscription)
  return types

def ExecutableSchema.introspectionType (schema : ExecutableSchema) (name : String) : Lean.Json :=
  (schema.introspectionTypes.find? (fun value => (value.getObjValAs? String "name").toOption == some name)).getD .null

def ExecutableSchema.introspection (schema : ExecutableSchema) : Lean.Json :=
  let directive := fun (name : String) => object [
    ("name", string name), ("description", null), ("isRepeatable", .bool false),
    ("locations", Lean.toJson (["FIELD", "FRAGMENT_SPREAD", "INLINE_FRAGMENT"] : List String)),
    ("args", .arr #[inputValue "if" (.scalar .boolean) "Boolean" true])]
  object [("description", null), ("queryType", rootValue "Query" schema.query),
    ("mutationType", if schema.mutation.isEmpty then null else rootValue "Mutation" schema.mutation),
    ("subscriptionType", if schema.subscription.isEmpty then null else rootValue "Subscription" schema.subscription),
    ("types", .arr schema.introspectionTypes), ("directives", .arr #[directive "skip", directive "include"])]

end LeanWeave.GraphQL
