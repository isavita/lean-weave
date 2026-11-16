import LeanWeave.GraphQL.Parser
import LeanWeave.GraphQL.Introspection

namespace LeanWeave.GraphQL

structure Limits where
  parse : ParseLimits := {}
  maxExpandedSelections : Nat := 1024
  maxDepth : Nat := 32
  maxResponseBytes : Nat := 1048576
  maxSourceBytes : Nat := 65536
  operationTimeoutMs : Nat := 30000
  deriving Inhabited

structure Request where
  query : String
  variables : Lean.Json := Lean.Json.mkObj []
  operationName : Option String := none
  deriving Inhabited

structure Result where
  data : Lean.Json := .null
  errors : Array Error := #[]

instance : Lean.ToJson Result where
  toJson result := Lean.Json.mkObj <|
    [("data", result.data)] ++ if result.errors.isEmpty then [] else [("errors", Lean.toJson result.errors)]

private structure BoundVariable where
  name : String
  type : TypeRef
  value : Option Lean.Json
  nonNullDefault : Bool := false

private def unwrap : TypeRef → TypeRef
  | .nonNull type => type
  | type => type

private partial def compatible (actual expected : TypeRef) (hasDefault : Bool) : Bool :=
  match actual, expected with
  | .nonNull actual, .nonNull expected => compatible actual expected false
  | actual, .nonNull expected => hasDefault && compatible actual expected false
  | .nonNull actual, expected => compatible actual expected false
  | .list actual, .list expected => compatible actual expected false
  | .named actual, .named expected => actual == expected
  | _, _ => false

private partial def shapeType (shape : Schema.Shape) (name : String) (required : Bool := true) : TypeRef :=
  let core := match shape with
    | .nullable child => unwrap (shapeType child name)
    | .array child _ => TypeRef.list (shapeType child name)
    | _ => .named (namedType shape name)
  if required && !shape.optional then .nonNull core else core

private partial def typeShape (schema : ExecutableSchema) (type : TypeRef) : Except String Schema.Shape := do
  match type with
  | .nonNull inner =>
    match ← typeShape schema inner with | .nullable shape => return shape | shape => return shape
  | .list inner => return .nullable (.array (← typeShape schema inner))
  | .named name =>
    let shape ← match name with
      | "String" | "ID" => pure (Schema.Shape.scalar .string)
      | "Int" => pure (.scalar .integer { minimum := some (-2147483648), maximum := some 2147483647 })
      | "Float" => pure (.scalar .number)
      | "Boolean" => pure (.scalar .boolean)
      | "JSON" => pure (.scalar .null)
      | _ =>
        let some definition := schema.types.find? (·.name == name) | throw s!"unknown GraphQL input type {name}"
        match definition.shape with
        | .object .. => unless definition.input do throw "output object cannot be a variable type"
        | _ => pure ()
        pure definition.shape
    return .nullable shape

private partial def coerceJson (shape : Schema.Shape) (json : Lean.Json) : Except String Lean.Json := do
  match shape with
  | .nullable child => if json == .null then return .null else coerceJson child json
  | .array child _ =>
    let values := match json with | .arr values => values | value => #[value]
    let values ← values.mapM (coerceJson child)
    let json := Lean.Json.arr values
    shape.validate json
    return json
  | .object fields _ =>
    let object ← json.getObj?
    let values ← object.toArray.mapM fun (name, value) => do
      let some (_, child, _) := fields.find? (·.1 == name) | throw s!"unknown input field {name}"
      return (name, ← coerceJson child value)
    let json := Lean.Json.mkObj values.toList
    shape.validate json
    return json
  | .scalar .integer _ =>
    let value ← json.getInt?
    if value < -2147483648 || value > 2147483647 then throw "GraphQL Int exceeds signed 32 bits"
    shape.validate json
    return json
  | .scalar .number _ =>
    let number ← json.getNum?
    if number.toFloat.isInf || number.toFloat.isNaN then throw "GraphQL Float requires a finite double"
    shape.validate json
    return json
  | _ => shape.validate json; return json

private partial def literal (shape : Schema.Shape) (name : String) (required : Bool)
    (value : Value) (variables : Array BoundVariable) : Except String (Option Lean.Json) := do
  if let .variable variableName := value then
    let some binding := variables.find? (·.name == variableName) | throw s!"undefined variable ${variableName}"
    unless compatible binding.type (shapeType shape name required) binding.nonNullDefault do
      throw s!"variable ${variableName} has an incompatible type"
    return binding.value
  match shape with
  | .nullable child =>
    if value == .literal .null then return some .null
    literal child name false value variables
  | .array child _ =>
    let values := match value with | .list values => values | other => #[other]
    let values ← values.mapM fun value => do
      let some value ← literal child name true value variables | throw "undefined variable in list"
      return value
    let json := Lean.Json.arr values
    shape.validate json
    return some json
  | .object fields _ =>
    let .object supplied := value | throw "object input requires an object literal"
    let values ← supplied.mapM fun (field, value) => do
      let some (_, child, required) := fields.find? (·.1 == field) | throw s!"unknown input field {field}"
      return (field, ← literal child (name ++ "_" ++ field) required value variables)
    let json := Lean.Json.mkObj (values.filterMap fun (name, value) => value.map (name, ·)).toList
    shape.validate json
    return some json
  | .enumeration _ =>
    let .enumValue value := value | throw "enum input requires an unquoted enum literal"
    let json := Lean.Json.str value
    shape.validate json
    return some json
  | .scalar .integer _ =>
    let .literal json := value | throw "Int requires an integer literal"
    return some (← coerceJson shape json)
  | .scalar .number _ =>
    let json ← match value with
      | .floatValue value => pure (Lean.Json.num value)
      | .literal json => pure json
      | _ => throw "Float requires a numeric literal"
    return some (← coerceJson shape json)
  | _ =>
    let .literal json := value | throw "scalar input literal has incompatible kind"
    return some (← coerceJson shape json)

private partial def untypedValue (value : Value) : Except String Lean.Json := do
  match value with
  | .literal json => return json
  | .floatValue value => return .num value
  | .enumValue name => return .str name
  | .list values => return .arr (← values.mapM untypedValue)
  | .object fields => return Lean.Json.mkObj (← fields.toList.mapM fun (name, value) => return (name, ← untypedValue value))
  | .variable _ => throw "variable default cannot reference another variable"

private def bindVariables (schema : ExecutableSchema) (operation : Operation) (provided : Lean.Json) : Except String (Array BoundVariable) := do
  let _ ← provided.getObj?
  operation.variables.mapM fun binding => do
    let shape ← typeShape schema binding.type
    let supplied := (provided.getObjVal? binding.name).toOption
    let value ← match supplied, binding.defaultValue with
      | some value, _ => pure (some value)
      | none, some value =>
        let value ← literal shape (match unwrap binding.type with | .named name => name | _ => "Input") true value #[]
        pure value
      | none, none => pure none
    if value.isNone then
      if let .nonNull _ := binding.type then throw s!"required variable ${binding.name} missing"
    let value ← value.mapM (coerceJson shape)
    return ⟨binding.name, binding.type, value, binding.defaultValue.any (· != .literal .null)⟩

private def directiveEnabled (directives : Array Directive) (variables : Array BoundVariable) : Except String Bool := do
  let mut enabled := true
  let mut names : Array String := #[]
  for directive in directives do
    if !["skip", "include"].contains directive.name || names.contains directive.name then throw "unknown/repeated GraphQL directive"
    names := names.push directive.name
    if directive.arguments.size != 1 || directive.arguments[0]!.1 != "if" then throw "directive requires exactly if: Boolean!"
    let some json ← literal (.scalar .boolean) "Boolean" true directive.arguments[0]!.2 variables
      | throw "directive variable is missing"
    let value ← json.getBool?
    enabled := enabled && (if directive.name == "skip" then !value else value)
  return enabled

private structure Expanded where
  alias : String
  name : String
  arguments : Array (String × Value)
  children : Array Selection
  enabled : Bool
  deriving Inhabited

private abbrev CompileM := StateT Nat (Except String)
private def consume : CompileM Unit := do
  let remaining ← get
  if remaining == 0 then throw "GraphQL expansion/selection budget exceeded"
  set (remaining - 1)

private partial def expand (document : Document) (variables : Array BoundVariable) (typeName : String)
    (selections : Array Selection) (visited : Array String := #[]) (enabled : Bool := true) : CompileM (Array Expanded) := do
  let mut fields := #[]
  for selection in selections do
    consume
    match selection with
    | .field alias name arguments directives children =>
      let included ← directiveEnabled directives variables
      -- Preserve validation of disabled contributions without allowing their
      -- children into an enabled duplicate field's merged result.
      let children := if !(enabled && included) && !children.isEmpty then
        #[Selection.inlineFragment none #[⟨"skip", #[("if", .literal (.bool true))]⟩] children]
        else children
      let alias := alias.getD name
      if let some previous := fields.find? (·.alias == alias) then
        if previous.name != name || previous.arguments != arguments then throw "conflicting GraphQL response aliases"
        let fields' := fields.map fun field => if field.alias == alias then
          { field with children := field.children ++ children, enabled := field.enabled || (enabled && included) } else field
        fields := fields'
      else fields := fields.push ⟨alias, name, arguments, children, enabled && included⟩
    | .spread name directives =>
      let included ← directiveEnabled directives variables
      if visited.contains name then throw "cyclic GraphQL fragment"
      let some fragment := document.fragments.find? (·.name == name) | throw "undefined GraphQL fragment"
      if !fragment.directives.isEmpty then throw "directives are unsupported at fragment definition"
      if fragment.typeCondition != typeName then throw "fragment cannot apply to this concrete object type"
      fields := fields ++ (← expand document variables typeName fragment.selections (visited.push name) (enabled && included))
    | .inlineFragment condition directives children =>
      let included ← directiveEnabled directives variables
      if condition.any (· != typeName) then throw "inline fragment cannot apply to this concrete object type"
      fields := fields ++ (← expand document variables typeName children visited (enabled && included))
  -- Spreads may contribute equal aliases; coalesce them with the same checks.
  let mut merged : Array Expanded := #[]
  for field in fields do
    if let some previous := merged.find? (·.alias == field.alias) then
      if previous.name != field.name || previous.arguments != field.arguments then throw "conflicting GraphQL fragment aliases"
      merged := merged.map fun previous => if previous.alias == field.alias then
        { previous with children := previous.children ++ field.children, enabled := previous.enabled || field.enabled } else previous
    else merged := merged.push field
  return merged

inductive Projection where
  | leaf
  | constant (value : Lean.Json)
  | nullable (child : Projection)
  | list (child : Projection)
  | object (fields : Array (String × String × Projection))

private partial def compileProjection (document : Document) (variables : Array BoundVariable)
    (shape : Schema.Shape) (typeName : String) (selections : Array Selection) (depth : Nat) : CompileM Projection := do
  if depth == 0 then throw "GraphQL projection depth exceeded"
  match shape with
  | .nullable child => return .nullable (← compileProjection document variables child typeName selections (depth - 1))
  | .array child _ => return .list (← compileProjection document variables child typeName selections (depth - 1))
  | .object definitions _ =>
    if selections.isEmpty then throw "object output requires a selection set"
    let fields ← expand document variables typeName selections
    let mut projected := #[]
    for field in fields do
      if !field.arguments.isEmpty then throw "nested value fields do not accept arguments"
      if field.name == "__typename" then
        if !field.children.isEmpty then throw "__typename is a leaf"
        if field.enabled then projected := projected.push (field.alias, field.name, .constant (.str typeName))
      else
        let some (_, child, required) := definitions.find? (·.1 == field.name) | throw s!"unknown field {typeName}.{field.name}"
        let child ← compileProjection document variables child (typeName ++ "_" ++ field.name) field.children (depth - 1)
        if field.enabled then projected := projected.push (field.alias, field.name, if required then child else .nullable child)
    return .object projected
  | _ =>
    if !selections.isEmpty then throw "scalar/enum output cannot have a selection set"
    return .leaf

partial def Projection.apply (projection : Projection) (value : Lean.Json) : Except String Lean.Json := do
  match projection with
  | .leaf => return value
  | .constant value => return value
  | .nullable child => if value == .null then return .null else child.apply value
  | .list child => return .arr (← (← value.getArr?).mapM child.apply)
  | .object fields =>
    return Lean.Json.mkObj (← fields.toList.mapM fun (alias, name, child) => do
      return (alias, ← child.apply ((value.getObjVal? name).toOption.getD .null)))

structure PreparedField where
  alias : String
  field : Field
  arguments : Lean.Json
  projection : Projection
  enabled : Bool

structure Prepared where
  kind : OperationKind
  fields : Array PreparedField
  constants : Array (String × Lean.Json)

private def argumentsFor (field : Field) (arguments : Array (String × Value))
    (variables : Array BoundVariable) : Except String Lean.Json := do
  let .object definitions _ := field.arguments.shape | throw "invalid resolver argument schema"
  let values ← arguments.mapM fun (name, value) => do
    let some (_, shape, required) := definitions.find? (·.1 == name) | throw s!"unknown argument {field.name}({name}:)"
    return (name, ← literal shape (field.arguments.name ++ "Input_" ++ name) required value variables)
  let json := Lean.Json.mkObj (values.filterMap fun (name, value) => value.map (name, ·)).toList
  field.arguments.shape.validate json
  return json

private def metaField (typeName field : String) : Option (Option String × Bool) := Id.run do
  let leaf := some (none, false)
  if typeName == "__Schema" then
    if field == "description" then return leaf
    if field == "types" then return some (some "__Type", true)
    if ["queryType", "mutationType", "subscriptionType"].contains field then return some (some "__Type", false)
    if field == "directives" then return some (some "__Directive", true)
  if typeName == "__Type" then
    if ["kind", "name", "description", "specifiedByURL", "isOneOf"].contains field then return leaf
    if field == "fields" then return some (some "__Field", true)
    if field == "inputFields" then return some (some "__InputValue", true)
    if field == "enumValues" then return some (some "__EnumValue", true)
    if ["interfaces", "possibleTypes"].contains field then return some (some "__Type", true)
    if field == "ofType" then return some (some "__Type", false)
  if typeName == "__Field" then
    if ["name", "description", "isDeprecated", "deprecationReason"].contains field then return leaf
    if field == "args" then return some (some "__InputValue", true)
    if field == "type" then return some (some "__Type", false)
  if typeName == "__InputValue" then
    if ["name", "description", "defaultValue", "isDeprecated", "deprecationReason"].contains field then return leaf
    if field == "type" then return some (some "__Type", false)
  if typeName == "__EnumValue" && ["name", "description", "isDeprecated", "deprecationReason"].contains field then return leaf
  if typeName == "__Directive" then
    if ["name", "description", "isRepeatable", "locations"].contains field then return leaf
    if field == "args" then return some (some "__InputValue", true)
  return none

private partial def compileMeta (document : Document) (variables : Array BoundVariable)
    (selections : Array Selection) (typeName : String) (depth : Nat) : CompileM Projection := do
  if depth == 0 then throw "introspection depth exceeded"
  if selections.isEmpty then throw "introspection object requires selections"
  let fields ← expand document variables typeName selections
  let mut projected := #[]
  for field in fields do
    if field.name == "__typename" then
      if !field.arguments.isEmpty || !field.children.isEmpty then throw "__typename is a leaf without arguments"
      if field.enabled then projected := projected.push (field.alias, field.name, .constant (.str typeName))
      continue
    let some (childType, list) := metaField typeName field.name | throw s!"unknown introspection field {typeName}.{field.name}"
    let allowsDeprecated := (typeName == "__Type" && ["fields", "enumValues", "inputFields"].contains field.name) ||
      (["__Field", "__Directive"].contains typeName && field.name == "args")
    for (name, value) in field.arguments do
      if !allowsDeprecated || name != "includeDeprecated" then throw "unknown introspection argument"
      let _ ← literal (.nullable (.scalar .boolean)) "Boolean" false value variables
    let child ← match childType with
      | none =>
        if !field.children.isEmpty then throw "introspection scalar cannot have selections"
        pure Projection.leaf
      | some name =>
        let child ← compileMeta document variables field.children name (depth - 1)
        pure (Projection.nullable (if list then .list child else child))
    if field.enabled then projected := projected.push (field.alias, field.name, child)
  return .object projected

private partial def valueVariables : Value → Array String
  | .variable name => #[name]
  | .list values => values.foldl (fun names value => names ++ valueVariables value) #[]
  | .object fields => fields.foldl (fun names (_, value) => names ++ valueVariables value) #[]
  | _ => #[]

private def directiveVariables (directives : Array Directive) : Array String :=
  directives.foldl (fun names directive => directive.arguments.foldl (fun names (_, value) => names ++ valueVariables value) names) #[]

private partial def selectionVariables (document : Document) (selections : Array Selection)
    (visited : Array String := #[]) : CompileM (Array String × Array String) := do
  let mut variables := #[]
  let mut fragments := #[]
  for selection in selections do
    consume
    match selection with
    | .field _ _ arguments directives children =>
      variables := arguments.foldl (fun names (_, value) => names ++ valueVariables value) variables ++ directiveVariables directives
      let (nested, used) ← selectionVariables document children visited
      variables := variables ++ nested
      fragments := fragments ++ used
    | .inlineFragment _ directives children =>
      variables := variables ++ directiveVariables directives
      let (nested, used) ← selectionVariables document children visited
      variables := variables ++ nested
      fragments := fragments ++ used
    | .spread name directives =>
      if visited.contains name then throw "cyclic GraphQL fragment"
      let some fragment := document.fragments.find? (·.name == name) | throw "undefined GraphQL fragment"
      variables := variables ++ directiveVariables directives
      let (nested, used) ← selectionVariables document fragment.selections (visited.push name)
      variables := variables ++ nested
      fragments := fragments.push name ++ used
  return (variables, fragments)

private partial def subscriptionRoot (document : Document) (selections : Array Selection)
    (depth : Nat) : Except String Unit := do
  if depth == 0 then throw "subscription fragment depth exceeded"
  for selection in selections do
    match selection with
    | .field _ _ _ directives _ =>
      if !directives.isEmpty then throw "subscription root cannot use skip/include directives"
    | .spread name directives =>
      if !directives.isEmpty then throw "subscription root fragment cannot use directives"
      let some fragment := document.fragments.find? (·.name == name) | throw "undefined subscription fragment"
      subscriptionRoot document fragment.selections (depth - 1)
    | .inlineFragment _ directives children =>
      if !directives.isEmpty then throw "subscription root fragment cannot use directives"
      subscriptionRoot document children (depth - 1)

/-- Parse, select an operation, bind/coerce variables, validate every selection
and argument, and compile a bounded projection before running any resolver. -/
def prepare (schema : ExecutableSchema) (request : Request) (limits : Limits := {}) : Except String Prepared := do
  let document ← parse request.query limits.parse
  let operation ← match request.operationName with
    | some name =>
      let some operation := document.operations.find? (·.name == some name) | throw "unknown operationName"
      pure operation
    | none =>
      if document.operations.size != 1 then throw "operationName required for multiple operations"
      pure document.operations[0]!
  if !operation.directives.isEmpty then throw "operation directives are unsupported"
  if operation.kind == .subscription then subscriptionRoot document operation.selections limits.maxDepth
  let ((usedVariables, _), _) ← (selectionVariables document operation.selections).run limits.maxExpandedSelections
  for binding in operation.variables do
    if !usedVariables.contains binding.name then throw s!"unused variable ${binding.name}"
  for name in usedVariables do
    if !operation.variables.any (·.name == name) then throw s!"undefined variable ${name}"
  let mut usedFragments : Array String := #[]
  for candidate in document.operations do
    let ((_, used), _) ← (selectionVariables document candidate.selections).run limits.maxExpandedSelections
    usedFragments := usedFragments ++ used
  if document.fragments.any (fun fragment => !usedFragments.contains fragment.name) then throw "unused GraphQL fragment"
  let variables ← bindVariables schema operation request.variables
  let rootName := match operation.kind with | .query => "Query" | .mutation => "Mutation" | .subscription => "Subscription"
  let definitions := match operation.kind with | .query => schema.query | .mutation => schema.mutation | .subscription => schema.subscription
  if definitions.isEmpty then throw "GraphQL operation root is unavailable"
  let compile : CompileM Prepared := do
    let fields ← expand document variables rootName operation.selections
    let mut prepared := #[]
    let mut constants := #[]
    for selected in fields do
      if selected.name == "__typename" then
        if !selected.arguments.isEmpty || !selected.children.isEmpty || operation.kind == .subscription then throw "invalid __typename selection"
        if selected.enabled then constants := constants.push (selected.alias, Lean.Json.str rootName)
      else if selected.name == "__schema" || selected.name == "__type" then
        if operation.kind != .query then throw "introspection requires query"
        let value ← if selected.name == "__schema" then
            if !selected.arguments.isEmpty then throw "__schema accepts no arguments"
            pure schema.introspection
          else
            if selected.arguments.size != 1 || selected.arguments[0]!.1 != "name" then throw "__type requires name"
            let some name ← literal (.scalar .string) "String" true selected.arguments[0]!.2 variables | throw "__type name missing"
            pure (schema.introspectionType (← name.getStr?))
        let projection ← compileMeta document variables selected.children (if selected.name == "__schema" then "__Schema" else "__Type") limits.maxDepth
        let value ← (Projection.nullable projection).apply value
        if value.compress.utf8ByteSize > limits.maxResponseBytes then throw "introspection response exceeds byte bound"
        if selected.enabled then constants := constants.push (selected.alias, value)
      else
        let some field := definitions.find? (·.name == selected.name) | throw s!"unknown GraphQL field {rootName}.{selected.name}"
        let arguments ← argumentsFor field selected.arguments variables
        let projection ← compileProjection document variables field.output.shape field.output.name selected.children limits.maxDepth
        prepared := prepared.push ⟨selected.alias, field, arguments, projection, selected.enabled⟩
    if operation.kind == .subscription && (prepared.size != 1 || !constants.isEmpty) then throw "subscription requires exactly one root field"
    return ⟨operation.kind, prepared, constants⟩
  return (← compile.run limits.maxExpandedSelections).1

/-- Mutations execute in document order. Validation has already completed for
the entire operation, so invalid sibling fields cannot trigger partial writes. -/
def execute (prepared : Prepared) (ctx : Context) (limits : Limits := {}) : AppM Result := do
  if prepared.kind == .subscription then throw { status := 400, message := "subscription requires streaming transport" }
  let mut values := prepared.constants
  let mut errors : Array Error := #[]
  let mut rootNull := false
  for selected in prepared.fields do
    if !selected.enabled then continue
    try
      let value ← selected.field.resolve ctx selected.arguments
      let .ok value := coerceJson selected.field.output.shape value | throw { status := 500, message := "invalid resolver output" }
      let .ok value := selected.projection.apply value | throw { status := 500, message := "invalid output projection" }
      values := values.push (selected.alias, value)
    catch error =>
      errors := errors.push ⟨if error.status >= 500 then "Internal server error" else error.message, #[.str selected.alias], "RESOLVER_ERROR"⟩
      values := values.push (selected.alias, .null)
      if !selected.field.output.shape.optional then
        rootNull := true
        if prepared.kind == .mutation then break
  let result := { data := if rootNull then .null else Lean.Json.mkObj values.toList, errors : Result }
  if (Lean.toJson result).compress.utf8ByteSize > limits.maxResponseBytes then
    throw { status := 503, message := "GraphQL response exceeds byte bound" }
  return result

end LeanWeave.GraphQL

namespace LeanWeave.GraphQL

def subscribe (prepared : Prepared) (ctx : Context) (limits : Limits := {}) : AppM (Source Result) := do
  if prepared.kind != .subscription || prepared.fields.size != 1 then
    throw { status := 400, message := "operation is not a subscription" }
  let some selected := prepared.fields[0]? | throw { status := 400, message := "subscription field missing" }
  let some start := selected.field.subscribe | throw { status := 400, message := "field is not subscribable" }
  let source ← start ctx selected.arguments
  if source.retainedBytes > limits.maxSourceBytes then
    source.close (some .overloaded)
    throw { status := 503, message := "GraphQL source exceeds retained byte bound" }
  return {
    retainedBytes := source.retainedBytes
    close := source.close
    poll := do
      let event ← source.poll
      match event with
      | none => return none
      | some (.error error) => return some (.error error)
      | some (.ok none) => return some (.ok none)
      | some (.ok (some value)) =>
        let .ok value := coerceJson selected.field.output.shape value
          | return some (.error ⟨"Subscription violated its output schema", #[], "INTERNAL_ERROR"⟩)
        match selected.projection.apply value with
        | .error _ => return some (.error ⟨"Invalid subscription projection", #[], "INTERNAL_ERROR"⟩)
        | .ok value =>
          let result := { data := Lean.Json.mkObj [(selected.alias, value)] : Result }
          if (Lean.toJson result).compress.utf8ByteSize > limits.maxResponseBytes then
            return some (.error ⟨"Subscription response exceeds byte bound", #[], "OVERLOADED"⟩)
          return some (.ok (some result)) }

end LeanWeave.GraphQL
