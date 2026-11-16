import Lean
import LeanWeave.Protobuf.Descriptor

open Lean Elab Command
open Lean.Parser.Command
open Lean.Parser.Term

namespace LeanWeave.Protobuf

/-- Compile-time RPC metadata exported with generated modules. -/
structure MethodInfo where
  name : Name
  input : Name
  output : Name
  path : String
  clientStreaming : Bool
  serverStreaming : Bool
  deriving Inhabited

initialize methodRegistry : SimplePersistentEnvExtension MethodInfo (NameMap MethodInfo) ←
  registerSimplePersistentEnvExtension {
    addEntryFn := fun state entry => state.insert entry.name entry
    addImportedFn := fun imports => Id.run do
      let mut state := {}
      for entries in imports do
        for entry in entries do state := state.insert entry.name entry
      return state }

def methodInfo? (environment : Environment) (name : Name) : Option MethodInfo :=
  (methodRegistry.getState environment).find? name

private def dotted (name : String) : Name := (name.splitOn ".").foldl Name.str .anonymous
private def typeId (name : String) : Ident := mkIdent (dotted name)
private def codecId (name suffix : String) : Ident := mkIdent (dotted name ++ Name.mkSimple suffix)

private def scalarTypeSyntax : ScalarType → CommandElabM Term
  | .double => `(Float) | .float => `(Float32)
  | .int32 | .sint32 | .sfixed32 => `(Int32)
  | .int64 | .sint64 | .sfixed64 => `(Int64)
  | .uint32 | .fixed32 => `(UInt32)
  | .uint64 | .fixed64 => `(UInt64)
  | .bool => `(Bool) | .string => `(String) | .bytes => `(ByteArray)

private partial def valueType (type : TypeRef) : CommandElabM Term :=
  match type with
  | .scalar type => scalarTypeSyntax type
  | .named name => pure (typeId name)
  | .map key value => do `(Array (($(← scalarTypeSyntax key)) × $(← valueType value)))

private def isMessage (descriptor : Descriptor) : TypeRef → Bool
  | .named name => descriptor.messages.any (·.name == name)
  | _ => false

private def fieldType (descriptor : Descriptor) (field : Field) : CommandElabM Term := do
  let type ← valueType field.type
  match field.type with
  | .map .. => return type
  | _ =>
    if field.label == .repeated then `(Array $type)
    else if field.label == .optional || isMessage descriptor field.type then `(Option $type)
    else pure type

private def scalarEncoder (type : ScalarType) : Term :=
  mkIdent (`LeanWeave.Protobuf.Scalar ++ Name.mkSimple type.name)

private def scalarDecoder (type : ScalarType) : Term :=
  let name := match type with
    | .double => "getDouble" | .float => "getFloat" | .int32 => "getInt32" | .int64 => "getInt64"
    | .uint32 => "getUint32" | .uint64 => "getUint64" | .sint32 => "getSint32" | .sint64 => "getSint64"
    | .fixed32 => "getFixed32" | .fixed64 => "getFixed64" | .sfixed32 => "getSfixed32" | .sfixed64 => "getSfixed64"
    | .bool => "getBool" | .string => "getString" | .bytes => "getBytes"
  mkIdent (`LeanWeave.Protobuf.Scalar ++ Name.mkSimple name)

private def primitive (descriptor : Descriptor) (type : TypeRef) : CommandElabM (Nat × Term × Term) := do
  match type with
  | .scalar type => return (type.wire, scalarEncoder type, scalarDecoder type)
  | .named name =>
    unless descriptor.enums.any (·.name == name) do throwError "expected primitive protobuf type"
    let toInt := codecId name "toInt32"
    let ofInt := codecId name "ofInt32"
    return (0, ← `(fun value => Scalar.int32 ($toInt value)), ← `(fun wire => $ofInt <$> Scalar.getInt32 wire))
  | _ => throwError "map is not a primitive protobuf type"

mutual
  private partial def readField (descriptor : Descriptor) (field : Field) (fields limits : Term) : CommandElabM Term := do
    let number := quote field.number
    match field.type with
    | .map key value =>
      let data := mkIdent `__proto_map_data
      let nested := mkIdent `__proto_map_limits
      let raw := mkIdent `__proto_map_fields
      let keyField : Field := { name := "key", number := 1, type := .scalar key }
      let valueField : Field := { name := "value", number := 2, type := value }
      let keyRead ← readField descriptor keyField raw nested
      let valueRead ← readField descriptor valueField raw nested
      let valueRead ← if isMessage descriptor value then `((Option.getD · default) <$> $valueRead) else pure valueRead
      let parser ← `(fun $data $nested => decodeRaw $data $nested >>= fun $raw =>
        Prod.mk <$> $keyRead <*> $valueRead)
      `(getMap $fields $number $parser $limits)
    | .named name =>
      if isMessage descriptor field.type then
        let decode := codecId name "decodeWith"
        if field.label == .repeated then `(embeddedRepeated $fields $number $decode $limits)
        else `(embedded $fields $number $decode $limits)
      else readPrimitive descriptor field fields limits
    | .scalar _ => readPrimitive descriptor field fields limits

  private partial def readPrimitive (descriptor : Descriptor) (field : Field) (fields limits : Term) : CommandElabM Term := do
    let (wire, _, decode) ← primitive descriptor field.type
    let number := quote field.number
    if field.label == .repeated then `(repeated $fields $number $(quote wire) $decode $limits)
    else if field.label == .optional then `(optional $fields $number $(quote wire) $decode $limits)
    else `(singular $fields $number $(quote wire) $decode default $limits)
end

private partial def writeField (descriptor : Descriptor) (field : Field)
    (output value limits : Term) (forced : Bool := false) : CommandElabM Term := do
  let number := quote field.number
  match field.type with
  | .map key mapValue =>
    let pair := mkIdent `__proto_map_pair
    let nested := mkIdent `__proto_map_limits
    let entry := mkIdent `__proto_map_output
    let keyField : Field := { name := "key", number := 1, type := .scalar key }
    let valueField : Field := { name := "value", number := 2, type := mapValue }
    let keyValue ← `($pair.1)
    let valueValue ← `($pair.2)
    let valueValue ← if isMessage descriptor mapValue then `(some $valueValue) else pure valueValue
    let keyWrite ← writeField descriptor keyField (← `(ByteArray.empty)) keyValue nested
    let valueWrite ← writeField descriptor valueField entry valueValue nested
    let writer ← `(fun $pair $nested => $keyWrite >>= fun $entry => $valueWrite)
    `(putMap $output $number $value $writer $limits)
  | .named name =>
    if isMessage descriptor field.type then
      let encode := codecId name "encodeWith"
      if field.label == .repeated then `(putEmbeddedRepeated $output $number $value $encode $limits)
      else
        let value ← if forced then `(some $value) else pure value
        `(putEmbedded $output $number $value $encode $limits)
    else writePrimitive descriptor field output value limits forced
  | .scalar _ => writePrimitive descriptor field output value limits forced
where
  writePrimitive (descriptor : Descriptor) (field : Field) (output value limits : Term) (forced : Bool) : CommandElabM Term := do
    let (_, encode, _) ← primitive descriptor field.type
    let number := quote field.number
    if field.label == .repeated then
      `(putPrimitiveRepeated $output $number $value $encode $(quote (field.packed.getD true)) $limits)
    else if field.label == .optional && !forced then
      `(match ($value) with | none => pure $output | some item => putScalar $output $number ($encode item) true $limits)
    else `(putScalar $output $number ($encode $value) $(quote forced) $limits)

private def int32Syntax (number : Int) : CommandElabM Term :=
  if number < 0 then `((-($(quote number.natAbs) : Int32))) else `(($(quote number.natAbs) : Int32))

private def emitEnum (enumeration : EnumDescriptor) : CommandElabM Unit := do
  if enumeration.values.any (fun pair => pair.1 == "__unknown") then throwError "enum value '__unknown' is reserved by the generated codec"
  let name := typeId enumeration.name
  let names := enumeration.values.map (fun pair => mkIdent (Name.mkSimple pair.1))
  let unknownName := mkIdent `__unknown
  elabCommand (← `(inductive $name where
    $[| $names:ident]*
    | $unknownName:ident (number : Int32)
    deriving Inhabited, BEq))
  let encode := codecId enumeration.name "toInt32"
  let decode := codecId enumeration.name "ofInt32"
  let unknown := codecId enumeration.name "__unknown"
  let mut alts : Array (TSyntax ``Lean.Parser.Term.matchAlt) := #[]
  let mut fromValue ← `($unknown value)
  for (field, number) in enumeration.values.toList.reverse do
    let constructor := codecId enumeration.name field
    let number ← int32Syntax number
    fromValue ← `(if value == $number then $constructor else $fromValue)
  for (field, number) in enumeration.values do
    let constructor := codecId enumeration.name field
    alts := alts.push (← `(matchAltExpr| | $constructor => $(← int32Syntax number)))
  alts := alts.push (← `(matchAltExpr| | $unknown number => number))
  elabCommand (← `(def $encode (value : $name) : Int32 := match value with $alts:matchAlt*))
  elabCommand (← `(def $decode (value : Int32) : $name := $fromValue))

private def oneofName (message group : String) : String := message ++ ".__Oneof_" ++ group

private def oneofDeclaration (_descriptor : Descriptor) (message : MessageDescriptor) (group : String) : CommandElabM Command := do
  let name := typeId (oneofName message.name group)
  let fields := message.fields.filter (·.oneof == some group)
  if fields.isEmpty then throwError "protobuf oneof must contain at least one field"
  let names := fields.map (fun field => mkIdent (Name.mkSimple field.name))
  let types ← fields.mapM (fun field => valueType field.type)
  `(inductive $name where
    $[| $names:ident (value : $types:term)]*
    deriving Inhabited)

private def messageDeclaration (descriptor : Descriptor) (message : MessageDescriptor) : CommandElabM Command := do
  let name := typeId message.name
  let mut names : Array Ident := #[]
  let mut types : Array Term := #[]
  for field in message.fields do
    if field.oneof.isNone then
      names := names.push (mkIdent (Name.mkSimple field.name))
      types := types.push (← fieldType descriptor field)
  for group in message.oneofs do
    names := names.push (mkIdent (Name.mkSimple group))
    types := types.push (← `(Option $(typeId (oneofName message.name group))))
  names := names.push (mkIdent `__unknownFields)
  types := types.push (← `(Array ByteArray))
  `(structure $name where
    $[$names:ident : $types:term]*
    deriving Inhabited)

private def wireEntry (descriptor : Descriptor) (field : Field) : CommandElabM Term := do
  let wire := match field.type with
    | .scalar scalar => scalar.wire
    | .named name => if descriptor.enums.any (·.name == name) then 0 else 2
    | .map _ _ => 2
  let wires : Array Term := (if field.label == .repeated && wire != 2 then #[wire, 2] else #[wire]).map quote
  `(($(quote field.number), #[$wires,*]))

private def readOneof (descriptor : Descriptor) (message : MessageDescriptor) (group : String)
    (raw limits : Term) : CommandElabM Term := do
  let fields := message.fields.filter (·.oneof == some group)
  let numbers : Array Term := fields.map (fun field => quote field.number)
  let wires ← fields.mapM (wireEntry descriptor)
  let selected := mkIdent `__proto_oneof_fields
  let number := mkIdent `__proto_oneof_number
  let mut body ← `(pure none)
  for field in fields.toList.reverse do
    let constructor := codecId (oneofName message.name group) field.name
    let reader ← readField descriptor { field with label := .optional } selected limits
    body ← `(if $number == $(quote field.number) then (Option.map $constructor) <$> $reader else $body)
  `(match oneofSelection $raw #[$numbers,*] #[$wires,*] with | none => pure none | some ($number, $selected) => $body)

private def writeOneof (descriptor : Descriptor) (message : MessageDescriptor) (group : String)
    (output value limits : Term) : CommandElabM Term := do
  let fields := message.fields.filter (·.oneof == some group)
  let item := mkIdent `__proto_oneof_item
  let mut alts : Array (TSyntax ``Lean.Parser.Term.matchAlt) := #[]
  for field in fields do
    let constructor := codecId (oneofName message.name group) field.name
    let write ← writeField descriptor field output item limits true
    alts := alts.push (← `(matchAltExpr| | $constructor $item => $write))
  `(match ($value) with | none => pure $output | some choice => match choice with $alts:matchAlt*)

private def codecDeclarations (descriptor : Descriptor) (message : MessageDescriptor) : CommandElabM (Array Command) := do
  let name := typeId message.name
  let encode := codecId message.name "encodeWith"
  let decode := codecId message.name "decodeWith"
  let value := mkIdent `__proto_value
  let output := mkIdent `__proto_output
  let raw := mkIdent `__proto_fields
  let bytes := mkIdent `__proto_bytes
  let limits := mkIdent `__proto_limits
  let unknown := mkIdent `__proto_unknown
  let metadataField := mkIdent `__unknownFields
  let numbers : Array Term := message.fields.map (fun field => quote field.number)
  let wireTypes ← message.fields.mapM (wireEntry descriptor)
  let unknownValue ← `($value.$metadataField)
  let mut encoding ← `(appendUnknown $output $unknownValue #[$numbers,*] $limits #[$wireTypes,*])
  for group in message.oneofs.toList.reverse do
    let projection := mkIdent (Name.mkSimple group)
    let write ← writeOneof descriptor message group output (← `($value.$projection)) limits
    encoding ← `($write >>= fun $output => $encoding)
  for field in message.fields.toList.reverse do
    if field.oneof.isNone then
      let projection := mkIdent (Name.mkSimple field.name)
      let write ← writeField descriptor field output (← `($value.$projection)) limits
      encoding ← `($write >>= fun $output => $encoding)
  let encoder ← `(partial def $encode ($value : $name) ($limits : Limits) : Except String ByteArray :=
    let $output := ByteArray.empty
    $encoding)
  let mut fieldNames : Array Ident := #[]
  let mut fieldValues : Array Term := #[]
  let mut reads : Array (Ident × Term) := #[]
  for field in message.fields do
    if field.oneof.isNone then
      let binding := mkIdent (Name.mkSimple ("__proto_field_" ++ field.name))
      reads := reads.push (binding, ← readField descriptor field raw limits)
      fieldNames := fieldNames.push (mkIdent (Name.mkSimple field.name))
      fieldValues := fieldValues.push binding
  for group in message.oneofs do
    let binding := mkIdent (Name.mkSimple ("__proto_group_" ++ group))
    reads := reads.push (binding, ← readOneof descriptor message group raw limits)
    fieldNames := fieldNames.push (mkIdent (Name.mkSimple group))
    fieldValues := fieldValues.push binding
  fieldNames := fieldNames.push metadataField
  fieldValues := fieldValues.push unknown
  let mut decoding ← `(pure (({ $[$fieldNames:ident := $fieldValues:term],* } : $name)))
  for (binding, read) in reads.toList.reverse do decoding ← `($read >>= fun $binding => $decoding)
  decoding ← `(unknownFields $raw #[$numbers,*] $limits #[$wireTypes,*] >>= fun $unknown => $decoding)
  let decoder ← `(partial def $decode ($bytes : ByteArray) ($limits : Limits) : Except String $name :=
    decodeRaw $bytes $limits >>= fun $raw => $decoding)
  return #[encoder, decoder]

/-- Parse and compile proto3 without invoking protoc or native plugins. Generated
    structures preserve unknown fields; enums preserve unrecognized numeric values.
    Recursive/nested messages, oneof, maps, optional and packed fields are supported. -/
scoped syntax (name := protobufSchema) "protobuf_schema " ident " := " str : command

@[command_elab protobufSchema] def elabProtobufSchema : CommandElab := fun stx => do
  let `(protobuf_schema $root:ident := $source:str) := stx | throwUnsupportedSyntax
  let descriptor ← match parseSchema source.getString with
    | .ok descriptor => pure descriptor
    | .error error => throwErrorAt source "{error}"
  elabCommand (← `(namespace $root))
  let currentNamespace ← getCurrNamespace
  for enumeration in descriptor.enums do emitEnum enumeration
  let mut declarations : Array Command := #[]
  for message in descriptor.messages do
    for group in message.oneofs do declarations := declarations.push (← oneofDeclaration descriptor message group)
    declarations := declarations.push (← messageDeclaration descriptor message)
  unless declarations.isEmpty do elabCommand (← `(mutual
    $declarations:command*
    end))
  let mut codecs : Array Command := #[]
  for message in descriptor.messages do codecs := codecs ++ (← codecDeclarations descriptor message)
  unless codecs.isEmpty do elabCommand (← `(mutual
    $codecs:command*
    end))
  for message in descriptor.messages do
    let type := typeId message.name
    let encode := codecId message.name "encodeWith"
    let decode := codecId message.name "decodeWith"
    elabCommand (← `(instance : Message $type := ⟨$encode, $decode⟩))
  for service in descriptor.services do
    for method in service.methods do
      let methodPrefix := service.name ++ "." ++ method.name
      let input := codecId methodPrefix "Request"
      let output := codecId methodPrefix "Response"
      let pathName := codecId methodPrefix "path"
      let path := "/" ++ (if descriptor.package.isEmpty then "" else descriptor.package ++ ".") ++ service.name ++ "/" ++ method.name
      elabCommand (← `(abbrev $input := $(typeId method.input)))
      elabCommand (← `(abbrev $output := $(typeId method.output)))
      elabCommand (← `(def $pathName : String := $(quote path)))
      let info : MethodInfo := ⟨currentNamespace ++ dotted methodPrefix, currentNamespace ++ dotted method.input,
        currentNamespace ++ dotted method.output, path, method.clientStreaming, method.serverStreaming⟩
      modifyEnv (methodRegistry.addEntry · info)
  elabCommand (← `(end $root))

end LeanWeave.Protobuf
