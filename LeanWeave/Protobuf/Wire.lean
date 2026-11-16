import Std

namespace LeanWeave.Protobuf

structure Limits where
  maxBytes : Nat := 1024 * 1024
  maxFieldBytes : Nat := 512 * 1024
  maxFields : Nat := 4096
  maxUnknownBytes : Nat := 65536
  maxDepth : Nat := 32
  deriving Repr, Inhabited

def Limits.descend (limits : Limits) : Except String Limits :=
  if limits.maxDepth == 0 then .error "protobuf recursion budget exceeded"
  else .ok { limits with maxDepth := limits.maxDepth - 1 }

inductive WireValue where
  | varint (value : UInt64)
  | bits64 (value : UInt64)
  | bytes (value : ByteArray)
  | bits32 (value : UInt32)
  deriving BEq, Inhabited

def WireValue.kind : WireValue → Nat
  | .varint _ => 0 | .bits64 _ => 1 | .bytes _ => 2 | .bits32 _ => 5

structure RawField where
  number : Nat
  /-- Groups are retained as raw unknown fields, not exposed as typed values. -/
  value : Option WireValue
  raw : ByteArray
  deriving Inhabited

def varint (value : UInt64) : ByteArray := Id.run do
  let mut value := value.toNat
  let mut out := ByteArray.emptyWithCapacity 10
  for _ in [:10] do
    if value < 128 then return out.push value.toUInt8
    out := out.push ((value % 128) + 128).toUInt8
    value := value / 128
  return out

private def fixed (value width : Nat) : ByteArray := Id.run do
  let mut out := ByteArray.emptyWithCapacity width
  let mut value := value
  for _ in [:width] do
    out := out.push (value % 256).toUInt8
    value := value / 256
  return out

def encodeField (number : Nat) (value : WireValue) (limits : Limits := {}) : Except String ByteArray := do
  if number == 0 || number > 536870911 then throw "invalid protobuf field number"
  let tag := varint (number * 8 + value.kind).toUInt64
  let out ← match value with
    | .varint value => pure (tag ++ varint value)
    | .bits64 value => pure (tag ++ fixed value.toNat 8)
    | .bits32 value => pure (tag ++ fixed value.toNat 4)
    | .bytes bytes =>
      if bytes.size > limits.maxFieldBytes then throw "protobuf field byte budget exceeded"
      pure (tag ++ varint bytes.size.toUInt64 ++ bytes)
  if out.size > limits.maxBytes then throw "protobuf message byte budget exceeded"
  return out

private structure Cursor where
  input : ByteArray
  position : Nat := 0
  fields : Nat := 0
  limits : Limits

private abbrev Parser := StateT Cursor (Except String)

private def byte : Parser UInt8 := do
  let state ← get
  if state.position >= state.input.size then throw "truncated protobuf field"
  set { state with position := state.position + 1 }
  return state.input[state.position]!

private def readVarint : Parser UInt64 := do
  let mut result : Nat := 0
  for i in [:10] do
    let value ← byte
    if i == 9 && value > 1 then throw "protobuf varint overflows uint64"
    result := result + (value.toNat % 128) * 2 ^ (7*i)
    if value < 128 then return result.toUInt64
  throw "protobuf varint exceeds ten bytes"

private def readFixed (width : Nat) : Parser Nat := do
  let mut result := 0
  for i in [:width] do result := result + (← byte).toNat * 256 ^ i
  return result

private def readBytes : Parser ByteArray := do
  let length := (← readVarint).toNat
  let state ← get
  if length > state.limits.maxFieldBytes then throw "protobuf field byte budget exceeded"
  if length > state.input.size - state.position then throw "truncated length-delimited protobuf field"
  set { state with position := state.position + length }
  return state.input.extract state.position (state.position + length)

private def readTag : Parser (Nat × Nat) := do
  let state ← get
  if state.fields >= state.limits.maxFields then throw "protobuf field count budget exceeded"
  set { state with fields := state.fields + 1 }
  let tag := (← readVarint).toNat
  let number := tag / 8
  if number == 0 || number > 536870911 then throw "invalid protobuf field number"
  return (number, tag % 8)

mutual
  private partial def skipGroup (number depth : Nat) : Parser Unit := do
    if depth == 0 then throw "protobuf group recursion budget exceeded"
    let (field, kind) ← readTag
    if kind == 4 then
      unless field == number do throw "mismatched protobuf end group"
    else
      skipValue field kind (depth - 1)
      skipGroup number depth

  private partial def skipValue (number kind depth : Nat) : Parser Unit := do
    match kind with
    | 0 => let _ ← readVarint; pure ()
    | 1 => let _ ← readFixed 8; pure ()
    | 2 => let _ ← readBytes; pure ()
    | 3 => skipGroup number depth
    | 5 => let _ ← readFixed 4; pure ()
    | _ => throw "invalid protobuf wire type"
end

private def readValue (kind : Nat) : Parser WireValue := do
  match kind with
  | 0 => return .varint (← readVarint)
  | 1 => return .bits64 (← readFixed 8).toUInt64
  | 2 => return .bytes (← readBytes)
  | 5 => return .bits32 (← readFixed 4).toUInt32
  | _ => throw "invalid protobuf wire type"

/-- Decode complete wire fields, preserving unknown fields byte-for-byte. Invalid
    tags, oversized varints, truncated fields and group nesting fail closed. -/
def decodeRaw (input : ByteArray) (limits : Limits := {}) : Except String (Array RawField) := do
  if input.size > limits.maxBytes then throw "protobuf message byte budget exceeded"
  let parser : Parser (Array RawField) := do
    let mut fields := #[]
    for _ in [:limits.maxFields + 1] do
      let state ← get
      if state.position == input.size then return fields
      let start := state.position
      let (number, kind) ← readTag
      let value ← if kind == 3 then do skipGroup number limits.maxDepth; pure none
        else some <$> readValue kind
      let finish := (← get).position
      fields := fields.push ⟨number, value, input.extract start finish⟩
    throw "protobuf field count budget exceeded"
  return (← parser.run ⟨input, 0, 0, limits⟩).1

def recognizedWire (field : RawField) (known : Array Nat)
    (wireTypes : Array (Nat × Array Nat)) : Bool :=
  field.value.any fun value => known.contains field.number &&
    ((wireTypes.find? (·.1 == field.number)).map (fun entry => entry.2.contains value.kind)).getD true

def unknownFields (fields : Array RawField) (known : Array Nat) (limits : Limits := {})
    (wireTypes : Array (Nat × Array Nat) := #[]) : Except String (Array ByteArray) := do
  let mut bytes := 0
  let mut unknown := #[]
  for field in fields do
    if !recognizedWire field known wireTypes then
      bytes := bytes + field.raw.size
      if bytes > limits.maxUnknownBytes then throw "protobuf unknown-field byte budget exceeded"
      unknown := unknown.push field.raw
  return unknown

/-- Accept packed and unpacked representations of repeated primitive fields. -/
def values (fields : Array RawField) (number expectedWire : Nat) (repeated : Bool := false)
    (limits : Limits := {}) : Except String (Array WireValue) := do
  let mut result := #[]
  for field in fields do
    if field.number == number then
      let some value := field.value | continue
      if value.kind == expectedWire then result := result.push value
      else if repeated && value.kind == 2 && [0,1,5].contains expectedWire then
        let .bytes input := value | throw "invalid packed protobuf value"
        let parser : Parser (Array WireValue) := do
          let mut packed := #[]
          for _ in [:limits.maxFields + 1] do
            if (← get).position == input.size then return packed
            packed := packed.push (← readValue expectedWire)
          throw "protobuf repeated-field budget exceeded"
        result := result ++ (← parser.run ⟨input, 0, 0, limits⟩).1
      else continue -- A known number with another wire type is an unknown field.
      if result.size > limits.maxFields then throw "protobuf repeated-field budget exceeded"
  return result

def encodePacked (number : Nat) (values : Array WireValue) (limits : Limits := {}) : Except String ByteArray := do
  if values.size > limits.maxFields then throw "protobuf repeated-field budget exceeded"
  let mut payload := ByteArray.empty
  for value in values do
    let bytes ← match value with
      | .varint value => pure (varint value)
      | .bits32 value => pure (fixed value.toNat 4)
      | .bits64 value => pure (fixed value.toNat 8)
      | .bytes _ => throw "length-delimited protobuf values cannot be packed"
    if bytes.size > limits.maxFieldBytes - payload.size then throw "protobuf packed field budget exceeded"
    payload := payload ++ bytes
  encodeField number (.bytes payload) limits

def appendChecked (output bytes : ByteArray) (limits : Limits) : Except String ByteArray :=
  if output.size > limits.maxBytes || bytes.size > limits.maxBytes - output.size then .error "protobuf message byte budget exceeded"
  else .ok (output ++ bytes)

def appendUnknown (output : ByteArray) (unknown : Array ByteArray) (known : Array Nat)
    (limits : Limits) (wireTypes : Array (Nat × Array Nat) := #[]) : Except String ByteArray := do
  if unknown.size > limits.maxFields then throw "protobuf unknown-field count budget exceeded"
  let mut result := output
  let mut retained := 0
  for bytes in unknown do
    retained := retained + bytes.size
    if retained > limits.maxUnknownBytes then throw "protobuf unknown-field byte budget exceeded"
    let fields ← decodeRaw bytes limits
    if fields.size != 1 || fields.any (fun field => recognizedWire field known wireTypes) then
      throw "invalid or colliding protobuf unknown field"
    result ← appendChecked result bytes limits
  let _ ← decodeRaw result limits
  return result

class Message (α : Type) where
  encodeWith : α → Limits → Except String ByteArray
  decodeWith : ByteArray → Limits → Except String α

def encode [Message α] (value : α) (limits : Limits := {}) : Except String ByteArray := Message.encodeWith value limits
def decode (α : Type) [Message α] (bytes : ByteArray) (limits : Limits := {}) : Except String α := Message.decodeWith bytes limits

end LeanWeave.Protobuf
