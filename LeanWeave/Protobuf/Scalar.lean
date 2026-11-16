import LeanWeave.Protobuf.Wire

namespace LeanWeave.Protobuf

inductive ScalarType where
  | double | float | int32 | int64 | uint32 | uint64 | sint32 | sint64
  | fixed32 | fixed64 | sfixed32 | sfixed64 | bool | string | bytes
  deriving Repr, BEq, Inhabited

def ScalarType.parse : String → Option ScalarType
  | "double" => some .double | "float" => some .float | "int32" => some .int32
  | "int64" => some .int64 | "uint32" => some .uint32 | "uint64" => some .uint64
  | "sint32" => some .sint32 | "sint64" => some .sint64 | "fixed32" => some .fixed32
  | "fixed64" => some .fixed64 | "sfixed32" => some .sfixed32 | "sfixed64" => some .sfixed64
  | "bool" => some .bool | "string" => some .string | "bytes" => some .bytes | _ => none

def ScalarType.name : ScalarType → String
  | .double => "double" | .float => "float" | .int32 => "int32" | .int64 => "int64"
  | .uint32 => "uint32" | .uint64 => "uint64" | .sint32 => "sint32" | .sint64 => "sint64"
  | .fixed32 => "fixed32" | .fixed64 => "fixed64" | .sfixed32 => "sfixed32" | .sfixed64 => "sfixed64"
  | .bool => "bool" | .string => "string" | .bytes => "bytes"

def ScalarType.wire : ScalarType → Nat
  | .double | .fixed64 | .sfixed64 => 1
  | .float | .fixed32 | .sfixed32 => 5
  | .string | .bytes => 2
  | _ => 0

def ScalarType.packable (type : ScalarType) : Bool := type.wire != 2

def WireValue.isDefault : WireValue → Bool
  | .varint value | .bits64 value => value == 0
  | .bits32 value => value == 0
  | .bytes value => value.isEmpty

namespace Scalar

def double (value : Float) : WireValue := .bits64 value.toBits
def float (value : Float32) : WireValue := .bits32 value.toBits
def int32 (value : Int32) : WireValue := .varint (UInt64.ofInt value.toInt)
def int64 (value : Int64) : WireValue := .varint (UInt64.ofInt value.toInt)
def uint32 (value : UInt32) : WireValue := .varint value.toUInt64
def uint64 (value : UInt64) : WireValue := .varint value
def sint32 (value : Int32) : WireValue :=
  let n := value.toInt
  .varint (if n < 0 then (2*n.natAbs-1).toUInt64 else (2*n.natAbs).toUInt64)
def sint64 (value : Int64) : WireValue :=
  let n := value.toInt
  .varint (if n < 0 then (2*n.natAbs-1).toUInt64 else (2*n.natAbs).toUInt64)
def fixed32 (value : UInt32) : WireValue := .bits32 value
def fixed64 (value : UInt64) : WireValue := .bits64 value
def sfixed32 (value : Int32) : WireValue := .bits32 (UInt32.ofInt value.toInt)
def sfixed64 (value : Int64) : WireValue := .bits64 (UInt64.ofInt value.toInt)
def bool (value : Bool) : WireValue := .varint (if value then 1 else 0)
def string (value : String) : WireValue := .bytes value.toUTF8
def bytes (value : ByteArray) : WireValue := .bytes value

def getDouble : WireValue → Except String Float
  | .bits64 value => .ok (Float.ofBits value) | _ => .error "expected fixed64 protobuf value"
def getFloat : WireValue → Except String Float32
  | .bits32 value => .ok (Float32.ofBits value) | _ => .error "expected fixed32 protobuf value"
def getUint64 : WireValue → Except String UInt64
  | .varint value => .ok value | _ => .error "expected varint protobuf value"
def getUint32 (value : WireValue) : Except String UInt32 := do
  let value ← getUint64 value
  if value.toNat > 4294967295 then throw "protobuf uint32 overflow"
  return value.toUInt32
def getInt64 (value : WireValue) : Except String Int64 := do
  let value ← getUint64 value
  return Int64.ofInt (if value.toNat >= 9223372036854775808 then
    Int.ofNat value.toNat - 18446744073709551616 else Int.ofNat value.toNat)
def getInt32 (value : WireValue) : Except String Int32 := do
  let n ← getInt64 value
  if n.toInt < -2147483648 || n.toInt > 2147483647 then throw "protobuf int32 overflow"
  return Int32.ofInt n.toInt
def getSint64 (value : WireValue) : Except String Int64 := do
  let value ← getUint64 value
  let n := value.toNat
  return Int64.ofInt (if n % 2 == 1 then -(Int.ofNat (n/2)) - 1 else Int.ofNat (n/2))
def getSint32 (value : WireValue) : Except String Int32 := do
  let value ← getUint32 value
  let n := value.toNat
  return Int32.ofInt (if n % 2 == 1 then -(Int.ofNat (n/2)) - 1 else Int.ofNat (n/2))
def getFixed32 : WireValue → Except String UInt32
  | .bits32 value => .ok value | _ => .error "expected fixed32 protobuf value"
def getFixed64 : WireValue → Except String UInt64
  | .bits64 value => .ok value | _ => .error "expected fixed64 protobuf value"
def getSfixed32 (value : WireValue) : Except String Int32 := do
  return Int32.ofInt (Int.ofNat (← getFixed32 value).toNat)
def getSfixed64 (value : WireValue) : Except String Int64 := do
  return Int64.ofInt (Int.ofNat (← getFixed64 value).toNat)
def getBool (value : WireValue) : Except String Bool := do return (← getUint64 value) != 0
def getBytes : WireValue → Except String ByteArray
  | .bytes value => .ok value | _ => .error "expected length-delimited protobuf value"
def getString (value : WireValue) : Except String String := do
  let bytes ← getBytes value
  let some text := String.fromUTF8? bytes | throw "invalid UTF-8 protobuf string"
  return text

end Scalar

def singular (fields : Array RawField) (number wire : Nat) (decode : WireValue → Except String α)
    (fallback : α) (limits : Limits) : Except String α := do
  let values ← values fields number wire false limits
  match values.back? with | none => pure fallback | some value => decode value

def optional (fields : Array RawField) (number wire : Nat) (decode : WireValue → Except String α)
    (limits : Limits) : Except String (Option α) := do
  let values ← values fields number wire false limits
  match values.back? with | none => pure none | some value => some <$> decode value

def repeated (fields : Array RawField) (number wire : Nat) (decode : WireValue → Except String α)
    (limits : Limits) : Except String (Array α) := do
  (← values fields number wire true limits).mapM decode

/-- Singular embedded messages merge all occurrences in wire order. -/
def embedded (fields : Array RawField) (number : Nat) (decode : ByteArray → Limits → Except String α)
    (limits : Limits) : Except String (Option α) := do
  let all ← values fields number 2 false limits
  if all.isEmpty then return none
  let nested ← limits.descend
  let mut payload := ByteArray.empty
  for value in all do
    payload ← appendChecked payload (← Scalar.getBytes value) limits
  return some (← decode payload nested)

def embeddedRepeated (fields : Array RawField) (number : Nat)
    (decode : ByteArray → Limits → Except String α) (limits : Limits) : Except String (Array α) := do
  let all ← values fields number 2 false limits
  if all.isEmpty then return #[]
  let nested ← limits.descend
  all.mapM fun value => do decode (← Scalar.getBytes value) nested

def putScalar (output : ByteArray) (number : Nat) (value : WireValue)
    (present : Bool) (limits : Limits) : Except String ByteArray := do
  if !present && value.isDefault then return output
  appendChecked output (← encodeField number value limits) limits

def putRepeated (output : ByteArray) (number : Nat) (values : Array WireValue)
    (packed : Bool) (limits : Limits) : Except String ByteArray := do
  if values.isEmpty then return output
  if values.size > limits.maxFields then throw "protobuf repeated-field budget exceeded"
  if packed then appendChecked output (← encodePacked number values limits) limits
  else
    let mut output := output
    for value in values do output ← appendChecked output (← encodeField number value limits) limits
    return output

def putPrimitiveRepeated (output : ByteArray) (number : Nat) (items : Array α)
    (convert : α → WireValue) (packed : Bool) (limits : Limits) : Except String ByteArray := do
  if items.size > limits.maxFields then throw "protobuf repeated-field budget exceeded"
  putRepeated output number (items.map convert) packed limits

def putEmbedded (output : ByteArray) (number : Nat) (value : Option α)
    (encode : α → Limits → Except String ByteArray) (limits : Limits) : Except String ByteArray := do
  let some value := value | return output
  let nested ← limits.descend
  putScalar output number (.bytes (← encode value nested)) true limits

def putEmbeddedRepeated (output : ByteArray) (number : Nat) (items : Array α)
    (encode : α → Limits → Except String ByteArray) (limits : Limits) : Except String ByteArray := do
  if items.isEmpty then return output
  if items.size > limits.maxFields then throw "protobuf repeated-field budget exceeded"
  let nested ← limits.descend
  let mut output := output
  for item in items do output ← putScalar output number (.bytes (← encode item nested)) true limits
  return output

def oneofSelection (fields : Array RawField) (numbers : Array Nat)
    (wireTypes : Array (Nat × Array Nat) := #[]) : Option (Nat × Array RawField) := Id.run do
  let mut selected : Option (Nat × Array RawField) := none
  for field in fields do
    if recognizedWire field numbers wireTypes then
      selected := match selected with
        | some (number, values) => if number == field.number then some (number, values.push field) else some (field.number, #[field])
        | none => some (field.number, #[field])
  return selected

def getMap [BEq α] (fields : Array RawField) (number : Nat)
    (decodeEntry : ByteArray → Limits → Except String (α × β)) (limits : Limits) : Except String (Array (α × β)) := do
  let values ← values fields number 2 false limits
  if values.isEmpty then return #[]
  let nested ← limits.descend
  let mut entries := #[]
  for value in values do
    let entry ← decodeEntry (← Scalar.getBytes value) nested
    entries := (entries.filter fun previous => previous.1 != entry.1).push entry
  return entries

def putMap (output : ByteArray) (number : Nat) (entries : Array (α × β))
    (encodeEntry : (α × β) → Limits → Except String ByteArray) (limits : Limits) : Except String ByteArray := do
  if entries.isEmpty then return output
  if entries.size > limits.maxFields then throw "protobuf map entry budget exceeded"
  let nested ← limits.descend
  let mut output := output
  for entry in entries do output ← putScalar output number (.bytes (← encodeEntry entry nested)) true limits
  return output

def mapValues [BEq κ] (fields : Array RawField) (number : Nat)
    (decode : ByteArray → Limits → Except String (κ × υ)) (limits : Limits) : Except String (Array (κ × υ)) := do
  let values ← values fields number 2 false limits
  if values.isEmpty then return #[]
  let nested ← limits.descend
  let mut result := #[]
  for value in values do
    let pair ← decode (← Scalar.getBytes value) nested
    result := (result.filter (fun previous => previous.1 != pair.1)).push pair
  return result

end LeanWeave.Protobuf
