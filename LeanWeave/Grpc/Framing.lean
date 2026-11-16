import LeanWeave.Core.Types
import LeanWeave.Protobuf
import LeanWeave.Security.Crypto

namespace LeanWeave.Grpc

inductive Code where
  | ok | cancelled | unknown | invalidArgument | deadlineExceeded | notFound
  | alreadyExists | permissionDenied | resourceExhausted | failedPrecondition
  | aborted | outOfRange | unimplemented | internal | unavailable | dataLoss | unauthenticated
  deriving Repr, BEq, Inhabited

def Code.toNat : Code → Nat
  | .ok => 0 | .cancelled => 1 | .unknown => 2 | .invalidArgument => 3 | .deadlineExceeded => 4
  | .notFound => 5 | .alreadyExists => 6 | .permissionDenied => 7 | .resourceExhausted => 8
  | .failedPrecondition => 9 | .aborted => 10 | .outOfRange => 11 | .unimplemented => 12
  | .internal => 13 | .unavailable => 14 | .dataLoss => 15 | .unauthenticated => 16

structure Status where
  code : Code := .ok
  message : String := ""
  deriving Repr, BEq, Inhabited

def Status.ofAppError (error : AppError) : Status :=
  match error.status with
  | 400 | 415 | 422 => ⟨.invalidArgument, error.message⟩
  | 401 => ⟨.unauthenticated, error.message⟩
  | 403 => ⟨.permissionDenied, error.message⟩
  | 404 => ⟨.notFound, error.message⟩
  | 409 => ⟨.aborted, error.message⟩
  | 413 | 429 => ⟨.resourceExhausted, error.message⟩
  | 408 | 504 => ⟨.deadlineExceeded, "RPC deadline exceeded"⟩
  | 503 => ⟨.unavailable, "RPC service unavailable"⟩
  | _ => ⟨.internal, "Internal RPC error"⟩

structure Limits where
  maxMessageBytes : Nat := 65536
  maxMessages : Nat := 4096
  maxTotalBytes : Nat := 4 * 1024 * 1024
  maxMetadataBytes : Nat := 16384
  maxTimeoutMs : Nat := 30000
  maxProtoFields : Nat := 4096
  maxProtoDepth : Nat := 16
  deriving Repr, Inhabited

def Limits.validate (limits : Limits) : Except String Unit := do
  if limits.maxMessageBytes == 0 || limits.maxMessageBytes > 16 * 1024 * 1024 ||
      limits.maxMessages == 0 || limits.maxMessages > 100000 || limits.maxTotalBytes < limits.maxMessageBytes ||
      limits.maxTotalBytes > 1024 * 1024 * 1024 || limits.maxMetadataBytes == 0 || limits.maxMetadataBytes > 65536 ||
      limits.maxTimeoutMs == 0 || limits.maxTimeoutMs > 3600000 || limits.maxProtoFields == 0 ||
      limits.maxProtoFields > 65536 || limits.maxProtoDepth == 0 || limits.maxProtoDepth > 64 then
    throw "invalid gRPC limits"

def Limits.protobuf (limits : Limits) : Protobuf.Limits := {
  maxBytes := limits.maxMessageBytes, maxFieldBytes := limits.maxMessageBytes,
  maxFields := limits.maxProtoFields, maxDepth := limits.maxProtoDepth,
  maxUnknownBytes := min limits.maxMessageBytes 65536 }

/-- Extra adapter reservation; configure Server.appRetainedBytes accordingly.
    Source callbacks additionally declare their own persistent application state. -/
def Limits.reservation (limits : Limits) : Nat :=
  limits.maxMessageBytes * 8 + limits.maxMetadataBytes * 2 + limits.maxProtoFields * 64

inductive Mode where
  | native | web | webText
  deriving Repr, BEq, Inhabited

def mode? (contentType : String) : Option Mode :=
  let media := (contentType.splitOn ";").head!.trimAscii.toString.toLower
  if ["application/grpc", "application/grpc+proto"].contains media then some .native
  else if ["application/grpc-web", "application/grpc-web+proto"].contains media then some .web
  else if ["application/grpc-web-text", "application/grpc-web-text+proto"].contains media then some .webText
  else none

def Mode.contentType : Mode → String
  | .native => "application/grpc+proto"
  | .web => "application/grpc-web+proto"
  | .webText => "application/grpc-web-text+proto"

def timeoutMs (header : Option String) (ceiling : Nat) : Except Status Nat := do
  let some text := header | return ceiling
  if text.length < 2 || text.length > 9 then throw ⟨.invalidArgument, "invalid grpc-timeout"⟩
  let number := (text.dropEnd 1).toString
  unless number.toList.all Char.isDigit do throw ⟨.invalidArgument, "invalid grpc-timeout"⟩
  let some number := number.toNat? | throw ⟨.invalidArgument, "invalid grpc-timeout"⟩
  let value ← match text.toList.getLast! with
    | 'H' => pure (number * 3600000) | 'M' => pure (number * 60000) | 'S' => pure (number * 1000)
    | 'm' => pure number | 'u' => pure ((number + 999)/1000) | 'n' => pure ((number + 999999)/1000000)
    | _ => throw ⟨.invalidArgument, "invalid grpc-timeout unit"⟩
  return min ceiling value

def frame (payload : ByteArray) (trailers : Bool := false) : ByteArray :=
  let n := payload.size
  (ByteArray.mk #[(if trailers then 128 else 0), (n/16777216).toUInt8,
    (n/65536).toUInt8, (n/256).toUInt8, n.toUInt8]) ++ payload

def frameLength (bytes : ByteArray) : Nat :=
  bytes[1]!.toNat * 16777216 + bytes[2]!.toNat * 65536 + bytes[3]!.toNat * 256 + bytes[4]!.toNat

def statusHeaders (status : Status) : Array LeanNet.HTTP1.Header := #[
  { name := "grpc-status", value := (toString status.code.toNat).toUTF8 },
  { name := "grpc-message", value := (percentEncode (status.message.take 256).toString).toUTF8 }]

def webTrailers (status : Status) : ByteArray :=
  let block := String.join ((statusHeaders status).toList.map fun header =>
    header.name ++ ": " ++ String.fromUTF8! header.value ++ "\r\n")
  frame block.toUTF8 true

def base64 (bytes : ByteArray) : ByteArray :=
  let text := (Security.Crypto.base64url bytes).replace "-" "+" |>.replace "_" "/"
  (text ++ String.ofList (List.replicate ((4-text.length%4)%4) '=')).toUTF8

/-- gRPC-Web permits padding at each flushed base64 segment. Decode complete
    quartets independently, retaining at most three bytes between HTTP chunks. -/
def unbase64Chunks (carry bytes : ByteArray) : Except Status (ByteArray × ByteArray) := do
  let bytes := carry ++ bytes
  let count := bytes.size / 4
  let mut result := ByteArray.empty
  for i in [:count] do
    let quartet := bytes.extract (4*i) (4*i+4)
    let some text := String.fromUTF8? quartet | throw ⟨.invalidArgument, "invalid gRPC-Web base64"⟩
    unless text.toUTF8.data.all (fun b => (b >= 65 && b <= 90) || (b >= 97 && b <= 122) ||
        (b >= 48 && b <= 57) || [43,47,61].contains b.toNat) do
      throw ⟨.invalidArgument, "invalid gRPC-Web base64 alphabet"⟩
    let stripped := text.dropEndWhile (· == '=') |>.toString
    if stripped.contains '=' || stripped.length < 2 then throw ⟨.invalidArgument, "invalid gRPC-Web base64 padding"⟩
    let decoded ← (Security.Crypto.unbase64url (stripped.replace "+" "-" |>.replace "/" "_") 3).mapError
      (fun _ => Status.mk .invalidArgument "invalid gRPC-Web base64 bits")
    unless base64 decoded == quartet do throw ⟨.invalidArgument, "noncanonical gRPC-Web base64"⟩
    result := result ++ decoded
  return (result, bytes.extract (4*count) bytes.size)

end LeanWeave.Grpc
