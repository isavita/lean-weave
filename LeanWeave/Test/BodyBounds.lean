import LeanWeave.Core.Body

namespace LeanWeave.Test.BodyBounds

private def check (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw (IO.userError ("JSON numeric bounds: " ++ message))
private def status (text : String) (limits : Body.Limits := {}) : Nat :=
  match Body.json text.toUTF8 limits with | .ok _ => 200 | .error error => error.status

private def roundtrip (text : String) (limits : Body.Limits := {}) : IO Unit := do
  let .ok value := Body.json text.toUTF8 limits | throw (IO.userError "roundtrip input rejected")
  let canonical := value.compress
  let .ok reparsed := Body.json canonical.toUTF8 limits | throw (IO.userError "canonical numeric representation rejected")
  check (reparsed.compress == canonical) "canonical JSON must be stable after bounded reparse"
  if let .num first := value then
    let .num second := reparsed | throw (IO.userError "roundtrip numeric type changed")
    check (!(first < second) && !(second < first)) "canonical number preserves exact mathematical value"

private structure TypedNumbers where
  natural : Nat
  integer : Int
  deriving Lean.FromJson

private def multipartBounds : IO Unit := do
  let headers := "Content-Disposition: form-data; name=\"fíle;category=data\"; filename=\"report=final;2026.pdf\"\r\nContent-Type: application/octet-stream"
  let payload := ByteArray.mk #[0, 13, 42, 255]
  let wire := ("--upload\r\n" ++ headers ++ "\r\n\r\n").toUTF8 ++ payload ++
    "\r\n--upload\r\nContent-Disposition: form-data; name=\"note\"\r\n\r\nhello\r\n--upload--\r\n".toUTF8
  let limits : Body.Limits := { maxPartHeaderBytes := headers.utf8ByteSize, maxFileBytes := payload.size }
  let .ok parts := Body.multipart wire "upload" limits
    | throw (IO.userError "multipart rejected a valid quoted parameter or exact byte limit")
  unless parts.size == 2 && parts[0]!.name == "fíle;category=data" &&
      parts[0]!.filename == some "report=final;2026.pdf" && parts[0]!.bytes == payload &&
      parts[1]!.name == "note" && parts[1]!.bytes == "hello".toUTF8 do
    throw (IO.userError "multipart did not preserve quoted parameters and binary bytes")
  let status := fun result : Except AppError (Array Body.Part) => match result with
    | .error error => error.status
    | .ok _ => 200
  unless status (Body.multipart wire "upload" { limits with maxPartHeaderBytes := headers.utf8ByteSize - 1 }) == 413 do
    throw (IO.userError "multipart accepted a header terminator beyond its byte limit")
  unless status (Body.multipart wire "upload" { limits with maxFileBytes := payload.size - 1 }) == 413 do
    throw (IO.userError "multipart accepted a file boundary beyond its byte limit")
  unless status (Body.multipart "--upload\r\nContent-Disposition: form-data".toUTF8 "upload" limits) == 400 do
    throw (IO.userError "multipart did not distinguish a truncated header within its byte budget")
  IO.println "PASS bounded multipart: quoted parameters, binary bytes, exact header and file limits"

def run : IO Unit := do
  -- These remain tiny source strings. They are never passed to Json.parse.
  for source in #["1e1000000000", "1e-1000000000", "0e1000000000", "-1E+1000000000",
      "[1e1000000000]", "{\"n\":1e1000000000}", "1e1000000000 trailing"] do
    check (match Body.checkJsonNumbers source.toUTF8 with
      | .error error => error.status == 413 | .ok () => false) "lexical preflight rejects before any parsing test"
    check (status source == 413) "huge exponent rejected before expansion, including malformed suffix"
  check (status (String.ofList (List.replicate 513 '9')) == 413) "long integer mantissa rejected"
  check (status ("0." ++ String.ofList (List.replicate 512 '1')) == 413) "fraction counts towards total mantissa digits"
  check (status ("1e" ++ String.ofList (List.replicate 513 '0')) == 413) "long leading-zero exponent bounded even at magnitude zero"
  check (status (String.ofList (List.replicate 512 '9')) == 200) "exact mantissa boundary accepted"
  check (status "1e511" == 200 && status "1e512" == 413) "expanded positive-exponent mantissa bounded"
  check (status "1e1024" == 413 && status "1e-1024" == 200 && status "1e-1025" == 413) "negative exponent boundary with bounded positive expansion"
  check (status "5e-324" == 200 && status "1.7976931348623157e308" == 200) "IEEE754 extremes remain representable"
  check (status "18446744073709551615" == 200 && status "-9223372036854775808" == 200) "UInt64/Int64 decimal ranges accepted"
  check (status "0.1e-1023" == 200 && status "0.1e-1024" == 413) "fraction and negative exponent share denominator budget"
  check (status "0.001" {maxExponentMagnitude := 2} == 413 && status "0.001e1" {maxExponentMagnitude := 2} == 200) "positive exponent cancels fraction places"
  check (status "12.34e3" {maxNumberDigits := 4} == 413 && status "12.34e2" {maxNumberDigits := 4} == 200) "positive expansion includes fraction offset"
  let boundarySmall := "0." ++ String.ofList (List.replicate 502 '9') ++ "e-522"
  let oversizedSmall := "0." ++ String.ofList (List.replicate 503 '9') ++ "e-521"
  check (status boundarySmall == 200 && status oversizedSmall == 413) "canonical leading decimal padding budget"
  for source in #["1.7e308", "1.7976931348623157e308", "5e-324", "1e511", "1e-1024", "0.1e-1023",
      "0e-1024", "-0e-1024", "0.0e-1023", "-0.000e10", "0.100e-1021", boundarySmall,
      String.ofList (List.replicate 512 '9'), "{\"n\":[0e-1024,1.7e308,5e-324]}"] do roundtrip source
  let .ok expanded := Body.json "1.7e308".toUTF8 | throw (IO.userError "finite float spelling rejected")
  check (expanded.compress.utf8ByteSize == 309) "IEEE normal canonical 309-digit integer remains within default"
  let .ok zero := Body.json "0e-1024".toUTF8 | throw (IO.userError "zero exponent spelling rejected")
  check (zero.compress == "0") "large-denominator zero normalizes before Lean's invalid 0.e serialization"
  for digits in #[1,2,3,10,11,12,32] do
    let limits : Body.Limits := { maxNumberDigits := digits, maxExponentMagnitude := 32 }
    for exponent in [:33] do
      for head in #["1e-", "10e-", "0.1e-", "0.0001e-", "0e-", "1e"] do
        let source := head ++ toString exponent
        if (Body.json source.toUTF8 limits).isOk then roundtrip source limits
  for source in #["0", "-0", "0.125", "-12.34E+2", "1e000000000000000000000001", "[1,-2,3.25,4e+2]"] do
    check (status source == 200) "standard numeric spelling preserved"
  for source in #["1e", "1e+", "1e-", "1e--2", "1e2.3", "1e2e3", "+1", "01", ".1", "1.", "--1", "[1 2]"] do
    check (status source == 400) "malformed bounded grammar rejected by parser"
  check (status "\"1e1000000000\"" == 200) "numeric-looking string is not a number"
  check (status "{\"quote\":\"\\\" 1e1000000000\",\"slashes\":\"\\\\\",\"ok\":2e2}" == 200) "escaped quote/backslash scanning"
  check (status "\"\\u0031e1000000000\"" == 200) "escaped numeric-looking string ignored"
  check (status "\"unterminated 1e1000000000" == 400) "malformed string remains parser error"
  check (status "{\"s\":\"1e1000000000\",\"n\":1e1000000000}" == 413) "number after string still bounded"
  check (status "1234" {maxNumberDigits := 3} == 413 && status "123" {maxNumberDigits := 3} == 200) "configured digit bound"
  check (status "1e3" {maxExponentMagnitude := 2} == 413 && status "1e-2" {maxExponentMagnitude := 2} == 200) "configured exponent bound"
  check (status "1e0" {maxExponentMagnitude := 0} == 200 && status "1e1" {maxExponentMagnitude := 0} == 413) "zero exponent policy"
  check (status "0" {maxNumberDigits := 0} == 500 && status "0" {maxNumberDigits := 4097} == 500 &&
    status "0" {maxExponentMagnitude := 4097} == 500) "numeric policy hard caps validated"
  let .ok json := Body.json "{\"natural\":1e20,\"integer\":-1e20}".toUTF8 | throw (IO.userError "valid typed JSON rejected")
  let .ok decoded := (Lean.fromJson? json : Except String TypedNumbers) | throw (IO.userError "valid typed numeric decoding rejected")
  check (decoded.natural == 100000000000000000000 && decoded.integer == -100000000000000000000) "typed Nat/Int scientific decoding preserved within limits"
  check (status "[[[0]]]" {maxDepth := 2} == 413 && status "[1,2,3]" {maxFields := 1} == 413) "existing depth/node limits preserved"
  IO.println "PASS bounded JSON numbers: preparse exponents/digits, lexical strings/grammar, configuration and typed decoding"
  multipartBounds

end LeanWeave.Test.BodyBounds
