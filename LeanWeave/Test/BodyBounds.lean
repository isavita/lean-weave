import LeanWeave.Core.Body

namespace LeanWeave.Test.BodyBounds

private def check (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw (IO.userError ("JSON numeric bounds: " ++ message))
private def status (text : String) (limits : Body.Limits := {}) : Nat :=
  match Body.json text.toUTF8 limits with | .ok _ => 200 | .error error => error.status

private structure TypedNumbers where
  natural : Nat
  integer : Int
  deriving Lean.FromJson

def run : IO Unit := do
  -- These remain tiny source strings. They are never passed to Json.parse.
  for source in #["1e1000000000", "1e-1000000000", "0e1000000000", "-1E+1000000000",
      "[1e1000000000]", "{\"n\":1e1000000000}", "1e1000000000 trailing"] do
    check (match Body.checkJsonNumbers source.toUTF8 with
      | .error error => error.status == 413 | .ok () => false) "lexical preflight rejects before any parsing test"
    check (status source == 413) "huge exponent rejected before expansion, including malformed suffix"
  check (status (String.ofList (List.replicate 129 '9')) == 413) "long integer mantissa rejected"
  check (status ("0." ++ String.ofList (List.replicate 128 '1')) == 413) "fraction counts towards total mantissa digits"
  check (status ("1e" ++ String.ofList (List.replicate 129 '0')) == 413) "long leading-zero exponent bounded even at magnitude zero"
  check (status (String.ofList (List.replicate 128 '9')) == 200) "exact mantissa boundary accepted"
  check (status "1e1024" == 200 && status "1e1025" == 413 && status "1e-1024" == 200 && status "1e-1025" == 413) "symmetric exponent boundary"
  check (status "5e-324" == 200 && status "1.7976931348623157e308" == 200) "IEEE754 extremes remain representable"
  check (status "18446744073709551615" == 200 && status "-9223372036854775808" == 200) "UInt64/Int64 decimal ranges accepted"
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

end LeanWeave.Test.BodyBounds
