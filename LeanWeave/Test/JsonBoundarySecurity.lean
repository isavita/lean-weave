import LeanWeave.Security.Jwt
import LeanWeave.Security.Journal

namespace LeanWeave.Test.JsonBoundarySecurity

private def check (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw (IO.userError ("JSON security boundaries: " ++ message))

private def rawToken (key : ByteArray) (header claims : String) : String :=
  let signed := Security.Crypto.base64url header.toUTF8 ++ "." ++ Security.Crypto.base64url claims.toUTF8
  signed ++ "." ++ Security.Crypto.base64url (Security.Crypto.hmacSha256 key signed.toUTF8)

private def numericFailure (result : Except String α) : Bool :=
  match result with
  | .error message => message.startsWith "JSON exponent" || message.startsWith "JSON mantissa"
  | .ok _ => false

/-- Exercise the public security entry points, without ever parsing a hostile
numeric literal in test setup. The dangerous exponents remain short strings. -/
def run : IO Unit := do
  let started ← IO.monoMsNow
  let key := ByteArray.mk (Array.replicate 32 42)
  let policy : Security.Jwt.Policy := { issuer := "issuer", audience := "app", clockSkewSeconds := 0 }
  let header := "{\"alg\":\"HS256\",\"typ\":\"JWT\"}"
  let claims := "{\"iss\":\"issuer\",\"aud\":\"app\",\"sub\":\"alice\",\"iat\":1000,\"exp\":2000}"
  check ((Security.Jwt.verify key policy (rawToken key header claims) 1500).isOk) "ordinary signed JWT remains valid"
  let literals := #["1e1000000000", "1e-1000000000", "-1E+1000000000",
    String.ofList (List.replicate 513 '9')]
  for literal in literals do
    let hostileHeader := "{\"alg\":\"HS256\",\"n\":" ++ literal ++ "}"
    let token := rawToken key hostileHeader claims
    check (numericFailure (Security.Jwt.verify key policy token 1500)) "signed malicious header fails at numeric guard"
    let unsigned := Security.Crypto.base64url hostileHeader.toUTF8 ++ "." ++ Security.Crypto.base64url claims.toUTF8 ++ "."
    check (numericFailure (Security.Jwt.verify key policy unsigned 1500)) "unauthenticated header fails before signature parsing"
    let hostileClaims := "{\"iss\":\"issuer\",\"aud\":\"app\",\"sub\":\"alice\",\"iat\":1000,\"exp\":" ++ literal ++ "}"
    check (numericFailure (Security.Jwt.verify key policy (rawToken key header hostileClaims) 1500)) "signed malicious claims fail before expansion"
    check (numericFailure (Security.Audit.verifyLine {} ("{\"sequence\":" ++ literal ++ "}"))) "audit replay rejects numeric expansion before record decoding"
  let quotedHeader := "{\"alg\":\"HS256\",\"n\":\"1e1000000000\"}"
  check ((Security.Jwt.verify key policy (rawToken key quotedHeader claims) 1500).isOk) "numeric-looking JWT string stays valid"
  for literal in #["1.7976931348623157e308", "5e-324", "0e-1024"] do
    let .ok canonical := Body.json ("{\"alg\":\"HS256\",\"n\":" ++ literal ++ "}").toUTF8
      | throw (IO.userError "valid finite numeric JWT header rejected")
    check ((Security.Jwt.verify key policy (rawToken key canonical.compress claims) 1500).isOk)
      "canonical finite numeric JWT header remains verifiable"
  let .ok record := Security.Audit.append {} { action := "test", actor := "actor", timeUnixNano := 10 }
    | throw (IO.userError "valid audit append failed")
  check ((Security.Audit.verifyLine {} record.json.compress).isOk) "canonical audit record remains valid"
  IO.FS.withTempDir fun directory => do
    let path := directory / "hostile-audit.jsonl"
    IO.FS.writeFile path "{\"sequence\":1e1000000000}\n"
    let rejected ← try
      let journal ← Security.Audit.Journal.open path { startupTimeoutMs := 1000 }
      journal.close
      let _ ← journal.writer.join 1000
      pure false
    catch _ => pure true
    check rejected "corrupted journal startup fails closed"
  check ((← IO.monoMsNow) - started < 5000) "bounded security parser corpus completes promptly"
  IO.println "PASS JWT/audit bounded JSON: signed and unauthenticated headers, claims, corrupt journal, valid controls"

end LeanWeave.Test.JsonBoundarySecurity
