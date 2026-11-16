import LeanWeave.Security.Crypto
import LeanWeave.Core.Body

namespace LeanWeave.Security.Jwt

structure Policy where
  issuer : String
  audience : String
  clockSkewSeconds : Nat := 30
  maxLifetimeSeconds : Nat := 3600
  maxTokenBytes : Nat := 8192

/-- Claims are minted only after signature, issuer, audience and time validation. -/
structure VerifiedClaims where
  private mk ::
  value : Lean.Json
  subject : String
  expires : Nat

private def objectKeys (json : Lean.Json) : Except String (Array String) :=
  match json with
  | .obj fields => .ok (fields.toArray.map Prod.fst)
  | _ => .error "expected JSON object"

private def uniqueObject (text : String) (maxBytes : Nat) : Except String Lean.Json := do
  -- Json.parse canonicalizes objects. Inspect keys before duplicates disappear.
  let bytes := text.toUTF8
  let mut seen : Array String := #[]
  let mut depth := 0
  let mut inString := false
  let mut escaped := false
  let mut keyExpected := false
  let mut stringStart := 0
  for i in [:bytes.size] do
    let b := bytes[i]!
    if inString then
      if escaped then escaped := false
      else if b == 92 then escaped := true
      else if b == 34 then
        inString := false
        if depth == 1 && keyExpected then
          -- This slice is exactly one quoted string, never a numeric token.
          let keyJson ← Lean.Json.parse (String.fromUTF8! (bytes.extract stringStart (i+1)))
          let name ← keyJson.getStr?
          if seen.contains name then throw "duplicate JWT claim"
          seen := seen.push name
          keyExpected := false
    else if b == 34 then
      inString := true
      stringStart := i
    else if b == 123 || b == 91 then
      depth := depth + 1
      if depth > 16 then throw "JWT JSON nesting too deep"
      if depth == 1 then keyExpected := true
    else if b == 125 || b == 93 then depth := depth - 1
    else if b == 44 && depth == 1 then keyExpected := true
  let json ← (Body.json bytes { maxBytes, maxDepth := 16 }).mapError (·.message)
  let keys ← objectKeys json
  if keys.size != keys.toList.eraseDups.length then throw "duplicate JWT claim"
  return json

private def decodeJson (part : String) (max : Nat) : Except String Lean.Json := do
  let bytes ← Crypto.unbase64url part max
  let some text := String.fromUTF8? bytes | throw "JWT JSON must be UTF-8"
  uniqueObject text max

private def sameBytes (a b : ByteArray) : Bool := Id.run do
  if a.size != b.size then return false
  let mut difference : UInt8 := 0
  for i in [:a.size] do difference := difference ||| (a[i]! ^^^ b[i]!)
  return difference == 0

/-- HS256 is the only accepted algorithm; alg=none and algorithm substitution fail.
    Keys need at least 256 bits; production callers obtain keys from a secret provider. -/
def verify (key : ByteArray) (policy : Policy) (token : String) (nowSeconds : Nat) : Except String VerifiedClaims := do
  if key.size < 32 || policy.issuer.isEmpty || policy.audience.isEmpty then throw "invalid JWT verification configuration"
  if token.utf8ByteSize > policy.maxTokenBytes then throw "JWT too large"
  let parts := token.splitOn "."
  if parts.length != 3 then throw "invalid JWT compact representation"
  let header ← decodeJson parts[0]! 2048
  let algorithm : String ← header.getObjValAs? _ "alg"
  if algorithm != "HS256" then throw "unsupported JWT algorithm"
  if (header.getObjVal? "crit").isOk || (header.getObjVal? "b64").isOk then throw "unsupported critical JWT extension"
  if let .ok (kind : String) := header.getObjValAs? _ "typ" then
    if kind != "JWT" && kind != "at+jwt" then throw "unsupported JWT type"
  let signature ← Crypto.unbase64url parts[2]! 32
  if signature.size != 32 || !sameBytes signature (Crypto.hmacSha256 key (parts[0]! ++ "." ++ parts[1]!).toUTF8) then
    throw "invalid JWT signature"
  let claims ← decodeJson parts[1]! policy.maxTokenBytes
  let issuer : String ← claims.getObjValAs? _ "iss"
  if issuer != policy.issuer then throw "invalid JWT issuer"
  let audience ← claims.getObjVal? "aud"
  let audienceMatches := match audience with
    | .str value => value == policy.audience
    | .arr values => !values.isEmpty && values.size <= 32 && values.all (fun v => v.getStr?.isOk) && values.any (fun v => v == Lean.Json.str policy.audience)
    | _ => false
  if !audienceMatches then throw "invalid JWT audience"
  let expires : Nat ← claims.getObjValAs? _ "exp"
  let issued : Nat ← claims.getObjValAs? _ "iat"
  if expires <= issued || expires - issued > policy.maxLifetimeSeconds then throw "invalid JWT lifetime"
  if nowSeconds >= expires + policy.clockSkewSeconds then throw "JWT expired"
  if issued > nowSeconds + policy.clockSkewSeconds then throw "JWT issued in the future"
  if let .ok notBefore := claims.getObjVal? "nbf" then
    let validFrom : Nat ← Lean.fromJson? notBefore
    if validFrom > nowSeconds + policy.clockSkewSeconds then throw "JWT not active"
  let subject : String ← claims.getObjValAs? _ "sub"
  if subject.isEmpty || subject.utf8ByteSize > 256 then throw "invalid JWT subject"
  return ⟨claims, subject, expires⟩

def VerifiedClaims.string (claims : VerifiedClaims) (name : String) : Option String :=
  (claims.value.getObjValAs? String name).toOption

/-- Signing helper for issuers/tests; caller supplies issuer/audience/time claims. -/
def sign (key : ByteArray) (claims : Lean.Json) : Except String String := do
  if key.size < 32 then throw "HS256 signing requires a 256-bit key"
  let head := Crypto.base64url "{\"alg\":\"HS256\",\"typ\":\"JWT\"}".toUTF8
  let body := Crypto.base64url claims.compress.toUTF8
  let signed := head ++ "." ++ body
  return signed ++ "." ++ Crypto.base64url (Crypto.hmacSha256 key signed.toUTF8)

def bearer (ctx : Context) : Except AppError String := do
  let values := ctx.request.headers.filter (fun h => h.name.toLower == "authorization")
  if values.size != 1 then throw { status := 401, message := "one bearer token required" }
  let some value := String.fromUTF8? values[0]!.value | throw { status := 401, message := "invalid bearer token" }
  if !value.startsWith "Bearer " then throw { status := 401, message := "bearer token required" }
  return (value.drop 7).toString

end LeanWeave.Security.Jwt
