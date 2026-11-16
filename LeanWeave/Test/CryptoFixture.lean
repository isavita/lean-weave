import LeanWeave.Security.Crypto

open LeanWeave.Security

def main : IO Unit := do
  let input ← IO.getStdin
  let output ← IO.getStdout
  repeat
    let line ← input.getLine
    if line.isEmpty then break
    let result : Except String Lean.Json := do
      let json ← Lean.Json.parse line
      let message : String ← json.getObjValAs? _ "message"
      let key : String ← json.getObjValAs? _ "key"
      let bytes ← Crypto.unbase64url message (2 * 1024 * 1024)
      let key ← Crypto.unbase64url key 65536
      return Lean.Json.mkObj [("sha256", Lean.toJson (Crypto.hex (Crypto.sha256 bytes))),
        ("hmac", Lean.toJson (Crypto.hex (Crypto.hmac256 key bytes))),
        ("base64url", Lean.toJson (Crypto.base64url bytes))]
    match result with
    | .ok value => output.putStrLn value.compress
    | .error _ => throw (IO.userError "invalid crypto fixture input")
