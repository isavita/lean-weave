import LeanWeaveHttp.Client
import LeanWeave.Secrets.Manager

namespace LeanWeave.Http.Vault

structure Binding where
  source : String
  mount : String := "secret"
  path : String
  field : String
  /-- KV v2 is static storage, so this is the application's refresh/expiry policy. -/
  ttlMs : Nat := 300000

structure Config where
  bindings : Array Binding
  /-- Explicit opt-in only for a local Vault dev fixture. -/
  allowPlaintextDevelopment : Bool := false

private def validSegment (text : String) : Bool :=
  !text.isEmpty && text != "." && text != ".." && text.utf8ByteSize <= 128 && text.toUTF8.data.all (fun b =>
    (b >= 65 && b <= 90) || (b >= 97 && b <= 122) || (b >= 48 && b <= 57) || b == 45 || b == 95 || b == 46)

/-- Vault KV v2 over certificate-verified native TLS. Token and decrypted values
are held only in memory, never emitted in spans, errors, command lines or files.
Rotating the referenced bootstrap token atomically affects the next refresh. -/
def backend (client : Client) (token : IO Secrets.Secret) (config : Config) : Except String Secrets.Backend := do
  if !client.config.endpoint.secure && !(config.allowPlaintextDevelopment &&
      ["127.0.0.1", "::1"].contains client.config.endpoint.hostAddr) then throw "Vault requires TLS"
  if config.bindings.size > 1024 || config.bindings.size != (config.bindings.toList.map (·.source)).eraseDups.length then
    throw "invalid Vault binding count"
  for binding in config.bindings do
    if binding.source.isEmpty || binding.source.utf8ByteSize > 128 || !validSegment binding.mount ||
        binding.path.utf8ByteSize > 1024 || !(binding.path.splitOn "/").all validSegment ||
        binding.field.isEmpty || binding.field.utf8ByteSize > 128 || binding.ttlMs < 100 || binding.ttlMs > 86400000 then
      throw "invalid Vault binding"
  return fun source => do
    let some binding := config.bindings.find? (·.source == source) | throw { status := 503, message := "Vault source unavailable" }
    let secret ← token
    let token ← match secret.use (← IO.monoMsNow) (fun bytes => String.fromUTF8? bytes) with
      | .ok (some value) => pure value
      | _ => throw { status := 503, message := "Vault credential unavailable" }
    let result ← client.request {
      path := "/v1/" ++ binding.mount ++ "/data/" ++ binding.path
      headers := #[("x-vault-token", token)] }
    if result.status != 200 then throw { status := 503, message := "Vault secret load failed" }
    let loaded : Except String Secrets.Loaded := do
      let json ← Lean.Json.parse (String.fromUTF8! result.body)
      let data ← json.getObjVal? "data"
      let values ← data.getObjVal? "data"
      let value : String ← values.getObjValAs? _ binding.field
      let metadata ← data.getObjVal? "metadata"
      let version : Nat ← metadata.getObjValAs? _ "version"
      let deleted : String ← metadata.getObjValAs? _ "deletion_time"
      let destroyed : Bool ← metadata.getObjValAs? _ "destroyed"
      if !deleted.isEmpty || destroyed || version == 0 then throw "Vault version unavailable"
      return { value := value.toUTF8, version := toString version, ttlMs := binding.ttlMs }
    match loaded with
    | .ok loaded => return loaded
    | .error _ => throw { status := 503, message := "Vault secret response invalid" }

end LeanWeave.Http.Vault
