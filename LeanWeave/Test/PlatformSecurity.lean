import LeanWeave.Security.Crypto
import LeanWeave.Security.Jwt
import LeanWeave.Security.Csp
import LeanWeave.Security.Policy
import LeanWeave.Tenant.Jwt
import LeanWeave.Tenant.Config
import LeanWeave.Tenant.Brand
import LeanWeave.Feature.Basic
import LeanWeave.Test.Client

namespace LeanWeave.Test.PlatformSecurity

open Security.Crypto

private def check (value : Bool) (message : String) : IO Unit :=
  unless value do throw (IO.userError s!"platform security: {message}")

private def require (value : Except ε α) : IO α :=
  match value with | .ok value => pure value | .error _ => throw (IO.userError "expected successful result")

private def baseContext : IO Context := require (LeanWeave.Test.context)
private def tenant (name : String) : IO Tenant.Id := require (Tenant.Id.parse name)
private def hdr (name value : String) : LeanNet.HTTP1.Header := { name, value := value.toUTF8 }

private def policy : Security.Csp.Policy :=
  csp! "default-src 'self'; script-src 'self' 'nonce' 'strict-dynamic'; object-src 'none'; base-uri 'self'; frame-ancestors 'none'; connect-src 'self' https://api.example.test:443"

/-- FIPS SHA-256/RFC4231, hostile JWT, tenant/feature isolation and typed CSP tests. -/
def run : IO Unit := do
  check (hex (sha256 "".toUTF8) == "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855") "SHA256 empty"
  check (hex (sha256 "abc".toUTF8) == "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad") "SHA256 abc"
  check (hex (hmac256 (ByteArray.mk (Array.replicate 20 11)) "Hi There".toUTF8) ==
    "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7") "HMAC RFC4231 case1"
  for n in [:130] do
    let bytes := ByteArray.mk ((Array.range n).map (fun i => (i*17).toUInt8))
    check ((Security.Crypto.unbase64url (Security.Crypto.base64url bytes)).toOption == some bytes) "base64 canonical roundtrip"
  for bad in ["A", "Zh", "Zm9=", "Zm9v+", "Zm9v/", "Zm9v\n"] do
    check (!(Security.Crypto.unbase64url bad).isOk) "noncanonical base64 rejected"
  let key := ByteArray.mk (Array.replicate 32 42)
  let claims := Lean.Json.mkObj [("iss", Lean.toJson "issuer"), ("aud", Lean.toJson "app"),
    ("sub", Lean.toJson "alice"), ("iat", Lean.toJson (1000 : Nat)), ("exp", Lean.toJson (2000 : Nat)),
    ("tenant_id", Lean.toJson "alpha")]
  let jwtPolicy : Security.Jwt.Policy := { issuer := "issuer", audience := "app", clockSkewSeconds := 0 }
  let token ← require (Security.Jwt.sign key claims)
  let verified ← require (Security.Jwt.verify key jwtPolicy token 1500)
  check (verified.subject == "alice" && verified.string "tenant_id" == some "alpha") "JWT verified claims"
  check (!(Security.Jwt.verify key jwtPolicy token 2000).isOk) "JWT expiry boundary"
  check (!(Security.Jwt.verify key { jwtPolicy with audience := "other" } token 1500).isOk) "JWT audience enforced"
  check (!(Security.Jwt.verify (ByteArray.mk (Array.replicate 32 43)) jwtPolicy token 1500).isOk) "JWT wrong key rejected"
  let parts := token.splitOn "."
  let noneHeader := Security.Crypto.base64url "{\"alg\":\"none\"}".toUTF8
  check (!(Security.Jwt.verify key jwtPolicy (noneHeader ++ "." ++ parts[1]! ++ ".") 1500).isOk) "alg none rejected"
  let duplicateBody := Security.Crypto.base64url "{\"iss\":\"issuer\",\"\\u0069ss\":\"issuer\",\"aud\":\"app\",\"sub\":\"a\",\"iat\":1000,\"exp\":2000}".toUTF8
  let signed := parts[0]! ++ "." ++ duplicateBody
  let duplicateToken := signed ++ "." ++ Security.Crypto.base64url (Security.Crypto.hmac256 key signed.toUTF8)
  check (!(Security.Jwt.verify key jwtPolicy duplicateToken 1500).isOk) "escaped duplicate claims rejected before JSON normalization"
  let ctx ← baseContext
  let access : Security.Authorization.Policy := auth_policy! all [authenticated, any [role "admin", permission "report.read"], tenant "alpha"]
  let authorized := { ctx with tenant := "alpha", authenticated := true, attributes := #[("role", "admin")] }
  check (access.allows authorized) "compiled authorization grants matching trusted claims"
  check (!(access.allows { authorized with authenticated := false })) "role condition requires authenticated identity"
  check (!(access.allows { authorized with tenant := "beta" })) "policy tenant boundary"
  check (access.allows { authorized with attributes := #[("permission.report.read", "true")] }) "compiled permission alternative"
  let denied ← ((access.middleware fun _ => pure (Response.text "ok")) { authorized with tenant := "beta" }).run
  check (match denied with | .error e => e.status == 403 | _ => false) "compiled policy gates handler"
  let alpha ← tenant "alpha"
  let beta ← tenant "beta"
  let resolution ← require (← ((Tenant.fromVerifiedJwt (fun _ => pure verified)) ctx).run)
  check (resolution.map (·.id.value) == some "alpha") "tenant from verified JWT"
  let hostCtx := { ctx with host := "alpha.example.test:8080" }
  let hostResolution ← require (← (Tenant.fromSubdomain "example.test" hostCtx).run)
  check (hostResolution.map (·.id.value) == some "alpha") "subdomain tenant with port"
  let pathCtx := { ctx with path := #["tenants", "alpha", "dashboard"] }
  let pathResolution ← require (← (Tenant.fromPath #["tenants"] pathCtx).run)
  check (pathResolution.bind (·.remainingPath) == some #["dashboard"]) "tenant path prefix removed once"
  let conflict := { hostCtx with request := { ctx.request with headers := ctx.request.headers.push (hdr "x-tenant-id" "beta") } }
  check (!(← ((Tenant.agree #[Tenant.fromSubdomain "example.test", Tenant.fromHeader "x-tenant-id"]) conflict).run).isOk) "conflicting tenant identities fail closed"
  let configs ← Tenant.Store.new 2
  let _ ← require (← configs.set alpha { brandName := "<Alpha>", flags := #[("new-ui", true)], requestsPerSecond := 2, burst := 2 })
  let _ ← require (← configs.set beta { brandName := "Beta", flags := #[("new-ui", false)] })
  let echoBrand : Handler := fun c => pure (Html.toResponse (Tenant.brand c))
  let configured := Tenant.configured configs echoBrand
  let response ← require (← (configured { ctx with tenant := "alpha" }).run)
  check (String.fromUTF8! response.body == "<div class=\"tenant-brand\">&lt;Alpha&gt;</div>") "escaped dynamic branding"
  let _ ← require (← configs.set alpha { brandName := "Changed" })
  let reloaded ← require (← (configured { ctx with tenant := "alpha" }).run)
  check (String.fromUTF8! reloaded.body == "<div class=\"tenant-brand\">Changed</div>") "config changes affect existing pipeline without restart"
  let rate ← Tenant.RateLimiter.new 2
  check (← rate.check alpha 2 2 1000) "tenant rate first token"
  check (← rate.check alpha 2 2 1000) "tenant rate burst"
  check (!(← rate.check alpha 2 2 1000)) "tenant rate exhausted"
  check (← rate.check beta 2 2 1000) "tenant rate isolation"
  check (← rate.check alpha 2 2 1500) "tenant rate refill"
  let flags ← Feature.Store.new
  let flag : Feature.Flag := { name := "new-ui", enabled := true, percentage := 0 }
  let _ ← require (← flags.define flag)
  let echoFlag : Handler := fun c => pure (Response.text (toString (Feature.enabled c "new-ui")))
  let featurePipeline := Feature.middleware flags #["new-ui"] echoFlag
  let disabled ← require (← (featurePipeline ctx).run)
  check (String.fromUTF8! disabled.body == "false") "zero rollout"
  let _ ← require (← flags.define { flag with percentage := 10000 })
  let enabled ← require (← (featurePipeline ctx).run)
  check (String.fromUTF8! enabled.body == "true") "flag reload without restart"
  check (Feature.bucket flag "alice" == Feature.bucket flag "alice") "stable cohorts"
  check (!(Feature.evaluate { flag with enabled := false } { ctx with attributes := #[("tenant.flag.new-ui", "true")] }).enabled) "global kill switch wins over tenant override"
  let echoNonce : Handler := fun c => pure (Response.text ((Security.Csp.nonce c).getD ""))
  let csp := Security.Csp.middleware policy echoNonce
  let first ← require (← (csp ctx).run)
  let second ← require (← (csp ctx).run)
  check (first.body != second.body) "fresh response nonce"
  let nonce := String.fromUTF8! first.body
  check ((((first.getHeader "content-security-policy").getD "").splitOn ("'nonce-" ++ nonce ++ "'")).length == 2) "nonce bound to response header"
  check (!(Security.Csp.Policy.parse "default-src *; script-src 'unsafe-inline'").isOk) "unsafe policy rejected at construction"
  IO.println "platform crypto/JWT/tenant/feature/CSP checks passed"

end LeanWeave.Test.PlatformSecurity
