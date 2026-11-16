import LeanWeave.Tenant.Basic
import LeanWeave.Security.Jwt

namespace LeanWeave.Tenant

/-- JWT tenant claims are read only after caller-supplied signature/claims verification. -/
def fromVerifiedJwt (verify : Context → AppM Security.Jwt.VerifiedClaims)
    (claim : String := "tenant_id") : Resolver := fun ctx => do
  let claims ← verify ctx
  let some value := claims.string claim | return none
  let id ← match Id.parse value with
    | .ok id => pure id
    | .error _ => throw { status := 401, message := "invalid verified tenant claim" }
  return some { id, source := "jwt" }

/-- Bound verification to a configured HS256 issuer/audience. Rotation can be
    implemented by the key resolver without changing this pipeline. -/
def fromJwt (key : Context → AppM ByteArray) (policy : Security.Jwt.Policy)
    (nowSeconds : IO Nat) (claim : String := "tenant_id") : Resolver :=
  fromVerifiedJwt (fun ctx => do
    let token ← match Security.Jwt.bearer ctx with | .ok token => pure token | .error error => throw error
    let secret ← key ctx
    let now ← nowSeconds
    match Security.Jwt.verify secret policy token now with
    | .ok claims => pure claims
    | .error _ => throw { status := 401, message := "invalid bearer token" }) claim

end LeanWeave.Tenant
