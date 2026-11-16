import LeanWeave.Core

namespace LeanWeave.Tenant

/-- Canonical bounded tenant key. Network claims must pass through a resolver and authorization. -/
structure Id where
  private mk ::
  value : String
  deriving Repr, BEq, Inhabited

def Id.parse (value : String) : Except String Id := do
  if value.isEmpty || value.utf8ByteSize > 64 || !value.toUTF8.data.all (fun b =>
      (b >= 97 && b <= 122) || (b >= 48 && b <= 57) || b == 45 || b == 95) then
    throw "tenant IDs must be 1..64 lowercase ASCII letters, digits, '-' or '_'"
  return ⟨value⟩

def Id.fromContext (ctx : Context) : Except AppError Id :=
  (Id.parse ctx.tenant).mapError (fun _ => { status := 403, message := "verified tenant context required" })

structure Resolution where
  id : Id
  /-- Optional tenant-path prefix removal, performed without decoding again. -/
  remainingPath : Option (Array String) := none
  source : String

abbrev Resolver := Context → AppM (Option Resolution)

private def parseResolution (name value : String) : AppM (Option Resolution) := do
  match Id.parse value with
  | .ok id => pure (some { id, source := name })
  | .error _ => throw { status := 400, message := "invalid tenant identifier" }

/-- Header extraction does not grant access; `middleware` always calls the authorization policy. -/
def fromHeader (name : String := "x-tenant-id") : Resolver := fun ctx => do
  let values := ctx.request.headers.filter (fun h => h.name.toLower == name.toLower)
  if values.isEmpty then return none
  if values.size != 1 then throw { status := 400, message := "ambiguous tenant header" }
  let some value := String.fromUTF8? values[0]!.value | throw { status := 400, message := "invalid tenant header" }
  parseResolution "header" value

/-- Select one subdomain label under a configured DNS base. Unrelated hosts do not match. -/
def fromSubdomain (baseDomain : String) : Resolver := fun ctx => do
  let base := baseDomain.toLower
  let authority := ctx.host.toLower
  if base.isEmpty || base.contains ':' || authority.startsWith "[" then return none
  let host := (authority.splitOn ":").head!
  let suffix := "." ++ base
  if !host.endsWith suffix then return none
  let label := (host.dropEnd suffix.length).toString
  if label.contains '.' then throw { status := 400, message := "one tenant subdomain required" }
  parseResolution "subdomain" label

/-- `/prefix/tenant/...`; the path is already decoded and the tenant prefix is removed once. -/
def fromPath (pathPrefix : Array String := #["tenants"]) : Resolver := fun ctx => do
  if ctx.path.size <= pathPrefix.size || ctx.path.extract 0 pathPrefix.size != pathPrefix then return none
  let value := ctx.path[pathPrefix.size]!
  let some result ← parseResolution "path" value | return none
  return some { result with remainingPath := some (ctx.path.extract (pathPrefix.size+1) ctx.path.size) }

/-- Multiple configured sources must agree; conflicting header/path/host identities fail closed. -/
def agree (sources : Array Resolver) : Resolver := fun ctx => do
  let mut result : Option Resolution := none
  for source in sources do
    if let some found ← source ctx then
      if let some previous := result then
        if previous.id != found.id then throw { status := 403, message := "conflicting tenant identities" }
        if previous.remainingPath.isSome && found.remainingPath.isSome && previous.remainingPath != found.remainingPath then
          throw { status := 400, message := "conflicting tenant path scopes" }
        result := some { found with remainingPath := found.remainingPath.or previous.remainingPath }
      else result := some found
  return result

/-- Explicit access policy: extracted tenant names never authorize themselves. -/
def middleware (resolve : Resolver) (authorize : Context → Id → AppM Bool) : Middleware := fun next ctx => do
  let some resolution ← resolve ctx | throw { status := 404, message := "tenant not found" }
  unless ← authorize ctx resolution.id do throw { status := 403, message := "tenant access denied" }
  let attributes := (ctx.attributes.filter (fun p => p.1 != "tenant.source")).push ("tenant.source", resolution.source)
  next { ctx with tenant := resolution.id.value, path := resolution.remainingPath.getD ctx.path, attributes }

end LeanWeave.Tenant
