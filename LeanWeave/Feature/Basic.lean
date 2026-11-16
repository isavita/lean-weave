import LeanWeave.Security.Crypto
import LeanWeave.Security.Audit
import LeanWeave.Tenant.Basic
import Std.Sync.Mutex

namespace LeanWeave.Feature

structure Flag where
  name : String
  enabled : Bool := false
  /-- 0..10000 basis points; changing percentage keeps cohorts stable. -/
  percentage : Nat := 10000
  salt : String := "v1"
  tenants : Array String := #[]
  principals : Array String := #[]
  requiredRole : Option String := none
  version : Nat := 1
  deriving Repr, BEq, Lean.ToJson

def Flag.validate (flag : Flag) : Except String Unit := do
  if flag.name.isEmpty || flag.name.utf8ByteSize > 128 || !flag.name.toUTF8.data.all (fun b =>
      (b >= 97 && b <= 122) || (b >= 48 && b <= 57) || [45,46,95].contains b.toNat) then
    throw "invalid feature flag name"
  if (Lean.toJson flag).compress.utf8ByteSize > 4096 then throw "feature definition byte limit exceeded"
  if flag.percentage > 10000 || flag.salt.utf8ByteSize > 128 || flag.tenants.size > 256 || flag.principals.size > 256 then
    throw "feature flag bounds exceeded"
  if flag.tenants.any (fun t => !(Tenant.Id.parse t).isOk) || flag.principals.any (fun p => p.isEmpty || p.utf8ByteSize > 256) then
    throw "invalid flag audience"
  if flag.requiredRole.any (fun r => r.isEmpty || r.utf8ByteSize > 128) then throw "invalid role condition"

structure Decision where
  enabled : Bool
  reason : String
  version : Nat
  bucket : Nat := 0
  deriving Repr, BEq

private structure State where
  flags : Array Flag := #[]
  records : Array Security.Audit.Record := #[]
  checkpoint : Security.Audit.Checkpoint := {}
  auditBytes : Nat := 0

structure Store where
  private state : Std.Mutex State
  capacity : Nat
  auditCapacity : Nat
  auditMaxBytes : Nat

def Store.new (capacity : Nat := 512) (auditCapacity : Nat := 4096) (auditMaxBytes : Nat := 16 * 1024 * 1024) : IO Store := do
  if capacity == 0 || capacity > 4096 || auditCapacity == 0 || auditCapacity > 65536 || auditMaxBytes == 0 || auditMaxBytes > 1024 * 1024 * 1024 then throw (IO.userError "invalid feature store capacity")
  return { state := ← Std.Mutex.new {}, capacity, auditCapacity, auditMaxBytes }

/-- Define/reload and audit atomically. A full audit ledger rejects the mutation. -/
def Store.define (store : Store) (flag : Flag) (actor : String := "system") : IO (Except AppError Unit) := do
  let timeUnixNano := (← Std.Time.Timestamp.now).toNanosecondsSinceUnixEpoch.val.toNat
  if let .error _ := flag.validate then return .error { status := 400, message := "invalid feature definition" }
  let result ← store.state.tryAtomically do
    let state ← getThe State
    let flags := state.flags
    if state.records.size >= store.auditCapacity then return .error { status := 503, message := "feature audit ledger full" }
    let previous := flags.find? (·.name == flag.name)
    if previous.isNone && flags.size >= store.capacity then return .error { status := 503, message := "feature capacity reached" }
    let version := max flag.version ((previous.map (·.version)).getD 0 + 1)
    let flag := { flag with version }
    let event : Security.Audit.Event := {
      action := "feature.define", actor, target := flag.name, timeUnixNano
      changes := #[("before", (Lean.toJson previous).compress), ("after", (Lean.toJson flag).compress)] }
    match Security.Audit.append state.checkpoint event with
    | .error _ => return .error { status := 400, message := "feature audit metadata invalid" }
    | .ok record =>
      let bytes := record.line.utf8ByteSize
      if bytes > store.auditMaxBytes - state.auditBytes then return .error { status := 503, message := "feature audit byte budget exhausted" }
      MonadStateOf.set ({
        flags := (flags.filter (fun f => f.name != flag.name)).push flag
        records := state.records.push record, checkpoint := record.checkpoint, auditBytes := state.auditBytes + bytes } : State)
      return .ok ()
  return result.getD (.error { status := 503, message := "feature configuration busy" })

def Store.snapshot (store : Store) : IO (Except AppError (Array Flag)) := do
  let result ← store.state.tryAtomically (do return (← getThe State).flags)
  return match result with
    | some flags => .ok flags
    | none => .error { status := 503, message := "feature configuration busy" }

/-- Pending immutable mutation history, including actor, time and full bounded snapshots. -/
def Store.audit (store : Store) : IO (Except AppError (Array Security.Audit.Record)) := do
  let result ← store.state.tryAtomically (do return (← getThe State).records)
  return match result with | some records => .ok records | none => .error { status := 503, message := "feature audit busy" }

/-- Acknowledge only after durable external persistence/checkpoint anchoring. -/
def Store.acknowledgeAudit (store : Store) (checkpoint : Security.Audit.Checkpoint) : IO (Except AppError Unit) := do
  let result ← store.state.tryAtomically do
    let state ← getThe State
    if !state.records.any (fun r => r.checkpoint == checkpoint) then
      return .error { status := 409, message := "feature audit checkpoint is not pending" }
    let records := state.records.filter (fun r => r.sequence > checkpoint.sequence)
    set { state with records, auditBytes := records.foldl (fun (n : Nat) (r : Security.Audit.Record) => n + r.line.utf8ByteSize) 0 }
    return .ok ()
  return result.getD (.error { status := 503, message := "feature audit busy" })

/-- Stable identity must originate from authenticated middleware or an application-
    verified subject. Anonymous callers share a tenant cohort unless a subject is set. -/
def subject (ctx : Context) : String :=
  if ctx.authenticated then (ctx.attribute "principal").getD ("tenant:" ++ ctx.tenant)
  else (ctx.attribute "feature.subject").getD (ctx.sessionId.getD ("tenant:" ++ ctx.tenant ++ ":anonymous"))

def bucket (flag : Flag) (identity : String) : Nat :=
  let digest := Security.Crypto.sha256 (s!"{flag.name.utf8ByteSize}:{flag.name}{flag.salt.utf8ByteSize}:{flag.salt}{identity.utf8ByteSize}:{identity}").toUTF8
  (digest.data.foldl (fun (value : UInt64) byte => value * 256 + byte.toUInt64) 0).toNat % 10000

/-- Global kill switch wins, then role/audience conditions, per-tenant overrides,
    then stable percentage rollout. All decisions carry a version for cache isolation. -/
def evaluate (flag : Flag) (ctx : Context) : Decision := Id.run do
  if !flag.enabled then return { enabled := false, reason := "disabled", version := flag.version }
  if flag.requiredRole.any (fun role => !ctx.authenticated || ctx.attribute "role" != some role) then
    return { enabled := false, reason := "role", version := flag.version }
  if !flag.tenants.isEmpty && !flag.tenants.contains ctx.tenant then
    return { enabled := false, reason := "tenant", version := flag.version }
  let identity := subject ctx
  if !flag.principals.isEmpty && (!ctx.authenticated || !flag.principals.contains identity) then
    return { enabled := false, reason := "principal", version := flag.version }
  if let some value := ctx.attribute ("tenant.flag." ++ flag.name) then
    return { enabled := value == "true", reason := "tenant-override", version := flag.version }
  let sample := bucket flag identity
  return { enabled := sample < flag.percentage, reason := "rollout", version := flag.version, bucket := sample }

def enabled (ctx : Context) (name : String) : Bool := ctx.attribute ("feature." ++ name) == some "true"

/-- Compose once; each request sees one atomic flag snapshot. Variant/version digest
    is available to cache consumers and structured logs, never a raw subject. -/
def middleware (store : Store) (names : Array String) : Middleware := fun next ctx => do
  if names.size > 128 then throw { message := "feature pipeline name limit exceeded" }
  let flags ← match ← store.snapshot with | .ok flags => pure flags | .error error => throw error
  let mut attributes := ctx.attributes.filter (fun p => !p.1.startsWith "feature." || p.1 == "feature.subject")
  let mut variants := ""
  for name in names do
    let decision := match flags.find? (·.name == name) with
      | some flag => evaluate flag ctx
      | none => { enabled := false, reason := "missing", version := 0 }
    attributes := attributes.push ("feature." ++ name, toString decision.enabled)
    variants := variants ++ s!"{name.utf8ByteSize}:{name}:{decision.version}:{decision.enabled};"
  attributes := attributes.push ("feature.variant", Security.Crypto.hex (Security.Crypto.sha256 variants.toUTF8))
  next { ctx with attributes }

end LeanWeave.Feature
