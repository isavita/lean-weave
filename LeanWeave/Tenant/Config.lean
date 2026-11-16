import LeanWeave.Tenant.Basic
import LeanWeave.Security.Audit
import Std.Sync.Mutex

namespace LeanWeave.Tenant

structure Config where
  /-- Overrides for named feature flags. -/
  flags : Array (String × Bool) := #[]
  requestsPerSecond : Nat := 100
  burst : Nat := 100
  cacheEnabled : Bool := true
  cacheTtlMs : Nat := 10000
  brandName : String := "LeanWeave"
  brandColor : String := "#334155"
  revision : Nat := 1
  deriving Repr, BEq, Lean.ToJson

def Config.validate (config : Config) : Except String Unit := do
  if config.revision > 9223372036854775807 then throw "tenant revision exceeds UInt63"
  if (Lean.toJson config).compress.utf8ByteSize > 4096 then throw "tenant configuration snapshot byte limit exceeded"
  if config.flags.size > 128 || config.flags.any (fun p => p.1.isEmpty || p.1.utf8ByteSize > 128) then throw "invalid tenant feature overrides"
  if config.flags.size != (config.flags.toList.map Prod.fst).eraseDups.length then throw "duplicate tenant feature override"
  if config.requestsPerSecond == 0 || config.burst == 0 || config.requestsPerSecond > 1000000 || config.burst > 1000000 then
    throw "invalid tenant rate limits"
  if config.cacheTtlMs > 86400000 || config.brandName.utf8ByteSize > 256 then throw "invalid tenant configuration bounds"
  if config.brandColor.utf8ByteSize != 7 || !config.brandColor.startsWith "#" ||
      !(config.brandColor.drop 1).toString.toUTF8.data.all (fun b =>
        (b >= 48 && b <= 57) || (b >= 65 && b <= 70) || (b >= 97 && b <= 102)) then
    throw "branding color must be #RRGGBB"

structure Entry where
  tenant : Id
  config : Config
  fingerprint : String

private structure State where
  entries : Array Entry := #[]
  records : Array Security.Audit.Record := #[]
  checkpoint : Security.Audit.Checkpoint := {}
  auditBytes : Nat := 0
  revisionWatermark : Nat := 0

/-- Shared configuration snapshots are replaced atomically; request callbacks use try-locks. -/
structure Store where
  private state : Std.Mutex State
  capacity : Nat
  auditCapacity : Nat
  auditMaxBytes : Nat

def Store.new (capacity : Nat := 1024) (auditCapacity : Nat := 4096)
    (auditMaxBytes : Nat := 16 * 1024 * 1024) : IO Store := do
  if capacity == 0 || capacity > 100000 || auditCapacity == 0 || auditCapacity > 65536 ||
      auditMaxBytes == 0 || auditMaxBytes > 1024 * 1024 * 1024 then
    throw (IO.userError "invalid tenant configuration/audit capacity")
  return { state := ← Std.Mutex.new {}, capacity, auditCapacity, auditMaxBytes }

private def Store.getEntry (store : Store) (id : Id) : IO (Except AppError (Option Entry)) := do
  let result ← store.state.tryAtomically do
    let state ← getThe State
    return state.entries.find? (fun e => e.tenant == id)
  return match result with
    | some value => .ok value
    | none => .error { status := 503, message := "tenant configuration busy" }

def Store.get (store : Store) (id : Id) : IO (Except AppError (Option Config)) := do
  return (← store.getEntry id).map (Option.map (·.config))

/-- Commit a bounded snapshot and audit record together. The shared revision
watermark survives removals and acknowledgement, preventing generation reuse. -/
def Store.set (store : Store) (id : Id) (config : Config) (actor : String := "system") : IO (Except AppError Unit) := do
  if let .error _ := config.validate then return .error { status := 400, message := "invalid tenant configuration" }
  let timeUnixNano := (← Std.Time.Timestamp.now).toNanosecondsSinceUnixEpoch.val.toNat
  let result ← store.state.tryAtomically do
    let state ← getThe State
    let entries := state.entries
    if state.records.size >= store.auditCapacity then return .error { status := 503, message := "tenant audit ledger full" }
    let old := entries.find? (fun e => e.tenant == id)
    if old.isNone && entries.size >= store.capacity then
      return .error { status := 503, message := "tenant configuration capacity reached" }
    if state.revisionWatermark >= 9223372036854775807 then
      return .error { status := 503, message := "tenant revision generation exhausted" }
    let revision := max config.revision (state.revisionWatermark + 1)
    let config := { config with revision }
    if let .error _ := config.validate then return .error { status := 400, message := "tenant configuration snapshot too large" }
    let event : Security.Audit.Event := {
      action := "tenant.config.set", actor, tenant := id.value, target := id.value, timeUnixNano
      changes := #[("before", (Lean.toJson (old.map (·.config))).compress), ("after", (Lean.toJson config).compress)] }
    match Security.Audit.append state.checkpoint event with
    | .error _ => return .error { status := 400, message := "tenant audit metadata invalid" }
    | .ok record =>
      let bytes := record.line.utf8ByteSize
      if bytes > store.auditMaxBytes - state.auditBytes then return .error { status := 503, message := "tenant audit byte budget exhausted" }
      MonadStateOf.set { state with
        entries := (entries.filter (fun (e : Entry) => e.tenant != id)).push {
          tenant := id, config, fingerprint := Security.Crypto.hex (Security.Crypto.sha256 (Lean.toJson config).compress.toUTF8) }
        records := state.records.push record
        checkpoint := record.checkpoint
        auditBytes := state.auditBytes + bytes
        revisionWatermark := revision }
      return .ok ()
  return result.getD (.error { status := 503, message := "tenant configuration busy" })

/-- Removal, including an already-absent ID, records full before/after metadata.
Audit admission failure leaves both the snapshot and revision watermark intact. -/
def Store.remove (store : Store) (id : Id) (actor : String := "system") : IO (Except AppError Unit) := do
  let timeUnixNano := (← Std.Time.Timestamp.now).toNanosecondsSinceUnixEpoch.val.toNat
  let result ← store.state.tryAtomically do
    let state ← getThe State
    if state.records.size >= store.auditCapacity then return .error { status := 503, message := "tenant audit ledger full" }
    let old := (state.entries.find? (fun e => e.tenant == id)).map (·.config)
    let event : Security.Audit.Event := {
      action := "tenant.config.remove", actor, tenant := id.value, target := id.value, timeUnixNano
      changes := #[("before", (Lean.toJson old).compress), ("after", "null")] }
    match Security.Audit.append state.checkpoint event with
    | .error _ => return .error { status := 400, message := "tenant audit metadata invalid" }
    | .ok record =>
      let bytes := record.line.utf8ByteSize
      if bytes > store.auditMaxBytes - state.auditBytes then return .error { status := 503, message := "tenant audit byte budget exhausted" }
      MonadStateOf.set { state with
        entries := state.entries.filter (fun e => e.tenant != id)
        records := state.records.push record
        checkpoint := record.checkpoint
        auditBytes := state.auditBytes + bytes }
      return .ok ()
  return result.getD (.error { status := 503, message := "tenant configuration busy" })

/-- Immutable pending mutation history; snapshots are bounded before admission. -/
def Store.audit (store : Store) : IO (Except AppError (Array Security.Audit.Record)) := do
  let result ← store.state.tryAtomically (do return (← getThe State).records)
  return match result with | some records => .ok records | none => .error { status := 503, message := "tenant audit busy" }

/-- Release a matching prefix only after external persistence and checkpoint anchoring. -/
def Store.acknowledgeAudit (store : Store) (checkpoint : Security.Audit.Checkpoint) : IO (Except AppError Unit) := do
  let result ← store.state.tryAtomically do
    let state ← getThe State
    unless state.records.any (fun record => record.checkpoint == checkpoint) do
      return .error { status := 409, message := "tenant audit checkpoint is not pending" }
    let records := state.records.filter (fun record => record.sequence > checkpoint.sequence)
    let bytes := records.foldl (fun (bytes : Nat) (record : Security.Audit.Record) => bytes + record.line.utf8ByteSize) 0
    MonadStateOf.set { state with records, auditBytes := bytes }
    return .ok ()
  return result.getD (.error { status := 503, message := "tenant audit busy" })

private def replace (attributes : Array (String × String)) (name value : String) : Array (String × String) :=
  (attributes.filter (fun p => p.1 != name)).push (name,value)

/-- Thread a bounded immutable snapshot through Context for feature, cache and UI consumers. -/
def configured (store : Store) : Middleware := fun next ctx => do
  let tenant ← match Id.fromContext ctx with | .ok id => pure id | .error error => throw error
  let entry ← match ← store.getEntry tenant with
    | .ok (some entry) => pure entry
    | .ok none => throw { status := 404, message := "tenant configuration missing" }
    | .error error => throw error
  let config := entry.config
  let mut attributes := ctx.attributes
  for (key,value) in #[
    ("tenant.config.revision", toString config.revision), ("tenant.config.variant", entry.fingerprint),
    ("tenant.rate.perSecond", toString config.requestsPerSecond), ("tenant.rate.burst", toString config.burst),
    ("tenant.cache.enabled", toString config.cacheEnabled), ("tenant.cache.ttlMs", toString config.cacheTtlMs),
    ("tenant.brand.name", config.brandName), ("tenant.brand.color", config.brandColor)] do
    attributes := replace attributes key value
  attributes := attributes.filter (fun p => !p.1.startsWith "tenant.flag.")
  for (name,value) in config.flags do attributes := attributes.push ("tenant.flag." ++ name, toString value)
  next { ctx with attributes }

structure Bucket where
  tenant : String
  tokens : Nat
  lastMs : Nat
  remainder : Nat := 0

/-- Per-worker token buckets; tenant snapshots can change rates without rebuilding pipelines. -/
structure RateLimiter where
  private buckets : IO.Ref (Array Bucket)
  capacity : Nat

def RateLimiter.new (capacity : Nat := 1024) : IO RateLimiter := do
  if capacity == 0 then throw (IO.userError "tenant rate capacity must be positive")
  return { buckets := ← IO.mkRef #[], capacity }

def RateLimiter.check (limiter : RateLimiter) (tenant : Id) (rate burst nowMs : Nat) : IO Bool := do
  if rate == 0 || burst == 0 then return false
  let buckets ← limiter.buckets.get
  let old := buckets.find? (fun b => b.tenant == tenant.value)
  if old.isNone && buckets.size >= limiter.capacity then return false
  let old := old.getD { tenant := tenant.value, tokens := burst, lastMs := nowMs }
  let credit := (nowMs - old.lastMs) * rate + old.remainder
  let tokens := min burst (old.tokens + credit / 1000)
  let remainder := if tokens == burst then 0 else credit % 1000
  let next := { old with tokens := tokens - 1, remainder, lastMs := nowMs }
  limiter.buckets.set ((buckets.filter (fun b => b.tenant != tenant.value)).push next)
  return tokens > 0

def RateLimiter.middleware (limiter : RateLimiter) : Middleware := fun next ctx => do
  let tenant ← match Id.fromContext ctx with | .ok id => pure id | .error error => throw error
  let rate := ((ctx.attribute "tenant.rate.perSecond").bind String.toNat?).getD 100
  let burst := ((ctx.attribute "tenant.rate.burst").bind String.toNat?).getD rate
  unless ← limiter.check tenant rate burst (← IO.monoMsNow) do
    return (Response.text "tenant request rate exceeded" 429).withHeader "retry-after" "1"
  next ctx

end LeanWeave.Tenant
