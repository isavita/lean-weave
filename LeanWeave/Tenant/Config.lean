import LeanWeave.Tenant.Basic
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
  deriving Repr, BEq

def Config.validate (config : Config) : Except String Unit := do
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

/-- Shared configuration snapshots are replaced atomically; request callbacks use try-locks. -/
structure Store where
  private state : Std.Mutex (Array Entry)
  capacity : Nat

def Store.new (capacity : Nat := 1024) : IO Store := do
  if capacity == 0 || capacity > 100000 then throw (IO.userError "invalid tenant configuration capacity")
  return { state := ← Std.Mutex.new #[], capacity }

def Store.get (store : Store) (id : Id) : IO (Except AppError (Option Config)) := do
  let result ← store.state.tryAtomically do
    let entries ← getThe (Array Entry)
    return (entries.find? (fun e => e.tenant == id)).map (·.config)
  return match result with
    | some value => .ok value
    | none => .error { status := 503, message := "tenant configuration busy" }

/-- A config reload increments its generation even when a caller reuses an old revision. -/
def Store.set (store : Store) (id : Id) (config : Config) : IO (Except AppError Unit) := do
  if let .error _ := config.validate then return .error { status := 400, message := "invalid tenant configuration" }
  let result ← store.state.tryAtomically do
    let entries ← getThe (Array Entry)
    let old := entries.find? (fun e => e.tenant == id)
    if old.isNone && entries.size >= store.capacity then
      return .error { status := 503, message := "tenant configuration capacity reached" }
    let revision := max config.revision ((old.map (·.config.revision)).getD 0 + 1)
    MonadStateOf.set ((entries.filter (fun e => e.tenant != id)).push { tenant := id, config := { config with revision } })
    return .ok ()
  return result.getD (.error { status := 503, message := "tenant configuration busy" })

def Store.remove (store : Store) (id : Id) : IO (Except AppError Unit) := do
  let result ← store.state.tryAtomically do modify (fun entries => entries.filter (fun e => e.tenant != id))
  return if result.isSome then .ok () else .error { status := 503, message := "tenant configuration busy" }

private def replace (attributes : Array (String × String)) (name value : String) : Array (String × String) :=
  (attributes.filter (fun p => p.1 != name)).push (name,value)

/-- Thread a bounded immutable snapshot through Context for feature, cache and UI consumers. -/
def configured (store : Store) : Middleware := fun next ctx => do
  let tenant ← match Id.fromContext ctx with | .ok id => pure id | .error error => throw error
  let config ← match ← store.get tenant with
    | .ok (some config) => pure config
    | .ok none => throw { status := 404, message := "tenant configuration missing" }
    | .error error => throw error
  let mut attributes := ctx.attributes
  for (key,value) in #[
    ("tenant.config.revision", toString config.revision),
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
