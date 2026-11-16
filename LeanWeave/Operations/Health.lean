import LeanWeave.Core
import LeanNet.AtomicFlag
import Std.Sync.Mutex

namespace LeanWeave.Operations

structure Dependency where
  name : String
  ready : Bool := false
  required : Bool := true
  deriving Repr, BEq

structure HealthSnapshot where
  initialized : Bool := false
  draining : Bool := false
  dependencies : Array Dependency := #[]
  deriving Repr, BEq

/-- Shared process state; checks consume snapshots and never run blocking probes
on a request worker. Dependency owners publish successful/failed connectivity. -/
structure Health where
  private state : Std.Mutex HealthSnapshot
  private started : LeanNet.AtomicFlag
  private draining : LeanNet.AtomicFlag

def Health.create (dependencies : Array Dependency := #[]) : IO Health := do
  if dependencies.size > 128 || dependencies.any (fun d => d.name.isEmpty || d.name.utf8ByteSize > 128) ||
      dependencies.size != (dependencies.toList.map (·.name)).eraseDups.length then
    throw (IO.userError "invalid health dependencies")
  return {
    state := ← Std.Mutex.new { dependencies }
    started := ← LeanNet.AtomicFlag.new
    draining := ← LeanNet.AtomicFlag.new }

/-- Publish monotone initialization with one lock-free store. Dependency
publication may contend; completed startup never becomes unavailable. -/
def Health.initialized (health : Health) : IO Bool := do
  health.started.set
  return true

/-- Once draining starts it cannot be undone, even while a dependency owner
holds the snapshot mutex. -/
def Health.beginDrain (health : Health) : IO Bool := do
  health.draining.set
  return true

def Health.dependency (health : Health) (name : String) (ready : Bool) : IO Bool := do
  let result ← health.state.tryAtomically do
    let state ← getThe HealthSnapshot
    if !state.dependencies.any (·.name == name) then return false
    MonadStateOf.set ({ state with dependencies := state.dependencies.map fun d => if d.name == name then { d with ready } else d } : HealthSnapshot)
    return true
  return result.getD false

def Health.snapshot (health : Health) : IO (Option HealthSnapshot) := do
  let snapshot ← health.state.tryAtomically get
  let initialized ← health.started.isSet
  let draining ← health.draining.isSet
  return snapshot.map fun state => { state with initialized, draining }

def HealthSnapshot.ready (state : HealthSnapshot) : Bool :=
  state.initialized && !state.draining && state.dependencies.all (fun d => !d.required || d.ready)

/-- Mount outside authentication; only bounded names and booleans are exposed.
Liveness stays healthy during startup/draining, readiness does not. -/
def Health.middleware (health : Health) : Middleware := fun next ctx => do
  let probe := if ctx.path == #["health", "live"] then some "live"
    else if ctx.path == #["health", "ready"] then some "ready"
    else if ctx.path == #["health", "startup"] then some "startup" else none
  let some probe := probe | return ← next ctx
  if ctx.request.method != "GET" && ctx.request.method != "HEAD" then
    return (Response.text "Method Not Allowed" 405).withHeader "allow" "GET, HEAD"
  let ok ← (do
    if probe == "live" then return true
    if probe == "startup" then return ← health.started.isSet
    let snapshot ← health.snapshot
    return snapshot.any (·.ready) : IO Bool)
  let response := (Response.json (Lean.Json.mkObj [("probe", Lean.toJson probe), ("ok", Lean.toJson ok)])
    (if ok then 200 else 503)).withHeader "cache-control" "no-store"
  -- Native framing suppresses HEAD bytes while retaining GET Content-Length.
  return response

end LeanWeave.Operations
