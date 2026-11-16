import LeanWeave.Core.Server

namespace LeanWeave.Plugin

/-- Construct on one application owner. Hooks must be bounded and cooperative;
the optional native adapter supplies a versioned, unloadable C ABI. -/
structure Definition where
  name : String
  version : String
  onStart : IO Unit := pure ()
  onRequest : Handler
  onShutdown : IO Unit := pure ()

structure Limits where
  maxGenerations : Nat := 4
  maxInFlight : Nat := 1024
  deriving Repr

private structure Generation where
  serial : Nat
  definition : Definition
  inFlight : Nat := 0
  retired : Bool := false

private structure State where
  generations : Array Generation := #[]
  active : Option Nat := none
  nextSerial : Nat := 1
  draining : Bool := false
  loading : Bool := false
  shutdownErrors : Nat := 0

structure Snapshot where
  generations : Nat
  retired : Nat
  inFlight : Nat
  activeVersion : Option String
  draining : Bool
  loading : Bool
  shutdownErrors : Nat
  deriving Repr, BEq

/-- Owner-local state. Never share this manager, its callbacks or native library
handles with job tasks or other reactor workers. Use one factory per worker. -/
structure Manager where
  private state : IO.Ref State
  limits : Limits

def Manager.create (limits : Limits := {}) : IO Manager := do
  if limits.maxGenerations < 2 || limits.maxGenerations > 32 ||
      limits.maxInFlight == 0 || limits.maxInFlight > 100000 then
    throw (IO.userError "invalid plugin generation/request bounds")
  return ⟨← IO.mkRef {},limits⟩

/-- Remove retired idle generations before invoking finalizers. A throwing
finalizer is counted and cannot make another generation disappear or run twice. -/
def Manager.collect (manager : Manager) : IO Unit := do
  let state ← manager.state.get
  let idle := state.generations.filter (fun g => g.retired && g.inFlight == 0)
  manager.state.set { state with generations := state.generations.filter (fun g => !g.retired || g.inFlight != 0) }
  for g in idle do
    try g.definition.onShutdown
    catch _ => manager.state.modify fun s => { s with shutdownErrors := s.shutdownErrors + 1 }

def Manager.snapshot (manager : Manager) : IO Snapshot := do
  let s ← manager.state.get
  return {
    generations := s.generations.size,retired := (s.generations.filter (·.retired)).size
    inFlight := s.generations.foldl (fun n g => n + g.inFlight) 0
    activeVersion := (s.generations.find? (fun g => some g.serial == s.active)).map (·.definition.version)
    draining := s.draining,loading := s.loading,shutdownErrors := s.shutdownErrors }

/-- The capacity check precedes factory execution/dlopen. A failed factory or
start leaves the current generation serving; a successful candidate is swapped
only after onStart returns. Distinct active version names prevent stale reloads. -/
def Manager.reload (manager : Manager) (factory : IO Definition) : IO (Except AppError Unit) := do
  manager.collect
  let state ← manager.state.get
  if state.draining || state.loading then return .error { status := 503,message := "plugin reload unavailable" }
  if state.generations.size >= manager.limits.maxGenerations then
    return .error { status := 503,message := "plugin retained generation limit reached" }
  manager.state.modify fun s => { s with loading := true }
  try
    let candidate ← factory
    let valid := !candidate.name.isEmpty && candidate.name.utf8ByteSize <= 128 &&
      !candidate.version.isEmpty && candidate.version.utf8ByteSize <= 128 &&
      !((← manager.state.get).generations.any (fun g => g.definition.name == candidate.name && g.definition.version == candidate.version))
    unless valid do
      try candidate.onShutdown catch _ => pure ()
      return .error { status := 422,message := "invalid or retained plugin identity" }
    try candidate.onStart
    catch _ =>
      try candidate.onShutdown catch _ => pure ()
      return .error { status := 503,message := "plugin start failed" }
    let state ← manager.state.get
    if state.draining then
      try candidate.onShutdown catch _ => pure ()
      return .error { status := 503,message := "plugin admission stopped" }
    let serial := state.nextSerial
    manager.state.set { state with
      generations := (state.generations.map fun g => { g with retired := true }).push { serial,definition := candidate }
      active := some serial,nextSerial := serial + 1 }
    manager.collect
    return .ok ()
  catch _ => return .error { status := 503,message := "plugin load failed" }
  finally manager.state.modify fun s => { s with loading := false }

private def release (manager : Manager) (serial : Nat) (released : IO.Ref Bool) : IO Unit := do
  unless ← released.get do
    released.set true
    manager.state.modify fun state => { state with generations := state.generations.map fun g =>
      if g.serial == serial then { g with inFlight := g.inFlight - 1 } else g }
    manager.collect

/-- A request owns its generation through every deferred continuation and through
stream.close (including disconnect/HEAD/cancellation), not merely header creation. -/
def Manager.handler (manager : Manager) : Handler := fun context => do
  let state ← manager.state.get
  if state.draining then throw { status := 503,message := "plugins draining" }
  let some generation := state.generations.find? (fun g => some g.serial == state.active && !g.retired)
    | throw { status := 503,message := "no active plugin" }
  if state.generations.foldl (fun n g => n + g.inFlight) 0 >= manager.limits.maxInFlight then
    throw { status := 503,message := "plugin request limit reached" }
  manager.state.modify fun state => { state with generations := state.generations.map fun g =>
    if g.serial == generation.serial then { g with inFlight := g.inFlight + 1 } else g }
  let released ← IO.mkRef false
  let transferred ← IO.mkRef false
  let action : AppM Response := do
    let response ← generation.definition.onRequest context
    if let some body := response.stream then
      let body := { body with close := fun reason => do
        try body.close reason finally release manager generation.serial released }
      transferred.set true
      return { response with stream := some body }
    return response
  action.ensuring (do unless ← transferred.get do release manager generation.serial released)

/-- Unload the active generation after its last request/stream closes. Future
requests get 503 until reload supplies a replacement. -/
def Manager.unload (manager : Manager) : IO Unit := do
  manager.state.modify fun state => { state with active := none,generations := state.generations.map fun g => { g with retired := true } }
  manager.collect

def Manager.beginDrain (manager : Manager) : IO Unit := do
  manager.state.modify fun state => { state with draining := true }
  manager.unload

def Manager.isDrained (manager : Manager) : IO Bool := do return (← manager.snapshot).inFlight == 0

/-- Finalization is nonblocking and never unloads code still leased by a request.
False requires the owner to finish/cancel outstanding exchanges before disposal. -/
def Manager.close (manager : Manager) : IO Bool := do
  manager.beginDrain
  return (← manager.snapshot).generations == 0

def Manager.application (manager : Manager) (app : Server.Application) : Server.Application :=
  { app with
    onTick := fun now => do app.onTick now; manager.collect
    onDrain := fun deadline => do manager.beginDrain; app.onDrain deadline
    isDrained := do return (← manager.isDrained) && (← app.isDrained)
    onShutdown := do let _ ← manager.close; app.onShutdown }

end LeanWeave.Plugin
