import LeanWeave.Core.Server
import LeanWeave.Operations.Health

namespace LeanWeave.Operations

/-- Built once on the resource's owner. Callbacks never wait on I/O or join a
thread: poll advances cooperative work and close cancels/relinquishes ownership.
The reactor supplies one absolute deadline to every participant. -/
structure Participant where
  name : String
  onTick : Nat → IO Unit := fun _ => pure ()
  onDrain : Nat → IO Unit := fun _ => pure ()
  isDrained : IO Bool := pure true
  close : IO Unit := pure ()

private structure DrainState where
  deadline : Option Nat := none
  failed : Bool := false
  closed : Bool := false

structure RuntimeStats where
  draining : Bool
  deadlineMs : Option Nat
  failed : Bool
  closed : Bool
  deriving Repr, BEq

structure Runtime where
  private participants : Array Participant
  private state : IO.Ref DrainState
  health : Health

def Runtime.create (health : Health) (participants : Array Participant := #[]) : IO Runtime := do
  if participants.size > 128 || participants.any (fun p => p.name.isEmpty || p.name.utf8ByteSize > 128) ||
      participants.size != (participants.toList.map (·.name)).eraseDups.length then
    throw (IO.userError "invalid runtime participants")
  return { participants, state := ← IO.mkRef {}, health }

private def Runtime.attempt (runtime : Runtime) (action : IO Unit) : IO Unit := do
  try action catch _ => runtime.state.modify fun s => { s with failed := true }

def Runtime.stats (runtime : Runtime) : IO RuntimeStats := do
  let s ← runtime.state.get
  return { draining := s.deadline.isSome, deadlineMs := s.deadline, failed := s.failed, closed := s.closed }

/-- Idempotent; one failing finalizer never suppresses another resource's cleanup. -/
def Runtime.close (runtime : Runtime) : IO Unit := do
  if (← runtime.state.get).closed then return
  runtime.state.modify fun s => { s with closed := true }
  for p in runtime.participants.reverse do runtime.attempt p.close

def Runtime.beginDrain (runtime : Runtime) (deadline : Nat) : IO Unit := do
  if (← runtime.state.get).deadline.isSome then return
  runtime.state.modify fun s => { s with deadline := some deadline }
  unless ← runtime.health.beginDrain do runtime.state.modify fun s => { s with failed := true }
  for p in runtime.participants do runtime.attempt (p.onDrain deadline)

def Runtime.tick (runtime : Runtime) (nowMs : Nat) : IO Unit := do
  if (← runtime.state.get).closed then return
  for p in runtime.participants do runtime.attempt (p.onTick nowMs)
  if (← runtime.state.get).deadline.any (nowMs >= ·) then runtime.close

def Runtime.isDrained (runtime : Runtime) : IO Bool := do
  let state ← runtime.state.get
  if state.closed then return true
  let mut drained := true
  for p in runtime.participants do
    try
      if !(← p.isDrained) then drained := false
    catch _ =>
      runtime.state.modify fun s => { s with failed := true }
      drained := false
  return drained

/-- Bracket the returned Application in `Server.serveApplications`. Native
SIGTERM admission/drain and application participants share the same deadline. -/
def Runtime.application (runtime : Runtime) (app : Server.Application) : Server.Application :=
  { app with
    handler := runtime.health.middleware app.handler
    onTick := fun now => do runtime.attempt (app.onTick now); runtime.tick now
    onDrain := fun deadline => do runtime.beginDrain deadline; runtime.attempt (app.onDrain deadline)
    isDrained := do return (← app.isDrained) && (← runtime.isDrained)
    onShutdown := do runtime.attempt app.onShutdown; runtime.close }

end LeanWeave.Operations
