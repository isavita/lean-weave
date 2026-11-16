import LeanNet.HTTP.Stream

namespace LeanWeave.Stream

abbrev Body := LeanNet.HTTP.Stream.Body
abbrev Event := LeanNet.HTTP.Stream.Event
abbrev CancelReason := LeanNet.HTTP.Deferred.CancelReason

structure Limits where
  maxChunks : Nat := 64
  maxBytes : Nat := 65536
  maxChunkBytes : Nat := 16384
  deriving Repr, Inhabited

private structure State where
  chunks : Array ByteArray := #[]
  bytes : Nat := 0
  ending : Option (Array LeanNet.HTTP1.Header) := none
  closed : Bool := false

/-- A bounded single-worker producer queue. No reference is shared with tasks;
external I/O adapters must hand off only owned bytes through their own bounds. -/
structure Queue where
  private state : IO.Ref State
  limits : Limits

def Queue.new (limits : Limits := {}) : IO Queue := do
  if limits.maxChunks == 0 || limits.maxBytes == 0 || limits.maxChunkBytes == 0 ||
      limits.maxChunkBytes > 65536 || limits.maxChunkBytes > limits.maxBytes then
    throw (IO.userError "invalid stream queue limits")
  return ⟨← IO.mkRef {}, limits⟩

def Queue.push (queue : Queue) (bytes : ByteArray) : IO Bool := do
  let state ← queue.state.get
  if state.closed || state.ending.isSome || bytes.isEmpty || bytes.size > queue.limits.maxChunkBytes ||
      state.chunks.size >= queue.limits.maxChunks || bytes.size > queue.limits.maxBytes - state.bytes then
    return false
  queue.state.set { state with chunks := state.chunks.push bytes, bytes := state.bytes + bytes.size }
  return true

def Queue.finish (queue : Queue) (trailers : Array LeanNet.HTTP1.Header := #[]) : IO Bool := do
  let state ← queue.state.get
  if state.closed || state.ending.isSome then return false
  queue.state.set { state with ending := some trailers }
  return true

def Queue.queuedBytes (queue : Queue) : IO Nat := do return (← queue.state.get).bytes

def Queue.closed (queue : Queue) : IO Bool := do return (← queue.state.get).closed

/-- Create one body per queue. Closing the exchange clears retained chunks and
notifies its owner exactly once. Budget splitting preserves binary contents. -/
def Queue.body (queue : Queue)
    (onClose : Option CancelReason → IO Unit := fun _ => pure ())
    (idleTimeoutMs : Nat := 30000) (lifetimeMs : Nat := 3600000)
    (declaredTrailers : Array String := #[]) : Body := {
  retainedBytes := queue.limits.maxBytes + queue.limits.maxChunks * 64
  idleTimeoutMs, lifetimeMs, declaredTrailers
  poll := fun budget => do
    let state ← queue.state.get
    if state.closed then return none
    if let some bytes := state.chunks[0]? then
      if budget == 0 then return none
      let count := min budget bytes.size
      let chunks := if count == bytes.size then state.chunks.extract 1 state.chunks.size
        else state.chunks.set! 0 (bytes.extract count bytes.size)
      queue.state.set { state with chunks, bytes := state.bytes - count }
      return some (.data (bytes.extract 0 count))
    return state.ending.map (fun trailers => LeanNet.HTTP.Stream.Event.finish trailers)
  close := fun reason => do
    let state ← queue.state.get
    if !state.closed then
      queue.state.set { state with chunks := #[], bytes := 0, closed := true }
      onClose reason
}

/-- Finite binary chunks, still emitted under native output/flow credits. -/
def chunks (values : Array ByteArray) (trailers : Array LeanNet.HTTP1.Header := #[])
    (onClose : Option CancelReason → IO Unit := fun _ => pure ()) : IO Body := do
  let total := values.foldl (fun n bytes => n + bytes.size) 0
  let queue ← Queue.new {
    maxChunks := max 1 values.size
    maxBytes := max 1 total
    maxChunkBytes := max 1 (min 65536 (values.foldl (fun n bytes => max n bytes.size) 0)) }
  for value in values do
    if !value.isEmpty && !(← queue.push value) then
      throw (IO.userError "stream chunks exceed bounded queue")
  let _ ← queue.finish trailers
  return { queue.body onClose (declaredTrailers := trailers.map (·.name)) with
    knownLength := if trailers.isEmpty then some total else none }

end LeanWeave.Stream
