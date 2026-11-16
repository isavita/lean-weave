import LeanWeave.Core.Types

namespace LeanWeave.SSE

structure Event where
  data : String
  id : Option String := none
  event : Option String := none
  retryMs : Option Nat := none
  deriving Repr, Inhabited

private def safeField (s : String) : Bool :=
  !s.toUTF8.data.any (fun b => b == 0 || b == 10 || b == 13)

/-- EventSource framing. Multiline data never injects id/event/retry fields. -/
def Event.encode (event : Event) : Except String ByteArray := do
  if event.id.any (!safeField ·) || event.event.any (!safeField ·) ||
      event.retryMs.any (fun n => n == 0 || n > 86400000) then
    throw "invalid SSE id/event/retry field"
  let mut bytes := ByteArray.empty
  if let some id := event.id then bytes := bytes ++ s!"id: {id}\n".toUTF8
  if let some name := event.event then bytes := bytes ++ s!"event: {name}\n".toUTF8
  if let some retry := event.retryMs then bytes := bytes ++ s!"retry: {retry}\n".toUTF8
  let data := event.data.replace "\r\n" "\n" |>.replace "\r" "\n"
  for line in data.splitOn "\n" do bytes := bytes ++ s!"data: {line}\n".toUTF8
  return bytes ++ "\n".toUTF8

structure Limits where
  maxEvents : Nat := 256
  maxBytes : Nat := 1024 * 1024
  maxEventBytes : Nat := 16384
  ttlMs : Nat := 60000
  keepAliveMs : Nat := 15000
  lifetimeMs : Nat := 3600000
  retryMs : Nat := 1000

private structure Entry where
  sequence : Nat
  id : String
  bytes : ByteArray
  expires : Nat
  deriving Inhabited

private structure State where
  entries : Array Entry := #[]
  bytes : Nat := 0
  sequence : Nat := 0

/-- A bounded worker-local replay log. Use an application/shared replay backend
for durable cross-node cursors; unknown/evicted IDs produce an explicit reset. -/
structure Store where
  private state : IO.Ref State
  private idPrefix : String
  limits : Limits

def Store.new (limits : Limits := {}) : IO Store := do
  if limits.maxEvents == 0 || limits.maxBytes == 0 || limits.maxEventBytes == 0 ||
      limits.maxEventBytes > limits.maxBytes || limits.maxEventBytes > 65536 ||
      limits.ttlMs == 0 || limits.keepAliveMs == 0 || limits.lifetimeMs == 0 ||
      limits.retryMs == 0 || limits.retryMs > 86400000 then
    throw (IO.userError "invalid SSE replay/timeout bounds")
  let random ← IO.getRandomBytes 16
  let idPrefix := String.intercalate "" (random.toList.map fun b => s!"{b.toNat}-")
  return ⟨← IO.mkRef {}, idPrefix, limits⟩

private def State.prune (state : State) (now : Nat) (limits : Limits) : State := Id.run do
  let mut entries := state.entries.filter (fun entry => now < entry.expires)
  let mut bytes := entries.foldl (fun n e => n + e.bytes.size) 0
  while entries.size > limits.maxEvents || bytes > limits.maxBytes do
    bytes := bytes - entries[0]!.bytes.size
    entries := entries.extract 1 entries.size
  return { state with entries, bytes }

def Store.publish (store : Store) (event : Event) : IO (Except String String) := do
  let now ← IO.monoMsNow
  let state := (← store.state.get).prune now store.limits
  let sequence := state.sequence + 1
  let id := s!"{store.idPrefix}:{sequence}"
  match ({ event with id := some id }).encode with
  | .error message => return .error message
  | .ok bytes =>
    if bytes.size > store.limits.maxEventBytes then return .error "SSE event exceeds byte bound"
    let entry := { sequence, id, bytes, expires := now + store.limits.ttlMs : Entry }
    let next := ({ state with sequence, entries := state.entries.push entry, bytes := state.bytes + bytes.size } : State).prune now store.limits
    store.state.set next
    return .ok id

/-- First-class reconnecting SSE, replaying IDs retained in the bounded log.
When a cursor is unavailable it emits `reset` before replaying current history.
Keep-alive comments continue while no application events are available. -/
def Store.response (store : Store) (ctx : Context) : IO Response := do
  let now ← IO.monoMsNow
  let state := (← store.state.get).prune now store.limits
  store.state.set state
  let last := ctx.header "last-event-id"
  let found := last.bind (fun id => state.entries.find? (·.id == id))
  let start := match found with
    | some entry => entry.sequence + 1
    | none => (state.entries[0]?.map (·.sequence)).getD (state.sequence + 1)
  let cursor ← IO.mkRef start
  let reset ← IO.mkRef (last.isSome && found.isNone)
  let pending ← IO.mkRef s!"retry: {store.limits.retryMs}\n\n".toUTF8
  let emitted ← IO.mkRef now
  let closed ← IO.mkRef false
  let body : Stream.Body := {
    retainedBytes := store.limits.maxEventBytes
    idleTimeoutMs := store.limits.keepAliveMs * 3
    lifetimeMs := store.limits.lifetimeMs
    poll := fun budget => do
      if ← closed.get then return none
      if (← pending.get).isEmpty then
        let now ← IO.monoMsNow
        let state := (← store.state.get).prune now store.limits
        store.state.set state
        let next ← cursor.get
        if (← reset.get) || state.entries[0]?.any (fun e => e.sequence > next) then
          reset.set false
          cursor.set ((state.entries[0]?.map (·.sequence)).getD (state.sequence + 1))
          pending.set "event: reset\ndata: replay-gap\n\n".toUTF8
        else if let some entry := state.entries.find? (fun e => e.sequence >= next) then
          cursor.set (entry.sequence + 1)
          pending.set entry.bytes
        else if now - (← emitted.get) >= store.limits.keepAliveMs then
          pending.set ": keep-alive\n\n".toUTF8
        else return none
      if budget == 0 then return none
      let bytes ← pending.get
      let count := min budget bytes.size
      pending.set (bytes.extract count bytes.size)
      emitted.set (← IO.monoMsNow)
      return some (.data (bytes.extract 0 count))
    close := fun _ => do closed.set true; pending.set ByteArray.empty }
  return (Response.streaming body).withHeader "content-type" "text/event-stream; charset=utf-8"
    |>.withHeader "cache-control" "no-store" |>.withHeader "x-accel-buffering" "no"

end LeanWeave.SSE
