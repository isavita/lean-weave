import LeanWeave.Core

namespace LeanWeave.Cache

structure Limits where
  maxEntries : Nat := 1024
  maxBytes : Nat := 8 * 1024 * 1024
  maxEntryBytes : Nat := 1024 * 1024
  maxInFlight : Nat := 128
  deriving Repr

structure Entry where
  key : String
  value : ByteArray
  expires : Nat
  used : Nat
  deriving Inhabited

structure State where
  entries : Array Entry := #[]
  bytes : Nat := 0
  clock : Nat := 0
  generation : Nat := 0
  inFlight : Array String := #[]

/-- Worker-local bounded LRU. Never share this reference with adapter threads. -/
structure Store where
  limits : Limits
  state : IO.Ref State

def Store.new (limits : Limits := {}) : IO Store := do
  if limits.maxEntries == 0 || limits.maxBytes == 0 || limits.maxEntryBytes == 0 ||
      limits.maxEntryBytes > limits.maxBytes || limits.maxInFlight == 0 then
    throw (IO.userError "invalid cache bounds")
  return { limits, state := ← IO.mkRef {} }

private def State.prune (state : State) (now : Nat) : State :=
  let entries := state.entries.filter (fun e => now < e.expires)
  { state with entries, bytes := entries.foldl (fun n e => n + e.value.size + e.key.utf8ByteSize) 0 }

def Store.get (store : Store) (key : String) (now : Nat) : IO (Option ByteArray) := do
  let state := (← store.state.get).prune now
  let value := (state.entries.find? fun e => e.key == key).map (·.value)
  let clock := state.clock + 1
  store.state.set { state with clock, entries := state.entries.map fun e =>
    if e.key == key then { e with used := clock } else e }
  return value

/-- Explicit invalidation is immediate on this worker. Broadcast for multiworker consistency. -/
def Store.invalidate (store : Store) (key : String) : IO Unit := do
  store.state.modify fun state =>
    let entries := state.entries.filter (fun e => e.key != key)
    { state with entries, generation := state.generation + 1, bytes := entries.foldl (fun n e => n + e.value.size + e.key.utf8ByteSize) 0 }

def Store.clear (store : Store) : IO Unit := store.state.modify fun s => { s with entries := #[], bytes := 0, generation := s.generation + 1 }

def Store.put (store : Store) (key : String) (value : ByteArray) (ttlMs now : Nat) : IO Bool := do
  let size := key.utf8ByteSize + value.size
  if ttlMs == 0 || size > store.limits.maxEntryBytes then return false
  store.state.modify fun state =>
    let entries := state.entries.filter (fun e => e.key != key)
    { state with entries, bytes := entries.foldl (fun n e => n + e.value.size + e.key.utf8ByteSize) 0 }
  let mut state := (← store.state.get).prune now
  for _ in [:state.entries.size] do
    if state.entries.size >= store.limits.maxEntries || state.bytes + size > store.limits.maxBytes then
      let oldest := state.entries.foldl (fun best e => min best e.used) state.clock
      let index := state.entries.findIdx? (fun e => e.used == oldest)
      if let some i := index then
        let e := state.entries[i]!
        state := { state with entries := state.entries.filter (fun x => x.key != e.key), bytes := state.bytes - e.value.size - e.key.utf8ByteSize }
  if state.entries.size >= store.limits.maxEntries || state.bytes + size > store.limits.maxBytes then return false
  let clock := state.clock + 1
  store.state.set { state with clock := clock, bytes := state.bytes + size, entries := state.entries.push { key, value, expires := now + ttlMs, used := clock } }
  return true

/-- A miss lease prevents duplicate generation, with bounded in-flight keys. -/
def Store.acquire (store : Store) (key : String) : IO Bool := do
  let state ← store.state.get
  if key.utf8ByteSize > store.limits.maxEntryBytes || state.inFlight.contains key || state.inFlight.size >= store.limits.maxInFlight then return false
  store.state.set { state with inFlight := state.inFlight.push key }
  return true

def Store.release (store : Store) (key : String) : IO Unit :=
  store.state.modify fun s => { s with inFlight := s.inFlight.filter (· != key) }

/-- Length-prefixed components prevent delimiter collisions between isolation dimensions. -/
def key (parts : Array String) : String :=
  String.join (parts.toList.map fun p => s!"{p.utf8ByteSize}:{p}")

/-- Fragment generation shares the store and miss leases used by response caching. -/
def fragment (store : Store) (cacheKey : String) (ttlMs : Nat) (generate : AppM ByteArray) : AppM ByteArray := do
  let now := (← IO.monoMsNow)
  if let some bytes ← store.get cacheKey now then return bytes
  unless ← store.acquire cacheKey do throw { status := 503, message := "cache generation busy" }
  let generation := (← store.state.get).generation
  (do
    let bytes ← generate
    if (← store.state.get).generation == generation then
      let _ ← store.put cacheKey bytes ttlMs (← IO.monoMsNow)
      pure ()
    return bytes).ensuring (store.release cacheKey)

end LeanWeave.Cache
