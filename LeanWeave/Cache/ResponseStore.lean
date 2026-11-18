import LeanWeave.Security.Crypto

namespace LeanWeave.Cache

/-- SHA-256 of length-prefixed key components. `digest` is the fixed-size hex form
    used by the worker-local index; construct keys with `ofParts` or `ofByteParts`. -/
structure CacheKey where
  digest : String
  deriving BEq, Hashable, Inhabited

namespace CacheKey

private def appendLength (bytes : ByteArray) (length : Nat) : ByteArray := Id.run do
  let mut bytes := bytes
  let word := length.toUInt64
  for i in [:8] do
    bytes := bytes.push (word >>> ((7 - i) * 8).toUInt64).toUInt8
  return bytes

/-- Byte components preserve opaque request-header values, including invalid
    UTF-8 and the distinction between absent and present-but-empty fields. -/
def ofByteParts (parts : Array ByteArray) : CacheKey := Id.run do
  let size := parts.foldl (fun size part => size + 8 + part.size) 8
  let mut bytes := appendLength (ByteArray.emptyWithCapacity size) parts.size
  for part in parts do
    bytes := (appendLength bytes part.size) ++ part
  return ⟨Security.Crypto.hex (Security.Crypto.sha256 bytes)⟩

def ofParts (parts : Array String) : CacheKey :=
  ofByteParts (parts.map String.toUTF8)

end CacheKey

/-- Owned response representation. Cache hits reuse the same `ByteArray` body
    through LeanWeave's response path. -/
structure CacheEntry where
  status : Nat
  headers : Array LeanNet.HTTP1.Header
  body : ByteArray
  ttlMs : Nat
  storedAt : Nat
  /-- Corrected initial HTTP age at storage, including Date/Age and fetch delay. -/
  initialAgeMs : Nat := 0
  baseKey : CacheKey
  varyNames : Array String
  varyFingerprint : CacheKey

/-- Account the owned payload and fixed key/entry metadata. Hash-table bucket
    overhead is bounded by `maxEntries` but is not included in this byte cost. -/
def CacheEntry.cost (entry : CacheEntry) : Nat :=
  entry.body.size +
  entry.headers.foldl (fun n h => n + h.name.utf8ByteSize + h.value.size) 0 +
  entry.varyNames.foldl (fun n name => n + name.utf8ByteSize) 0 +
  entry.baseKey.digest.utf8ByteSize + entry.varyFingerprint.digest.utf8ByteSize + 128

structure ResponseLimits where
  maxEntries : Nat := 1024
  maxBytes : Nat := 8 * 1024 * 1024
  maxEntryBytes : Nat := 1024 * 1024
  maxVariantsPerBase : Nat := 16
  maxInFlight : Nat := 128
  maxWaitersPerKey : Nat := 64
  maxWaiters : Nat := 1024
  maxPinnedBytes : Nat := 8 * 1024 * 1024
  protectedPercent : Nat := 80
  deriving Repr

private inductive Segment where
  | probation
  | protectedQ
  deriving BEq

private structure Queue where
  head : Option CacheKey := none
  tail : Option CacheKey := none
  count : Nat := 0
  bytes : Nat := 0

private structure Node where
  entry : CacheEntry
  cost : Nat
  segment : Segment
  prev : Option CacheKey := none
  next : Option CacheKey := none

private structure FlightState where
  /-- `none` is pending; `some none` means retry without sharing. -/
  result : Option (Option CacheEntry) := none
  waiters : Nat := 0
  pinnedBytes : Nat := 0

/-- A flight and its waiters are owned by the creating reactor worker. -/
structure Flight where
  key : CacheKey
  id : Nat
  generation : Nat
  state : IO.Ref FlightState

inductive FlightClaim where
  | leader (flight : Flight)
  | follower (flight : Flight)
  | overloaded

private structure ResponseState where
  nodes : Std.HashMap CacheKey Node := {}
  variants : Std.HashMap CacheKey (Array CacheKey) := {}
  probation : Queue := {}
  protectedQ : Queue := {}
  bytes : Nat := 0
  generation : Nat := 0
  flights : Std.HashMap CacheKey Flight := {}
  nextFlightId : Nat := 0
  waiters : Nat := 0
  pinnedBytes : Nat := 0

/-- Instantiate inside the worker factory; never share with other workers or
    adapter threads. All methods are synchronous owner-worker operations. -/
structure ResponseStore where
  limits : ResponseLimits
  state : IO.Ref ResponseState

def ResponseStore.new (limits : ResponseLimits := {}) : IO ResponseStore := do
  if limits.maxEntries == 0 || limits.maxBytes == 0 || limits.maxEntryBytes == 0 ||
      limits.maxEntryBytes > limits.maxBytes || limits.maxVariantsPerBase == 0 ||
      limits.maxInFlight == 0 || limits.maxWaitersPerKey == 0 || limits.maxWaiters == 0 ||
      limits.maxPinnedBytes == 0 || limits.protectedPercent > 100 then
    throw (IO.userError "invalid response cache bounds")
  return { limits, state := ← IO.mkRef {} }

private def ResponseState.queue (state : ResponseState) : Segment → Queue
  | .probation => state.probation
  | .protectedQ => state.protectedQ

private def ResponseState.withQueue (state : ResponseState) (segment : Segment) (queue : Queue) : ResponseState :=
  match segment with
  | .probation => { state with probation := queue }
  | .protectedQ => { state with protectedQ := queue }

/-- Unlink a node while retaining it in the index. -/
private def ResponseState.unlink (state : ResponseState) (key : CacheKey) : ResponseState := Id.run do
  let some node := state.nodes.get? key | return state
  let queue := state.queue node.segment
  let mut nodes := state.nodes
  -- Drop the state's reference before mutating the worker-owned table.
  let state := { state with nodes := {} }
  if let some previous := node.prev then
    if let some neighbor := nodes.get? previous then
      nodes := nodes.insert previous { neighbor with next := node.next }
  if let some next := node.next then
    if let some neighbor := nodes.get? next then
      nodes := nodes.insert next { neighbor with prev := node.prev }
  nodes := nodes.insert key { node with prev := none, next := none }
  let queue := { queue with
    head := if queue.head == some key then node.next else queue.head
    tail := if queue.tail == some key then node.prev else queue.tail
    count := queue.count - 1
    bytes := queue.bytes - node.cost }
  return { state.withQueue node.segment queue with nodes := nodes }

private def ResponseState.linkFront (state : ResponseState) (key : CacheKey) (segment : Segment) : ResponseState := Id.run do
  let some node := state.nodes.get? key | return state
  let queue := state.queue segment
  let mut nodes := state.nodes
  let state := { state with nodes := {} }
  nodes := nodes.insert key { node with segment, prev := none, next := queue.head }
  if let some oldHead := queue.head then
    if let some neighbor := nodes.get? oldHead then
      nodes := nodes.insert oldHead { neighbor with prev := some key }
  let queue := { queue with
    head := some key
    tail := if queue.tail.isNone then some key else queue.tail
    count := queue.count + 1
    bytes := queue.bytes + node.cost }
  return { state.withQueue segment queue with nodes := nodes }

private def ResponseState.remove (state : ResponseState) (key : CacheKey) : ResponseState := Id.run do
  let some node := state.nodes.get? key | return state
  let state := state.unlink key
  let variants := (state.variants.getD node.entry.baseKey #[]).filter (· != key)
  let index := if variants.isEmpty then state.variants.erase node.entry.baseKey
    else state.variants.insert node.entry.baseKey variants
  let state := { state with nodes := state.nodes.erase key }
  let state := { state with variants := index }
  return { state with bytes := state.bytes - node.cost }

private def ResponseState.touch (state : ResponseState) (limits : ResponseLimits)
    (key : CacheKey) (node : Node) : ResponseState := Id.run do
  let protectedCountLimit := limits.maxEntries * limits.protectedPercent / 100
  let protectedByteLimit := limits.maxBytes * limits.protectedPercent / 100
  let destination := if protectedCountLimit == 0 || node.cost > protectedByteLimit then
    Segment.probation else Segment.protectedQ
  if node.segment == destination && node.prev.isNone then return state
  let mut state := (state.unlink key).linkFront key destination
  for _ in [:limits.maxEntries] do
    if state.protectedQ.count <= protectedCountLimit && state.protectedQ.bytes <= protectedByteLimit then break
    let some tail := state.protectedQ.tail | break
    state := (state.unlink tail).linkFront tail .probation
  return state

private def ResponseState.evictToFit (state : ResponseState) (limits : ResponseLimits)
    (addedCost : Nat) : ResponseState := Id.run do
  let mut state := state
  for _ in [:limits.maxEntries + 1] do
    if state.nodes.size < limits.maxEntries && state.bytes + addedCost <= limits.maxBytes then break
    let candidate := state.probation.tail.orElse fun _ => state.protectedQ.tail
    let some key := candidate | break
    state := state.remove key
  return state

private def ResponseState.insertEntry (state : ResponseState) (limits : ResponseLimits)
    (key : CacheKey) (entry : CacheEntry) (cost : Nat) : Bool × ResponseState := Id.run do
  let mut state := state.remove key
  let variants := state.variants.getD entry.baseKey #[]
  if variants.size >= limits.maxVariantsPerBase then
    state := state.remove variants[0]!
  state := state.evictToFit limits cost
  if state.nodes.size >= limits.maxEntries || state.bytes + cost > limits.maxBytes then
    return (false, state)
  let node : Node := { entry, cost, segment := .probation }
  let index := state.variants.insert entry.baseKey ((state.variants.getD entry.baseKey #[]).push key)
  state := { state with nodes := state.nodes.insert key node }
  state := { state with variants := index }
  state := { state with bytes := state.bytes + cost }
  return (true, state.linkFront key .probation)

private def CacheEntry.fresh (entry : CacheEntry) (now : Nat) : Bool :=
  entry.ttlMs > 0 && now >= entry.storedAt && now - entry.storedAt < entry.ttlMs

/-- A hit reuses the stored body. Moving a node updates its neighbors in the
    worker's hash table. Promotions can demote entries to meet the segment budget. -/
def ResponseStore.lookup (store : ResponseStore) (key : CacheKey) : IO (Option CacheEntry) := do
  let now ← IO.monoMsNow
  -- Consume the previous state. A separate get/set leaves the table shared
  -- with the reference and can copy its bucket array on every move.
  store.state.modifyGet fun state =>
    match state.nodes.get? key with
    | none => (none, state)
    | some node =>
        if node.entry.fresh now then (some node.entry, state.touch store.limits key node)
        else (none, state.remove key)

/-- Find the newest matching Vary variant without scanning unrelated routes.
    The per-base candidate array is capped by `maxVariantsPerBase`. -/
def ResponseStore.lookupMatching (store : ResponseStore) (base : CacheKey)
    (accepts : CacheEntry → Bool) : IO (Option CacheEntry) := do
  let now ← IO.monoMsNow
  store.state.modifyGet fun state => Id.run do
    let mut state := state
    let mut best : Option (CacheKey × Node) := none
    for key in state.variants.getD base #[] do
      if let some node := state.nodes.get? key then
        if !node.entry.fresh now then state := state.remove key
        else if node.entry.baseKey == base && accepts node.entry then
          match best with
          | none => best := some (key, node)
          | some (_, previous) =>
            if node.entry.storedAt >= previous.entry.storedAt then
              best := some (key, node)
    if let some (key, node) := best then
      return (some node.entry, state.touch store.limits key node)
    return (none, state)

/-- Reject oversized and already-expired entries before mutating the store.
    The caller must check request/response cache policy before calling this. -/
def ResponseStore.storeChecked (store : ResponseStore) (key : CacheKey) (entry : CacheEntry) : IO Bool := do
  let now ← IO.monoMsNow
  let cost := entry.cost + key.digest.utf8ByteSize
  if !entry.fresh now || cost > store.limits.maxEntryBytes || cost > store.limits.maxBytes then return false
  store.state.modifyGet fun state =>
    state.insertEntry store.limits key entry cost

def ResponseStore.store (store : ResponseStore) (key : CacheKey) (entry : CacheEntry) : IO Unit := do
  let _ ← store.storeChecked key entry
  pure ()

def ResponseStore.generation (store : ResponseStore) : IO Nat := do
  return (← store.state.get).generation

private def ResponseStore.cancelActiveFlights (_store : ResponseStore)
    (state : ResponseState) : IO ResponseState := do
  -- Invalidation changes the global generation, so every old producer is
  -- invalid. Wake its waiters immediately and allow a fresh leader to claim.
  for (_, flight) in state.flights.toList do
    let flightState ← flight.state.get
    if flightState.result.isNone then
      flight.state.set { flightState with result := some none }
  return { state with flights := {} }

def ResponseStore.invalidate (store : ResponseStore) (key : CacheKey) : IO Unit := do
  let state ← store.cancelActiveFlights (← store.state.get)
  let state := state.remove key
  store.state.set { state with generation := state.generation + 1 }

def ResponseStore.invalidateBase (store : ResponseStore) (base : CacheKey) : IO Unit := do
  let mut state ← store.cancelActiveFlights (← store.state.get)
  for key in state.variants.getD base #[] do state := state.remove key
  store.state.set { state with generation := state.generation + 1 }

def ResponseStore.clear (store : ResponseStore) : IO Unit := do
  let state ← store.cancelActiveFlights (← store.state.get)
  let fresh : ResponseState := {}
  let fresh := { fresh with generation := state.generation + 1 }
  let fresh := { fresh with nextFlightId := state.nextFlightId }
  let fresh := { fresh with waiters := state.waiters }
  store.state.set { fresh with pinnedBytes := state.pinnedBytes }

/-- Small read-only metrics for tests and operators. -/
structure ResponseStats where
  entries : Nat
  bytes : Nat
  probationEntries : Nat
  protectedEntries : Nat
  inFlight : Nat
  waiters : Nat
  pinnedBytes : Nat
  deriving Repr

def ResponseStore.stats (store : ResponseStore) : IO ResponseStats := do
  let state ← store.state.get
  return ⟨state.nodes.size, state.bytes, state.probation.count,
    state.protectedQ.count, state.flights.size, state.waiters, state.pinnedBytes⟩

private def ResponseStore.sameFlight (store : ResponseStore) (flight : Flight) : IO Bool := do
  let state ← store.state.get
  return (state.flights.get? flight.key).any (·.id == flight.id)

/-- Claim one producer or register a bounded waiter. When limits are reached,
    callers should fail fast or bypass caching; no unbounded queue is formed. -/
def ResponseStore.claim (store : ResponseStore) (key : CacheKey) : IO FlightClaim := do
  let state ← store.state.get
  if let some flight := state.flights.get? key then
    let flightState ← flight.state.get
    if flightState.waiters >= store.limits.maxWaitersPerKey || state.waiters >= store.limits.maxWaiters then
      return .overloaded
    flight.state.set { flightState with waiters := flightState.waiters + 1 }
    store.state.set { state with waiters := state.waiters + 1 }
    return .follower flight
  if state.flights.size >= store.limits.maxInFlight then return .overloaded
  let flight : Flight := ⟨key, state.nextFlightId, state.generation, ← IO.mkRef {}⟩
  let state := { state with flights := state.flights.insert key flight }
  store.state.set { state with nextFlightId := state.nextFlightId + 1 }
  return .leader flight

/-- Finish a producer exactly once. `entry` must have been accepted by
    `storeChecked`. A generation change or pin-budget overflow wakes followers
    with `none` so they can recheck the store or start another fill. -/
def ResponseStore.finish (store : ResponseStore) (flight : Flight)
    (entry : Option CacheEntry) : IO Bool := do
  let flightState ← flight.state.get
  if flightState.result.isSome || !(← store.sameFlight flight) then return false
  let state ← store.state.get
  let entry := if state.generation != flight.generation then none else entry
  let entry := entry.filter fun value =>
    flightState.waiters == 0 || state.pinnedBytes + value.cost <= store.limits.maxPinnedBytes
  let pinCost := if flightState.waiters == 0 then 0 else entry.map CacheEntry.cost |>.getD 0
  flight.state.set { flightState with result := some entry, pinnedBytes := pinCost }
  let state := { state with flights := state.flights.erase flight.key }
  store.state.set { state with pinnedBytes := state.pinnedBytes + pinCost }
  return entry.isSome

def ResponseStore.abort (store : ResponseStore) (flight : Flight) : IO Unit := do
  let _ ← store.finish flight none
  pure ()

private def ResponseStore.releaseWaiter (store : ResponseStore) (flight : Flight) : IO Unit := do
  let flightState ← flight.state.get
  if flightState.waiters == 0 then return
  let remaining := flightState.waiters - 1
  let unpin := if remaining == 0 then flightState.pinnedBytes else 0
  let flightState := { flightState with waiters := remaining }
  let flightState := { flightState with pinnedBytes := if remaining == 0 then 0 else flightState.pinnedBytes }
  flight.state.set { flightState with result := if remaining == 0 then none else flightState.result }
  store.state.modify fun state =>
    { { state with waiters := state.waiters - 1 } with pinnedBytes := state.pinnedBytes - unpin }

/-- Suspend without blocking the reactor. Completion, timeout and disconnect
    each release the waiter once. `some entry` is pinned until the last waiter
    resumes or cancels. -/
def ResponseStore.wait (store : ResponseStore) (flight : Flight)
    (timeoutMs : Nat := 30000) : AppM (Option CacheEntry) := do
  let released ← IO.mkRef false
  let release := do
    unless ← released.get do
      released.set true
      store.releaseWaiter flight
  (AppM.await (do
      if (← store.generation) != flight.generation then
        return some (.ok none)
      let flightState ← flight.state.get
      let now ← IO.monoMsNow
      return flightState.result.map (fun result =>
        .ok (result.filter (·.fresh now))))
    (fun _ => release) timeoutMs).ensuring release

end LeanWeave.Cache
