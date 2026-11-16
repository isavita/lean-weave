import LeanWeave.Realtime.PubSub

namespace LeanWeave.Presence

/-- Dotted state-based observed-remove set. A replica has one random,
never-reused actor ID; the bounded causal context is retained even after leave.
Capacity rejects new actors rather than evicting removal knowledge. -/
structure Dot where
  actor : String
  counter : Nat
  deriving BEq, Inhabited, Lean.ToJson, Lean.FromJson

structure Entry where
  dot : Dot
  topic : String
  key : String
  metadata : Lean.Json
  connection : String := "default"
  deriving Inhabited, Lean.ToJson, Lean.FromJson

structure Snapshot where
  context : Array (String × Nat) := #[]
  entries : Array Entry := #[]
  deriving Inhabited, Lean.ToJson, Lean.FromJson

structure Limits where
  maxActors : Nat := 64
  maxEntries : Nat := 1024
  maxSnapshotBytes : Nat := 262144
  maxMetadataBytes : Nat := 4096
  leaseMs : Nat := 30000
  heartbeatMs : Nat := 5000
  deriving Inhabited

private def seen (context : Array (String × Nat)) (dot : Dot) : Bool :=
  context.any (fun (actor, counter) => actor == dot.actor && dot.counter <= counter)

private def clock (context : Array (String × Nat)) (actor : String) : Nat :=
  ((context.find? (·.1 == actor)).map (·.2)).getD 0

private def setClock (context : Array (String × Nat)) (actor : String) (counter : Nat) : Array (String × Nat) :=
  (context.filter (·.1 != actor)).push (actor, counter)

private def validName (name : String) : Bool :=
  !name.isEmpty && name.utf8ByteSize <= 256 && name.toUTF8.data.all (fun b => b >= 32 && b != 127)

def Snapshot.validate (value : Snapshot) (limits : Limits := {}) : Except String Unit := do
  if value.context.size > limits.maxActors || value.entries.size > limits.maxEntries ||
      (Lean.toJson value).compress.utf8ByteSize > limits.maxSnapshotBytes then
    throw "presence snapshot capacity exhausted"
  let mut actors : Array String := #[]
  for (actor, counter) in value.context do
    if !validName actor || counter > 18446744073709551615 || actors.contains actor then
      throw "invalid presence causal context"
    actors := actors.push actor
  let mut dots : Array Dot := #[]
  for entry in value.entries do
    if !validName entry.topic || !validName entry.key || !validName entry.connection || !seen value.context entry.dot ||
        entry.dot.counter == 0 || dots.contains entry.dot ||
        entry.metadata.compress.utf8ByteSize > limits.maxMetadataBytes then
      throw "invalid presence entry"
    dots := dots.push entry.dot

/-- Associative, commutative, idempotent OR-set merge for valid snapshots.
A dot absent from a peer that has observed it has been removed. -/
def Snapshot.merge (left right : Snapshot) (limits : Limits := {}) : Except String Snapshot := do
  left.validate limits
  right.validate limits
  let mut context := left.context
  for (actor, counter) in right.context do
    context := setClock context actor (max counter (clock context actor))
  let mut entries := left.entries.filter fun entry =>
    right.entries.any (·.dot == entry.dot) || !seen right.context entry.dot
  for entry in right.entries do
    if !left.entries.any (·.dot == entry.dot) && !seen left.context entry.dot then
      entries := entries.push entry
  let value := { context, entries : Snapshot }
  value.validate limits
  return value

private structure LocalEntry where
  topic : String
  key : String
  connection : String
  metadata : Lean.Json

private structure State where
  snapshot : Snapshot := {}
  localKeys : Array LocalEntry := #[]
  lastSeen : Array (String × Nat) := #[]
  nextHeartbeat : Nat := 0

/-- Worker-owned replica. Periodic snapshots provide anti-entropy after PubSub
loss. Leases hide partitioned peers; surviving local memberships receive fresh
dots each heartbeat, so they reappear after rejoin without reviving leaves. -/
structure Replica where
  private state : IO.Ref State
  actor : String
  limits : Limits

def Replica.new (actor : String) (limits : Limits := {}) : IO Replica := do
  if !validName actor || limits.maxActors == 0 || limits.maxActors > 1024 ||
      limits.maxEntries == 0 || limits.maxEntries > 65536 || limits.maxSnapshotBytes == 0 ||
      limits.heartbeatMs == 0 || limits.leaseMs < limits.heartbeatMs * 2 then
    throw (IO.userError "invalid presence replica bounds")
  return ⟨← IO.mkRef {}, actor, limits⟩

private def refreshLocal (replica : Replica) (state : State) : Except String State := do
  let mut snapshot := { state.snapshot with entries := state.snapshot.entries.filter (·.dot.actor != replica.actor) }
  let mut counter := clock snapshot.context replica.actor
  for entry in state.localKeys do
    counter := counter + 1
    snapshot := { snapshot with entries := snapshot.entries.push ⟨⟨replica.actor, counter⟩, entry.topic, entry.key, entry.metadata, entry.connection⟩ }
  -- An empty replica still advances causal knowledge on heartbeat/removal.
  counter := max (clock snapshot.context replica.actor + 1) counter
  snapshot := { snapshot with context := setClock snapshot.context replica.actor counter }
  snapshot.validate replica.limits
  return { state with snapshot }

/-- Track each connection independently, allowing one user key on many local
sockets and nodes. Closing one connection does not remove another connection. -/
def Replica.trackConnection (replica : Replica) (topic key connection : String)
    (metadata : Lean.Json := .null) : IO (Except String Unit) := do
  if !validName topic || !validName key || !validName connection || metadata.compress.utf8ByteSize > replica.limits.maxMetadataBytes then
    return .error "invalid presence topic/key/connection/metadata"
  let state ← replica.state.get
  let localKeys := (state.localKeys.filter fun entry => entry.topic != topic || entry.key != key || entry.connection != connection).push ⟨topic, key, connection, metadata⟩
  match refreshLocal replica { state with localKeys } with
  | .error error => return .error error
  | .ok state => replica.state.set state; return .ok ()

def Replica.track (replica : Replica) (topic key : String) (metadata : Lean.Json := .null) : IO (Except String Unit) :=
  replica.trackConnection topic key "default" metadata

def Replica.untrackConnection (replica : Replica) (topic key connection : String) : IO (Except String Unit) := do
  let state ← replica.state.get
  match refreshLocal replica { state with localKeys := state.localKeys.filter fun entry => entry.topic != topic || entry.key != key || entry.connection != connection } with
  | .error error => return .error error
  | .ok state => replica.state.set state; return .ok ()

def Replica.untrack (replica : Replica) (topic key : String) : IO (Except String Unit) := do
  let state ← replica.state.get
  match refreshLocal replica { state with localKeys := state.localKeys.filter fun entry => entry.topic != topic || entry.key != key } with
  | .error error => return .error error
  | .ok state => replica.state.set state; return .ok ()

def Replica.snapshot (replica : Replica) : IO Snapshot := do return (← replica.state.get).snapshot

def Replica.merge (replica : Replica) (snapshot : Snapshot) (now : Nat) : IO (Except String Unit) := do
  let state ← replica.state.get
  match state.snapshot.merge snapshot replica.limits with
  | .error error => return .error error
  | .ok merged =>
    let mut lastSeen := state.lastSeen
    for (actor, counter) in snapshot.context do
      -- Replayed stale snapshots must not extend a disconnected node's lease.
      if actor != replica.actor && counter > clock state.snapshot.context actor then
        lastSeen := (lastSeen.filter (·.1 != actor)).push (actor, now)
    replica.state.set { state with snapshot := merged, lastSeen }
    return .ok ()

/-- Call from the reactor maintenance hook. Some snapshot means broadcast it;
none means no heartbeat is due. Remote lease expiry records observed removal. -/
def Replica.tick (replica : Replica) (now : Nat) : IO (Except String (Option Snapshot)) := do
  let state ← replica.state.get
  let entries := state.snapshot.entries.filter fun entry => entry.dot.actor == replica.actor ||
    state.lastSeen.any (fun (actor, seenAt) => actor == entry.dot.actor && now - seenAt < replica.limits.leaseMs)
  let state := { state with snapshot := { state.snapshot with entries } }
  if now < state.nextHeartbeat then replica.state.set state; return .ok none
  match refreshLocal replica { state with nextHeartbeat := now + replica.limits.heartbeatMs } with
  | .error error => return .error error
  | .ok state => replica.state.set state; return .ok (some state.snapshot)

/-- One metadata entry per live connection. Use users for a unique user list. -/
def Replica.list (replica : Replica) (topic : String) : IO (Array (String × Lean.Json)) := do
  return (← replica.snapshot).entries.filterMap fun entry =>
    if entry.topic == topic then some (entry.key, entry.metadata) else none

def Replica.users (replica : Replica) (topic : String) : IO (Array String) := do
  return (← replica.list topic).foldl (fun users (key, _) => if users.contains key then users else users.push key) #[]

/-- PubSub bridge with bounded anti-entropy. Snapshots are accepted only from
this application-controlled topic; enforce Redis namespace/access controls. -/
structure Cluster where
  replica : Replica
  private subscription : Realtime.Subscription
  publish : Realtime.Message → IO Bool
  topic : String
  maxMessagesPerTick : Nat := 16

def Cluster.new (replica : Replica) (broker : Realtime.Broker)
    (publish : Realtime.Message → IO Bool) (topic : String := "__presence") : IO Cluster := do
  let .ok subscription ← broker.subscribe topic | throw (IO.userError "presence subscription capacity exhausted")
  return ⟨replica, subscription, publish, topic, 16⟩

def Cluster.tick (cluster : Cluster) (now : Nat) : IO (Except String Unit) := do
  for _ in [:min 256 cluster.maxMessagesPerTick] do
    match ← cluster.subscription.poll with
    | none => break
    | some (.closed _) => return .error "presence subscription closed"
    | some (.message message) =>
      if message.event != "snapshot" then continue
      match Lean.fromJson? message.payload with
      | .error error => return .error error
      | .ok snapshot =>
        if let .error error ← cluster.replica.merge snapshot now then return .error error
  match ← cluster.replica.tick now with
  | .error error => return .error error
  | .ok none => return .ok ()
  | .ok (some snapshot) =>
    if !(← cluster.publish ⟨cluster.topic, "snapshot", Lean.toJson snapshot⟩) then
      return .error "presence broadcast capacity exhausted"
    return .ok ()

def Cluster.close (cluster : Cluster) : IO Unit := do
  let _ ← cluster.subscription.close
  pure ()

end LeanWeave.Presence
