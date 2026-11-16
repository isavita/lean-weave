import LeanWeave.Realtime.PubSub
import LeanWeaveRedis.Client

namespace LeanWeave.Redis.PubSub

structure Config where
  host : String := "127.0.0.1"
  port : UInt16 := 6379
  channel : String := "lean-weave:pubsub"
  maxMessageBytes : Nat := 16384
  maxPendingPublishes : Nat := 64
  maxPendingBytes : Nat := 1048576
  maxFramesPerTick : Nat := 32
  reconnectMs : Nat := 100
  connectTimeoutMs : Nat := 2000
  commandTimeoutMs : Nat := 2000
  deriving Inhabited

private structure Command where
  handle : Native.Handle
  deadline : Nat
  bytes : Nat
  received : ByteArray := ByteArray.empty

private structure State where
  subscriber : Option Native.Handle := none
  buffer : ByteArray := ByteArray.empty
  connected : Bool := false
  connectingUntil : Nat := 0
  retryAt : Nat := 0
  commands : Array Command := #[]
  pendingBytes : Nat := 0
  closed : Bool := false
  received : Nat := 0
  reconnects : Nat := 0

/-- One adapter per reactor worker. Only owned messages enter the shared local
broker; sockets stay on their worker. Redis PubSub is at-most-once: reconnect
loses transient messages. Presence periodically broadcasts full CRDT snapshots. -/
structure Adapter where
  private state : IO.Ref State
  broker : LeanWeave.Realtime.Broker
  config : Config

private def limits (config : Config) : Limits := {
  maxBulkBytes := config.maxMessageBytes
  maxRequestBytes := config.maxMessageBytes + 1024
  maxResponseBytes := config.maxMessageBytes + 2048
  maxElements := 32
  maxDepth := 4 }

def Adapter.new (broker : LeanWeave.Realtime.Broker) (config : Config := {}) : IO Adapter := do
  if config.host.isEmpty || config.port == 0 || config.channel.isEmpty || config.channel.utf8ByteSize > 256 ||
      config.maxMessageBytes == 0 || config.maxMessageBytes > 1048576 ||
      config.maxPendingPublishes == 0 || config.maxPendingPublishes > 1024 ||
      config.maxPendingBytes < config.maxMessageBytes || config.maxFramesPerTick == 0 ||
      config.maxFramesPerTick > 256 || config.reconnectMs == 0 || config.connectTimeoutMs == 0 ||
      config.commandTimeoutMs == 0 then throw (IO.userError "invalid Redis PubSub bounds")
  Native.init
  return ⟨← IO.mkRef {}, broker, config⟩

private def closeHandle (handle : Native.Handle) : IO Unit :=
  try Native.close handle catch _ => pure ()

private def Adapter.disconnected (adapter : Adapter) (now : Nat) : IO Unit := do
  let state ← adapter.state.get
  if let some handle := state.subscriber then closeHandle handle
  adapter.state.set { state with
    subscriber := none
    buffer := ByteArray.empty
    connected := false
    retryAt := now + adapter.config.reconnectMs }

/-- Explicit subscription partition; the next tick reconnects after backoff. -/
def Adapter.disconnect (adapter : Adapter) (now : Nat) : IO Unit := adapter.disconnected now

def Adapter.ready (adapter : Adapter) : IO Bool := do return (← adapter.state.get).connected

def Adapter.statistics (adapter : Adapter) : IO (Nat × Nat × Nat) := do
  let state ← adapter.state.get
  return (state.received, state.reconnects, state.commands.size)

/-- Queues one PUBLISH. Redis delivers it to this node too, avoiding duplicate
local delivery. True means queued, not remote application acknowledgement. -/
def Adapter.publish (adapter : Adapter) (message : LeanWeave.Realtime.Message) : IO Bool := do
  let bytes := message.encode
  let state ← adapter.state.get
  if state.closed || bytes.size > adapter.config.maxMessageBytes ||
      state.commands.size >= adapter.config.maxPendingPublishes ||
      bytes.size > adapter.config.maxPendingBytes - state.pendingBytes then return false
  let .ok query := encode #["PUBLISH".toUTF8, adapter.config.channel.toUTF8, bytes] (limits adapter.config)
    | return false
  if query.size > adapter.config.maxPendingBytes - state.pendingBytes then return false
  let handle ← try Native.start adapter.config.host adapter.config.port query catch _ => return false
  let now ← IO.monoMsNow
  adapter.state.set { state with
    commands := state.commands.push {
      handle
      deadline := now + adapter.config.commandTimeoutMs
      bytes := query.size }
    pendingBytes := state.pendingBytes + query.size }
  return true

private def valueText : Value → Option String
  | .bulk (some bytes) => String.fromUTF8? bytes
  | .simple text => some text
  | _ => none

private def Adapter.receiveValue (adapter : Adapter) (value : Value) : IO Unit := do
  match value with
  | .array values =>
    match values[0]?.bind valueText with
    | some "subscribe" => adapter.state.modify fun state => { state with connected := true }
    | some "message" =>
      if (values[1]?.bind valueText) != some adapter.config.channel then throw (IO.userError "Redis PubSub channel mismatch")
      let some (Value.bulk (some bytes)) := values[2]? | throw (IO.userError "Redis PubSub malformed payload")
      let .ok message := LeanWeave.Realtime.Message.decode bytes | throw (IO.userError "Redis PubSub invalid message")
      let .ok _ ← adapter.broker.publish message | throw (IO.userError "Redis PubSub local broker overloaded")
      adapter.state.modify fun state => { state with received := state.received + 1 }
    | _ => throw (IO.userError "unexpected Redis PubSub frame")
  | _ => throw (IO.userError "unexpected Redis PubSub reply")

/-- One read, maxFramesPerTick decodes, maxPendingPublishes command polls.
Call from owner-worker onTick. No DNS, waits or per-message tasks. -/
def Adapter.tick (adapter : Adapter) (now : Nat) : IO Unit := do
  let state ← adapter.state.get
  if state.closed then return
  if state.subscriber.isNone && now >= state.retryAt then
    let .ok query := encode #["SUBSCRIBE".toUTF8, adapter.config.channel.toUTF8] (limits adapter.config)
      | throw (IO.userError "invalid Redis subscription command")
    try
      let handle ← Native.start adapter.config.host adapter.config.port query
      adapter.state.modify fun state => { state with
        subscriber := some handle
        connectingUntil := now + adapter.config.connectTimeoutMs
        reconnects := state.reconnects + 1 }
    catch _ => adapter.disconnected now
  if let some handle := (← adapter.state.get).subscriber then
    try
      let state ← adapter.state.get
      if !state.connected && now >= state.connectingUntil then throw (IO.userError "Redis subscription connect timeout")
      let capacity := (limits adapter.config).maxResponseBytes - state.buffer.size
      if capacity == 0 then throw (IO.userError "Redis subscription buffer full")
      let bytes ← Native.poll handle capacity.toUSize
      adapter.state.modify fun state => { state with buffer := state.buffer ++ bytes }
      for _ in [:adapter.config.maxFramesPerTick] do
        let state ← adapter.state.get
        match parsePrefix state.buffer (limits adapter.config) with
        | .error error => throw (IO.userError error)
        | .ok none => break
        | .ok (some (value, used)) =>
          adapter.state.modify fun state => { state with buffer := state.buffer.extract used state.buffer.size }
          adapter.receiveValue value
    catch _ => adapter.disconnected now
  let commands := (← adapter.state.get).commands
  let mut pending := #[]
  let mut pendingBytes := 0
  for original in commands do
    let mut finished : Bool := now >= original.deadline
    let mut command := original
    if !finished then
      try
        let bytes ← Native.poll command.handle (1024 - command.received.size).toUSize
        command := { command with received := command.received ++ bytes }
        match parse command.received { (limits adapter.config) with maxResponseBytes := 1024, maxBulkBytes := 1024 } with
        | .error _ => finished := true
        | .ok (some _) => finished := true
        | .ok none => pure ()
      catch _ => finished := true
    if finished then closeHandle command.handle
    else
      pending := pending.push command
      pendingBytes := pendingBytes + command.bytes
  adapter.state.modify fun state => { state with commands := pending, pendingBytes }

def Adapter.close (adapter : Adapter) : IO Unit := do
  let state ← adapter.state.get
  adapter.state.set { state with closed := true, subscriber := none, commands := #[], buffer := ByteArray.empty, pendingBytes := 0 }
  if let some handle := state.subscriber then closeHandle handle
  for command in state.commands do closeHandle command.handle

end LeanWeave.Redis.PubSub
