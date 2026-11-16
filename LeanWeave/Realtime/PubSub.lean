import LeanWeave.Core.Types
import LeanWeave.Core.Body
import Std.Sync.Mutex

namespace LeanWeave.Realtime

structure Message where
  topic : String
  event : String
  payload : Lean.Json
  deriving Inhabited

def Message.encode (message : Message) : ByteArray :=
  (Lean.Json.mkObj [("topic", .str message.topic), ("event", .str message.event),
    ("payload", message.payload)]).compress.toUTF8

def Message.decode (bytes : ByteArray) : Except String Message := do
  let json ← (Body.json bytes { maxBytes := 1048576 }).mapError (·.message)
  return ⟨← json.getObjValAs? String "topic", ← json.getObjValAs? String "event", ← json.getObjVal? "payload"⟩

structure Limits where
  maxSubscribers : Nat := 1024
  maxTopics : Nat := 256
  maxMessages : Nat := 64
  maxBytes : Nat := 262144
  maxMessageBytes : Nat := 16384
  subscriptionLeaseMs : Nat := 60000
  deriving Inhabited

inductive CloseReason where
  | expired | overflow | closed
  deriving BEq, Repr, Inhabited

inductive Delivery where
  | message (value : Message)
  | closed (reason : CloseReason)

private structure Slot where
  id : Nat
  topic : String
  queue : Array (Message × Nat) := #[]
  bytes : Nat := 0
  expires : Nat
  closed : Option CloseReason := none
  deriving Inhabited

private structure State where
  slots : Array Slot := #[]
  nextId : Nat := 0

/-- Owned immutable message values may cross workers. The bounded shared store
uses try-lock only: contention is explicit admission failure/pending, never a
reactor-blocking lock. No application callback executes under the lock. -/
structure Broker where
  private state : Std.Mutex State
  limits : Limits

structure Subscription where
  private broker : Broker
  private id : Nat

def Broker.new (limits : Limits := {}) : IO Broker := do
  if limits.maxSubscribers == 0 || limits.maxSubscribers > 65536 || limits.maxTopics == 0 ||
      limits.maxMessages == 0 || limits.maxBytes == 0 || limits.maxMessageBytes == 0 ||
      limits.maxMessageBytes > limits.maxBytes || limits.maxMessageBytes > 1048576 ||
      limits.subscriptionLeaseMs == 0 then
    throw (IO.userError "invalid bounded PubSub configuration")
  return ⟨← Std.Mutex.new {}, limits⟩

private def validName (value : String) : Bool :=
  !value.isEmpty && value.utf8ByteSize <= 256 && value.toUTF8.data.all (fun b => b >= 32 && b != 127)

def Broker.subscribe (broker : Broker) (topic : String) : IO (Except String Subscription) := do
  if !validName topic then return .error "invalid PubSub topic"
  let now ← IO.monoMsNow
  let result : Option (Except String Nat) ← broker.state.tryAtomically do
    let state ← get
    let slots := state.slots.filter (fun slot => now < slot.expires)
    if slots.size >= broker.limits.maxSubscribers then return .error "PubSub subscriber capacity exhausted"
    let topics := slots.foldl (fun (topics : Array String) slot => if topics.contains slot.topic then topics else topics.push slot.topic) #[]
    if !topics.contains topic && topics.size >= broker.limits.maxTopics then return .error "PubSub topic capacity exhausted"
    let id := state.nextId + 1
    set { slots := slots.push { id, topic, expires := now + broker.limits.subscriptionLeaseMs }, nextId := id : State }
    return .ok id
  return match result with
    | none => .error "PubSub store busy"
    | some (.error error) => .error error
    | some (.ok id) => .ok ⟨broker, id⟩

/-- At-most-once local fanout. A slow subscriber is closed, never silently
skipped; callers can reconnect/replay using an application event log. -/
def Broker.publish (broker : Broker) (message : Message) : IO (Except String Nat) := do
  if !validName message.topic || !validName message.event then return .error "invalid PubSub topic/event"
  let size := message.encode.size
  if size > broker.limits.maxMessageBytes then return .error "PubSub message exceeds byte bound"
  let now ← IO.monoMsNow
  let result ← broker.state.tryAtomically do
    let state ← get
    let mut count := 0
    let mut slots := #[]
    for slot in state.slots do
      if now >= slot.expires then continue
      if slot.topic != message.topic || slot.closed.isSome then
        slots := slots.push slot
      else if slot.queue.size >= broker.limits.maxMessages || size > broker.limits.maxBytes - slot.bytes then
        slots := slots.push { slot with queue := #[], bytes := 0, closed := some .overflow }
      else
        count := count + 1
        slots := slots.push { slot with queue := slot.queue.push (message, size), bytes := slot.bytes + size }
    set { state with slots }
    return count
  return match result with | none => .error "PubSub store busy" | some count => .ok count

/-- Poll renews a finite subscription lease. Busy is pending; an expired or
slow subscription is a terminal result. Each poll consumes one message only. -/
def Subscription.poll (sub : Subscription) : IO (Option Delivery) := do
  let now ← IO.monoMsNow
  let result : Option (Option Delivery) ← sub.broker.state.tryAtomically do
    let state ← get
    let some index := state.slots.findIdx? (·.id == sub.id) | return some (.closed .closed)
    let slot := state.slots[index]!
    if now >= slot.expires then
      set { state with slots := state.slots.filter (·.id != sub.id) }
      return some (.closed .expired)
    if let some reason := slot.closed then
      set { state with slots := state.slots.filter (·.id != sub.id) }
      return some (.closed reason)
    let renewed := { slot with expires := now + sub.broker.limits.subscriptionLeaseMs }
    if let some (message, size) := slot.queue[0]? then
      set { state with slots := state.slots.set! index { renewed with queue := slot.queue.extract 1 slot.queue.size, bytes := slot.bytes - size } }
      return some (.message message)
    set { state with slots := state.slots.set! index renewed }
    return none
  return result.join

/-- False means try-lock contention; retry from maintenance if immediate slot
reclamation matters. The finite lease bounds abandoned subscriptions anyway. -/
def Subscription.close (sub : Subscription) : IO Bool := do
  return (← sub.broker.state.tryAtomically do
    modify fun state => { state with slots := state.slots.filter (·.id != sub.id) }).isSome

end LeanWeave.Realtime
