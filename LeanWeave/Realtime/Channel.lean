import LeanWeave.Realtime.PubSub
import LeanWeave.SSE
import LeanNet.WebSocket.Application

namespace LeanWeave.Realtime

/-- Successful admission receipt, not an end-to-end delivery acknowledgement.
Local fanout can report the number of subscriber queues that accepted a message;
distributed transports may know only that an outgoing publication was queued. -/
structure Publication where
  recipients : Option Nat := none
  deriving Repr, BEq, Inhabited

/-- A nonblocking owner-local transport publisher. Exceptions/rejection must not
be interpreted as a remote acknowledgement or silently fall back to local fanout. -/
abbrev Publisher := Message → IO (Except String Publication)

def Message.validate (message : Message) (maximumBytes : Nat) : Except String Unit := do
  let validName := fun (value : String) => !value.isEmpty && value.utf8ByteSize <= 256 &&
    value.toUTF8.data.all (fun b => b >= 32 && b != 127)
  if !validName message.topic || !validName message.event then throw "invalid channel topic/event"
  if message.encode.size > maximumBytes then throw "channel message exceeds byte bound"

def Broker.publisher (broker : Broker) : Publisher := fun message => do
  return (← broker.publish message).map fun count => { recipients := some count }

/-- Authorization is checked before joining either transport. The callback
must use verified context/session claims rather than a client-supplied topic. -/
structure Channel where
  broker : Broker
  authorize : Context → String → IO Bool
  /-- Select local or distributed publication independently of WS/SSE delivery.
      None preserves local Broker.publish behavior. Construct per transport owner. -/
  publisher : Option Publisher := none

def Channel.join (channel : Channel) (ctx : Context) (topic : String) : IO (Except String Subscription) := do
  if !(← channel.authorize ctx topic) then return .error "channel authorization denied"
  channel.broker.subscribe topic

/-- Trusted server-side producers select one publication transport. Authorization
for incoming WS/SSE joins remains independent; validate command authorization
before calling broadcast on behalf of a client. Bounds apply before custom hooks. -/
def Channel.publish (channel : Channel) (message : Message) : IO (Except String Publication) := do
  if let .error error := message.validate channel.broker.limits.maxMessageBytes then return .error error
  try (channel.publisher.getD channel.broker.publisher) message
  catch _ => return .error "channel publisher failed"

/-- Typed transport-independent publishing. A queued Redis publication has no
known recipient count; a local publication reports accepted subscriber queues. -/
def Channel.broadcast [Lean.ToJson α] (channel : Channel) (topic event : String) (value : α) : IO (Except String Publication) :=
  channel.publish ⟨topic, event, Lean.toJson value⟩

def Channel.webSocket (channel : Channel) (ctx : Context) (topic : String)
    (receive : Message → IO (Option Message) := fun _ => pure none)
    (keepAliveMs : Nat := 15000) :
    IO (Except String LeanNet.WebSocket.Callbacks) := do
  if keepAliveMs == 0 then return .error "invalid channel keepalive"
  let .ok sub ← channel.join ctx topic | return .error "channel authorization/capacity denied"
  let emitted ← IO.mkRef (← IO.monoMsNow)
  return .ok {
    onMessage := fun opcode bytes => do
      if opcode != .text || bytes.size > channel.broker.limits.maxMessageBytes then
        throw (IO.userError "invalid channel WebSocket message")
      let .ok message := Message.decode bytes | throw (IO.userError "invalid channel JSON")
      if message.topic != topic then throw (IO.userError "channel topic mismatch")
      let reply ← receive message
      return reply.map fun message => { opcode := .text, payload := message.encode }
    onPoll := do
      match ← sub.poll with
      | none =>
        let now ← IO.monoMsNow
        if now - (← emitted.get) < keepAliveMs then return none
        emitted.set now
        return some { opcode := .ping, payload := ByteArray.empty }
      | some (.closed reason) => throw (IO.userError s!"channel closed: {reprStr reason}")
      | some (.message message) =>
        emitted.set (← IO.monoMsNow)
        return some { opcode := .text, payload := message.encode }
    onClosed := fun _ => do let _ ← sub.close; pure () }

/-- SSE receives the same channel messages; send commands over authenticated
HTTP endpoints. Last-Event-ID replay belongs to `SSE.Store`, not ephemeral PubSub. -/
def Channel.sse (channel : Channel) (ctx : Context) (topic : String)
    (keepAliveMs : Nat := 15000) : IO (Except String Response) := do
  if keepAliveMs == 0 then return .error "invalid channel keepalive"
  let .ok sub ← channel.join ctx topic | return .error "channel authorization/capacity denied"
  let pending ← IO.mkRef ByteArray.empty
  let emitted ← IO.mkRef (← IO.monoMsNow)
  let body : Stream.Body := {
    retainedBytes := channel.broker.limits.maxMessageBytes + 1024
    idleTimeoutMs := keepAliveMs * 3
    poll := fun budget => do
      if (← pending.get).isEmpty then
        match ← sub.poll with
        | some (.closed reason) => throw (IO.userError s!"channel closed: {reprStr reason}")
        | some (.message message) =>
          let .ok bytes := (SSE.Event.mk (String.fromUTF8! message.encode) none (some "message") none).encode
            | throw (IO.userError "invalid channel event")
          pending.set bytes
        | none =>
          if (← IO.monoMsNow) - (← emitted.get) < keepAliveMs then return none
          pending.set ": keep-alive\n\n".toUTF8
      if budget == 0 then return none
      let bytes ← pending.get
      let count := min budget bytes.size
      pending.set (bytes.extract count bytes.size)
      emitted.set (← IO.monoMsNow)
      return some (.data (bytes.extract 0 count))
    close := fun _ => do let _ ← sub.close; pending.set ByteArray.empty }
  return .ok ((Response.streaming body).withHeader "content-type" "text/event-stream"
    |>.withHeader "cache-control" "no-store")

end LeanWeave.Realtime
