import LeanWeave.Realtime.PubSub
import LeanWeave.SSE
import LeanNet.WebSocket.Application

namespace LeanWeave.Realtime

/-- Authorization is checked before joining either transport. The callback
must use verified context/session claims rather than a client-supplied topic. -/
structure Channel where
  broker : Broker
  authorize : Context → String → IO Bool

def Channel.join (channel : Channel) (ctx : Context) (topic : String) : IO (Except String Subscription) := do
  if !(← channel.authorize ctx topic) then return .error "channel authorization denied"
  channel.broker.subscribe topic

/-- A channel is transport-independent; typed publishers serialize explicitly. -/
def Channel.broadcast [Lean.ToJson α] (channel : Channel) (topic event : String) (value : α) : IO (Except String Nat) :=
  channel.broker.publish ⟨topic, event, Lean.toJson value⟩

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
