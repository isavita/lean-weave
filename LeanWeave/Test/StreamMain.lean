import LeanWeave.Stream
import LeanWeave.SSE
import LeanWeave.Realtime
import LeanWeave.Presence

open LeanWeave

private def check (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw (IO.userError message)

private def ok (value : Except String α) : IO α :=
  match value with | .ok value => pure value | .error error => throw (IO.userError error)

private def drain (body : Stream.Body) (budget : Nat := 7) : IO ByteArray := do
  let mut bytes := ByteArray.empty
  for _ in [:1000] do
    match ← body.poll budget with
    | none => pure ()
    | some (.data chunk) =>
      check (!chunk.isEmpty && chunk.size <= budget) "producer violated budget"
      bytes := bytes ++ chunk
    | some (.finish _) => body.close none; return bytes
  throw (IO.userError "stream failed to finish")

private def streams : IO Unit := do
  let closed ← IO.mkRef (0 : Nat)
  let body ← Stream.chunks #["abc".toUTF8, "def".toUTF8] #[] (fun _ => closed.modify (· + 1))
  check ((← drain body 2) == "abcdef".toUTF8) "binary stream splitting"
  body.close (some .timeout)
  check ((← closed.get) == 1) "stream cleanup was not once"
  let queue ← Stream.Queue.new { maxChunks := 1, maxBytes := 4, maxChunkBytes := 4 }
  check (← queue.push "1234".toUTF8) "queue rejected capacity"
  check (!(← queue.push "x".toUTF8)) "queue overflow accepted"
  let _ ← queue.finish
  check ((← drain queue.body 1) == "1234".toUTF8) "queue lost data"
  let first ← Stream.chunks #["one".toUTF8]
  let second ← Stream.chunks #["two".toUTF8]
  let multipart ← Stream.multipart "test" #[⟨#[{ name := "content-type", value := "text/plain".toUTF8 }], first⟩, ⟨#[], second⟩]
  let bytes ← drain multipart 3
  check (bytes == "--test\r\ncontent-type: text/plain\r\n\r\none\r\n--test\r\n\r\ntwo\r\n--test--\r\n".toUTF8) "multipart framing"

private def sse : IO Unit := do
  check (!(SSE.Event.mk "x" (some "bad\nid") none none).encode.isOk) "SSE field injection accepted"
  let encoded ← ok (SSE.Event.mk "one\r\ntwo\revent: injected" (some "7") (some "update") (some 1000)).encode
  check (String.fromUTF8! encoded == "id: 7\nevent: update\nretry: 1000\ndata: one\ndata: two\ndata: event: injected\n\n") "SSE multiline escaping"
  let store ← SSE.Store.new { maxEvents := 2, maxBytes := 4096, maxEventBytes := 1024 }
  let one ← ok (← store.publish ⟨"one", none, none, none⟩)
  let _ ← ok (← store.publish ⟨"two", none, none, none⟩)
  let _ ← ok (← store.publish ⟨"three", none, none, none⟩)
  let ctx : Context := { request := { method := "GET", target := "/".toUTF8, headers := #[{ name := "last-event-id", value := one.toUTF8 }], body := ByteArray.empty } }
  let response ← store.response ctx
  let some body := response.stream | throw (IO.userError "SSE missing stream")
  let _ ← body.poll 4096
  let reset ← body.poll 4096
  check (match reset with | some (.data bytes) => (String.fromUTF8! bytes).startsWith "event: reset" | _ => false) "SSE replay gap missing"
  body.close none

private def pubsub : IO Unit := do
  let broker ← Realtime.Broker.new { maxSubscribers := 2, maxMessages := 1, maxBytes := 1024, maxMessageBytes := 512 }
  let one ← ok (← broker.subscribe "one")
  let two ← ok (← broker.subscribe "two")
  check (!(← broker.subscribe "three").isOk) "PubSub subscriber bound"
  let _ ← ok (← broker.publish ⟨"one", "change", .str "hello"⟩)
  check (match ← one.poll with | some (.message m) => m.payload == .str "hello" | _ => false) "local PubSub delivery"
  check (← two.poll).isNone "PubSub topic isolation"
  let _ ← ok (← broker.publish ⟨"one", "change", .null⟩)
  let _ ← ok (← broker.publish ⟨"one", "change", .null⟩)
  check (match ← one.poll with | some (.closed .overflow) => true | _ => false) "slow subscriber not closed"
  let _ ← two.close

  let channel : Realtime.Channel := { broker, authorize := fun ctx topic => pure (ctx.authenticated && topic == "room") }
  let ctx : Context := { request := { method := "GET",target := "/".toUTF8,headers := #[],body := ByteArray.empty } }
  check (!(← channel.join ctx "room").isOk) "unauthenticated channel join accepted"
  check (!(← channel.sse ctx "room").isOk) "unauthenticated SSE channel accepted"
  check (!(← channel.webSocket ctx "room").isOk) "unauthenticated WS channel accepted"
  let sub ← ok (← channel.join { ctx with authenticated := true } "room")
  let localReceipt ← ok (← channel.broadcast "room" "change" (42 : Nat))
  check (localReceipt.recipients == some 1) "local channel publication receipt"
  check (match ← sub.poll with | some (.message m) => m.payload == Lean.toJson (42 : Nat) | _ => false) "local channel typed payload"
  let sent ← IO.mkRef (#[] : Array Realtime.Message)
  let custom := { channel with publisher := some (fun message => do sent.modify (·.push message); return .ok {}) }
  let queued ← ok (← custom.broadcast "room" "remote" (43 : Nat))
  check queued.recipients.isNone "distributed receipt invented recipient count"
  check ((← sent.get).size == 1 && (← sent.get)[0]!.payload == Lean.toJson (43 : Nat)) "custom publisher not selected"
  check (← sub.poll).isNone "custom publish duplicated immediate local fanout"
  check (!(← custom.broadcast "" "remote" ()).isOk) "custom publisher bypassed topic validation"
  check (!(← custom.broadcast "room" "bad\nevent" ()).isOk) "custom publisher bypassed event validation"
  check (!(← custom.broadcast "room" "remote" (String.ofList (List.replicate 1024 'x'))).isOk) "custom publisher bypassed byte budget"
  check ((← sent.get).size == 1) "invalid message reached custom publisher"
  let rejecting := { channel with publisher := some (fun _ => pure (.error "transport full")) }
  check (!(← rejecting.broadcast "room" "remote" ()).isOk) "publisher rejection silently fell back to local"
  check (← sub.poll).isNone "rejected publish produced local delivery"
  let _ ← sub.close

private def present (replica : Presence.Replica) (key : String) : IO Bool := do
  return (← replica.list "room").any (·.1 == key)

private def presence : IO Unit := do
  let limits : Presence.Limits := { heartbeatMs := 10, leaseMs := 30 }
  let a ← Presence.Replica.new "A" limits
  let b ← Presence.Replica.new "B" limits
  let _ ← ok (← a.track "room" "alice")
  let _ ← ok (← b.track "room" "bob")
  let sa ← a.snapshot
  let sb ← b.snapshot
  let _ ← ok (← a.merge sb 0)
  let _ ← ok (← b.merge sa 0)
  check ((← present a "bob") && (← present b "alice")) "presence join convergence"
  let _ ← ok (← a.tick 31)
  check (!(← present a "bob")) "partitioned presence lease did not expire"
  let _ ← ok (← b.tick 40)
  let _ ← ok (← a.merge (← b.snapshot) 40)
  check (← present a "bob") "partition rejoin failed"
  let stale ← b.snapshot
  let _ ← ok (← b.untrack "room" "bob")
  let _ ← ok (← a.merge (← b.snapshot) 41)
  let _ ← ok (← a.merge stale 42)
  check (!(← present a "bob")) "observed removal resurrected by stale snapshot"
  let _ ← ok (← b.merge (← a.snapshot) 42)
  check (!(← present b "bob")) "leave did not converge"
  let _ ← ok (← b.trackConnection "room" "bob" "socket-one")
  let _ ← ok (← b.trackConnection "room" "bob" "socket-two")
  let _ ← ok (← b.untrackConnection "room" "bob" "socket-one")
  check (← present b "bob") "closing one connection removed another"
  check (((← b.users "room").filter (· == "bob")).size == 1) "presence user grouping"
  let _ ← ok (← b.untrack "room" "bob")
  check (!(← present b "bob")) "local user leave retained a connection"
  let merged ← ok ((← a.snapshot).merge (← a.snapshot) limits)
  check (merged.entries.size == (← a.snapshot).entries.size) "CRDT idempotence"
  let tight ← Presence.Replica.new "only" { limits with maxActors := 1 }
  let _ ← ok (← tight.track "room" "local")
  check (!(← tight.merge sb 0).isOk) "bounded causal context accepted extra actor"

def main : IO Unit := do
  streams
  sse
  pubsub
  presence
  IO.println "Stream/SSE/PubSub/CRDT presence tests passed"
