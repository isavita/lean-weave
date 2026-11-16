import LeanWeave.Core.Server
import LeanWeave.Presence
import LeanWeave.Realtime.Channel
import LeanWeaveRedis.PubSub

open LeanWeave

private def query (ctx : Context) (key : String) (fallback : String := "") : String :=
  ((ctx.query.find? (·.1 == key)).map (·.2)).getD fallback

private def application (port : UInt16) (actor : String) : IO Server.Application := do
  let broker ← Realtime.Broker.new { maxMessages := 128, maxMessageBytes := 32768 }
  let adapter ← Redis.PubSub.Adapter.new broker { port, channel := "leanweave:acceptance:pubsub", maxMessageBytes := 32768, reconnectMs := 10 }
  let replica ← Presence.Replica.new actor { maxEntries := 64, maxSnapshotBytes := 16384, leaseMs := 120, heartbeatMs := 20 }
  let cluster ← Presence.Cluster.new replica broker adapter.publish
  let channel := adapter.channel (fun _ _ => pure true)
  let partitionUntil ← IO.mkRef (0 : Nat)
  let handler : Handler := fun ctx => do
    let path := ctx.path.toList
    if path == ["ready"] then return Response.text (if ← adapter.ready then "ready" else "pending")
    if path == ["partition"] then
      let now ← IO.monoMsNow
      partitionUntil.set (now + 350)
      adapter.disconnect now
      return Response.text "partitioned"
    if path == ["broadcast"] then
      match ← channel.broadcast "room" "message" (query ctx "value") with
      | .error message => throw { status := 503,message }
      | .ok receipt => return Response.json (Lean.Json.mkObj [
          ("queued",Lean.toJson true),("recipients",Lean.toJson receipt.recipients)])
    if path == ["events"] then
      match ← channel.sse ctx "room" 30 with
      | .ok response => return response
      | .error message => throw { status := 503, message }
    if path == ["track"] || path == ["untrack"] then
      let result ← if path == ["track"] then replica.track "room" (query ctx "key")
        else replica.untrack "room" (query ctx "key")
      match result with
      | .ok () => return Response.text "ok"
      | .error message => throw { status := 503, message }
    if path == ["members"] then
      return Response.json (Lean.toJson ((← replica.list "room").map (·.1)))
    if path == ["stats"] then
      let (received, reconnects, pending) ← adapter.statistics
      return Response.json (Lean.Json.mkObj [("received", Lean.toJson received), ("reconnects", Lean.toJson reconnects), ("pending", Lean.toJson pending)])
    return Response.text "unknown" 404
  return {
    handler
    tickIntervalMs := 2
    onTick := fun now => do
      if now < (← partitionUntil.get) then return
      adapter.tick now
      let .ok () ← cluster.tick now | pure ()
    onShutdown := do cluster.close; adapter.close }

def main (args : List String) : IO Unit := do
  let some http := args[0]?.bind String.toNat? | throw (IO.userError "HTTP_PORT REDIS_PORT ACTOR")
  let some redis := args[1]?.bind String.toNat? | throw (IO.userError "HTTP_PORT REDIS_PORT ACTOR")
  let actor := args[2]?.getD "node"
  Server.serveApplications (fun _ use => do use (← application redis.toUInt16 actor)) {
    port := http.toUInt16
    net := { workers := 1, shutdownDrainMs := 100 } }
