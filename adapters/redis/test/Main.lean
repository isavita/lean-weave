import LeanWeaveRedis

open LeanWeave LeanWeave.Redis

private def publicPolicy : Cache.Policy :=
  { admit := some (fun ctx => pure (.cache ctx "public:redis:test")) }

private def admittedResponseKey (ctx : Context) : String :=
  LeanWeave.Redis.responseKey ctx "public:redis:test" publicPolicy

private def check (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw (IO.userError message)

private def protocol : IO Unit := do
  let binary := ByteArray.mk #[0, 255, 13, 10, 65]
  check ((encode #["SET".toUTF8, "x\r\nGET other".toUTF8, binary]).isOk) "binary-safe encoding"
  for count in [:11] do
    let bytes := "$5\r\nhello\r\n".toUTF8.extract 0 count
    check (match parse bytes with | .ok none => true | _ => false) "fragmented bulk prematurely completed"
  check (match parse "$5\r\nhello\r\n".toUTF8 with
    | .ok (some (.bulk (some value))) => value == "hello".toUTF8 | _ => false) "bulk decode"
  for bytes in #["$99999999\r\n", "*99999999\r\n", "+x\ny\r\n", "+OK\r\ntrailing", "$1\r\nxXX"] do
    check (match parse bytes.toUTF8 with | .error _ => true | _ => false) s!"malformed RESP accepted: {bytes}"
  check (match parse "*1\r\n*1\r\n*1\r\n+ok\r\n".toUTF8 { maxDepth := 2 } with
    | .error _ => true | _ => false) "nested array depth bound"
  check (match encode #[ByteArray.mk (Array.replicate 100 65)] { maxRequestBytes := 32 } with
    | .error _ => true | _ => false) "request byte bound"

private def drive (action : AppM α) (reservation : Nat := 131072) : IO (Except AppError α) := do
  let execution ← AppM.execute action reservation
  for _ in [:3000] do
    if let some value ← execution.poll then return value
    IO.sleep 1
  execution.cancel .timeout
  throw (IO.userError "Redis acceptance exceeded external 3-second deadline")

private def value (action : AppM α) : IO α := do
  match ← drive action with
  | .ok value => return value
  | .error error => throw (IO.userError s!"Redis action failed: {error.status}: {error.message}")

private def context (path : String) (host : String := "one.test")
    (extra : Array LeanNet.HTTP1.Header := #[]) : IO Context := do
  let request : LeanNet.HTTP1.Request := {
    method := "GET"
    target := path.toUTF8
    headers := #[{ name := "host", value := host.toUTF8 }] ++ extra
    body := ByteArray.empty }
  match Context.fromRequest request with
  | .ok context => return context
  | .error error => throw (IO.userError error.message)

private def cachedResponses (client : Client) : IO Unit := do
  let calls ← IO.mkRef (0 : Nat)
  let origin : Handler := fun _ => do
    calls.modify (· + 1)
    return (Response.text s!"render:{← calls.get}").withHeader "cache-control" "public, max-age=10"
  let handler := middleware client publicPolicy origin
  let ctx ← context "/cached"
  let first ← value (handler ctx)
  let second ← value (handler ctx)
  check (first.body == second.body && (← calls.get) == 1) "Redis response cache missed public fill"
  let cold ← context "/head-cache-miss"
  let headTag : LeanNet.HTTP1.Header := { name := "if-none-match", value := (Cache.etag ByteArray.empty).toUTF8 }
  let coldHead := { cold with request := { cold.request with method := "HEAD", headers := cold.request.headers.push headTag } }
  let headOrigin := middleware client publicPolicy (fun _ =>
    pure ((Response.text "").withHeader "cache-control" "public, max-age=10"))
  let headMiss ← value (headOrigin coldHead)
  check (headMiss.status == 200 && (headMiss.getHeader "etag").isNone)
    "Redis HEAD miss does not synthesize a validator from a HEAD-only body"
  let defaultCalls ← IO.mkRef (0 : Nat)
  let defaultHandler := middleware client {} (fun _ => do
    defaultCalls.modify (· + 1)
    return (Response.text "default").withHeader "cache-control" "public")
  let defaultCtx ← context "/no-admission"
  let _ ← value (defaultHandler defaultCtx)
  let _ ← value (defaultHandler defaultCtx)
  check ((← defaultCalls.get) == 2) "Redis default policy must bypass cache without admission"
  let allowed ← IO.mkRef true
  let accessPolicy : Cache.Policy := { admit := some (fun c => do
    unless ← allowed.get do throw { status := 403, message := "access revoked" }
    return .cache c "public:redis:access") }
  let accessHandler := middleware client accessPolicy (fun _ =>
    pure ((Response.text "access").withHeader "cache-control" "public, max-age=10"))
  let accessCtx ← context "/recheck-access"
  let _ ← value (accessHandler accessCtx)
  allowed.set false
  check (match ← drive (accessHandler accessCtx) with
    | .error error => error.status == 403
    | .ok _ => false) "Redis admission must recheck access before a cache hit"
  let tag := first.getHeader "etag"
  let conditional := { ctx with request := { ctx.request with
    headers := ctx.request.headers.push { name := "if-none-match", value := tag.getD "" |>.toUTF8 } } }
  let result ← value (handler conditional)
  check (result.status == 304 && result.body.isEmpty) "Redis conditional GET failed"
  let head := { ctx with request := { ctx.request with method := "HEAD" } }
  let result ← value (handler head)
  check (result.body == first.body && (← calls.get) == 1) "Redis HEAD failed to reuse GET metadata"
  let foreign ← context "/cached" "two.test"
  let _ ← value (handler foreign)
  let _ ← value (handler { ctx with tenant := "other" })
  let _ ← value (handler { ctx with locale := "fr" })
  let authenticated ← context "/cached" "one.test" #[{ name := "authorization", value := "secret".toUTF8 }]
  let _ ← value (handler authenticated)
  check ((← calls.get) == 5) "Redis host/tenant/locale/auth cache isolation failed"
  for marker in #["private", "no-store", "set-cookie", "vary-star", "vary-unknown"] do
    let privateCalls ← IO.mkRef (0 : Nat)
    let origin : Handler := fun _ => do
      privateCalls.modify (· + 1)
      let response := (Response.text "private").withHeader "cache-control" "public"
      return match marker with
        | "private" => response.withHeader "cache-control" "private"
        | "no-store" => response.withHeader "cache-control" "public, no-store"
        | "set-cookie" => response.withHeader "set-cookie" "session=opaque"
        | "vary-unknown" => response.withHeader "vary" "X-Device"
        | _ => response.withHeader "vary" "*"
    let handler := middleware client publicPolicy origin
    let ctx ← context ("/" ++ marker)
    let _ ← value (handler ctx)
    let _ ← value (handler ctx)
    check ((← privateCalls.get) == 2) s!"Redis cached forbidden response: {marker}"
  let fragments ← IO.mkRef (0 : Nat)
  let build : AppM ByteArray := do
    fragments.modify (· + 1)
    return "fragment".toUTF8
  let _ ← value (fragment client ctx "component" build publicPolicy)
  let _ ← value (fragment client ctx "component" build publicPolicy)
  check ((← fragments.get) == 1) "Redis fragment cache missed fill"
  -- Invalidate while a producer is suspended. Its stale token must not publish.
  let entered ← IO.mkRef false
  let ready ← IO.mkRef false
  let origin : Handler := fun _ => do
    entered.set true
    let _ ← AppM.await (do
      if ← ready.get then return some (.ok ())
      return none) (fun _ => pure ())
    return (Response.text "stale-fill").withHeader "cache-control" "public"
  let ctx ← context "/invalidate-in-flight"
  let execution ← AppM.execute ((middleware client publicPolicy origin) ctx) 131072
  for _ in [:1000] do
    let _ ← execution.poll
    if ← entered.get then break
    IO.sleep 1
  check (← entered.get) "cache producer never suspended"
  let _ ← value (client.delete (admittedResponseKey ctx))
  ready.set true
  let mut finished := false
  for _ in [:1000] do
    if (← execution.poll).isSome then finished := true; break
    IO.sleep 1
  check finished "invalidated producer failed to complete"
  check ((← value (client.get (admittedResponseKey ctx))).isNone) "invalidated producer repopulated Redis"

def main (args : List String) : IO Unit := do
  protocol
  if args == ["--protocol-only"] then
    IO.println "Redis RESP protocol bounds passed"
    return
  let some port := args[0]?.bind String.toNat? | throw (IO.userError "usage: test PORT REFUSED_PORT")
  let some refused := args[1]?.bind String.toNat? | throw (IO.userError "missing refused port")
  let client ← Client.new { port := port.toUInt16, namespacePrefix := "weave-test:" }
  let other ← Client.new { port := port.toUInt16, namespacePrefix := "other-test:" }
  let binary := ByteArray.mk #[0, 255, 13, 10, 65]
  value (client.set "binary" binary 10000)
  check ((← value (client.get "binary")) == some binary) "binary Redis round-trip"
  check ((← value (other.get "binary")).isNone) "Redis namespace isolation"
  check (← value (client.delete "binary")) "explicit Redis invalidation"
  check ((← value (client.get "binary")).isNone) "invalidated key survived"
  value (client.set "expiry" "short".toUTF8 30)
  IO.sleep 60
  check ((← value (client.get "expiry")).isNone) "Redis finite TTL expiration"
  cachedResponses client
  let ownerA := "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  let ownerB := "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
  check (← value (client.acquire "fill" ownerA 5000)) "first fill lease failed"
  check (!(← value (client.acquire "fill" ownerB 5000))) "fill lease allowed simultaneous owner"
  check (!(← value (client.release "fill" ownerB))) "wrong owner released fill lease"
  check (← value (client.release "fill" ownerA)) "lease owner could not release"
  check (← value (client.acquire "fill" ownerB 20)) "second owner failed to acquire"
  check (!(← value (client.release "fill" ownerA))) "stale owner released replacement lease"
  IO.sleep 40
  check (← value (client.acquire "fill" ownerA 5000)) "lease TTL failed to release owner"
  let _ ← value (client.release "fill" ownerA)
  let bounded ← Client.new {
    port := port.toUInt16, timeoutMs := 50, maxConcurrent := 1, namespacePrefix := "weave-test:" }
  let first ← AppM.execute (bounded.command #["BLPOP".toUTF8, "weave-test:wait".toUTF8, "0".toUTF8])
  check ((← bounded.pending) == 1) "pending socket capacity not charged"
  check (match ← drive (bounded.get "x") with
    | .error error => error.status == 503 | _ => false) "concurrent socket bound failed"
  first.cancel .disconnected
  first.cancel .timeout
  check ((← bounded.pending) == 0) "disconnect cancellation failed to release one slot"
  check (match ← drive (bounded.command #["BLPOP".toUTF8, "weave-test:wait".toUTF8, "0".toUTF8]) with
    | .error error => error.status == 504 | _ => false) "Redis command timeout failed"
  check ((← bounded.pending) == 0) "timeout leaked socket capacity"
  check (match ← drive (bounded.get "x") 1 with
    | .error error => error.status == 503 | _ => false) "execution buffer reservation failed"
  check ((← bounded.pending) == 0) "buffer rejection leaked socket capacity"
  let invalid ← Client.new { port := port.toUInt16, host := "localhost" }
  check (match ← drive (invalid.get "x") with
    | .error error => error.status == 503 | _ => false) "nonnumeric DNS host accepted"
  check ((← invalid.pending) == 0) "invalid host leaked capacity"
  let unavailable ← Client.new { port := refused.toUInt16, timeoutMs := 100 }
  check (match ← drive (unavailable.get "x") with
    | .error error => error.status == 503 | _ => false) "connection refusal not surfaced"
  check ((← unavailable.pending) == 0) "connection refusal leaked capacity"
  for _ in [:300] do
    check ((← value (client.get "binary")).isNone) "command churn returned stale cache data"
  check ((← client.pending) == 0) "completed commands leaked capacity"
  IO.println "Redis protocol, real-server TTL/isolation/invalidation, bounds, timeout, cancellation, refusal and churn passed"
