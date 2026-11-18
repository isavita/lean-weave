import LeanWeave
import LeanWeave.Test.Client

namespace LeanWeave.Test

private def check (value : Bool) (message : String) : IO Unit :=
  unless value do throw (IO.userError s!"security: {message}")

private def ok (value : Except AppError α) : IO α :=
  match value with
  | .ok value => pure value
  | .error error => throw (IO.userError error.message)

private def ctx (target : String := "/") : IO Context := do
  match context "GET" target with
  | .ok ctx => return ctx
  | .error error => throw (IO.userError error.message)

private def header (name value : String) : LeanNet.HTTP1.Header := { name, value := value.toUTF8 }

private def status (handler : Handler) (context : Context) : IO Nat := do
  match ← (Middleware.errors handler context).run with
  | .ok response => return response.status
  | .error _ => return 0

private def responseStoreTests : IO Unit := do
  let context ← ctx
  let cacheNow ← IO.monoMsNow
  let keyA := Cache.CacheKey.ofParts #["slru", "a"]
  let keyB := Cache.CacheKey.ofParts #["slru", "b"]
  let keyC := Cache.CacheKey.ofParts #["slru", "c"]
  let keyD := Cache.CacheKey.ofParts #["slru", "d"]
  let keyE := Cache.CacheKey.ofParts #["slru", "e"]
  let mkEntry := fun (base : Cache.CacheKey) (body : ByteArray) (storedAt : Nat) (ttlMs : Nat) =>
    ({ status := 200, headers := #[], body, ttlMs, storedAt, baseKey := base,
       varyNames := #[], varyFingerprint := Cache.CacheKey.ofParts #[] } : Cache.CacheEntry)
  let slruLimits : Cache.ResponseLimits := { maxEntries := 3, maxBytes := 4096, maxEntryBytes := 1024, protectedPercent := 34 }
  let slru ← Cache.ResponseStore.new slruLimits
  check (← slru.storeChecked keyA (mkEntry keyA "A".toUTF8 cacheNow 60000)) "SLRU first insert"
  check ((← slru.lookup keyA).isSome) "SLRU promotes a hot entry"
  for key in #[keyB, keyC, keyD, keyE] do
    check (← slru.storeChecked key (mkEntry key "cold".toUTF8 cacheNow 60000)) "SLRU cold insert"
  check ((← slru.lookup keyA).isSome) "SLRU protects hot entry from cold scan"
  let mut live := 0
  for key in #[keyA, keyB, keyC, keyD, keyE] do
    if (← slru.lookup key).isSome then live := live + 1
  check (live <= 3) "response store hard item count"
  let slruStats ← slru.stats
  check (slruStats.entries <= 3 && slruStats.bytes <= 4096) "SLRU enforces count and byte bounds"
  let uneven ← Cache.ResponseStore.new { maxBytes := 2048, maxEntryBytes := 2048, protectedPercent := 50 }
  uneven.store keyA (mkEntry keyA "hot".toUTF8 cacheNow 60000)
  let _ ← uneven.lookup keyA
  uneven.store keyB (mkEntry keyB (ByteArray.mk (Array.replicate 1000 65)) cacheNow 60000)
  let _ ← uneven.lookup keyB
  check ((← uneven.stats).protectedEntries == 1) "a large hit cannot displace the entire protected segment"
  let tiny ← Cache.ResponseStore.new { maxEntries := 10, maxBytes := 700, maxEntryBytes := 700 }
  let largeBody := ByteArray.mk (Array.replicate 220 65)
  check (← tiny.storeChecked keyA (mkEntry keyA largeBody cacheNow 60000)) "response store bounded first byte insert"
  let _ ← tiny.storeChecked keyB (mkEntry keyB largeBody cacheNow 60000)
  check (!((← tiny.lookup keyA).isSome && (← tiny.lookup keyB).isSome)) "response store hard byte limit"
  check ((← tiny.stats).bytes <= 700) "response store reports bounded owned bytes"
  check (!(← tiny.storeChecked keyC (mkEntry keyC (ByteArray.mk (Array.replicate 600 65)) cacheNow 60000))) "oversize response rejected"
  let expired ← Cache.ResponseStore.new
  check (← expired.storeChecked keyA (mkEntry keyA "old".toUTF8 (← IO.monoMsNow) 20)) "short-lived response stored"
  IO.sleep 30
  check ((← expired.lookup keyA).isNone) "expired response never served"
  let isolated ← Cache.ResponseStore.new
  check ((← isolated.lookup keyA).isNone) "worker-local response stores remain independent"
  let coalescedStore ← Cache.ResponseStore.new { maxInFlight := 1, maxWaitersPerKey := 9, maxWaiters := 9 }
  let ready ← IO.mkRef false
  let backendCalls ← IO.mkRef 0
  let slowBackend : Handler := fun _ => do
    backendCalls.modify (· + 1)
    AppM.await
      (do
        if ← ready.get then
          return some (.ok ((Response.text "one generation").withHeader "cache-control" "public, max-age=30"))
        return none)
      (fun _ => pure ()) 10000
  let coalesced := Cache.middleware coalescedStore {
    admit := some (fun c => pure (.cache c "public:coalescing")) } slowBackend
  let executions ← (Array.range 10).mapM (fun _ => AppM.execute (coalesced context))
  check ((← backendCalls.get) == 1) "ten misses start one backend request"
  let pendingStats ← coalescedStore.stats
  check (pendingStats.inFlight == 1 && pendingStats.waiters == 9) "coalescing registers nine bounded followers"
  ready.set true
  for execution in executions do
    match ← execution.poll with
    | some (.ok response) => check (response.body == "one generation".toUTF8) "coalesced response reused"
    | _ => throw (IO.userError "coalesced response did not complete")
  check ((← backendCalls.get) == 1) "ten coalesced requests share the first fill"
  let finishedStats ← coalescedStore.stats
  check (finishedStats.inFlight == 0 && finishedStats.waiters == 0 && finishedStats.pinnedBytes == 0) "coalescing releases all waiter and pin budgets"
  let cancellationStore ← Cache.ResponseStore.new { maxInFlight := 1, maxWaitersPerKey := 2, maxWaiters := 2 }
  let release ← IO.mkRef false
  let cancelCalls ← IO.mkRef 0
  let cancelledBackend : Handler := fun _ => do
    cancelCalls.modify (· + 1)
    AppM.await
      (do
        if ← release.get then
          return some (.ok ((Response.text "survived").withHeader "cache-control" "public, max-age=30"))
        return none)
      (fun _ => pure ()) 10000
  let cancellable := Cache.middleware cancellationStore {
    admit := some (fun c => pure (.cache c "public:cancellation")) } cancelledBackend
  let active ← (Array.range 3).mapM (fun _ => AppM.execute (cancellable context))
  let some follower := active[1]? | throw (IO.userError "missing coalesced follower")
  follower.cancel .disconnected
  check ((← cancellationStore.stats).waiters == 1) "disconnect releases only its follower slot"
  release.set true
  let some leader := active[0]? | throw (IO.userError "missing coalesced leader")
  let some survivor := active[2]? | throw (IO.userError "missing surviving follower")
  check (match ← leader.poll with | some (.ok response) => response.body == "survived".toUTF8 | _ => false) "leader completes after follower cancellation"
  check (match ← survivor.poll with | some (.ok response) => response.body == "survived".toUTF8 | _ => false) "remaining follower completes after cancellation"
  check ((← cancelCalls.get) == 1) "follower cancellation does not restart backend"
  check (match ← (cancellable context).run with | .ok response => response.body == "survived".toUTF8 | _ => false) "later lookup reuses completed fill"
  check ((← cancellationStore.stats).waiters == 0) "cancellation leaves no pinned waiter"
  let invalidating ← Cache.ResponseStore.new
  let invalidationKey := Cache.CacheKey.ofParts #["invalidate", "flight"]
  let .leader pendingFlight ← invalidating.claim invalidationKey |
    throw (IO.userError "expected initial flight leader")
  let .follower waitingFlight ← invalidating.claim invalidationKey |
    throw (IO.userError "expected invalidation follower")
  let waiting ← AppM.execute (invalidating.wait waitingFlight 1000)
  invalidating.invalidateBase invalidationKey
  check (match ← waiting.poll with | some (.ok none) => true | _ => false) "invalidation wakes a suspended follower"
  check (!(← invalidating.finish pendingFlight (some (mkEntry invalidationKey "stale".toUTF8 (← IO.monoMsNow) 60000)))) "late invalidated producer cannot publish"
  let invalidatedStats ← invalidating.stats
  check (invalidatedStats.inFlight == 0 && invalidatedStats.waiters == 0 && invalidatedStats.pinnedBytes == 0) "invalidation releases flight accounting"
  let pinnedStore ← Cache.ResponseStore.new
  let pinnedKey := Cache.CacheKey.ofParts #["invalidate", "pinned"]
  let .leader pinnedLeader ← pinnedStore.claim pinnedKey |
    throw (IO.userError "expected pinned flight leader")
  let .follower pinnedFollower ← pinnedStore.claim pinnedKey |
    throw (IO.userError "expected pinned flight follower")
  let pinnedWait ← AppM.execute (pinnedStore.wait pinnedFollower 1000)
  let pinnedEntry := mkEntry pinnedKey "old generation".toUTF8 (← IO.monoMsNow) 60000
  check (← pinnedStore.storeChecked pinnedKey pinnedEntry) "pinned flight entry stored"
  check (← pinnedStore.finish pinnedLeader (some pinnedEntry)) "pinned flight published"
  check ((← pinnedStore.stats).pinnedBytes > 0) "completed flight holds a bounded response for waiters"
  pinnedStore.invalidateBase pinnedKey
  check (match ← pinnedWait.poll with | some (.ok none) => true | _ => false) "invalidation hides a pinned old generation"
  check ((← pinnedStore.lookup pinnedKey).isNone && (← pinnedStore.stats).pinnedBytes == 0) "pinned response released after invalidation"

private def responseGateRegressionTests : IO Unit := do
  let context ← ctx "/cache-regression"
  let calls ← IO.mkRef 0
  let backend : Handler := fun _ => do
    calls.modify (· + 1)
    pure ((Response.text (toString (← calls.get))).withHeader "cache-control" "public, max-age=30")
  let responseStore ← Cache.ResponseStore.new
  let sanitize : Cache.Admission := fun incoming => do
    let filtered := incoming.request.headers.filter
      (fun h => h.name.toLower != "authorization" && h.name.toLower != "cookie")
    let sanitized := { incoming with
      authenticated := false
      sessionId := none
      request := { incoming.request with headers := filtered } }
    return .cache sanitized "public:sanitized"
  let guarded := Cache.middleware responseStore { admit := some sanitize } backend
  let _ ← ok (← (guarded context).run)
  let protectedCtx := { context with authenticated := true, sessionId := some "secret" }
  let authorized := { context with request := { context.request with
    headers := context.request.headers.push (header "authorization" "Bearer secret") } }
  let cookied := { context with request := { context.request with
    headers := context.request.headers.push (header "cookie" "sid=secret") } }
  for input in #[protectedCtx, authorized, cookied] do
    let _ ← ok (← (guarded input).run)
  check ((← calls.get) == 4) "original auth and cookie state bypasses cache after admission changes"
  for name in #["range", "if-range", "if-match", "if-unmodified-since", "if-modified-since",
      "cache-control", "pragma"] do
    let value := if name == "range" then "bytes=0-1"
      else if name == "cache-control" then "max-age=1"
      else if name == "pragma" then "no-cache"
      else "example"
    let input := { context with request := { context.request with
      headers := context.request.headers.push (header name value) } }
    check (!Cache.eligibleRequest input) s!"{name} request bypasses unsupported cache semantics"
  let minFresh := { context with request := { context.request with
    headers := context.request.headers.push (header "cache-control" "min-fresh=60") } }
  let spacedMaxAge := { context with request := { context.request with
    headers := context.request.headers.push (header "cache-control" "max-age = 0") } }
  let bodyGet := { context with request := { context.request with body := "a=1".toUTF8 } }
  check (!Cache.eligibleRequest minFresh && !Cache.eligibleRequest spacedMaxAge && !Cache.eligibleRequest bodyGet)
    "request max-age, min-fresh and GET bodies bypass response cache"
  let rangeCtx := { context with request := { context.request with
    headers := context.request.headers.push (header "range" "bytes=0-1") } }
  for input in #[minFresh, bodyGet, rangeCtx] do
    let _ ← ok (← (guarded input).run)
  check ((← calls.get) == 7) "unsupported request semantics bypass a seeded cache"

  let nowUnixMs := (← Std.Time.Timestamp.now).toMillisecondsSinceUnixEpoch.val
  let oldDate := LeanNet.HTTP.Static.formatHttpDate (nowUnixMs / 1000 - 7200)
  let yesterday := (Response.text "old").withHeader "cache-control" "public, max-age=3600"
  let yesterday := { yesterday with headers := yesterday.headers.push { name := "date", value := oldDate } }
  check ((Cache.freshnessAt? {} yesterday nowUnixMs 0).map Prod.fst == some 0)
    "old Date exhausts max-age even without Age"
  let spacedLifetime := (Response.text "x").withHeader "cache-control" "public, max-age = 0"
  check ((Cache.freshnessAt? {} spacedLifetime nowUnixMs 0).map Prod.fst == some 0)
    "optional whitespace cannot hide zero max-age"
  let badAge := yesterday.withHeader "age" "bogus"
  check ((Cache.freshnessAt? {} badAge nowUnixMs 0).isNone) "invalid Age bypasses storage"
  let duplicateAge := { yesterday with headers := yesterday.headers ++ #[header "age" "1", header "age" "2"] }
  check ((Cache.freshnessAt? {} duplicateAge nowUnixMs 0).isNone) "duplicate Age bypasses storage"
  let pastExpires := LeanNet.HTTP.Static.formatHttpDate (nowUnixMs / 1000 - 60)
  let expiring := (Response.text "expired").withHeader "cache-control" "public"
  let expiring := { expiring with headers := expiring.headers.push { name := "expires", value := pastExpires } }
  check ((Cache.freshnessAt? {} expiring nowUnixMs 0).map Prod.fst == some 0)
    "past Expires cannot be extended by route TTL"

  let datedCalls ← IO.mkRef 0
  let datedStore ← Cache.ResponseStore.new
  let dated := Cache.middleware datedStore { admit := some (fun c => pure (.cache c "public:dated")) }
    (fun _ => do
      datedCalls.modify (· + 1)
      let now := (← Std.Time.Timestamp.now).toSecondsSinceUnixEpoch.val
      let response := (Response.text (toString (← datedCalls.get))).withHeader "cache-control" "public, max-age=30"
      let response := response.withHeader "age" "0"
      let dateHeader : LeanNet.HTTP1.Header :=
        { name := "date", value := LeanNet.HTTP.Static.formatHttpDate (now - 6) }
      pure { response with headers := response.headers.push dateHeader })
  let firstDated ← ok (← (dated context).run)
  let secondDated ← ok (← (dated context).run)
  check (firstDated.body == secondDated.body && (← datedCalls.get) == 1 &&
    ((secondDated.getHeader "age").bind String.toNat?).getD 0 >= 5)
    "Date apparent age is reported on a cache hit"

  let ttlCalls ← IO.mkRef 0
  let ttlStore ← Cache.ResponseStore.new
  let ttlHandler := Cache.middleware ttlStore {
    ttlMs := 30000
    admit := some (fun c => pure (.cache c "public:ttl-reduction")) }
    (fun _ => do
      ttlCalls.modify (· + 1)
      pure ((Response.text (toString (← ttlCalls.get))).withHeader "cache-control" "public, max-age=30"))
  let _ ← ok (← (ttlHandler context).run)
  IO.sleep 15
  let reduced := { context with attributes := context.attributes.push ("tenant.cache.ttlMs", "1") }
  let _ ← ok (← (ttlHandler reduced).run)
  check ((← ttlCalls.get) == 2) "runtime TTL reduction rejects an older response"

  let varyStore ← Cache.ResponseStore.new
  let varyCalls ← IO.mkRef 0
  let ready ← IO.mkRef false
  let varying : Handler := fun request => do
    varyCalls.modify (· + 1)
    let count ← varyCalls.get
    AppM.await
      (do
        if ← ready.get then
          let result := (Response.text s!"{(request.header "x-device").getD "none"}:{count}").withHeader
            "cache-control" "public, max-age=30"
          return some (.ok (result.withHeader "vary" "X-Device"))
        return none)
      (fun _ => pure ()) 10000
  let varyingCache := Cache.middleware varyStore {
    admit := some (fun c => pure (.cache c "public:mixed-variants")) } varying
  let device := fun (name id : String) => { context with request := { context.request with
    headers := context.request.headers ++ #[header "x-device" name, header "x-request-id" id] } }
  let requests := #[device "desktop" "one", device "desktop" "two",
    device "mobile" "three", device "mobile" "four"]
  let executions ← requests.mapM (fun request => AppM.execute (varyingCache request))
  check ((← varyCalls.get) == 1) "unrelated request headers share one cold base flight"
  ready.set true
  let mut bodies : Array ByteArray := #[]
  for execution in executions do
    let mut completed : Option Response := none
    for _ in [:10] do
      if completed.isSome then break
      if let some result ← execution.poll then completed := some (← ok result)
    let some response := completed | throw (IO.userError "mixed Vary follower did not complete")
    bodies := bodies.push response.body
  check (bodies[0]! == bodies[1]! && bodies[2]! == bodies[3]! && bodies[0]! != bodies[2]! &&
    (← varyCalls.get) == 2) "cold mixed Vary requests produce one fill per variant"
  let mobileHit ← ok (← (varyingCache (device "mobile" "new-id")).run)
  check (mobileHit.body == bodies[2]! && (← varyCalls.get) == 2)
    "newly discovered Vary variant is stored for later requests"
  varyStore.invalidateBase (Cache.responseBaseKey "public:mixed-variants" context)
  check ((← varyStore.stats).entries == 0) "invalidateBase uses the same key as middleware"

private def conditionalCacheTests : IO Unit := do
  let context ← ctx
  let tagged := (Response.text "example").withHeader "etag" "W/\"version,2\""
  let request := { context.request with headers := context.request.headers ++ #[
    header "if-none-match" "\"version1\"", header "If-None-Match" "\"version,2\""] }
  check ((Cache.conditional { context with request } tagged).status == 304)
    "weak validation compares quoted tags across field lines"
  let store ← Cache.ResponseStore.new
  let origin := Cache.middleware store { admit := some fun c => pure (.cache c "public:head") }
    (fun _ => pure ((Response.text "").withHeader "cache-control" "public"))
  let headers := context.request.headers.push (header "if-none-match" (Cache.etag ByteArray.empty))
  let request := { context.request with method := "HEAD", headers }
  let head ← ok (← (origin { context with request }).run)
  check (head.status == 200 && (head.getHeader "etag").isNone)
    "HEAD miss does not synthesize a validator from a HEAD-only body"

/-- Hostile input and isolation checks for middleware, decoders, sessions and caches. -/
def securityTests : IO Unit := do
  check ((Body.form "a=1&a=2".toUTF8).toOption.map (·.size) == some 2) "form preserves duplicate fields"
  check (!(Body.form "a=1&b=2".toUTF8 { maxFields := 1 }).isOk) "form field bound"
  check (!(Body.form "a=%GG".toUTF8).isOk) "strict malformed percent escape"
  check (!(Body.form (ByteArray.mk #[255])).isOk) "strict UTF8 form"
  check (!(Body.json "[[[0]]]".toUTF8 { maxDepth := 2 }).isOk) "JSON depth checked before parse"
  check ((Body.json "{\"braces\":\"[[[\"}".toUTF8 { maxDepth := 1 }).isOk) "JSON string escapes ignored for depth"
  check (!(Body.json "{}".toUTF8 { maxBytes := 1 }).isOk) "JSON byte bound"
  let multipart := "--x\r\nContent-Disposition: form-data; name=\"title\"\r\n\r\nhello\r\n--x--\r\n".toUTF8
  check ((Body.multipart multipart "x").toOption.map (·.size) == some 1) "multipart decoded"
  check (!(Body.multipart multipart "x" { maxFieldBytes := 2 }).isOk) "multipart field bound"
  check (!(Body.multipart multipart "wrong").isOk) "multipart boundary checked"
  let boundaryPrefix := "--b\r\nContent-Disposition: form-data; name=\"file\"; filename=\"a.bin\"\r\n\r\na\r\n--bX\r\nz\r\n--b--\r\n".toUTF8
  check ((Body.multipart boundaryPrefix "b").toOption.map (fun parts => parts[0]!.bytes) == some "a\r\n--bX\r\nz".toUTF8) "multipart boundary prefix stays payload"
  let nested := "--b\r\nContent-Disposition: form-data; name=\"x\"\r\nContent-Type: multipart/mixed\r\n\r\nx\r\n--b--\r\n".toUTF8
  check (!(Body.multipart nested "b").isOk) "nested multipart rejected"
  let validation := Validation.map2 Prod.mk
    (Validation.field #[("name", "")] "name" (Validation.required "name"))
    (Validation.field #[("age", "oops")] "age" (Validation.natural "age"))
  check (match validation with | .error e => e.size == 2 | _ => false) "validation accumulates independent errors"
  let store ← Session.Store.new { maxSessions := 2, lifetimeMs := 100 }
  let one ← ok (← store.create 1000)
  let two ← ok (← store.create 1000)
  check (Session.validToken one.id && one.id != two.id && one.id != one.csrf) "opaque session tokens"
  check (!(← store.create 1000).isOk) "bounded session capacity"
  let _ ← ok (← store.update one.id #[("userId", "alice")] #[("info", "hello")] 1000)
  check (!(← store.update one.id (Array.replicate 129 ("", "")) #[] 1000).isOk) "session metadata count bound"
  let flashes ← ok (← store.takeFlash one.id 1000)
  check (flashes == #[("info", "hello")]) "flash consumed atomically"
  check ((← store.takeFlash one.id 1000).toOption == some #[]) "flash is once only"
  let rotated ← ok (← store.rotate one.id 1000)
  check ((← store.get one.id 1000).toOption.map (·.isNone) == some true) "rotation invalidates old ID"
  check (rotated.id != one.id && rotated.csrf != one.csrf) "rotation regenerates both secrets"
  check (rotated.values == #[("userId", "alice")]) "rotation retains data"
  check ((← store.get rotated.id 1100).toOption.map (·.isNone) == some true) "expiry enforced"
  check (!(Session.cookieValue { secure := false } one.id).isOk) "Host prefix Secure enforced"
  check (!(Session.cookieValue { name := "dev", secure := false, sameSite := .none } one.id).isOk) "SameSite None Secure enforced"
  let cookie ← match Session.cookieValue {} one.id with | .ok x => pure x | .error e => throw (IO.userError e)
  check ((cookie.splitOn "HttpOnly").length == 2 && (cookie.splitOn "Secure").length == 2) "secure cookie flags"
  let context ← ctx
  let post := { context with request := { context.request with method := "POST", headers := context.request.headers.push (header "content-type" "application/x-www-form-urlencoded"), body := ("_csrf=" ++ one.csrf).toUTF8 }, attributes := #[("csrf", one.csrf)] }
  let next : Handler := fun _ => pure (Response.text "ok")
  check ((← status (Middleware.csrf {} next) post) == 200) "valid CSRF token"
  let uploadToken := ("--csrf\r\nContent-Disposition: form-data; name=\"_csrf\"\r\n\r\n" ++ one.csrf ++ "\r\n--csrf--\r\n").toUTF8
  let upload := { post with request := { post.request with headers := #[header "host" "example.test", header "content-type" "multipart/form-data; boundary=\"csrf\""], body := uploadToken } }
  check ((← status (Middleware.csrf {} next) upload) == 200) "multipart browser form CSRF token"
  let forged := { post with request := { post.request with body := "_csrf=forged".toUTF8 } }
  check ((← status (Middleware.csrf {} next) forged) == 403) "CSRF forgery rejected"
  let duplicate := { post with request := { post.request with body := ("_csrf=" ++ one.csrf ++ "&_csrf=" ++ one.csrf).toUTF8 } }
  check ((← status (Middleware.csrf {} next) duplicate) == 403) "duplicate CSRF fields rejected"
  check ((← status (Middleware.requireAuth next) context) == 401) "auth default deny"
  check ((← status (Middleware.authorize (fun _ => false) next) { context with authenticated := true }) == 403) "authorization default deny"
  let metricState ← IO.mkRef ({} : Middleware.Metrics)
  let failing : Handler := fun _ => throw { status := 500, message := "failure" }
  check ((← status (Middleware.metrics metricState failing) context) == 500) "metrics application failure boundary"
  let counts ← metricState.get
  check (counts.requests == 1 && counts.errors == 1 && counts.cancelled == 0 && counts.active == 0) "metrics errors are completed requests"
  let preflightHead : LeanNet.HTTP1.RequestHead := { method := "POST", target := "/".toUTF8, headers := #[header "origin" "https://evil.test"] }
  check (!(Middleware.sameOrigin "https://example.test" (fun _ => true) false preflightHead).isOk) "cross origin early rejection"
  let proxyContext := { context with request := { context.request with headers := context.request.headers.push (header "x-forwarded-host" "evil.test") } }
  let echoHost : Handler := fun c => pure (Response.text c.host)
  let response ← ok (← (Middleware.trustedProxies (fun _ => false) echoHost proxyContext).run)
  check (String.fromUTF8! response.body == "example.test") "untrusted forwarded host ignored"
  let duplicateProxy := { proxyContext with request := { proxyContext.request with headers := proxyContext.request.headers.push (header "x-forwarded-host" "second.test") } }
  check ((← status (Middleware.trustedProxies (fun _ => true) next) duplicateProxy) == 400) "repeated proxy headers rejected"
  let resolved := pipeline #[Middleware.authenticate (fun _ => pure (some "first")), Middleware.authenticate (fun _ => pure (some "second"))] (fun c => pure (Response.text ((c.attribute "principal").getD "")))
  let principal ← ok (← (resolved context).run)
  check (String.fromUTF8! principal.body == "second") "later principal replaces prior identity"
  let corsContext := { context with request := { context.request with headers := context.request.headers.push (header "origin" "https://good.test") } }
  let varying : Handler := fun _ => pure { headers := #[header "vary" "Accept", header "vary" "X-Tenant"] }
  let corsResponse ← ok (← (Middleware.cors { origins := #["https://good.test"] } varying corsContext).run)
  check (corsResponse.getHeader "vary" == some "Accept, X-Tenant, Origin") "CORS preserves all Vary dimensions"
  let cache ← Cache.Store.new { maxEntries := 2, maxBytes := 256, maxEntryBytes := 128, maxInFlight := 1 }
  let _ ← cache.put "a" "one".toUTF8 10 100
  let _ ← cache.put "b" "two".toUTF8 10 100
  let _ ← cache.get "a" 100
  let _ ← cache.put "c" "three".toUTF8 10 100
  check ((← cache.get "b" 100).isNone) "LRU eviction"
  check ((← cache.get "a" 110).isNone) "cache TTL"
  check (!(← cache.put "large" (ByteArray.mk (Array.replicate 129 65)) 10 100)) "cache entry byte bound"
  check (Cache.key #["a:b", "c"] != Cache.key #["a", "b:c"]) "key delimiters cannot collide"
  let duplicateCookies := { context with request := { context.request with headers := context.request.headers ++ #[header "cookie" ("__Host-weave=" ++ one.id), header "cookie" ("__Host-weave=" ++ two.id)] } }
  check ((Session.cookieId duplicateCookies).isNone) "duplicate cookie headers rejected consistently"
  let absentVary := Cache.responseKey context {}
  let emptyVary := { context with request := { context.request with headers := context.request.headers.push (header "accept" "") } }
  let opaqueVary := { context with request := { context.request with headers := context.request.headers.push { name := "accept", value := ByteArray.mk #[255] } } }
  check (Cache.responseKey emptyVary {} != absentVary && Cache.responseKey opaqueVary {} != absentVary) "raw Vary presence and bytes isolated"
  let baseKey := Cache.responseKey context {}
  for other in [ { context with host := "other.test" }, { context with tenant := "tenant-b" },
      { context with path := #["other"] }, { context with query := #[("q", "other")] },
      { context with locale := "fr" }, { context with routeName := "other" },
      { context with request := { context.request with headers := context.request.headers.push (header "accept" "application/json") } } ] do
    check (Cache.responseKey other {} != baseKey) "host/tenant/path/query/locale/route/Vary isolation"
  let calls ← IO.mkRef 0
  let generating : Handler := fun _ => do
    calls.modify (· + 1)
    pure ((Response.text "public").withHeader "cache-control" "public, max-age=30")
  let responses ← Cache.ResponseStore.new
  let cached := Cache.middleware responses { admit := some (fun ctx => pure (.cache ctx "public:test")) } generating
  let first ← ok (← (cached context).run)
  let second ← ok (← (cached context).run)
  check ((← calls.get) == 1 && first.body == second.body) "public cache hit"
  let head := { context with request := { context.request with method := "HEAD" } }
  let headResponse ← ok (← (cached head).run)
  check ((← calls.get) == 1 && headResponse.body == first.body) "HEAD reuses GET metadata"
  let conditional := { context with request := { context.request with headers := context.request.headers.push (header "if-none-match" ((first.getHeader "etag").getD "")) } }
  let notModified ← ok (← (cached conditional).run)
  check (notModified.status == 304 && notModified.body.isEmpty) "conditional GET"
  let tenantCalls ← IO.mkRef 0
  let tenantBody : Handler := fun c => do
    tenantCalls.modify (· + 1)
    pure ((Response.text (c.tenant ++ ":" ++ toString (← tenantCalls.get))).withHeader "cache-control" "public, max-age=30")
  let tenantHeader := fun (name : String) => { context with request := { context.request with headers := context.request.headers.push (header "x-tenant-id" name) } }
  let alphaCtx := tenantHeader "alpha"
  let betaCtx := tenantHeader "beta"
  let unsafeStore ← Cache.ResponseStore.new
  let unsafeOrder := Cache.middleware unsafeStore {} (Tenant.middleware (Tenant.fromHeader) (fun _ _ => pure true) tenantBody)
  let unsafeAlpha ← ok (← (unsafeOrder alphaCtx).run)
  let unsafeBeta ← ok (← (unsafeOrder betaCtx).run)
  check (unsafeAlpha.body != unsafeBeta.body && (← status unsafeOrder context) == 404) "unadmitted outer cache cannot bypass tenant resolution"
  check ((← unsafeStore.stats).entries == 0) "default cache policy never stores before admission"
  let admittedStore ← Cache.ResponseStore.new
  let access ← IO.mkRef true
  let admitted := Cache.middleware admittedStore { admit := some (fun c => do
    let some resolution ← Tenant.fromHeader "x-tenant-id" c | throw { status := 404, message := "tenant not found" }
    unless ← access.get do throw { status := 403, message := "tenant access denied" }
    let verified := { c with tenant := resolution.id.value }
    return .cache verified ("tenant:" ++ resolution.id.value)) } tenantBody
  let admittedAlpha ← ok (← (admitted alphaCtx).run)
  let admittedAlphaHit ← ok (← (admitted alphaCtx).run)
  let admittedBeta ← ok (← (admitted betaCtx).run)
  check (admittedAlpha.body == admittedAlphaHit.body && admittedAlpha.body != admittedBeta.body) "admitted cache isolates verified tenants"
  check ((← status admitted context) == 404) "admission still rejects missing tenant after cache fill"
  access.set false
  check ((← status admitted alphaCtx) == 403) "authorization revocation runs before cache hit"
  let bypassed := Cache.middleware admittedStore { admit := some (fun c => pure (.bypass { c with locale := "fr" })) }
    (fun c => pure (Response.text c.locale))
  let bypassResponse ← ok (← (bypassed context).run)
  check (String.fromUTF8! bypassResponse.body == "fr") "cache bypass retains admitted context changes"
  let fragmentStore ← Cache.Store.new
  let fragmentCalls ← IO.mkRef 0
  let fragmentGenerate : Context → AppM ByteArray := fun c => do
    fragmentCalls.modify (· + 1)
    pure (c.tenant ++ ":" ++ toString (← fragmentCalls.get)).toUTF8
  let fragmentPolicy : Cache.Policy := { admit := some (fun c =>
    if c.tenant.isEmpty then pure (.bypass c) else pure (.cache c ("tenant:" ++ c.tenant))) }
  let fragmentAlpha := { context with tenant := "alpha" }
  let fragmentBeta := { context with tenant := "beta" }
  let firstFragment ← ok (← (Cache.requestFragment fragmentStore fragmentAlpha "card" fragmentGenerate fragmentPolicy).run)
  let secondFragment ← ok (← (Cache.requestFragment fragmentStore fragmentAlpha "card" fragmentGenerate fragmentPolicy).run)
  let betaFragment ← ok (← (Cache.requestFragment fragmentStore fragmentBeta "card" fragmentGenerate fragmentPolicy).run)
  check (firstFragment == secondFragment && firstFragment != betaFragment && (← fragmentCalls.get) == 2) "request fragments use admitted tenant scope"
  for other in [ { context with authenticated := true }, { context with sessionId := some one.id },
      { context with request := { context.request with headers := context.request.headers.push (header "authorization" "Bearer x") } },
      { context with request := { context.request with headers := context.request.headers.push (header "cookie" "a=b") } } ] do
    check (!Cache.eligibleRequest other) "auth/session cache bypass"
  for extra in [header "set-cookie" "a=b", header "vary" "*", header "cache-control" "private",
      header "cache-control" "no-store", header "connection" "close",
      header "transfer-encoding" "chunked", header "content-range" "bytes 0-1/10"] do
    check (!Cache.eligibleResponse {} { first with headers := first.headers.push extra }) "private response bypass including duplicate header fields"
  check (!Cache.eligibleResponse {} ((Response.text "x").withHeader "cache-control" "public=foo, max-age=30"))
    "public requires a bare cache-control directive"
  check (!Cache.eligibleResponse {} { first with headers := first.headers.push { name := "set-cookie", value := ByteArray.mk #[255] } }) "opaque Set-Cookie bypass"
  check (Cache.parseVary #[header "Vary" " X-Device , Accept-Encoding ", header "vary" "x-device"] ==
    some #["accept-encoding", "x-device"]) "Vary merges case-insensitive field names without changing headers"
  for invalid in #[header "vary" "*", header "vary" "X-Device,", header "vary" "X Device",
      header "vary" "", { name := "vary", value := ByteArray.mk #[255] }] do
    check ((Cache.parseVary #[invalid]).isNone) "malformed or opaque Vary rejected"
    check (!Cache.eligibleResponse {} { first with headers := first.headers.push invalid }) "malformed Vary bypasses cache"
  check (Cache.CacheKey.ofByteParts #[ByteArray.empty] != Cache.CacheKey.ofByteParts #[]) "key distinguishes absent from empty fields"
  check (Cache.CacheKey.ofByteParts #[ByteArray.mk #[255]] != Cache.CacheKey.ofByteParts #["�".toUTF8]) "opaque key bytes are preserved"
  check (Cache.freshnessMs {} ((Response.text "x").withHeader "cache-control" "public, max-age=0") == 0) "zero freshness not cached"
  check ((Cache.conditional conditional (Response.text "oops" 500)).status == 500) "conditional error cannot become 304"
  let leases ← Cache.Store.new
  check (← leases.acquire "work") "fragment cache miss lease"
  check (!(← leases.acquire "work")) "fragment stampede prevention"
  leases.release "work"
  let ready ← IO.mkRef false
  let fragments ← Cache.Store.new
  let job ← AppM.execute (Cache.fragment fragments "f" 1000 (AppM.await
    (do if ← ready.get then return some (.ok "old".toUTF8) else return none)
    (fun _ => pure ()) 10000))
  fragments.invalidate "f"
  ready.set true
  let _ ← job.poll
  check ((← fragments.get "f" (← IO.monoMsNow)).isNone) "invalidation blocks stale in-flight refill"
  let state ← fragments.state.get
  check (state.inFlight.isEmpty) "cache lease released after completion"
  let pending ← AppM.execute (Cache.fragment fragments "cancel" 1000 (AppM.await (pure none) (fun _ => pure ()) 10000))
  pending.cancel .disconnected
  check ((← fragments.state.get).inFlight.isEmpty) "cache lease released on cancellation"
  responseStoreTests
  responseGateRegressionTests
  conditionalCacheTests
  IO.println "security/cache/session/body checks passed"

end LeanWeave.Test
