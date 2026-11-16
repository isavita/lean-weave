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
  let responses ← Cache.Store.new
  let cached := Cache.middleware responses {} generating
  let first ← ok (← (cached context).run)
  let second ← ok (← (cached context).run)
  check ((← calls.get) == 1 && first.body == second.body) "public cache hit"
  let head := { context with request := { context.request with method := "HEAD" } }
  let headResponse ← ok (← (cached head).run)
  check ((← calls.get) == 1 && headResponse.body == first.body) "HEAD reuses GET metadata"
  let conditional := { context with request := { context.request with headers := context.request.headers.push (header "if-none-match" ((first.getHeader "etag").getD "")) } }
  let notModified ← ok (← (cached conditional).run)
  check (notModified.status == 304 && notModified.body.isEmpty) "conditional GET"
  for other in [ { context with authenticated := true }, { context with sessionId := some one.id },
      { context with request := { context.request with headers := context.request.headers.push (header "authorization" "Bearer x") } },
      { context with request := { context.request with headers := context.request.headers.push (header "cookie" "a=b") } } ] do
    check (!Cache.eligibleRequest other) "auth/session cache bypass"
  for extra in [header "set-cookie" "a=b", header "vary" "*", header "cache-control" "private", header "cache-control" "no-store"] do
    check (!Cache.eligibleResponse {} { first with headers := first.headers.push extra }) "private response bypass including duplicate header fields"
  check (!Cache.eligibleResponse {} { first with headers := first.headers.push { name := "set-cookie", value := ByteArray.mk #[255] } }) "opaque Set-Cookie bypass"
  check (Cache.freshnessMs {} ((Response.text "x").withHeader "cache-control" "public, max-age=0") == 0) "zero freshness not cached"
  check ((Cache.conditional conditional (Response.text "oops" 500)).status == 500) "conditional error cannot become 304"
  check (← responses.acquire "work") "cache miss lease"
  check (!(← responses.acquire "work")) "stampede prevention"
  responses.release "work"
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
  IO.println "security/cache/session/body checks passed"

end LeanWeave.Test
