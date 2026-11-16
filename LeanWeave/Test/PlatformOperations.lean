import LeanWeave.Operations.Runtime
import LeanWeave.Observability.Export
import LeanWeave.Observability.Logging
import LeanWeave.Observability.Jobs
import LeanWeave.Secrets.Manager
import LeanWeave.Security.Audit
import LeanWeave.Security.Journal
import LeanWeave.Security.Csp
import LeanWeave.Observability.File
import LeanWeave.Feature.Basic
import LeanWeave.Tenant.ConfigTest
import LeanWeave.Cache.Response
import LeanWeave.Test.Client

namespace LeanWeave.Test.PlatformOperations

open Observability

private def check (value : Bool) (message : String) : IO Unit := unless value do throw (IO.userError s!"operations: {message}")
private def require (value : Except ε α) : IO α := match value with
  | .ok value => pure value | .error _ => throw (IO.userError "expected successful operation")

private def tracing : IO Unit := do
  let tracer ← Tracer.create { maxActive := 2, maxPending := 2, maxBytes := 4096, sampleBasisPoints := 0 }
  let parent := "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
  let a ← tracer.start "db.query" (some parent) .client #[{ key := "password", value := .text "never" }]
  let b ← tracer.start "http.request" (some parent)
  let dropped ← tracer.start "capacity" (some parent)
  a.finish .ok #[] ""
  a.finish .error #[] "duplicate"
  b.finish .error #[] "cancelled"
  dropped.finish .ok #[] ""
  let state ← tracer.stats
  check (state.active == 0 && state.pending == 2 && state.dropped == 1) "trace bounds/idempotence"
  let spans ← tracer.take
  let some span := spans[0]? | throw (IO.userError "missing span")
  check (Security.Crypto.hex span.context.traceId == "4bf92f3577b34da6a3ce929d0e0e4736") "trace propagation"
  check (span.attributes.any (fun a => a.key == "password" && a.value == .text "[REDACTED]")) "trace redaction"
  check ((span.toOtlp.getObjValAs? Nat "flags").toOption == some 1) "OTLP sampled flag propagation"
  check (span.endTimeUnixNano >= span.startTimeUnixNano) "trace monotonic duration"
  check (boundedText "ab😀cd" 4 == "ab") "UTF8 byte truncation"
  let absent ← tracer.start "not-sampled"
  absent.finish .ok #[] ""
  check ((← tracer.stats).pending == 0) "root sampling reload"
  tracer.sampling 10000
  let span ← tracer.start "exported"
  span.finish .ok #[] ""
  let attempts ← IO.mkRef 0
  let exporter ← Exporter.create tracer (fun _ => do
    let n ← attempts.get
    attempts.set (n+1)
    return { accepted := n > 0, retryable := n == 0 }) { retryInitialMs := 1, retryMaxMs := 1 }
  exporter.tick 100
  exporter.tick 101
  exporter.tick 102
  exporter.tick 103
  check ((← exporter.stats).exported == 1 && (← attempts.get) == 2) "OTLP bounded retry"
  check (← exporter.isDrained) "exporter drained"
  let synthetic ← tracer.start "local-root" (some "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-00") .server #[] true
  synthetic.finish .ok #[] ""
  let localSpans ← tracer.take
  check (localSpans.size == 1 && localSpans.all (·.parentSpanId.isNone)) "native generated roots follow SDK sampling"
  let queue ← Stream.Queue.new
  let ctx ← require LeanWeave.Test.context
  let handler := tracer.middleware (fun _ => pure (Response.streaming queue.body))
  let response ← require (← (handler ctx).run)
  check ((← tracer.stats).active == 1) "streamed request span survives headers"
  if let some body := response.stream then body.close (some .disconnected)
  check ((← tracer.stats).active == 0 && (← tracer.stats).pending == 1) "stream disconnect completes span"
  let _ ← tracer.take
  let jobs := jobsHooks tracer
  let job ← jobs.startSpan { jobId := "j", kind := "test", tenant := "alpha", requestId := "r", traceparent := parent, state := .running }
  job.finish { jobId := "j", kind := "test", tenant := "alpha", requestId := "r", traceparent := job.traceparent, state := .succeeded }
  check ((← tracer.stats).pending == 1) "automatic real job span"

private def cacheIsolation : IO Unit := do
  let store ← Cache.Store.new
  let ctx ← require LeanWeave.Test.context
  let ctx := { ctx with host := "example.test", tenant := "alpha", routeName := "dashboard", attributes := #[("tenant.config.revision", "1"),("feature.variant", "a")] }
  check (Cache.responseKey { ctx with request := { ctx.request with target := "/dashboard?q=a+b".toUTF8 } } {} !=
    Cache.responseKey { ctx with request := { ctx.request with target := "/dashboard?q=a%2Bb".toUTF8 } } {}) "raw target avoids protocol-specific query decoding collisions"
  let calls ← IO.mkRef 0
  let handler := Cache.middleware store { admit := some (fun ctx => pure (.cache ctx ("tenant:" ++ ctx.tenant))) } fun _ => do
    calls.modify (· + 1)
    return (Response.text (toString (← calls.get))).withHeader "cache-control" "public, max-age=60"
  let run : Context → IO String := fun c => do return String.fromUTF8! (← require (← (handler c).run)).body
  check ((← run ctx) == "1" && (← run ctx) == "1") "cache stable snapshot hit"
  check ((← run { ctx with tenant := "beta" }) == "2") "cache tenant isolation"
  check ((← run { ctx with attributes := #[("tenant.config.revision", "2"),("feature.variant", "a")] }) == "3") "cache config revision isolation"
  check ((← run { ctx with attributes := #[("tenant.config.revision", "1"),("feature.variant", "b")] }) == "4") "cache feature variant isolation"
  let disabled := { ctx with attributes := ctx.attributes.push ("tenant.cache.enabled", "false") }
  check ((← run disabled) == "5" && (← run disabled) == "6") "dynamic tenant cache bypass"
  let zero := { ctx with attributes := ctx.attributes.push ("tenant.cache.ttlMs", "0") }
  check ((← run zero) == "7") "zero TTL bypasses an existing cache entry"
  let role : Feature.Flag := { name := "admin", enabled := true, requiredRole := some "admin" }
  check (!(Feature.evaluate role { ctx with attributes := #[("tenant.flag.admin", "true")] }).enabled) "tenant flag override cannot bypass audience"

private def cspCacheIsolation : IO Unit := do
  let ctx ← require LeanWeave.Test.context
  let policy ← require (Security.Csp.Policy.parse "default-src 'self'; script-src 'nonce'; object-src 'none'; base-uri 'self'")
  let origin : Handler := fun ctx => pure ((Response.text ((Security.Csp.nonce ctx).getD "missing")).withHeader "cache-control" "public, max-age=60")
  for outside in [true,false] do
    let store ← Cache.Store.new
    let cachePolicy : Cache.Policy := { admit := some (fun ctx => pure (.cache ctx "public:csp")) }
    let handler := if outside then Cache.middleware store cachePolicy (Security.Csp.middleware policy origin)
      else Security.Csp.middleware policy (Cache.middleware store cachePolicy origin)
    let first ← require (← (handler ctx).run)
    let second ← require (← (handler ctx).run)
    check (first.body != second.body) "CSP/cache middleware order cannot reuse a nonce"
    let nonce := String.fromUTF8! second.body
    check ((((second.getHeader "content-security-policy").getD "").splitOn ("'nonce-" ++ nonce ++ "'")).length == 2) "cached body/header nonce mismatch"
    check ((← store.state.get).entries.isEmpty) "nonce-bearing response was stored"
  let store ← Cache.Store.new
  let fragment : Handler := fun ctx => do
    let bytes ← Cache.requestFragment store ctx "nonce" (fun c => pure ((Security.Csp.nonce c).getD "missing").toUTF8)
    return Response.text (String.fromUTF8! bytes)
  let handler := Security.Csp.middleware policy fragment
  let first ← require (← (handler ctx).run)
  let second ← require (← (handler ctx).run)
  check (first.body != second.body && (← store.state.get).entries.isEmpty) "nonce-sensitive request fragments bypass caching"
  let raw : Response := { headers := #[
    { name := "cache-control", value := "public".toUTF8 },
    { name := "content-security-policy", value := "script-src 'nonce-abc'".toUTF8.push 255 }] }
  check (!(Cache.eligibleResponse {} raw)) "non-UTF8 raw CSP fails closed for cache storage"
  check (!(Cache.eligibleResponse {} ((Response.text "x").withHeader "cache-control" "public" |>.withHeader "content-security-policy-report-only" "script-src 'nonce-abc'"))) "report-only nonce responses bypass storage"

private def logging : IO Unit := do
  let logger ← Observability.Logger.create { sampleBasisPoints := 0 }
  let ctx ← require LeanWeave.Test.context
  let ctx := { ctx with requestId := "r1", tenant := "alpha", authenticated := true, attributes := #[("principal", "alice")] }
  check (!(← logger.emit .info "sampled" (some ctx))) "log sampling"
  check (← logger.emit .error "failed" (some ctx) #[("access_token", "never"), ("operation", "read")]) "errors survive sampling"
  let records ← logger.take
  check (records.size == 1) "bounded log batch"
  let json ← require (Lean.Json.parse (String.fromUTF8! records[0]!))
  check ((json.getObjValAs? String "tenant").toOption == some "alpha") "log tenant correlation"
  check ((json.getObjValAs? String "user").toOption == some "alice") "log authenticated user"
  let fields ← require (json.getObjVal? "fields")
  check ((fields.getObjValAs? String "access_token").toOption == some "[REDACTED]") "secret redaction"
  logger.configure 10000
  check (← logger.emit .info "reloaded" (some ctx)) "dynamic sampling"

private def healthDrain : IO Unit := do
  let health ← Operations.Health.create #[{ name := "database" }]
  let ctx ← require LeanWeave.Test.context
  let handler := health.middleware (fun _ => pure (Response.text "application"))
  let probe := fun name => (handler { ctx with path := #["health",name] }).run
  check ((← require (← probe "live")).status == 200) "liveness during startup"
  check ((← require (← probe "startup")).status == 503) "startup before initialization"
  let _ ← health.initialized
  check ((← require (← probe "startup")).status == 200) "startup complete"
  check ((← require (← probe "ready")).status == 503) "readiness dependency missing"
  let _ ← health.dependency "database" true
  check ((← require (← probe "ready")).status == 200) "ready dependencies"
  -- A distinct runtime thread deliberately holds the dependency-state mutex.
  -- Every probe runs before release, so startup cannot accidentally pass by
  -- acquiring the lock after the contending publisher has finished.
  let ⟨state, _, _⟩ := health
  let locked ← IO.Promise.new
  let release ← IO.Promise.new
  let holder ← IO.asTask do
    state.atomically do
      locked.resolve ()
      let _ ← IO.wait release.result?
      pure ()
  let _ ← IO.wait locked.result?
  try
    check ((← health.snapshot).isNone) "fixture holds dependency mutex on another thread"
    for _ in [:128] do
      check ((← require (← probe "startup")).status == 200) "startup stays initialized during dependency contention"
      check ((← require (← probe "live")).status == 200) "liveness avoids dependency contention"
      check ((← require (← probe "ready")).status == 503) "readiness fails closed during dependency contention"
    check (← health.beginDrain) "drain latch publishes during dependency contention"
  finally
    release.resolve ()
    match ← IO.wait holder with
    | .ok () => pure ()
    | .error error => throw error
  check ((← health.snapshot).any (fun s => s.initialized && s.draining)) "monotone latches survive snapshot contention"
  check ((← require (← probe "startup")).status == 200) "draining preserves completed startup"
  let closed ← IO.mkRef (#[] : Array String)
  let deadlines ← IO.mkRef (#[] : Array Nat)
  let runtime ← Operations.Runtime.create health #[
    { name := "one", onDrain := fun d => deadlines.modify (·.push d), isDrained := pure false, close := closed.modify (·.push "one") },
    { name := "two", onDrain := fun d => deadlines.modify (·.push d), close := do closed.modify (·.push "two"); throw (IO.userError "injected") }]
  runtime.beginDrain 100
  runtime.beginDrain 200
  check ((← deadlines.get) == #[100,100]) "one shared absolute deadline"
  check ((← require (← probe "ready")).status == 503) "draining not ready"
  check ((← require (← probe "live")).status == 200) "draining stays live"
  runtime.tick 99
  check ((← closed.get).isEmpty) "no early close"
  runtime.tick 100
  runtime.close
  check ((← closed.get) == #["two","one"]) "close reverse order once despite failures"
  check ((← runtime.stats).failed && (← runtime.isDrained)) "deadline force close"

private def secrets : IO Unit := do
  let value ← IO.mkRef "one"
  let fails ← IO.mkRef false
  let backend : Secrets.Backend := fun _ => do
    if ← fails.get then throw { status := 503, message := "fixture failure" }
    let value ← value.get
    return { value := value.toUTF8, version := value, ttlMs := 100 }
  let manager ← Secrets.Manager.create #[{ name := "key", source := "test" }] backend
  let now ← IO.monoMsNow
  manager.tick now
  manager.tick (now+1)
  let first ← require (← manager.get "key")
  check ((← require (first.use now String.fromUTF8!)) == "one") "memory secret load"
  value.set "two"
  manager.tick (now+68)
  manager.tick (now+69)
  let rotated ← require (← manager.get "key")
  check (rotated.version == "two") "automatic secret rotation"
  check (!(rotated.use (now+169) id).isOk) "lease expiry boundary"
  fails.set true
  manager.tick (now+137)
  manager.tick (now+138)
  let retained ← require (← manager.get "key")
  check (retained.version == "two") "refresh failure retains original unextended lease"
  manager.close
  check (!(← manager.get "key").isOk) "closed secret unavailable"

private def audit : IO Unit := do
  let e : Security.Audit.Event := { action := "login", actor := "alice", tenant := "alpha", timeUnixNano := 1 }
  let a ← require (Security.Audit.append {} e)
  let b ← require (Security.Audit.append a.checkpoint { e with action := "export", timeUnixNano := 2 })
  let lines := #[a.json.compress,b.json.compress]
  check ((Security.Audit.verify lines {} (some b.checkpoint)).isOk) "audit verified chain"
  check (!(Security.Audit.verify #[b.json.compress,a.json.compress]).isOk) "audit reorder"
  check (!(Security.Audit.verify #[a.json.compress] {} (some b.checkpoint)).isOk) "anchored audit truncation"
  check (!(Security.Audit.verify #[a.json.compress,b.json.compress.replace "export" "import"]).isOk) "audit tampering"
  let ledger ← Security.Audit.Ledger.create 1
  let _ ← require (← ledger.record e)
  check (!(← ledger.record e).isOk) "audit fails closed when full"
  let flags ← Feature.Store.new 2 1
  let _ ← require (← flags.define { name := "release", enabled := true } "admin:alice")
  check (!(← flags.define { name := "release", enabled := false } "admin:bob").isOk) "mutation rejected without audit capacity"
  let snapshot ← require (← flags.snapshot)
  check (snapshot.any (fun f => f.name == "release" && f.enabled)) "unaudited mutation did not commit"
  let records ← require (← flags.audit)
  let some record := records[0]? | throw (IO.userError "missing flag audit")
  check (record.event.actor == "admin:alice" && record.event.target == "release" && record.event.changes.size == 2) "full flag mutation metadata"

private def journal : IO Unit := do
  IO.FS.withTempDir fun directory => do
    let path := directory / "audit.jsonl"
    let journal ← Security.Audit.Journal.open path
    let event : Security.Audit.Event := { action := "permission.change", actor := "admin:alice", tenant := "alpha", timeUnixNano := 1 }
    let execution ← AppM.execute (journal.record event) 1024
    let mut checkpoint : Option Security.Audit.Checkpoint := none
    try
      for _ in [:2000] do
        if let some result ← execution.poll then checkpoint := some (← require result); break
        IO.sleep 1
      check checkpoint.isSome "audit append receipt deadline"
      check (← journal.isDrained) "audit queue flushed"
      let competing ← try
        let other ← Security.Audit.Journal.open path { startupTimeoutMs := 100 }
        other.close
        let _ ← other.writer.join
        pure true
        catch _ => pure false
      check (!competing) "second writer rejected by lock"
    finally
      execution.cancel .shutdown
      journal.close
      check (← journal.writer.join) "audit writer stopped"
    let some anchor := checkpoint | throw (IO.userError "missing audit checkpoint")
    let lines := ((← IO.FS.readFile path).splitOn "\n").toArray.filter (· != "")
    check ((Security.Audit.verify lines {} (some anchor)).isOk) "on-disk immutable chain"
    let reopened ← Security.Audit.Journal.open path {} (some anchor)
    check ((← reopened.checkpoint) == anchor) "verified append resumes persisted chain"
    reopened.close
    check (← reopened.writer.join) "reopened writer stopped"
    IO.FS.writeFile path ((← IO.FS.readFile path).replace "permission.change" "permission.forged")
    let accepted ← try
      let corrupted ← Security.Audit.Journal.open path {} (some anchor)
      corrupted.close
      let _ ← corrupted.writer.join
      pure true
      catch _ => pure false
    check (!accepted) "tampered journal rejects startup"
    let logger ← Logger.create
    let writer ← FileWriter.start (directory / "logs.jsonl")
    try
      for _ in [:1000] do
        if (← writer.stats).any (·.ready) then break
        IO.sleep 1
      let _ ← logger.emit .info "native.file.test"
      logger.flushTo writer
    finally check (← writer.join) "structured file writer stopped"
    let logs ← IO.FS.readFile (directory / "logs.jsonl")
    check (((Lean.Json.parse logs).bind fun json => json.getObjValAs? String "event").toOption == some "native.file.test") "structured JSON persisted"

def run : IO Unit := do
  LeanWeave.Tenant.ConfigTest.run
  tracing
  logging
  cacheIsolation
  cspCacheIsolation
  healthDrain
  secrets
  audit
  journal
  IO.println "platform observability/health/drain/secrets/audit checks passed"

end LeanWeave.Test.PlatformOperations
