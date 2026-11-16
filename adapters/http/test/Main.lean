import LeanWeaveHttp

open LeanWeave

@[extern "weave_http_backend_info"]
private opaque backendInfo : IO String

def drive (action : AppM α) (budget : Nat := 2 * 1024 * 1024) : IO (Except AppError α) := do
  let execution ← AppM.execute action budget
  try
    let cutoff := (← IO.monoMsNow) + 10000
    while (← IO.monoMsNow) < cutoff do
      if let some result ← execution.poll then return result
      IO.sleep 1
    throw (IO.userError "client test timed out")
  finally execution.cancel .shutdown

private def check (value : Bool) (message : String) : IO Unit := unless value do throw (IO.userError message)

private def response (label : String) (value : Except AppError α) : IO α :=
  match value with
  | .ok value => pure value
  | .error error => throw (IO.userError s!"{label} failed: status={error.status}, message={(error.message.take 256).toString}")

private def verificationRejected (client : Http.Client) (label : String) : IO Unit := do
  let started ← IO.monoMsNow
  match ← drive (client.request {}) with
  | .ok _ => throw (IO.userError (label ++ " was accepted"))
  | .error error =>
    check (error.status == 502) s!"{label} failed for an unexpected reason: status={error.status}, message={(error.message.take 256).toString}"
    check ((← IO.monoMsNow) - started < client.config.timeoutMs - 1000) (label ++ " rejection consumed the request timeout")

private def rejectedStartup (config : Http.Config) : IO Bool := do
  try
    let client ← Http.Client.create config
    client.close
    return false
  catch _ => return true

def main (args : List String) : IO Unit := do
  let backend ← backendInfo
  IO.eprintln ("HTTP_BACKEND " ++ backend)
  if args == ["--backend-info"] then
    IO.println backend
    return
  check ((Http.decodeOtlpResponse 200 "{}".toUTF8).accepted) "OTLP empty object"
  check ((Http.decodeOtlpResponse 200 "{\"partialSuccess\":{\"rejectedSpans\":\"1\"}}".toUTF8).rejected == 1) "OTLP partial success int64"
  for bad in ["null", "[]", "{\"partialSuccess\":true}", "{\"partialSuccess\":{\"rejectedSpans\":1e1000000000}}",
      "{\"partialSuccess\":{\"rejectedSpans\":\"9223372036854775808\"}}",
      "{\"partialSuccess\":{\"rejectedSpans\":\"" ++ String.ofList (List.replicate 50000 '9') ++ "\"}}"] do
    check (!(Http.decodeOtlpResponse 200 bad.toUTF8).accepted) "unbounded/malformed OTLP response accepted"
  let port := ((← IO.getEnv "WEAVE_HTTP_PORT").bind String.toNat?).getD 0
  let ca := (← IO.getEnv "WEAVE_HTTP_CA").getD ""
  let secure := (← IO.getEnv "WEAVE_HTTP_TLS") == some "1"
  let preferHttp2 := (← IO.getEnv "WEAVE_HTTP_H2") == some "1"
  let expectedProtocol := if preferHttp2 then "h2" else "http/1.1"
  let tracer ← Observability.Tracer.create
  let client ← Http.Client.create {
    endpoint := { hostName := "localhost", hostAddr := "127.0.0.1", port := port.toUInt16, secure, caBundle := ca,preferHttp2 }
    maxActive := 2
    timeoutMs := 5000
    maxResponseBytes := 4096 } (some tracer)
  try
    let ctx : Context := { request := { method := "GET", target := "/".toUTF8, headers := #[], body := ByteArray.empty }, attributes := #[
      ("otel.traceparent", "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")] }
    -- Remove the explicit CA source after startup. TLS must use the preloaded
    -- immutable blob, even on the very first handshake.
    let movedCA := ca ++ ".after-startup"
    if secure then IO.FS.rename ca movedCA
    let echo ← try
      drive (client.request { method := "POST", path := "/echo", body := "native-http".toUTF8 } (some ctx))
    finally
      if secure then IO.FS.rename movedCA ca
    let echo ← response "echo" echo
    check (echo.status == 200 && String.fromUTF8! echo.body == "native-http") "echo mismatch"
    check (echo.header "x-protocol" == some expectedProtocol) "negotiated protocol mismatch or HTTP/2 fallback"
    let chunked ← response "chunked" (← drive (client.request { path := "/chunked" }))
    check (String.fromUTF8! chunked.body == "onetwo") "chunked decoding"
    check (chunked.header "x-end" == some "yes") "chunked/HTTP2 trailer decoding"
    check (chunked.header "x-protocol" == some expectedProtocol) "chunked/DATA protocol mismatch"
    let redirect ← response "redirect" (← drive (client.request { path := "/redirect" }))
    check (redirect.status == 302) "redirect was followed"
    check (redirect.header "x-protocol" == some expectedProtocol) "redirect protocol mismatch"
    check (!(← drive (client.request { path := "/oversized" })).isOk) "body bound not enforced"
    let timeoutClient ← Http.Client.create { client.config with timeoutMs := 1000 } (some tracer)
    try
      let started ← IO.monoMsNow
      let result ← drive (timeoutClient.request { path := "/timeout" })
      let elapsed := (← IO.monoMsNow) - started
      match result with
      | .ok _ => throw (IO.userError "deliberately withheld response did not time out")
      | .error error => check ([502,504].contains error.status) s!"unexpected timeout failure status: {error.status}"
      check (elapsed >= 900 && elapsed < 2000) s!"timeout did not follow its configured 1000ms deadline: {elapsed}ms"
      check (← timeoutClient.isIdle) "timeout leaked client handle"
    finally timeoutClient.close
    let pending ← AppM.execute (client.request { path := "/slow" }) client.config.retainedBytes
    let _ ← pending.poll
    pending.cancel .disconnected
    check (← client.isIdle) "disconnect leaked client handle"
    let spans ← tracer.take
    check (spans.size == 6) "automatic HTTP span missing or duplicated"
    let some first := spans[0]? | throw (IO.userError "missing HTTP span")
    check (Security.Crypto.hex first.context.traceId == "4bf92f3577b34da6a3ce929d0e0e4736") "trace ID propagation failed"
    check (!(← drive (client.request { path := "//wrong-origin" })).isOk) "authority path injection accepted"
    check (!(← drive (client.request { headers := #[("x-test", "ok\r\ninjected: yes")] })).isOk) "header CRLF accepted"
    let limited ← Http.Client.create { client.config with maxActive := 1 }
    try
      let pending ← AppM.execute (limited.request { path := "/slow" }) limited.config.retainedBytes
      try
        check (!(← (limited.request { path := "/echo" }).run).isOk) "active operation bound not enforced"
      finally pending.cancel .shutdown
      check (← limited.isIdle) "capacity lease not released after cancel"
    finally limited.close
    if secure then
      check (← rejectedStartup { client.config with endpoint := { client.config.endpoint with caBundle := "",systemTrust := false } })
        "strict TLS allowed absent startup CA"
      let missing := (← IO.getEnv "WEAVE_HTTP_MISSING_CA").getD ""
      check (← rejectedStartup { client.config with endpoint := { client.config.endpoint with caBundle := missing } })
        "missing CA file accepted during startup"
      let oversized := (← IO.getEnv "WEAVE_HTTP_LARGE_CA").getD ""
      check (← rejectedStartup { client.config with endpoint := { client.config.endpoint with caBundle := oversized } })
        "oversized CA file accepted during startup"
      let wrongName ← Http.Client.create { client.config with endpoint := { client.config.endpoint with hostName := "wrong.localhost" } }
      try verificationRejected wrongName "TLS hostname mismatch"
      finally wrongName.close
      let badCA := (← IO.getEnv "WEAVE_HTTP_BAD_CA").getD ""
      let wrongTrust ← Http.Client.create { client.config with endpoint := { client.config.endpoint with caBundle := badCA } }
      try verificationRejected wrongTrust "untrusted TLS certificate"
      finally wrongTrust.close
      let systemTrust ← Http.Client.create { client.config with endpoint := { client.config.endpoint with caBundle := "",systemTrust := true } }
      try verificationRejected systemTrust "opt-in system trust with an untrusted certificate"
      finally systemTrust.close
      let verified ← Http.Client.create client.config
      try
        let health ← response "verified endpoint after negative TLS cases" (← drive (verified.request {}))
        check (health.status == 200 && health.header "x-protocol" == some expectedProtocol) "verified endpoint lost after rejection controls"
      finally verified.close
    IO.println ((tracer.otlp spans).compress)
    IO.println s!"native HTTP bounded/trace/chunked/TLS/cancel tests passed ({expectedProtocol})"
  finally client.close
