import LeanWeaveHttp

open LeanWeave

def drive (action : AppM α) (budget : Nat := 2 * 1024 * 1024) : IO (Except AppError α) := do
  let execution ← AppM.execute action budget
  try
    for _ in [:10000] do
      if let some result ← execution.poll then return result
      IO.sleep 1
    throw (IO.userError "client test timed out")
  finally execution.cancel .shutdown

private def check (value : Bool) (message : String) : IO Unit := unless value do throw (IO.userError message)

def main : IO Unit := do
  let port := ((← IO.getEnv "WEAVE_HTTP_PORT").bind String.toNat?).getD 0
  let ca := (← IO.getEnv "WEAVE_HTTP_CA").getD ""
  let secure := (← IO.getEnv "WEAVE_HTTP_TLS") == some "1"
  let tracer ← Observability.Tracer.create
  let client ← Http.Client.create {
    endpoint := { hostName := "localhost", hostAddr := "127.0.0.1", port := port.toUInt16, secure, caBundle := ca }
    maxActive := 2
    timeoutMs := 200
    maxResponseBytes := 4096 } (some tracer)
  try
    let ctx : Context := { request := { method := "GET", target := "/".toUTF8, headers := #[], body := ByteArray.empty }, attributes := #[
      ("otel.traceparent", "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01")] }
    let echo ← drive (client.request { method := "POST", path := "/echo", body := "native-http".toUTF8 } (some ctx))
    let .ok echo := echo | throw (IO.userError "echo failed")
    check (echo.status == 200 && String.fromUTF8! echo.body == "native-http") "echo mismatch"
    let .ok chunked ← drive (client.request { path := "/chunked" }) | throw (IO.userError "chunked failed")
    check (String.fromUTF8! chunked.body == "onetwo") "chunked decoding"
    let .ok redirect ← drive (client.request { path := "/redirect" }) | throw (IO.userError "redirect failed")
    check (redirect.status == 302) "redirect was followed"
    check (!(← drive (client.request { path := "/oversized" })).isOk) "body bound not enforced"
    check (!(← drive (client.request { path := "/slow" })).isOk) "timeout not enforced"
    let pending ← AppM.execute (client.request { path := "/slow" }) client.config.retainedBytes
    let _ ← pending.poll
    pending.cancel .disconnected
    check (← client.isIdle) "disconnect leaked client handle"
    let spans ← tracer.take
    check (spans.size == 6) "automatic HTTP span missing or duplicated"
    let some first := spans[0]? | throw (IO.userError "missing HTTP span")
    check (Security.Crypto.hex first.context.traceId == "4bf92f3577b34da6a3ce929d0e0e4736") "trace ID propagation failed"
    IO.println ((tracer.otlp spans).compress)
    IO.println "native HTTP bounded/trace/chunked/TLS/cancel tests passed"
  finally client.close
