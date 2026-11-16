import LeanWeaveHttp.Vault
import LeanWeaveHttp.Otlp
import LeanWeavePostgres
import LeanWeave.Observability.Jobs
import LeanWeave.Jobs.Worker

open LeanWeave

private def need (value : Except ε α) : IO α := match value with
  | .ok value => pure value | .error _ => throw (IO.userError "platform integration operation failed")
private def check (value : Bool) (message : String) : IO Unit := unless value do throw (IO.userError message)
private def drive (action : AppM α) : IO α := do
  let execution ← AppM.execute action (8 * 1024 * 1024)
  try
    for _ in [:20000] do
      if let some result ← execution.poll then return ← need result
      IO.sleep 1
    throw (IO.userError "integration operation deadline")
  finally execution.cancel .shutdown

private def vault : IO Unit := do
  let port := ((← IO.getEnv "WEAVE_VAULT_PORT").bind String.toNat?).getD 0
  let ca := (← IO.getEnv "WEAVE_VAULT_CA").getD ""
  let bootstrap := (← IO.getEnv "WEAVE_VAULT_TOKEN").getD ""
  let token ← IO.mkRef (← need (Secrets.Secret.bootstrap bootstrap.toUTF8))
  let client ← Http.Client.create {
    endpoint := { hostName := "localhost", hostAddr := "127.0.0.1", port := port.toUInt16, caBundle := ca }
    timeoutMs := 2000 }
  let headers := fun (credential : String) => #[("x-vault-token", credential),("content-type", "application/json")]
  let put := fun credential value => drive (client.request {
    method := "POST", path := "/v1/secret/data/app", headers := headers credential
    body := (Lean.Json.mkObj [("data", Lean.Json.mkObj [("key", Lean.toJson value)])]).compress.toUTF8 })
  let provider ← need (Http.Vault.backend client token.get { bindings := #[{ source := "app", path := "app", field := "key", ttlMs := 200 }] })
  let manager ← Secrets.Manager.create #[{ name := "key", source := "app" }] provider
  let awaitVersion := fun version value => do
    let mut loaded := false
    for _ in [:3000] do
      manager.tick (← IO.monoMsNow)
      if let .ok secret ← manager.get "key" then
        if secret.version == version then
          let actual ← need (secret.use (← IO.monoMsNow) String.fromUTF8!)
          check (actual == value) "Vault secret value mismatch"
          loaded := true
          break
      IO.sleep 1
    check loaded "Vault rotation deadline"
  try
    check ((← put bootstrap "rotation-one").status == 200) "Vault initial write"
    awaitVersion "1" "rotation-one"
    let orphan ← drive (client.request {
      method := "POST", path := "/v1/auth/token/create-orphan", headers := headers bootstrap
      body := "{\"policies\":[\"root\"],\"ttl\":\"1h\"}".toUTF8 })
    check (orphan.status == 200) "Vault token rotation setup"
    let json ← need (Lean.Json.parse (String.fromUTF8! orphan.body))
    let auth ← need (json.getObjVal? "auth")
    let fresh : String ← need (auth.getObjValAs? _ "client_token")
    token.set (← need (Secrets.Secret.bootstrap fresh.toUTF8))
    check ((← drive (client.request { method := "POST", path := "/v1/auth/token/revoke-self", headers := headers bootstrap })).status == 204) "Vault revoke old token"
    check ((← put fresh "rotation-two").status == 200) "Vault rotated credential write"
    awaitVersion "2" "rotation-two"
    check ((← drive (client.request { method := "DELETE", path := "/v1/secret/data/app", headers := headers fresh })).status == 204) "Vault delete latest"
    let mut expired := false
    for _ in [:3000] do
      manager.tick (← IO.monoMsNow)
      if !(← manager.get "key").isOk then expired := true; break
      IO.sleep 1
    check expired "failed Vault refresh extended secret expiry"
    IO.println "official Vault TLS rotation, credential rotation, expiry and memory-only reads passed"
  finally
    manager.close
    client.close

private def otel : IO Unit := do
  let port := ((← IO.getEnv "WEAVE_OTLP_PORT").bind String.toNat?).getD 0
  let endpoint : Http.Endpoint := { hostName := "localhost", hostAddr := "127.0.0.1", port := port.toUInt16, secure := false }
  let tracer ← Observability.Tracer.create { serviceName := "leanweave-integration" }
  let client ← Http.Client.create { endpoint } (some tracer)
  let exportClient ← Http.Client.create { endpoint }
  let pgConfig ← need (← Postgres.Config.fromEnv)
  let pool ← Postgres.Pool.create pgConfig (some tracer)
  let request ← tracer.start "integration.request" (some "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01") .server
  let ctx : Context := {
    request := { method := "GET", target := "/integration".toUTF8, headers := #[], body := ByteArray.empty }
    tenant := "alpha", requestId := "integration-request", attributes := #[("otel.traceparent", Observability.formatTraceparent request.context)] }
  let effect := fun (ctx : Context) => do
    let result ← (pool.withTraceContext ctx).query "SELECT $1::text" #[some "parameter-is-never-exported"]
    if result.rows != #[#[some "parameter-is-never-exported"]] then throw { message := "database trace probe failed" }
    let response ← client.request { method := "POST", path := "/v1/traces", headers := #[("content-type", "application/json")], body := "{\"resourceSpans\":[]}".toUTF8 } (some ctx)
    if response.status != 200 then throw { message := "HTTP trace probe failed" }
  try
    drive (effect ctx)
    let registry ← need (Jobs.Registry.create #[{
      kind := "integration", validate := fun value => .ok value
      run := fun job _ => effect { ctx with attributes := #[("otel.traceparent", job.metadata.traceparent)] } }])
    let outcome ← Jobs.performSync registry {
      id := ← Jobs.newId, kind := "integration", payload := .null
      metadata := { tenant := "alpha", requestId := "integration-request", traceparent := Observability.formatTraceparent request.context } }
      (Observability.jobsHooks tracer)
    check (outcome == .success) "instrumented job failed"
    request.finish .ok #[] ""
    let exporter ← Observability.Exporter.create tracer (Http.otlpTransport exportClient)
    exporter.beginDrain ((← IO.monoMsNow)+5000)
    try
      for _ in [:6000] do
        exporter.tick (← IO.monoMsNow)
        if ← exporter.isDrained then break
        IO.sleep 1
      let stats ← exporter.stats
      check (stats.exported == 6 && stats.dropped == 0) "official OTLP collector rejected spans"
      IO.println "official OTLP collector accepted six real request/DB/HTTP/job spans"
    finally exporter.close
  finally
    pool.close
    client.close
    exportClient.close

def main (arguments : List String) : IO Unit := do
  match arguments with
  | ["vault"] => vault
  | ["otel"] => otel
  | _ => throw (IO.userError "expected vault or otel integration mode")
