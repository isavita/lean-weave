import LeanWeaveHttp.Client
import LeanWeave.Observability.Export

namespace LeanWeave.Http

/-- Use a client created without a tracer to prevent exporter self-instrumentation.
Only OTLP/HTTP JSON is emitted; request/response sizes inherit client bounds. -/
def otlpTransport (client : Client) (path : String := "/v1/traces")
    (headers : Array (String × String) := #[]) : Observability.ExportTransport := fun bytes => do
  let result ← client.request {
    method := "POST", path, body := bytes
    headers := headers.push ("content-type", "application/json") }
  if result.status == 200 then
    if result.body.isEmpty then return { accepted := true }
    let .ok json := Lean.Json.parse (String.fromUTF8! result.body) |
      return { accepted := false, retryable := false }
    let rejected := ((json.getObjVal? "partialSuccess").bind fun result => result.getObjVal? "rejectedSpans").toOption
    let rejected := rejected.map (fun value => match value.getStr? with
      | .ok text => text.toNat?.getD 0
      | .error _ => (Lean.fromJson? value : Except String Nat).toOption.getD 0) |>.getD 0
    return { accepted := true, rejected }
  return { accepted := false, retryable := [408,429,502,503,504].contains result.status }

end LeanWeave.Http
