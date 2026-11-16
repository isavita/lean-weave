import LeanWeaveHttp.Client
import LeanWeave.Core.Body
import LeanWeave.Observability.Export

namespace LeanWeave.Http

/-- Bounded OTLP/HTTP response decoding. Malformed success envelopes are terminal
failures; rejectedSpans is a nonnegative int64, including its decimal-string form. -/
def decodeOtlpResponse (status : Nat) (body : ByteArray) (maxBytes : Nat := 65536) : Observability.ExportResult := Id.run do
  if status != 200 then return { accepted := false, retryable := [408,429,502,503,504].contains status }
  if body.isEmpty then return { accepted := true }
  let .ok json := Body.json body { maxBytes, maxDepth := 8, maxFields := 16 } |
    return { accepted := false }
  if !json.getObj?.isOk then return { accepted := false }
  let partialSuccess := (json.getObjVal? "partialSuccess").toOption
  if partialSuccess.any (fun value => !value.getObj?.isOk) then return { accepted := false }
  let rejected := partialSuccess.bind fun value => (value.getObjVal? "rejectedSpans").toOption
  let rejected : Option Nat := match rejected with
    | none => some 0
    | some value => match value.getStr? with
      | .ok text =>
        if text.isEmpty || text.utf8ByteSize > 19 || !text.toUTF8.data.all (fun b => b >= 48 && b <= 57) then none
        else text.toNat?.filter (· <= 9223372036854775807)
      | .error _ => (Lean.fromJson? value : Except String Nat).toOption.filter (· <= 9223372036854775807)
  let some rejected := rejected | return { accepted := false }
  return { accepted := true, rejected }

/-- Export calls suppress instrumentation, including when given a traced client.
Only OTLP/HTTP JSON is emitted; request/response sizes inherit client bounds. -/
def otlpTransport (client : Client) (path : String := "/v1/traces")
    (headers : Array (String × String) := #[]) : Observability.ExportTransport := fun bytes => do
  let client := client.withoutTracing
  let result ← client.request {
    method := "POST", path, body := bytes
    headers := headers.push ("content-type", "application/json") }
  return decodeOtlpResponse result.status result.body client.config.maxResponseBytes

end LeanWeave.Http
