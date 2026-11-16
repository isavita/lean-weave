import LeanWeave.Observability.Trace
import LeanWeave.Observability.Logging
import LeanWeave.Jobs.Types

namespace LeanWeave.Observability

/-- Create tracer/logger inside Workers.startOwned's factory. Never pass a single
owner-local tracer to multiple worker hooks. Backend.tick can advance its exporter. -/
def jobsHooks (tracer : Tracer) (logger : Option Logger := none) : Jobs.Hooks := {
  startSpan := fun event => do
    let span ← tracer.start ("job " ++ event.kind) (if event.traceparent.isEmpty then none else some event.traceparent) .consumer #[
      { key := "messaging.system", value := .text "leanweave.jobs" },
      { key := "messaging.message.id", value := .text event.jobId },
      { key := "tenant.id", value := .text event.tenant },
      { key := "request.id", value := .text event.requestId },
      { key := "job.attempt", value := .integer (Int.ofNat event.attempt) }]
    return {
      traceparent := formatTraceparent span.context
      finish := fun final =>
        span.finish (if final.state == .succeeded then .ok else .error) #[
        { key := "job.state", value := .text (reprStr final.state) },
        { key := "job.duration_ms", value := .integer (Int.ofNat final.durationMs) }]
          (final.errorCode.getD "") }
  emit := fun event => do
    if let some logger := logger then
      let ctx : Context := {
        request := { method := "JOB", target := "/".toUTF8, headers := #[], body := ByteArray.empty }
        tenant := event.tenant, requestId := event.requestId, attributes := #[("otel.traceparent", event.traceparent)] }
      let _ ← logger.emit (if event.errorStatus.isSome then .error else .info) "job.state" (some ctx)
        #[("job.id", event.jobId), ("job.kind", event.kind), ("job.state", reprStr event.state), ("attempt", toString event.attempt)]
      pure () }

end LeanWeave.Observability
