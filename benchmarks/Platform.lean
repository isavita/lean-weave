import LeanWeave.Tenant
import LeanWeave.Feature
import LeanWeave.Security.Audit
import LeanWeave.Observability.Trace
import LeanWeave.Observability.Logging
import LeanWeave.Realtime

open LeanWeave

private def sample (name : String) (count : Nat) (action : Nat → IO Nat) : IO Unit := do
  let _ ← action 0
  let allocationStart ← IO.getNumHeartbeats
  let start ← IO.monoNanosNow
  let mut checksum := 0
  for i in [:count] do checksum := checksum + (← action i)
  let elapsed := (← IO.monoNanosNow) - start
  let allocations := (← IO.getNumHeartbeats) - allocationStart
  IO.println (Lean.Json.mkObj [("name", Lean.toJson name), ("iterations", Lean.toJson count),
    ("nanoseconds", Lean.toJson elapsed), ("small_allocations", Lean.toJson allocations), ("checksum", Lean.toJson checksum)]).compress

def main (arguments : List String) : IO Unit := do
  let count := (arguments.head?.bind String.toNat?).getD 5000
  if count == 0 || count > 1000000 then throw (IO.userError "iterations must be 1..1000000")
  let ctx : Context := {
    request := { method := "GET", target := "/dashboard".toUTF8, headers := #[], body := ByteArray.empty }
    host := "alpha.example.test", tenant := "alpha", requestId := "bench-request" }
  sample "tenant_subdomain_resolution" count fun _ => do
    match ← (Tenant.fromSubdomain "example.test" ctx).run with
    | .ok (some resolution) => return resolution.id.value.utf8ByteSize
    | _ => throw (IO.userError "tenant benchmark failed")
  let flag : Feature.Flag := { name := "release", enabled := true, percentage := 5000 }
  sample "feature_sticky_cohort" count fun i => do
    return (Feature.evaluate flag { ctx with attributes := #[("feature.subject", s!"user-{i % 100}")] }).bucket
  let tracer ← Observability.Tracer.create { maxPending := 1 }
  sample "sampled_span_start_finish_take" count fun _ => do
    let span ← tracer.start "benchmark.request" none .server #[{ key := "tenant.id", value := .text "alpha" }]
    span.finish .ok #[] ""
    return (← tracer.take 1).size
  let logger ← Observability.Logger.create { maxPending := 1 }
  sample "structured_log_enqueue_take" count fun i => do
    let _ ← logger.emit .info "benchmark.request" (some ctx) #[("status", "200"),("iteration", toString i)]
    return (← logger.take 1).foldl (fun n record => n + record.size) 0
  let checkpoint ← IO.mkRef ({} : Security.Audit.Checkpoint)
  sample "audit_hash_chain_append" count fun i => do
    let event : Security.Audit.Event := { action := "export", actor := "benchmark", tenant := "alpha", timeUnixNano := i }
    match Security.Audit.append (← checkpoint.get) event with
    | .error error => throw (IO.userError error)
    | .ok record => checkpoint.set record.checkpoint; return record.line.utf8ByteSize
  let broker ← Realtime.Broker.new
  let subscription ← match ← broker.subscribe "benchmark" with
    | .ok subscription => pure subscription | .error error => throw (IO.userError error)
  sample "local_pubsub_fanout_poll" count fun i => do
    match ← broker.publish { topic := "benchmark", event := "tick", payload := Lean.toJson i } with
    | .error error => throw (IO.userError error)
    | .ok n =>
      let _ ← subscription.poll
      return n
