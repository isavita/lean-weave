import LeanWeave.Jobs.Cron

namespace LeanWeave.Jobs.Cron.Test

/-- error: cron value outside field range -/
#guard_msgs in
#check cron! "60 * * * *"

private def check (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw (IO.userError s!"cron: {message}")

private def ok (result : Except String α) : IO α :=
  match result with | .ok value => pure value | .error error => throw (IO.userError error)

def run : IO Unit := do
  for source in ["* * * *", "* * * * * *", "60 * * * *", "* 24 * * *", "*/0 * * * *", "* * 0 * *", "* * * 13 *", "* * * * 8"] do
    check (!(parse source).isOk) s!"reject invalid expression {source}"
  check ((cron! "0 0 1 * *").matches 0) "epoch calendar DOM"
  check ((cron! "0 0 * * 4").matches 0) "Thursday convention"
  check (!(cron! "0 0 * * 0").matches 0) "Sunday convention"
  check ((cron! "0 0 1 * 0").matches 0) "restricted DOM or DOW"
  check (!(cron! "0 0 2 * 0").matches 0) "unmatched DOM and DOW"
  check ((cron! "*/15 0-2 * 1,2 *").matches (15 * 60000)) "ranges lists steps"
  let registry ← ok (Registry.create #[Definition.typed "count" (fun (n : Nat) => .ok n) (fun _ _ => pure ())])
  let jobs ← IO.mkRef (#[] : Array Envelope)
  let submit := fun (job : Envelope) => do jobs.modify (·.push job); return Except.ok job.id
  let scheduler ← Scheduler.create #[{ name := "minute", schedule := cron! "* * * * *", kind := "count", payload := Lean.toJson (1 : Nat) }] registry submit
  check ((← scheduler.tick 0).submitted == 1) "first epoch occurrence emitted"
  check ((← scheduler.tick 0).submitted == 0) "same minute not repeated"
  check ((← scheduler.tick 60000).submitted == 1) "next minute emitted"
  let captured ← jobs.get
  check (captured.size == 2 && captured[0]!.idempotencyKey == "cron:minute:0") "stable occurrence key"
  check ((← scheduler.nextDue 60000) == 120000) "minute timer boundary"
  let limited ← Scheduler.create #[
    { name := "one", schedule := cron! "* * * * *", kind := "count", payload := Lean.toJson (1 : Nat) },
    { name := "two", schedule := cron! "* * * * *", kind := "count", payload := Lean.toJson (2 : Nat) }] registry submit { maxEmissionsPerTick := 1 }
  let first ← limited.tick 0
  check (first.submitted == 1 && first.backlogged) "tick emission budget"
  check ((← limited.nextDue 0) == 100) "backlog schedules short timer retry"
  check ((← limited.tick 0).submitted == 1) "bounded continuation emits remaining schedule"
  let failure ← IO.mkRef true
  let retry ← Scheduler.create #[{ name := "retry", schedule := cron! "* * * * *", kind := "count", payload := Lean.toJson (1 : Nat) }] registry
    (fun job => do if ← failure.get then return .error { status := 503, message := "full" } else submit job)
  check ((← retry.tick 0).failed == 1) "backpressure reported"
  failure.set false
  check ((← retry.tick 0).submitted == 1) "failed occurrence retried"
  retry.stop
  check ((← retry.tick 60000).submitted == 0) "draining scheduler admits no later occurrences"
  IO.println "cron compile/calendar/timer/backpressure checks passed"

end LeanWeave.Jobs.Cron.Test
