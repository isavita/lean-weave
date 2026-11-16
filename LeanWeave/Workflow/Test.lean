import LeanWeave.Workflow.Local
import LeanWeave.Jobs.Worker

namespace LeanWeave.Workflow.Test

private def check (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw (IO.userError s!"workflow: {message}")
private def appOk (result : Except AppError α) : IO α :=
  match result with | .ok value => pure value | .error e => throw (IO.userError e.message)
private def parseOk (result : Except String α) : IO α :=
  match result with | .ok value => pure value | .error e => throw (IO.userError e)
private def action (name : String) : Action := { kind := "step",payload := Lean.toJson name }
private def saga : Plan := { name := "saga",steps := #[
  { name := "a",action := action "a",compensation := some (action "undo-a"),maxAttempts := 1 },
  { name := "b",action := action "b",dependsOn := #["a"],compensation := some (action "undo-b"),maxAttempts := 1 },
  { name := "c",action := action "fail",dependsOn := #["b"],maxAttempts := 1 }] }

/-- Execute dependent jobs, permanent failure and reverse saga compensation on
real dedicated workers; tenant isolation and plan graph errors are checked too. -/
def run : IO Unit := do
  let seen ← Std.Mutex.new (#[] : Array String)
  let record := fun value => (seen.atomically (modify (·.push value)) : IO Unit)
  let registry ← parseOk (Jobs.Registry.create #[Jobs.Definition.typed "step" (fun (s : String) =>
      if s.isEmpty || s.utf8ByteSize > 32 then .error "bad step" else .ok s)
    (fun _ value => do
      record value
      if value == "fail" then throw { status := 422,message := "permanent" })])
  check (!(saga.validate registry).toOption.isNone) "valid DAG"
  let cycle : Plan := { saga with steps := saga.steps.map fun (step : Step) => if step.name == "a" then { step with dependsOn := #["c"] } else step }
  check (!(cycle.validate registry).isOk) "dependency cycle rejected"
  check (!({ saga with steps := #[{ name := "x",action := action "x",dependsOn := #["missing"] }] }.validate registry).isOk) "missing dependency rejected"
  let admission ← Jobs.Admission.new
  let engine ← Engine.create registry {} (some admission)
  let queue ← Jobs.Queue.create registry { maxEntries := 1,maxHistory := 1 } {} 30000 (some admission)
  let id ← appOk (← engine.start saga { tenant := "tenant-a" })
  check ((← appOk (← engine.get "tenant-b" id)).isNone) "tenant isolated workflow read"
  Jobs.withWorkers (fun _ => pure (engine.backend queue,registry)) (fun workers => do
    let result ← workers.drain 5000
    check result.drained "saga worker drain"
    let some flow ← appOk (← engine.get "tenant-a" id) | throw (IO.userError "missing saga")
    check (flow.status == .failed) "failed saga compensated"
    check ((← seen.atomically get) == #["a","b","fail","undo-b","undo-a"]) "reverse topological compensation"
    check (flow.steps[0]!.stage == .compensated && flow.steps[1]!.stage == .compensated) "compensation state persisted"
  ) { workers := 2,pollMs := 2 } {} (some queue.admission)
  let encoded := Lean.toJson ((← appOk (← engine.get "tenant-a" id)).get!)
  check ((Lean.fromJson? encoded : Except String Instance).isOk) "state serializes for durable backend"
  check (← appOk (← engine.remove "tenant-a" id)) "terminal retention removal"

  let enabled ← Std.Mutex.new false
  let retriableRegistry ← parseOk (Jobs.Registry.create #[Jobs.Definition.typed "step" (fun (s : String) => .ok s)
    (fun _ value => do
      if value == "fail" then throw { status := 422,message := "forward failed" }
      if value == "undo" && !(← (enabled.atomically get : IO Bool)) then
        throw { status := 422,message := "operator required" })])
  let retryEngine ← Engine.create retriableRegistry
  let retryQueue ← Jobs.Queue.create retriableRegistry { maxEntries := 8,maxHistory := 8 }
  let retryPlan : Plan := { name := "retry-compensation",steps := #[
    { name := "a",action := action "a",compensation := some (action "undo"),maxAttempts := 1 },
    { name := "b",action := action "fail",dependsOn := #["a"],maxAttempts := 1 }] }
  let retryId ← appOk (← retryEngine.start retryPlan { tenant := "tenant-a" })
  let waitStatus := fun expected => do
    let cutoff := (← IO.monoMsNow) + 3000
    repeat
      if let .ok (some flow) ← retryEngine.get "tenant-a" retryId then
        if flow.status == expected then break
      if (← IO.monoMsNow) >= cutoff then throw (IO.userError "workflow compensation retry deadline")
      IO.sleep 1
  Jobs.withWorkers (fun _ => pure (retryEngine.backend retryQueue,retriableRegistry)) (fun workers => do
    waitStatus .compensationFailed
    enabled.atomically (set true)
    let cutoff := (← IO.monoMsNow) + 1000
    repeat
      match ← retryEngine.retryCompensation retryQueue "tenant-a" retryId with
      | .ok true => break
      | .ok false => throw (IO.userError "failed compensation not retried")
      | .error error =>
        unless error.status == 503 && (← IO.monoMsNow) < cutoff do throw (IO.userError error.message)
        IO.sleep 1
    waitStatus .failed
    check (← appOk (← retryEngine.retryCompensation retryQueue "tenant-b" retryId) >>= fun accepted => pure (!accepted)) "retry tenant isolated"
    check (← workers.drain 1000).drained "retried compensation drains"
  ) { workers := 4,pollMs := 1 } {} (some retryQueue.admission)
  IO.println "workflow DAG/dependency/compensation/serialization/tenant checks passed"

end LeanWeave.Workflow.Test
