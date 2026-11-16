import LeanWeave.Core.Server

namespace LeanWeave.Test.Execution

private def check (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw (IO.userError s!"execution: {message}")

private def request : LeanNet.HTTP1.Request := {
  method := "GET", target := "/".toUTF8,
  headers := #[{ name := "host", value := "example.test".toUTF8 }], body := ByteArray.empty }

private def sequential : IO Unit := do
  let polls ← IO.mkRef (0 : Nat)
  let cancels ← IO.mkRef (0 : Nat)
  let cleanups ← IO.mkRef (0 : Nat)
  let action : AppM Nat := do
    let first ← AppM.await (do
      polls.modify (· + 1)
      if (← polls.get) < 2 then return none
      return some (.ok 20)) (fun _ => cancels.modify (· + 1))
    let second ← AppM.await (pure (some (.ok 22))) (fun _ => cancels.modify (· + 1))
    return first + second
  let execution ← AppM.execute (AppM.ensuring action (cleanups.modify (· + 1)))
  check (← execution.result.get).isNone "await suspends initial evaluation"
  check (← execution.poll).isNone "first poll remains pending"
  check (← execution.poll).isNone "first continuation suspends at second await"
  let result ← execution.poll
  check (match result with | some (.ok 42) => true | _ => false) "sequential awaits preserve continuation"
  check ((← cancels.get) == 0) "successful awaits do not cancel"
  check ((← cleanups.get) == 1) "success cleanup exactly once"

private def cancellation : IO Unit := do
  let cancels ← IO.mkRef (0 : Nat)
  let cleanups ← IO.mkRef (0 : Nat)
  let completion ← IO.mkRef (none : Option (Except AppError Nat → IO Unit))
  let action : AppM Nat := ⟨fun runtime done => do
    completion.set (some done)
    runtime.suspend {
      poll := pure false
      retainedBytes := 1
      cancel := fun _ => cancels.modify (· + 1) }⟩
  let execution ← AppM.execute (AppM.ensuring action (cleanups.modify (· + 1)))
  execution.cancel .disconnected
  execution.cancel .timeout
  if let some done ← completion.get then done (.ok 99)
  check (← execution.poll).isNone "late completion cannot publish after disconnect"
  check (← execution.result.get).isNone "cancelled execution discards retained continuation"
  check ((← cancels.get) == 1 && (← cleanups.get) == 1) "cancellation and cleanup exactly once"

private def failures : IO Unit := do
  let cancels ← IO.mkRef (0 : Nat)
  let cleanups ← IO.mkRef (0 : Nat)
  let action : AppM Nat := AppM.await
    (throw (IO.userError "injected poll failure"))
    (fun _ => cancels.modify (· + 1))
  let execution ← AppM.execute (AppM.ensuring action (cleanups.modify (· + 1)))
  let failed ← try let _ ← execution.poll; pure false catch _ => pure true
  check failed "adapter failure reaches transport boundary"
  execution.cancel .failed
  check ((← cancels.get) == 1 && (← cleanups.get) == 1) "poll exception cancellation cleanup once"
  let timeouts ← IO.mkRef (0 : Nat)
  let timeoutCleanups ← IO.mkRef (0 : Nat)
  let action : AppM Nat := AppM.await (pure none) (fun _ => do
    timeouts.modify (· + 1)
    throw (IO.userError "injected cancellation failure")) 0
  let execution ← AppM.execute (AppM.ensuring action (timeoutCleanups.modify (· + 1)))
  let _ ← try let _ ← execution.poll; pure false catch _ => pure true
  execution.cancel .disconnected
  check ((← timeouts.get) == 1 && (← timeoutCleanups.get) == 1) "throwing timeout cancellation remains once"
  let rejected ← IO.mkRef (0 : Nat)
  let cleaned ← IO.mkRef (0 : Nat)
  let action : AppM Nat := AppM.await (pure none) (fun _ => rejected.modify (· + 1)) 30000 5
  let execution ← AppM.execute (AppM.ensuring action (cleaned.modify (· + 1))) 4
  check (match ← execution.poll with | some (.error e) => e.status == 503 | _ => false) "buffer overflow is explicit 503"
  check ((← rejected.get) == 1 && (← cleaned.get) == 1) "overflow releases adapter and resource"

private def serverAdapter : IO Unit := do
  let calls ← IO.mkRef (0 : Nat)
  let action : Handler := fun _ => do
    let _ ← AppM.await (do
      calls.modify (· + 1)
      return some (.ok ())) (fun _ => pure ())
    return Response.text "deferred"
  let handlers := Server.handlers action { appRetainedBytes := 4096 }
  let some start := handlers.onRequestDeferred | throw (IO.userError "missing deferred adapter")
  match ← start request with
  | .immediate _ => throw (IO.userError "await incorrectly completed inline")
  | .deferred job =>
    check (job.retainedBytes == 4096) "native job reserves whole AppM buffer allowance"
    let result ← job.poll
    check (result.any (fun value => value.response.body == "deferred".toUTF8)) "native completion receives rendered response"
  check ((← calls.get) == 1) "poll runs once on owner"
  let immediate := Server.handlers (fun _ => pure (Response.text "ready"))
  match ← immediate.onRequestDeferred.get! request with
  | .immediate value => check (value.response.body == "ready".toUTF8) "immediate reply avoids queue"
  | .deferred _ => throw (IO.userError "immediate handler suspended")
  let blocked := Server.handlers action {
    headerChecks := #[fun _ => .error { status := 403, message := "blocked" }] }
  let head : LeanNet.HTTP1.RequestHead := {
    method := request.method, target := request.target, headers := request.headers }
  match ← blocked.onHeaders head .none with
  | .reject value => check (value.response.status == 403) "header check early rejection"
  | _ => throw (IO.userError "header check failed open")
  let preflight := Server.handlers action {
    onHeaders := fun _ => pure (.ok (some { status := 204 })) }
  match ← preflight.onHeaders head .none with
  | .reject value => check (value.response.status == 204) "header preflight early response"
  | _ => throw (IO.userError "preflight not returned")
  check (match Server.Config.validate { port := 0 } with | .error _ => true | .ok _ => false) "typed startup rejects zero port"
  check (match Server.Config.validate { appRetainedBytes := 0 } with | .error _ => true | .ok _ => false) "startup rejects zero reservation"

/-- Cooperative execution tests require no sleeps, tasks or network mocks. -/
def run : IO Unit := do
  sequential
  cancellation
  failures
  serverAdapter

end LeanWeave.Test.Execution
