import LeanWeave.Stream

namespace LeanWeave.Test.Generator

private def check (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw (IO.userError ("stream generator: " ++ message))

private def data (event : Option Stream.Event) : IO ByteArray := match event with
  | some (.data bytes) => pure bytes
  | _ => throw (IO.userError "expected generator data")

private def failed (action : IO α) : IO Bool := do
  try let _ ← action; return false catch _ => return true

private def deferredOrder : IO Unit := do
  let starts ← IO.mkRef (#[] : Array (Nat × Nat))
  let polls ← IO.mkRef (0 : Nat)
  let cancellations ← IO.mkRef (0 : Nat)
  let closes ← IO.mkRef (#[] : Array (Option Stream.CancelReason))
  let step := fun (state budget : Nat) => do
    starts.modify (·.push (state,budget))
    let attempts ← IO.mkRef (0 : Nat)
    let _ ← AppM.await (do
      polls.modify (· + 1)
      let count ← attempts.get
      attempts.modify (· + 1)
      return if count == 0 then none else some (.ok ()))
      (fun _ => cancellations.modify (· + 1)) 10000 8
    if state == 2 then return Stream.Step.finish #[{ name := "x-end", value := "yes".toUTF8 }]
    return Stream.Step.next ((if state == 0 then "abc" else "de").toUTF8) (state + 1)
  let config : Stream.GeneratorConfig := {
    stateRetainedBytes := 8, operationRetainedBytes := 8
    declaredTrailers := #["x-end"], idleTimeoutMs := 1234, lifetimeMs := 5678 }
  let body ← Stream.unfoldM 0 step (fun _ => 8) config (fun reason => closes.modify (·.push reason))
  check (body.retainedBytes == config.retainedBytes && body.idleTimeoutMs == 1234 && body.lifetimeMs == 5678) "fixed reservations and native deadlines"
  check (← body.poll 0).isNone "zero initial credit remains pending"
  check ((← starts.get).isEmpty) "zero credit does not execute producer"
  check (← body.poll 5).isNone "actual AppM await suspends"
  check ((← starts.get) == #[(0,5)] && (← polls.get) == 1) "one deferred operation"
  check (← body.poll 0).isNone "zero credit does not advance a pending operation"
  check ((← polls.get) == 1) "pending operation not polled without credit"
  check ((← data (← body.poll 1)) == "a".toUTF8) "smaller completion credit splits result"
  check ((← data (← body.poll 1)) == "b".toUTF8 && (← data (← body.poll 1)) == "c".toUTF8) "retained chunk drains in order"
  check ((← starts.get).size == 1) "no eager next state while chunk remains"
  check (← body.poll 2).isNone "next state awaits independently"
  check ((← data (← body.poll 2)) == "de".toUTF8) "state transition order"
  check (← body.poll 4).isNone "terminal transition awaits"
  let terminal ← body.poll 4
  check (match terminal with | some (.finish fields) => header? fields "x-end" == some "yes" | _ => false) "terminal trailers preserved"
  check (match ← body.poll 0 with | some (.finish _) => true | _ => false) "known terminal completion needs no DATA credit"
  check ((← starts.get) == #[(0,5),(1,2),(2,4)] && (← cancellations.get) == 0) "exactly one execution per ordered transition"
  check ((← closes.get).isEmpty) "cleanup waits for native finish admission"
  body.close none
  body.close (some .timeout)
  check ((← closes.get) == #[none] && (← body.poll 4).isNone) "normal cleanup exactly once"

private def headAndCancel : IO Unit := do
  let starts ← IO.mkRef (0 : Nat)
  let closes ← IO.mkRef (0 : Nat)
  let head ← Stream.unfoldM () (fun _ _ => do starts.modify (· + 1); return .finish #[])
    (fun _ => 0) {} (fun _ => closes.modify (· + 1))
  head.close none
  head.close none
  check ((← starts.get) == 0 && (← closes.get) == 1 && (← head.poll 1).isNone) "HEAD closes without any execution"
  let cancelReasons ← IO.mkRef (#[] : Array Stream.CancelReason)
  let closeReasons ← IO.mkRef (#[] : Array (Option Stream.CancelReason))
  let continuation ← IO.mkRef (none : Option (Except AppError (Stream.Step Nat) → IO Unit))
  let step : Nat → Nat → AppM (Stream.Step Nat) := fun _ _ => ⟨fun runtime done => do
    continuation.set (some done)
    runtime.suspend { poll := pure false, cancel := fun reason => cancelReasons.modify (·.push reason), retainedBytes := 8 }⟩
  let body ← Stream.unfoldM 0 step (fun _ => 8) { stateRetainedBytes := 8, operationRetainedBytes := 8 }
    (fun reason => closeReasons.modify (·.push reason))
  check (← body.poll 4).isNone "manual deferred completion pending"
  body.close (some .streamReset)
  if let some done ← continuation.get then done (.ok (.next "late".toUTF8 1))
  body.close (some .disconnected)
  check ((← body.poll 4).isNone && (← cancelReasons.get) == #[.streamReset] &&
    (← closeReasons.get) == #[some .streamReset]) "cancellation terminal; late emission suppressed and cleanup once"

private def errorCases : IO Unit := do
  for action in #[
      (fun (_ _ : Nat) => (throw ({status := 500, message := "failure"} : AppError) : AppM (Stream.Step Nat))),
      (fun (_ _ : Nat) => do let _ ← (throw (IO.userError "failure") : IO Unit); return Stream.Step.finish #[]),
      (fun (_ _ : Nat) => pure (Stream.Step.next ByteArray.empty 1)),
      (fun (_ budget : Nat) => pure (Stream.Step.next (ByteArray.mk (Array.replicate (budget+1) 65)) 1)),
      (fun (_ _ : Nat) => pure (Stream.Step.next "x".toUTF8 99)),
      (fun (_ _ : Nat) => pure (Stream.Step.finish #[{ name := "x-undeclared", value := ByteArray.empty }]))
    ] do
    let closes ← IO.mkRef (0 : Nat)
    let body ← Stream.unfoldM 0 action (fun state => state + 1) {stateRetainedBytes := 8} (fun _ => closes.modify (· + 1))
    check (← failed (body.poll 4)) "application/IO/chunk/state/trailer violation rejected"
    body.close (some .failed)
    check ((← closes.get) == 1 && (← body.poll 4).isNone) "producer failure cleanup once"
  let cancel ← IO.mkRef (#[] : Array Stream.CancelReason)
  let close ← IO.mkRef (#[] : Array (Option Stream.CancelReason))
  let body ← Stream.unfoldM () (fun _ _ => do
    let _ ← AppM.await (α := Unit) (pure none) (fun reason => cancel.modify (·.push reason)) 10000 9
    return .finish #[]) (fun _ => 0) {operationRetainedBytes := 8} (fun reason => close.modify (·.push reason))
  check (← failed (body.poll 4)) "oversized deferred reservation fails closed"
  check ((← cancel.get) == #[.overloaded] && (← close.get) == #[some .overloaded]) "operation admission/cancel reason preserved"
  let cleaned ← IO.mkRef (0 : Nat)
  check (← failed (Stream.unfoldM 9 (fun _ _ => pure (.finish #[])) (fun n => n)
    {stateRetainedBytes := 8} (fun _ => cleaned.modify (· + 1)))) "constructor validates initial reservation"
  check ((← cleaned.get) == 1) "constructor failure releases owner"
  let wrongLength ← Stream.unfoldM () (fun _ _ => pure (.finish #[])) (fun _ => 0) {knownLength := some 1}
  check (← failed (wrongLength.poll 4)) "terminal known length validated"

private def deadline : IO Unit := do
  let cancelled ← IO.mkRef (#[] : Array Stream.CancelReason)
  let closed ← IO.mkRef (#[] : Array (Option Stream.CancelReason))
  let body ← Stream.unfoldM () (fun _ _ => do
    let _ ← AppM.await (α := Unit) (pure none) (fun reason => cancelled.modify (·.push reason)) 10000
    return .finish #[]) (fun _ => 0) {stepTimeoutMs := 100} (fun reason => closed.modify (·.push reason))
  let _ ← body.poll 1
  IO.sleep 150
  check (← failed (body.poll 1)) "whole step timeout is terminal"
  body.close (some .failed)
  check ((← cancelled.get) == #[.timeout] && (← closed.get) == #[some .timeout]) "step timeout cancels producer exactly once"
  let awaitClosed ← IO.mkRef (#[] : Array (Option Stream.CancelReason))
  let awaiting ← Stream.unfoldM () (fun _ _ => do
    let _ ← AppM.await (α := Unit) (pure none) (fun _ => pure ()) 1
    return .finish #[]) (fun _ => 0) {} (fun reason => awaitClosed.modify (·.push reason))
  let _ ← try awaiting.poll 1 catch _ => pure none
  IO.sleep 5
  let _ ← try awaiting.poll 1 catch _ => pure none
  check ((← awaitClosed.get) == #[some .timeout]) "AppM await deadline propagated"

private def multipartBounds : IO Unit := do
  let polls ← IO.mkRef (0 : Nat)
  let closes ← IO.mkRef (0 : Nat)
  let body : Stream.Body := {
    poll := fun _ => do polls.modify (· + 1); return some (.finish #[])
    close := fun _ => closes.modify (· + 1) }
  let huge : LeanNet.HTTP1.Header := {name := "", value := ByteArray.mk (Array.replicate 65536 65)}
  let error ← try let _ ← Stream.multipart "x" #[⟨#[huge],body⟩]; pure "" catch error => pure error.toString
  check ((error.splitOn "16KiB").length > 1) "multipart sums bytes before header scanning/prefix construction"
  check ((← closes.get) == 1 && (← polls.get) == 0) "oversized multipart closes child without producing"
  let header : LeanNet.HTTP1.Header := {name := "x", value := ByteArray.mk (Array.replicate 8190 65)}
  check (← failed (Stream.multipart "x" #[⟨#[header,header],body⟩])) "multipart cumulative header bytes bounded"
  check ((← closes.get) == 2) "failed multipart construction closes each admitted child once"

def run : IO Unit := do
  deferredOrder
  headAndCancel
  errorCases
  deadline
  multipartBounds
  IO.println "PASS typed stream generator: deferred order/flow, reservations, cancellation, cleanup, deadlines, trailers, multipart bounds"

end LeanWeave.Test.Generator
