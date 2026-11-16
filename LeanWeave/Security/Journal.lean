import LeanWeave.Security.Audit
import LeanWeave.Observability.File

namespace LeanWeave.Security.Audit

structure JournalConfig where
  capacity : Nat := 1024
  maxFileBytes : Nat := 32 * 1024 * 1024
  maxInFlight : Nat := 64
  startupTimeoutMs : Nat := 5000
  writeTimeoutMs : Nat := 5000
  file : Observability.FileConfig := {}

private structure Flight where
  checkpoint : Checkpoint
  receipt : IO.Promise Bool

private structure JournalState where
  flights : Array Flight := #[]
  checkpoint : Checkpoint
  fileBytes : Nat
  failed : Bool := false
  closed : Bool := false

/-- Single-chain append-only journal. File ownership/verification/writes/flushes
run on a dedicated writer. The controller and pulses stay on one reactor owner. -/
structure Journal where
  config : JournalConfig
  ledger : Ledger
  writer : Observability.FileWriter
  private state : IO.Ref JournalState

private def replay (path : System.FilePath) (maximum : Nat) (expected : Option Checkpoint) : IO (Checkpoint × Nat) :=
  IO.FS.withFile path .read fun file => do
    let mut checkpoint : Checkpoint := {}
    let mut total := 0
    let mut pending := ByteArray.empty
    repeat
      let chunk ← file.read 4096
      if chunk.isEmpty then break
      total := total + chunk.size
      if total > maximum then throw (IO.userError "audit file exceeds configured bound")
      for byte in chunk.data do
        if byte == 10 then
          let some line := String.fromUTF8? pending | throw (IO.userError "audit record is not UTF8")
          match verifyLine checkpoint line with
          | .error _ => throw (IO.userError "audit journal integrity failed")
          | .ok record => checkpoint := record.checkpoint
          pending := ByteArray.empty
        else
          pending := pending.push byte
          if pending.size > 32768 then throw (IO.userError "audit record exceeds byte limit")
    if !pending.isEmpty || expected.any (· != checkpoint) then throw (IO.userError "audit checkpoint/truncated record mismatch")
    return (checkpoint,total)

/-- Startup may wait for bounded verification before accepting connections.
Pass an externally trusted checkpoint to detect tail truncation/replacement. -/
def Journal.open (path : System.FilePath) (config : JournalConfig := {}) (expected : Option Checkpoint := none) : IO Journal := do
  if config.capacity == 0 || config.capacity > 65536 || config.maxFileBytes < 32768 || config.maxFileBytes > 1024 * 1024 * 1024 ||
      config.maxInFlight == 0 || config.maxInFlight > config.capacity || config.startupTimeoutMs == 0 || config.writeTimeoutMs == 0 then
    throw (IO.userError "invalid audit journal bounds")
  let loaded ← IO.Promise.new
  let writer ← Observability.FileWriter.start path config.file (do loaded.resolve (← replay path config.maxFileBytes expected)) (some (path.addExtension "lock"))
  let deadline := (← IO.monoMsNow) + config.startupTimeoutMs
  repeat
    if ← loaded.isResolved then break
    if (← writer.stats).any (·.failed) || (← IO.monoMsNow) >= deadline then
      writer.abort
      let _ ← writer.join 1000
      throw (IO.userError "audit journal startup verification failed")
    IO.sleep 1
  let (checkpoint,fileBytes) := loaded.result!.get
  let ledger ← Ledger.create config.capacity checkpoint
  return { config, ledger, writer, state := ← IO.mkRef { checkpoint, fileBytes } }

/-- Advance append receipts and fail closed on any writer/chain error. Acknowledged
records leave the bounded ledger; their durable checkpoint remains available. -/
def Journal.tick (journal : Journal) : IO Unit := do
  let s ← journal.state.get
  if s.closed || s.failed then return
  if (← journal.writer.stats).any (·.failed) then
    journal.state.modify fun s => { s with failed := true }
    return
  let mut flights := s.flights
  for flight in s.flights do
    if !(← flight.receipt.isResolved) then break
    let succeeded := flight.receipt.result!.get
    if !succeeded then journal.state.modify fun s => { s with failed := true }; return
    match ← journal.ledger.acknowledge flight.checkpoint with
    | .error _ => journal.state.modify fun s => { s with failed := true }; return
    | .ok () => pure ()
    flights := flights.extract 1 flights.size
    journal.state.modify fun s => { s with checkpoint := flight.checkpoint, flights }
  let .ok records ← journal.ledger.snapshot | return
  for record in records do
    let s ← journal.state.get
    if s.flights.size >= journal.config.maxInFlight then break
    if s.flights.any (·.checkpoint == record.checkpoint) then continue
    let bytes := record.line.toUTF8
    if bytes.size > journal.config.maxFileBytes - s.fileBytes then
      journal.state.modify fun s => { s with failed := true }
      return
    let some receipt ← journal.writer.enqueue bytes | break
    journal.state.modify fun s => { s with flights := s.flights.push { checkpoint := record.checkpoint, receipt }, fileBytes := s.fileBytes + bytes.size }

def Journal.checkpoint (journal : Journal) : IO Checkpoint := do return (← journal.state.get).checkpoint

/-- Security actions can await persistence before taking effect. Timeout/disconnect
stops waiting but does not erase an already admitted audit event. -/
def Journal.record (journal : Journal) (event : Event) : AppM Checkpoint := do
  let state ← journal.state.get
  if state.closed || state.failed then throw { status := 503, message := "audit journal unavailable" }
  let checkpoint ← match ← journal.ledger.record event with | .ok cp => pure cp | .error error => throw error
  AppM.await (do
    journal.tick
    let s ← journal.state.get
    if s.failed || s.closed then return some (.error { status := 503, message := "audit persistence failed" })
    if s.checkpoint.sequence >= checkpoint.sequence then return some (.ok checkpoint)
    return none) (fun _ => pure ()) journal.config.writeTimeoutMs 256

def Journal.isDrained (journal : Journal) : IO Bool := do
  let .ok records ← journal.ledger.snapshot | return false
  let s ← journal.state.get
  return !s.failed && records.isEmpty && s.flights.isEmpty

def Journal.close (journal : Journal) : IO Unit := do
  journal.state.modify fun s => { s with closed := true }
  journal.writer.stop

def Journal.participant (journal : Journal) : Operations.Participant := {
  name := "security.audit"
  onTick := fun _ => journal.tick
  isDrained := journal.isDrained
  close := journal.close }

end LeanWeave.Security.Audit
