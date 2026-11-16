import LeanWeave.Observability.Logging
import LeanWeave.Operations.Runtime
import Std.Sync.Mutex

namespace LeanWeave.Observability

structure FileConfig where
  maxRecords : Nat := 1024
  maxBytes : Nat := 1024 * 1024
  maxRecordBytes : Nat := 32768
  pollMs : UInt32 := 2

private structure Write where
  bytes : ByteArray
  receipt : IO.Promise Bool

private structure FileState where
  queue : Array Write := #[]
  bytes : Nat := 0
  records : Nat := 0
  written : Nat := 0
  ready : Bool := false
  finished : Bool := false
  failed : Bool := false

structure FileStats where
  pending : Nat
  bytes : Nat
  written : Nat
  ready : Bool
  finished : Bool
  failed : Bool
  deriving Repr, BEq

/-- One dedicated append-only writer. Only owned bytes/receipts cross threads;
request workers use try-lock admission and never perform file I/O. OS file flush
is provided, not a filesystem power-loss/fsync guarantee. -/
structure FileWriter where
  config : FileConfig
  private state : Std.Mutex FileState
  private stopping : IO.Promise Unit
  private aborting : IO.Promise Unit
  private task : Task (Except IO.Error Unit)

private def fileLoop (path : System.FilePath) (config : FileConfig) (state : Std.Mutex FileState)
    (stopping aborting : IO.Promise Unit) (setup : IO Unit) (lockPath : Option System.FilePath) : IO Unit := do
  try
    IO.FS.withFile path .append fun file => do
      let withLock := fun (lock : IO.FS.Handle) => do
        unless ← lock.tryLock do throw (IO.userError "log file already locked")
        try
          setup
          state.atomically (modify fun s => { s with ready := true })
          repeat
            if ← aborting.isResolved then break
            let item ← state.atomically do
              let s ← getThe FileState
              match s.queue[0]? with
              | none => return none
              | some item =>
                set { s with queue := s.queue.extract 1 s.queue.size }
                return some item
            if let some item := item then
              try
                file.write item.bytes
                file.flush
                item.receipt.resolve true
                state.atomically (modify fun s => { s with records := s.records - 1, bytes := s.bytes - item.bytes.size, written := s.written + 1 })
              catch error =>
                item.receipt.resolve false
                state.atomically (modify fun s => { s with records := s.records - 1, bytes := s.bytes - item.bytes.size })
                throw error
            else
              if ← stopping.isResolved then break
              IO.sleep config.pollMs
        finally lock.unlock
      match lockPath with
      | none => withLock file
      | some lockPath => IO.FS.withFile lockPath .append withLock
  catch _ => state.atomically (modify fun s => { s with failed := true })
  let pending ← state.atomically do
    let s ← getThe FileState
    set { s with queue := #[], bytes := 0, records := 0, ready := false, finished := true }
    return s.queue
  for item in pending do item.receipt.resolve false

def FileWriter.start (path : System.FilePath) (config : FileConfig := {}) (setup : IO Unit := pure ()) (lockPath : Option System.FilePath := none) : IO FileWriter := do
  if config.maxRecords == 0 || config.maxRecords > 65536 || config.maxBytes == 0 || config.maxRecordBytes == 0 ||
      config.maxRecordBytes > config.maxBytes || config.pollMs == 0 || config.pollMs > 100 then throw (IO.userError "invalid file writer bounds")
  let state ← Std.Mutex.new ({} : FileState)
  let stopping ← IO.Promise.new
  let aborting ← IO.Promise.new
  let task ← IO.asTask (fileLoop path config state stopping aborting setup lockPath) Task.Priority.dedicated
  return { config, state, stopping, aborting, task }

/-- A receipt resolves only after a successful append + OS flush. Failed admission
allocates no queue entry. Cancellation of a waiter never undoes an admitted log. -/
def FileWriter.enqueue (writer : FileWriter) (bytes : ByteArray) : IO (Option (IO.Promise Bool)) := do
  if bytes.isEmpty || bytes.size > writer.config.maxRecordBytes || (← writer.stopping.isResolved) || (← writer.aborting.isResolved) then return none
  let receipt ← IO.Promise.new
  let result ← writer.state.tryAtomically do
    let s ← getThe FileState
    if !s.ready || s.failed || s.finished || s.records >= writer.config.maxRecords || bytes.size > writer.config.maxBytes - s.bytes then return false
    set { s with queue := s.queue.push { bytes, receipt }, bytes := s.bytes + bytes.size, records := s.records + 1 }
    return true
  return if result == some true then some receipt else none

def FileWriter.stats (writer : FileWriter) : IO (Option FileStats) := do
  writer.state.tryAtomically do
    let s ← getThe FileState
    return { pending := s.records, bytes := s.bytes, written := s.written, ready := s.ready, finished := s.finished, failed := s.failed }

def FileWriter.stop (writer : FileWriter) : IO Unit := writer.stopping.resolve ()
def FileWriter.abort (writer : FileWriter) : IO Unit := do writer.stopping.resolve (); writer.aborting.resolve ()

/-- Use from a CLI/managed finalizer after the reactor has returned, never a
request callback. Deadline expiry requests cancellation and reports false. -/
def FileWriter.join (writer : FileWriter) (timeoutMs : Nat := 1000) : IO Bool := do
  writer.stop
  let deadline := (← IO.monoMsNow) + timeoutMs
  repeat
    if ← IO.hasFinished writer.task then
      let _ ← IO.ofExcept writer.task.get
      return true
    if (← IO.monoMsNow) >= deadline then writer.abort; return false
    IO.sleep 1

/-- Flush bounded logger batches on owner ticks. Backpressure retains records in
Logger until the writer has capacity; no task or blocking write occurs here. -/
def Logger.flushTo (logger : Logger) (writer : FileWriter) (maximum : Nat := 64) : IO Unit := do
  let records ← logger.take maximum
  let mut retry := #[]
  for bytes in records do
    if (← writer.enqueue bytes).isNone then retry := retry.push bytes
  if !retry.isEmpty then
    logger.restore retry

/-- Participant observes pending logger and writer data, so the transport waits
for final request logs until the shared cutoff. Final stop is nonblocking. -/
def FileWriter.participant (writer : FileWriter) (logger : Logger) : Operations.Participant := {
  name := "logging.file"
  onTick := fun _ => logger.flushTo writer
  isDrained := do
    let logs ← logger.stats
    return (← writer.stats).any (fun s => logs.pending == 0 && s.pending == 0 && !s.failed)
  close := writer.stop }

end LeanWeave.Observability
