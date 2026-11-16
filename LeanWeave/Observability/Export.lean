import LeanWeave.Observability.Trace
import LeanWeave.Operations.Runtime

namespace LeanWeave.Observability

structure ExportResult where
  accepted : Bool
  retryable : Bool := false
  rejected : Nat := 0

abbrev ExportTransport := ByteArray → AppM ExportResult

structure ExportConfig where
  maxBatch : Nat := 64
  maxBatchBytes : Nat := 256 * 1024
  operationTimeoutMs : Nat := 5000
  retainedBytes : Nat := 2 * 1024 * 1024
  flushIntervalMs : Nat := 1000
  retryInitialMs : Nat := 200
  retryMaxMs : Nat := 10000
  maxAttempts : Nat := 5

private structure ExportState where
  batch : Array Span := #[]
  bytes : ByteArray := ByteArray.empty
  operation : Option (AppM.Execution ExportResult) := none
  operationStartedMs : Nat := 0
  attempt : Nat := 0
  nextMs : Nat := 0
  deadline : Option Nat := none
  exported : Nat := 0
  dropped : Nat := 0
  failures : Nat := 0
  closed : Bool := false

structure ExportStats where
  pending : Nat
  active : Bool
  exported : Nat
  dropped : Nat
  failures : Nat
  deriving Repr, BEq

structure Exporter where
  config : ExportConfig
  tracer : Tracer
  private transport : ExportTransport
  private state : IO.Ref ExportState

def Exporter.create (tracer : Tracer) (transport : ExportTransport) (config : ExportConfig := {}) : IO Exporter := do
  if config.maxBatch == 0 || config.maxBatch > 1024 || config.maxBatchBytes < 1024 || config.retainedBytes < config.maxBatchBytes ||
      config.flushIntervalMs == 0 || config.flushIntervalMs > 60000 || config.retryInitialMs == 0 ||
      config.retryMaxMs < config.retryInitialMs || config.retryMaxMs > 60000 || config.maxAttempts == 0 || config.maxAttempts > 16 ||
      config.operationTimeoutMs == 0 || config.operationTimeoutMs > 60000 then throw (IO.userError "invalid OTLP exporter bounds")
  return { config, tracer, transport, state := ← IO.mkRef {} }

def Exporter.stats (exporter : Exporter) : IO ExportStats := do
  let s ← exporter.state.get
  return { pending := s.batch.size, active := s.operation.isSome, exported := s.exported, dropped := s.dropped, failures := s.failures }

def Exporter.close (exporter : Exporter) : IO Unit := do
  let s ← exporter.state.get
  if s.closed then return
  if let some operation := s.operation then operation.cancel .shutdown
  let remaining ← exporter.tracer.take exporter.tracer.config.maxPending
  exporter.state.set { s with
    closed := true, operation := none, batch := #[], bytes := ByteArray.empty
    dropped := s.dropped + s.batch.size + remaining.size }

/-- One retained batch, one cooperative operation. Network failure is at-least-once:
retry may duplicate accepted spans. Partial success is terminal and never retried. -/
def Exporter.tick (exporter : Exporter) (nowMs : Nat) : IO Unit := do
  let s ← exporter.state.get
  if s.closed then return
  if s.deadline.any (nowMs >= ·) then exporter.close; return
  if let some operation := s.operation then
    if nowMs - s.operationStartedMs >= exporter.config.operationTimeoutMs then
      operation.cancel .timeout
      exporter.state.modify fun state => { state with operation := none, failures := state.failures + 1, nextMs := nowMs }
      if s.attempt >= exporter.config.maxAttempts then
        exporter.state.modify fun state => { state with batch := #[], bytes := ByteArray.empty, attempt := 0, dropped := state.dropped + state.batch.size }
      return
    let result ← try operation.poll catch _ => pure (some (.error { status := 503, message := "export failed" }))
    let some result := result | return
    let s ← exporter.state.get
    let retryable := match result with | .ok result => result.retryable && !result.accepted | .error _ => true
    let accepted := match result with | .ok result => result.accepted | .error _ => false
    if retryable && s.attempt < exporter.config.maxAttempts then
      exporter.state.set { s with
        operation := none, failures := s.failures + 1
        nextMs := nowMs + min exporter.config.retryMaxMs (exporter.config.retryInitialMs * 2 ^ (s.attempt - 1)) }
      return
    let rejected := match result with | .ok result => min s.batch.size result.rejected | .error _ => s.batch.size
    exporter.state.set { s with
      operation := none, batch := #[], bytes := ByteArray.empty, attempt := 0
      exported := s.exported + (if accepted then s.batch.size - rejected else 0)
      dropped := s.dropped + (if accepted then rejected else s.batch.size), failures := s.failures + (if accepted then 0 else 1)
      nextMs := if s.deadline.isSome then nowMs else nowMs + exporter.config.flushIntervalMs }
  let s ← exporter.state.get
  if nowMs < s.nextMs then return
  if s.batch.isEmpty then
    let batch ← exporter.tracer.take exporter.config.maxBatch
    if batch.isEmpty then return
    let bytes := (exporter.tracer.otlp batch).compress.toUTF8
    if bytes.size > exporter.config.maxBatchBytes then
      exporter.state.modify fun s => { s with dropped := s.dropped + batch.size }
      return
    exporter.state.modify fun s => { s with batch, bytes, attempt := 0 }
  let s ← exporter.state.get
  let execution ← AppM.execute (exporter.transport s.bytes) exporter.config.retainedBytes
  exporter.state.set { s with operation := some execution, operationStartedMs := nowMs, attempt := s.attempt + 1 }

def Exporter.beginDrain (exporter : Exporter) (deadlineMs : Nat) : IO Unit :=
  exporter.state.modify fun s => { s with deadline := some deadlineMs, nextMs := 0 }

def Exporter.isDrained (exporter : Exporter) : IO Bool := do
  let s ← exporter.state.get
  let trace ← exporter.tracer.stats
  return s.closed || (s.operation.isNone && s.batch.isEmpty && trace.pending == 0 && trace.active == 0)

def Exporter.participant (exporter : Exporter) : Operations.Participant := {
  name := "otel.export"
  onTick := exporter.tick
  onDrain := exporter.beginDrain
  isDrained := exporter.isDrained
  close := exporter.close }

end LeanWeave.Observability
