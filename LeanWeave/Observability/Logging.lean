import LeanWeave.Observability.Trace

namespace LeanWeave.Observability

inductive Level where
  | debug | info | warn | error
  deriving Repr, BEq, Ord

def Level.text : Level → String
  | .debug => "debug" | .info => "info" | .warn => "warn" | .error => "error"

structure LogConfig where
  maxPending : Nat := 1024
  maxBytes : Nat := 1024 * 1024
  maxRecordBytes : Nat := 8192
  maxFields : Nat := 32
  sampleBasisPoints : Nat := 10000
  minimumLevel : Level := .info

private structure LogState where
  pending : Array ByteArray := #[]
  bytes : Nat := 0
  dropped : Nat := 0
  sampledOut : Nat := 0
  sampleBasisPoints : Nat := 10000
  minimumLevel : Level := .info

/-- Owner-local bounded JSON-line producer. `take` transfers owned batches to an
async sink; callbacks never write stdout/files or spawn a per-request task. -/
structure Logger where
  config : LogConfig
  private state : IO.Ref LogState

structure LogStats where
  pending : Nat
  bytes : Nat
  dropped : Nat
  sampledOut : Nat
  deriving Repr, BEq

def Logger.create (config : LogConfig := {}) : IO Logger := do
  if config.maxPending == 0 || config.maxBytes == 0 || config.maxRecordBytes < 512 ||
      config.maxRecordBytes > config.maxBytes || config.maxFields > 128 || config.sampleBasisPoints > 10000 then
    throw (IO.userError "invalid logger bounds")
  return { config, state := ← IO.mkRef { sampleBasisPoints := config.sampleBasisPoints, minimumLevel := config.minimumLevel } }

/-- Runtime sampling and level reload affects already-composed middleware. -/
def Logger.configure (logger : Logger) (sampleBasisPoints : Nat) (minimumLevel : Level := .info) : IO Unit := do
  if sampleBasisPoints > 10000 then throw (IO.userError "sampling exceeds 10000 basis points")
  logger.state.modify fun s => { s with sampleBasisPoints, minimumLevel }

private def sensitive (key : String) : Bool :=
  let key := key.toLower
  ["authorization", "cookie", "password", "secret", "token", "body", "sql", "session"].any (fun part =>
    (key.splitOn part).length > 1)

private def bucket (identity : String) : Nat :=
  (Security.Crypto.sha256 identity.toUTF8).data.foldl (fun (n : UInt64) byte => n * 256 + byte.toUInt64) 0 |>.toNat |>(· % 10000)

/-- Errors always survive sampling (subject to explicit minimum level and bounds).
Sensitive field names are redacted; messages are event codes, never exceptions. -/
def Logger.emit (logger : Logger) (level : Level) (event : String) (ctx : Option Context := none)
    (fields : Array (String × String) := #[]) : IO Bool := do
  let state ← logger.state.get
  if compare level state.minimumLevel == .lt then return false
  let identity := ctx.map (fun c => if c.requestId.isEmpty then c.tenant else c.requestId) |>.getD event
  if level != .error && bucket identity >= state.sampleBasisPoints then
    logger.state.modify fun s => { s with sampledOut := s.sampledOut + 1 }
    return false
  let values := (fields.extract 0 logger.config.maxFields).map fun (key,value) =>
    (boundedText key 128, Lean.toJson (if sensitive key then "[REDACTED]" else boundedText value 512))
  let values := Lean.Json.mkObj (values.toList)
  let json := Lean.Json.mkObj [
    ("timeUnixNano", Lean.toJson (toString (← unixNanos))), ("level", Lean.toJson level.text),
    ("event", Lean.toJson (boundedText event 128)), ("requestId", Lean.toJson (boundedText ((ctx.map (·.requestId)).getD "") 128)),
    ("tenant", Lean.toJson (boundedText ((ctx.map (·.tenant)).getD "") 128)),
    ("user", Lean.toJson (boundedText ((ctx.bind fun c => if c.authenticated then c.attribute "principal" else none).getD "") 256)),
    ("traceparent", Lean.toJson ((ctx.bind traceparent).bind parseTraceparent |>.map formatTraceparent |>.getD "")), ("fields", values)]
  let bytes := (json.compress ++ "\n").toUTF8
  if bytes.size > logger.config.maxRecordBytes || state.pending.size >= logger.config.maxPending || bytes.size > logger.config.maxBytes - state.bytes then
    logger.state.modify fun s => { s with dropped := s.dropped + 1 }
    return false
  logger.state.modify fun s => { s with pending := s.pending.push bytes, bytes := s.bytes + bytes.size }
  return true

def Logger.take (logger : Logger) (maximum : Nat := 64) : IO (Array ByteArray) := do
  let state ← logger.state.get
  let count := min maximum state.pending.size
  let batch := state.pending.extract 0 count
  logger.state.set { state with
    pending := state.pending.extract count state.pending.size
    bytes := state.bytes - batch.foldl (fun n item => n + item.size) 0 }
  return batch

/-- Return unaccepted sink records without silently exceeding producer limits. -/
def Logger.restore (logger : Logger) (records : Array ByteArray) : IO Unit := do
  logger.state.modify fun state => Id.run do
    let mut state := state
    for bytes in records.reverse do
      if bytes.size <= logger.config.maxRecordBytes && state.pending.size < logger.config.maxPending && bytes.size <= logger.config.maxBytes - state.bytes then
        state := { state with pending := #[bytes] ++ state.pending, bytes := state.bytes + bytes.size }
      else state := { state with dropped := state.dropped + 1 }
    return state

def Logger.stats (logger : Logger) : IO LogStats := do
  let state ← logger.state.get
  return { pending := state.pending.size, bytes := state.bytes, dropped := state.dropped, sampledOut := state.sampledOut }

def Logger.middleware (logger : Logger) : Middleware := fun next ctx => do
  let started ← IO.monoMsNow
  try
    let response ← next ctx
    let _ ← logger.emit (if response.status >= 500 then .error else .info) "http.request" (some ctx) #[
      ("method", ctx.request.method), ("route", ctx.routeName), ("status", toString response.status),
      ("durationMs", toString ((← IO.monoMsNow) - started))]
    return response
  catch error =>
    let _ ← logger.emit .error "http.error" (some ctx) #[("status", toString error.status)]
    throw error

end LeanWeave.Observability
