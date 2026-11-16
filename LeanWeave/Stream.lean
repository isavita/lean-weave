import LeanWeave.Core.Stream
import LeanWeave.Core.Types

namespace LeanWeave.Stream

/-- One typed generator transition. Chunks are nonempty owned bytes; terminal
trailers must have been declared in the generator configuration. -/
inductive Step (State : Type) where
  | next (bytes : ByteArray) (state : State)
  | finish (trailers : Array LeanNet.HTTP1.Header := #[])

structure GeneratorConfig where
  stateRetainedBytes : Nat := 65536
  operationRetainedBytes : Nat := 65536
  maxChunkBytes : Nat := 65536
  maxTrailerBytes : Nat := 16384
  stepTimeoutMs : Nat := 30000
  idleTimeoutMs : Nat := 30000
  lifetimeMs : Nat := 3600000
  knownLength : Option Nat := none
  declaredTrailers : Array String := #[]

/-- Fixed reservations include both old/new state during a transition and one
chunk while splitting it. `operationRetainedBytes` covers the suspended adapter;
the native body owner separately charges its own output queue. -/
def GeneratorConfig.retainedBytes (config : GeneratorConfig) : Nat :=
  2 * config.stateRetainedBytes + config.operationRetainedBytes + 2 * config.maxChunkBytes +
    2 * config.maxTrailerBytes + 8192 +
    config.declaredTrailers.foldl (fun bytes name => bytes + name.utf8ByteSize + 32) 0

private structure GeneratorState (State : Type) where
  value : Option State
  operation : Option (AppM.Execution (Step State)) := none
  operationBudget : Nat := 0
  startedMs : Nat := 0
  pending : ByteArray := ByteArray.empty
  trailers : Option (Array LeanNet.HTTP1.Header) := none
  produced : Nat := 0
  closed : Bool := false

/-- A typed cooperative generator, constructed on its eventual reactor owner.
The conservative `stateBytes` measurement must include all state retained by the
step closure. No step starts or advances without positive output credit, and
there is at most one AppM execution. Deferred results are split if later credit
shrinks. Native HEAD suppression closes without ever starting a step.

All callbacks stay on the owner and must be nonblocking. The terminal event is
sticky until native close; cleanup runs exactly once on admission, cancellation,
constructor failure or producer failure, never on physical peer acknowledgement. -/
def unfoldM (initial : State) (step : State → Nat → AppM (Step State))
    (stateBytes : State → Nat) (config : GeneratorConfig := {})
    (onClose : Option CancelReason → IO Unit := fun _ => pure ()) : IO Body := do
  let state ← IO.mkRef ({ value := some initial } : GeneratorState State)
  let close := fun reason => do
    let current ← state.get
    unless current.closed do
      state.set { current with
        value := none
        operation := none
        pending := ByteArray.empty
        trailers := none
        closed := true }
      try
        if let some operation := current.operation then operation.cancel (reason.getD .failed)
      finally onClose reason
  try
    if config.stateRetainedBytes > 16 * 1024 * 1024 || config.operationRetainedBytes > 16 * 1024 * 1024 ||
        config.maxChunkBytes == 0 || config.maxChunkBytes > 65536 ||
        config.maxTrailerBytes == 0 || config.maxTrailerBytes > 65536 ||
        config.stepTimeoutMs == 0 || config.stepTimeoutMs > 3600000 ||
        stateBytes initial > config.stateRetainedBytes then
      throw (IO.userError "invalid stream generator reservations")
    let body : Body := {
      retainedBytes := config.retainedBytes
      knownLength := config.knownLength
      declaredTrailers := config.declaredTrailers
      idleTimeoutMs := config.idleTimeoutMs
      lifetimeMs := config.lifetimeMs
      close
      poll := fun budget => do
        try
          let mut current ← state.get
          if current.closed then return none
          if let some trailers := current.trailers then return some (.finish trailers)
          if budget == 0 then return none
          if current.pending.isEmpty then
            if current.operation.isNone then
              let some value := current.value | throw (IO.userError "stream generator lost owner state")
              let credit := min budget config.maxChunkBytes
              let startedMs ← IO.monoMsNow
              let execution ← AppM.execute (step value credit) config.operationRetainedBytes
              current := { current with operation := some execution, operationBudget := credit, startedMs }
              state.set current
            if (← IO.monoMsNow) - current.startedMs >= config.stepTimeoutMs then
              close (some .timeout)
              throw (IO.userError "stream generator step deadline exceeded")
            let some operation := current.operation | throw (IO.userError "stream generator lost execution")
            let some result ← operation.poll | return none
            current := { current with operation := none }
            state.set current
            match result with
            | .error error =>
              close (some (if error.status == 504 then .timeout else if error.status == 503 then .overloaded else .failed))
              throw (IO.userError "stream generator operation failed")
            | .ok (.next bytes value) =>
              if bytes.isEmpty || bytes.size > current.operationBudget || bytes.size > config.maxChunkBytes ||
                  stateBytes value > config.stateRetainedBytes ||
                  config.knownLength.any (current.produced + bytes.size > ·) then
                throw (IO.userError "stream generator violated state/chunk reservation")
              current := { current with value := some value, pending := bytes, produced := current.produced + bytes.size }
              state.set current
            | .ok (.finish trailers) =>
              if trailers.size > 100 || config.knownLength.any (· != current.produced) then
                throw (IO.userError "stream generator terminal length/trailer count invalid")
              let mut trailerBytes := 0
              for trailer in trailers do
                trailerBytes := trailerBytes + trailer.name.utf8ByteSize + trailer.value.size + 4
                if trailerBytes > config.maxTrailerBytes ||
                    !config.declaredTrailers.any (·.toLower == trailer.name.toLower) ||
                    trailer.value.data.any (fun b => b != 9 && (b < 32 || b == 127)) then
                  throw (IO.userError "stream generator trailer bounds/declaration invalid")
              state.set { current with trailers := some trailers }
              return some (.finish trailers)
          let count := min budget current.pending.size
          let bytes := current.pending.extract 0 count
          state.set { current with pending := current.pending.extract count current.pending.size }
          return some (.data bytes)
        catch error =>
          try close (some .failed) catch _ => pure ()
          throw error }
    match body.validate with
    | .error error => throw (IO.userError error)
    | .ok () => return body
  catch error =>
    try close (some .failed) catch _ => pure ()
    throw error

structure Part where
  headers : Array LeanNet.HTTP1.Header := #[]
  body : Body

instance : Inhabited Part := ⟨{ body := { poll := fun _ => pure (some (.finish #[])) } }⟩

private def validHeader (field : LeanNet.HTTP1.Header) : Bool :=
  !field.name.isEmpty && field.name.toUTF8.data.all (fun b =>
    (b >= 48 && b <= 57) || (b >= 65 && b <= 90) || (b >= 97 && b <= 122) ||
    [33,35,36,37,38,39,42,43,45,46,94,95,96,124,126].contains b.toNat) &&
  !["content-length", "transfer-encoding", "connection", "trailer"].contains field.name.toLower &&
  field.value.data.all (fun b => b == 9 || (b >= 32 && b != 127))

/-- Streaming MIME parts. Prefixes/suffixes are precomputed; body generators are
polled individually under native flow credits. Every child closes once. -/
def multipart (boundary : String) (parts : Array Part) : IO Body := do
  try
    unless !boundary.isEmpty && boundary.utf8ByteSize <= 70 &&
        boundary.toUTF8.data.all (fun b => (b >= 48 && b <= 57) ||
          (b >= 65 && b <= 90) || (b >= 97 && b <= 122) || b == 45 || b == 95) do
      throw (IO.userError "multipart boundary must be 1..70 ASCII token bytes")
    if parts.size > 64 then throw (IO.userError "multipart part count exceeds 64")
    let mut prefixes : Array ByteArray := #[]
    for part in parts do
      if part.headers.size > 32 ||
          !part.body.declaredTrailers.isEmpty then
        throw (IO.userError "invalid multipart part headers/trailers")
      let mut headerBytes := 0
      for field in part.headers do
        headerBytes := headerBytes + field.name.utf8ByteSize + field.value.size + 4
        if headerBytes > 16384 then throw (IO.userError "multipart part headers exceed 16KiB")
      if !part.headers.all validHeader then throw (IO.userError "invalid multipart part headers/trailers")
      let fields := part.headers.foldl (fun bytes h => bytes ++ h.name.toUTF8 ++ ": ".toUTF8 ++ h.value ++ "\r\n".toUTF8) ByteArray.empty
      prefixes := prefixes.push (s!"--{boundary}\r\n".toUTF8 ++ fields ++ "\r\n".toUTF8)
    -- Capture only child bodies, releasing the original header arrays after
    -- their bounded serialization rather than retaining two copies per part.
    let bodies := parts.map (·.body)
    let index ← IO.mkRef (0 : Nat)
    let phase ← IO.mkRef (0 : Nat)
    let pending ← IO.mkRef ByteArray.empty
    let closed ← IO.mkRef (Array.replicate bodies.size false)
    let final := s!"--{boundary}--\r\n".toUTF8
    return {
      retainedBytes := bodies.foldl (fun n body => n + body.retainedBytes) 0 +
        prefixes.foldl (fun n p => n + p.size) 0 + final.size + bodies.size * 64 + 256
      poll := fun budget => do
        let i ← index.get
        let p ← phase.get
        if i == bodies.size && p == 2 && (← pending.get).isEmpty then return some (.finish #[])
        if (← pending.get).isEmpty then
          if i == bodies.size then
            pending.set final
            phase.set 2
          else if p == 0 then
            pending.set prefixes[i]!
            phase.set 1
          else
            let some body := bodies[i]? | throw (IO.userError "multipart child index invalid")
            match ← body.poll budget with
            | none => return none
            | some (.data bytes) => return some (.data bytes)
            | some (.finish trailers) =>
              if !trailers.isEmpty then throw (IO.userError "multipart child emitted HTTP trailers")
              closed.modify (·.set! i true)
              body.close none
              index.set (i + 1)
              phase.set 0
              pending.set "\r\n".toUTF8
        let bytes ← pending.get
        if budget == 0 then return none
        let count := min budget bytes.size
        pending.set (bytes.extract count bytes.size)
        return some (.data (bytes.extract 0 count))
      close := fun reason => do
        for i in [:bodies.size] do
          if !(← closed.get)[i]! then
            closed.modify (·.set! i true)
            if let some body := bodies[i]? then
              try body.close reason catch _ => pure () }
  catch error =>
    for part in parts do try part.body.close (some .failed) catch _ => pure ()
    throw error

end LeanWeave.Stream
