import LeanWeave.Grpc.Framing

namespace LeanWeave.Grpc

/-- Cooperative, worker-local message stream. `none` is pending; `.ok none` is EOF.
Callbacks never block and own their bytes; `close` must release captured resources. -/
structure Source (α : Type) where
  poll : IO (Option (Except Status (Option α)))
  close : Option Stream.CancelReason → IO Unit := fun _ => pure ()
  retainedBytes : Nat := 0

namespace Source

def empty : Source α := { poll := pure (some (.ok none)) }

def failed (status : Status) : Source α := { poll := pure (some (.error status)) }

def ofArray (values : Array α) (retainedBytes : Nat) : IO (Source α) := do
  let state ← IO.mkRef (some values)
  return {
    retainedBytes
    poll := do
      let some values ← state.get | return some (.ok none)
      let some first := values[0]? | state.set none; return some (.ok none)
      state.set (some (values.extract 1 values.size))
      return some (.ok (some first))
    close := fun _ => state.set none }

def mapResult (f : α → Except Status β) (source : Source α) : Source β := {
  source with poll := do
    return (← source.poll).map (fun result => result.bind fun value => value.mapM f) }

/-- Await one event without moving the source or continuation away from its owner. -/
def next (source : Source α) (timeout : Nat) : AppM (Except Status (Option α)) :=
  AppM.await (do return (← source.poll).map Except.ok) (fun reason => source.close (some reason))
    timeout source.retainedBytes

end Source

private structure InputState where
  bytes : ByteArray := ByteArray.empty
  carry : ByteArray := ByteArray.empty
  offset : Nat := 0
  count : Nat := 0
  total : Nat := 0
  ended : Bool := false
  closed : Bool := false
  failure : Option Status := none

/-- Parse message frames incrementally. Native request reads grant HTTP/2 receive
credit only when consumed. Logical completion clears adapter state; the native
transport drains an unfinished upload without resetting already-sent trailers. -/
def input (ctx : Context) (mode : Mode) (limits : Limits) (deadline : Nat) : IO (Source ByteArray) := do
  let incoming := ctx.request.incoming
  let wholeBody := if incoming.isSome then ByteArray.empty else ctx.request.body
  let state ← IO.mkRef ({} : InputState)
  let close := fun (_ : Option Stream.CancelReason) => do
    state.modify fun current => { current with bytes := ByteArray.empty, carry := ByteArray.empty, closed := true }
  let fail := fun (current : InputState) (status : Status) => do
    state.set { current with failure := some status, bytes := ByteArray.empty, carry := ByteArray.empty }
    return some (.error status)
  return {
    retainedBytes := limits.reservation
    close
    poll := do
      let mut current ← state.get
      if current.closed then return some (.ok none)
      if let some error := current.failure then return some (.error error)
      if (← IO.monoMsNow) >= deadline then return ← fail current ⟨.deadlineExceeded, "RPC deadline exceeded"⟩
      -- At most one bounded read per poll makes work fair among streams.
      if current.bytes.size < 5 || (current.bytes.size >= 5 && current.bytes.size < 5 + frameLength current.bytes) then
        if !current.ended then
          let readBudget := max 1 (min 16384 (limits.maxMessageBytes + 5 - current.bytes.size))
          let event ← match incoming with
            | some source => source.poll (if mode == .webText then max 4 (readBudget / 4 * 4) else readBudget)
            | none => pure (some (if current.offset < wholeBody.size then
                .data (wholeBody.extract current.offset (current.offset + readBudget)) else .finish #[]))
          match event with
          | none => return none
          | some (.cancelled _) => return ← fail current ⟨.cancelled, "RPC cancelled"⟩
          | some (.finish _) => current := { current with ended := true }
          | some (.data bytes) =>
            current := { current with offset := current.offset + bytes.size }
            let decoded ← match mode with
              | .webText => pure (unbase64Chunks current.carry bytes)
              | _ => pure (.ok (bytes, ByteArray.empty))
            match decoded with
            | .error error => return ← fail current error
            | .ok (bytes, carry) =>
              if bytes.size > limits.maxMessageBytes + 5 - current.bytes.size then
                return ← fail current ⟨.resourceExhausted, "RPC frame buffer exceeded"⟩
              current := { current with bytes := current.bytes ++ bytes, carry }
      state.set current
      if current.bytes.isEmpty && current.ended then
        if !current.carry.isEmpty then return ← fail current ⟨.invalidArgument, "truncated gRPC-Web base64"⟩
        return some (.ok none)
      if current.bytes.size < 5 then
        if current.ended then return ← fail current ⟨.invalidArgument, "truncated RPC frame header"⟩
        return none
      let flag := current.bytes[0]!
      if flag == 1 then return ← fail current ⟨.unimplemented, "RPC message compression is not enabled"⟩
      if flag != 0 then return ← fail current ⟨.invalidArgument, "invalid RPC frame flags"⟩
      let length := frameLength current.bytes
      if length > limits.maxMessageBytes then return ← fail current ⟨.resourceExhausted, "RPC message too large"⟩
      if current.count >= limits.maxMessages || length > limits.maxTotalBytes - current.total then
        return ← fail current ⟨.resourceExhausted, "RPC message stream limit exceeded"⟩
      if current.bytes.size < length + 5 then
        if current.ended then return ← fail current ⟨.invalidArgument, "truncated RPC message"⟩
        return none
      let payload := current.bytes.extract 5 (5 + length)
      state.set { current with
        bytes := current.bytes.extract (5 + length) current.bytes.size
        count := current.count + 1
        total := current.total + length }
      return some (.ok (some payload)) }

end LeanWeave.Grpc
