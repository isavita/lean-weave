namespace LeanWeave.Redis

/-- RESP2 replies. Binary cache values never pass through UTF-8 decoding. -/
inductive Value where
  | simple (value : String)
  | error (message : String)
  | integer (value : Int)
  | bulk (value : Option ByteArray)
  | array (values : Array Value)
  deriving Inhabited

structure Limits where
  maxRequestBytes : Nat := 16384
  maxResponseBytes : Nat := 16384
  maxBulkBytes : Nat := 16384
  maxElements : Nat := 256
  maxDepth : Nat := 16
  maxLineBytes : Nat := 1024
  deriving Repr, Inhabited

/-- Binary-safe command encoding; command/argument bytes cannot inject RESP. -/
def encode (arguments : Array ByteArray) (limits : Limits := {}) : Except String ByteArray := do
  if arguments.isEmpty || arguments.size > limits.maxElements then throw "Redis command argument count exceeds bound"
  let mut bytes := s!"*{arguments.size}\r\n".toUTF8
  for argument in arguments do
    if argument.size > limits.maxBulkBytes then throw "Redis argument exceeds bulk bound"
    let size := bytes.size + argument.size + 32
    if size > limits.maxRequestBytes + 32 then throw "Redis request exceeds byte bound"
    bytes := bytes ++ s!"${argument.size}\r\n".toUTF8 ++ argument ++ "\r\n".toUTF8
    if bytes.size > limits.maxRequestBytes then throw "Redis request exceeds byte bound"
  return bytes

private def line (bytes : ByteArray) (offset : Nat) (limits : Limits) : Except String (Option (ByteArray × Nat)) := do
  let ceiling := min bytes.size (offset + limits.maxLineBytes + 2)
  for index in [offset:ceiling] do
    if bytes[index]! == 10 then throw "Redis response has bare LF"
    if bytes[index]! == 13 then
      if index - offset > limits.maxLineBytes then throw "Redis response line exceeds bound"
      if index + 1 >= bytes.size then return none
      if bytes[index + 1]! != 10 then throw "Redis response has malformed CRLF"
      return some (bytes.extract offset index, index + 2)
  if bytes.size > offset + limits.maxLineBytes then throw "Redis response line exceeds bound"
  return none

private def text (bytes : ByteArray) : Except String String :=
  match String.fromUTF8? bytes with
  | some value => .ok value
  | none => .error "Redis metadata is not UTF-8"

private def parseAt (bytes : ByteArray) (offset depth : Nat) (limits : Limits) :
    Except String (Option (Value × Nat × Nat)) := do
  match depth with
  | 0 => throw "Redis array nesting exceeds depth bound"
  | remaining + 1 => do
    if offset >= bytes.size then return none
    let some (metadata, next) ← line bytes (offset + 1) limits | return none
    match bytes[offset]!.toNat with
    | 43 => return some (.simple (← text metadata), next, 1)
    | 45 => return some (.error (← text metadata), next, 1)
    | 58 =>
      let some value := (← text metadata).toInt? | throw "invalid Redis integer"
      return some (.integer value, next, 1)
    | 36 =>
      let metadata ← text metadata
      if metadata == "-1" then return some (.bulk none, next, 1)
      let some size := metadata.toNat? | throw "invalid Redis bulk length"
      if size > limits.maxBulkBytes || next + size + 2 > limits.maxResponseBytes then
        throw "Redis bulk exceeds response bound"
      if bytes.size < next + size + 2 then return none
      if bytes[next + size]! != 13 || bytes[next + size + 1]! != 10 then throw "invalid Redis bulk terminator"
      return some (.bulk (some (bytes.extract next (next + size))), next + size + 2, 1)
    | 42 =>
      let some count := (← text metadata).toNat? | throw "invalid Redis array length"
      if count > limits.maxElements then throw "Redis array exceeds element bound"
      let mut cursor := next
      let mut values := #[]
      let mut nodes := 1
      for _ in [:count] do
        let some (value, following, used) ← parseAt bytes cursor remaining limits | return none
        nodes := nodes + used
        if nodes > limits.maxElements then throw "Redis total response nodes exceed bound"
        values := values.push value
        cursor := following
      return some (.array values, cursor, nodes)
    | _ => throw "unsupported Redis RESP2 marker"
termination_by depth

/-- Decode one bounded RESP value from a persistent subscription buffer. -/
def parsePrefix (bytes : ByteArray) (limits : Limits := {}) : Except String (Option (Value × Nat)) := do
  if bytes.size > limits.maxResponseBytes then throw "Redis response exceeds byte bound"
  let some (value, used, _) ← parseAt bytes 0 limits.maxDepth limits | return none
  return some (value, used)

/-- `none` means a fragmented reply. Exactly one response is accepted; trailing
bytes are rejected to prevent silent protocol desynchronization. -/
def parse (bytes : ByteArray) (limits : Limits := {}) : Except String (Option Value) := do
  if bytes.size > limits.maxResponseBytes then throw "Redis response exceeds byte bound"
  let some (value, used, _) ← parseAt bytes 0 limits.maxDepth limits | return none
  if used != bytes.size then throw "unexpected trailing Redis response bytes"
  return some value

end LeanWeave.Redis
