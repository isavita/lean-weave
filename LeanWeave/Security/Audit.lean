import LeanWeave.Security.Crypto
import LeanWeave.Core.Body
import Std.Sync.Mutex

namespace LeanWeave.Security.Audit

/-- Deliberate metadata only: no credential, payload, SQL or arbitrary exceptions. -/
structure Event where
  action : String
  actor : String
  tenant : String := ""
  target : String := ""
  requestId : String := ""
  timeUnixNano : Nat
  outcome : String := "success"
  changes : Array (String × String) := #[]
  deriving Repr, BEq, Lean.ToJson, Lean.FromJson

def Event.validate (event : Event) : Except String Unit := do
  if event.action.isEmpty || event.actor.isEmpty || event.action.utf8ByteSize > 128 || event.actor.utf8ByteSize > 256 ||
      event.tenant.utf8ByteSize > 128 || event.target.utf8ByteSize > 256 || event.requestId.utf8ByteSize > 128 ||
      event.outcome.utf8ByteSize > 64 || event.changes.size > 32 || event.timeUnixNano > 18446744073709551615 then
    throw "audit event bounds exceeded"
  if (Lean.toJson event).compress.utf8ByteSize > 24576 then throw "audit event byte limit exceeded"
  for (key,value) in event.changes do
    if key.isEmpty || key.utf8ByteSize > 128 || value.utf8ByteSize > 4096 then throw "audit change bounds exceeded"
    if ["secret", "password", "token", "cookie", "authorization", "body", "sql"].any (fun word =>
        (key.toLower.splitOn word).length > 1) then throw "sensitive audit change rejected"

structure Checkpoint where
  sequence : Nat := 0
  hash : String := String.ofList (List.replicate 64 '0')
  deriving Repr, BEq, Inhabited, Lean.ToJson, Lean.FromJson

/-- Constructor is private. Public reads cannot mutate an existing record. -/
structure Record where
  private mk ::
  sequence : Nat
  previousHash : String
  event : Event
  hash : String
  deriving Repr, BEq

private def payload (sequence : Nat) (previousHash : String) (event : Event) : Lean.Json :=
  Lean.Json.mkObj [("v", Lean.toJson (1 : Nat)), ("sequence", Lean.toJson sequence),
    ("previousHash", Lean.toJson previousHash), ("event", Lean.toJson event)]

private def digest (sequence : Nat) (previousHash : String) (event : Event) : String :=
  Crypto.hex (Crypto.sha256 ("LeanWeave.audit.v1\n" ++ (payload sequence previousHash event).compress).toUTF8)

private def validHash (hash : String) : Bool := hash.utf8ByteSize == 64 && hash.toUTF8.data.all
  (fun b => (b >= 48 && b <= 57) || (b >= 97 && b <= 102))

def append (checkpoint : Checkpoint) (event : Event) : Except String Record := do
  event.validate
  if !validHash checkpoint.hash || checkpoint.sequence >= 9223372036854775807 then throw "invalid audit checkpoint"
  let sequence := checkpoint.sequence + 1
  return ⟨sequence, checkpoint.hash, event, digest sequence checkpoint.hash event⟩

def Record.checkpoint (record : Record) : Checkpoint := { sequence := record.sequence, hash := record.hash }

def Record.json (record : Record) : Lean.Json :=
  Lean.Json.mkObj [("v", Lean.toJson (1 : Nat)), ("sequence", Lean.toJson record.sequence),
    ("previousHash", Lean.toJson record.previousHash), ("event", Lean.toJson record.event), ("hash", Lean.toJson record.hash)]

def Record.line (record : Record) : String := record.json.compress ++ "\n"

/-- Reject noncanonical encodings, reordered/deleted/duplicated entries and hash
changes. A trusted externally stored final checkpoint also detects truncation or
complete chain replacement; a hash chain alone cannot detect those two attacks. -/
def verifyLine (checkpoint : Checkpoint) (line : String) : Except String Record := do
  if line.utf8ByteSize > 32768 then throw "audit record too large"
  let json ← (Body.json line.toUTF8 { maxBytes := 32768, maxDepth := 8 }).mapError (·.message)
  let version : Nat ← json.getObjValAs? _ "v"
  let sequence : Nat ← json.getObjValAs? _ "sequence"
  let previousHash : String ← json.getObjValAs? _ "previousHash"
  let event : Event ← json.getObjValAs? _ "event"
  let hash : String ← json.getObjValAs? _ "hash"
  if version != 1 || sequence != checkpoint.sequence + 1 || previousHash != checkpoint.hash then throw "audit chain discontinuity"
  let record ← append checkpoint event
  if hash != record.hash || line != record.json.compress then throw "audit record integrity failed"
  return record

def verify (lines : Array String) (start : Checkpoint := {}) (expected : Option Checkpoint := none) : Except String Checkpoint := do
  let mut checkpoint := start
  for line in lines do checkpoint := (← verifyLine checkpoint line).checkpoint
  if expected.any (· != checkpoint) then throw "audit trusted checkpoint mismatch"
  return checkpoint

private structure State where
  records : Array Record := #[]
  checkpoint : Checkpoint := {}
  bytes : Nat := 0

/-- Shared fail-closed bounded ledger. Export/anchor before capacity is exhausted;
records are never silently evicted. -/
structure Ledger where
  private state : Std.Mutex State
  capacity : Nat
  maxBytes : Nat

def Ledger.create (capacity : Nat := 4096) (checkpoint : Checkpoint := {}) (maxBytes : Nat := 16 * 1024 * 1024) : IO Ledger := do
  if capacity == 0 || capacity > 65536 || maxBytes == 0 || maxBytes > 1024 * 1024 * 1024 || !validHash checkpoint.hash then throw (IO.userError "invalid audit ledger bounds")
  return { state := ← Std.Mutex.new { checkpoint }, capacity, maxBytes }

def Ledger.record (ledger : Ledger) (event : Event) : IO (Except AppError Checkpoint) := do
  let result ← ledger.state.tryAtomically do
    let state ← getThe State
    if state.records.size >= ledger.capacity then return .error { status := 503, message := "audit ledger full" }
    match append state.checkpoint event with
    | .error _ => return .error { status := 400, message := "invalid audit event" }
    | .ok record =>
      let bytes := record.line.utf8ByteSize
      if bytes > ledger.maxBytes - state.bytes then return .error { status := 503, message := "audit byte budget exhausted" }
      set ({ records := state.records.push record, checkpoint := record.checkpoint, bytes := state.bytes + bytes } : State)
      return .ok record.checkpoint
  return result.getD (.error { status := 503, message := "audit ledger busy" })

def Ledger.snapshot (ledger : Ledger) : IO (Except AppError (Array Record)) := do
  let result ← ledger.state.tryAtomically do return (← getThe State).records
  return match result with | some records => .ok records | none => .error { status := 503, message := "audit ledger busy" }

/-- Release only an externally persisted and verified prefix. Its checkpoint stays
as the anchor for subsequent appends; unrelated/unknown checkpoints fail. -/
def Ledger.acknowledge (ledger : Ledger) (checkpoint : Checkpoint) : IO (Except AppError Unit) := do
  let result ← ledger.state.tryAtomically do
    let state ← getThe State
    if !state.records.any (fun r => r.checkpoint == checkpoint) then
      return .error { status := 409, message := "audit checkpoint is not pending" }
    let records := state.records.filter (fun r => r.sequence > checkpoint.sequence)
    set { state with records, bytes := records.foldl (fun (n : Nat) (r : Record) => n + r.line.utf8ByteSize) 0 }
    return .ok ()
  return result.getD (.error { status := 503, message := "audit ledger busy" })

end LeanWeave.Security.Audit
