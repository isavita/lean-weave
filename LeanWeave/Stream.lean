import LeanWeave.Core.Stream
import LeanWeave.Core.Types

namespace LeanWeave.Stream

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
      if part.headers.size > 32 || !part.headers.all validHeader ||
          !part.body.declaredTrailers.isEmpty then
        throw (IO.userError "invalid multipart part headers/trailers")
      let fields := part.headers.foldl (fun bytes h => bytes ++ h.name.toUTF8 ++ ": ".toUTF8 ++ h.value ++ "\r\n".toUTF8) ByteArray.empty
      if fields.size > 16384 then throw (IO.userError "multipart part headers exceed 16KiB")
      prefixes := prefixes.push (s!"--{boundary}\r\n".toUTF8 ++ fields ++ "\r\n".toUTF8)
    let index ← IO.mkRef (0 : Nat)
    let phase ← IO.mkRef (0 : Nat)
    let pending ← IO.mkRef ByteArray.empty
    let closed ← IO.mkRef (Array.replicate parts.size false)
    let final := s!"--{boundary}--\r\n".toUTF8
    return {
      retainedBytes := parts.foldl (fun n p => n + p.body.retainedBytes) 0 +
        prefixes.foldl (fun n p => n + p.size) 0 + final.size
      poll := fun budget => do
        let i ← index.get
        let p ← phase.get
        if i == parts.size && p == 2 && (← pending.get).isEmpty then return some (.finish #[])
        if (← pending.get).isEmpty then
          if i == parts.size then
            pending.set final
            phase.set 2
          else if p == 0 then
            pending.set prefixes[i]!
            phase.set 1
          else
            match ← parts[i]!.body.poll budget with
            | none => return none
            | some (.data bytes) => return some (.data bytes)
            | some (.finish trailers) =>
              if !trailers.isEmpty then throw (IO.userError "multipart child emitted HTTP trailers")
              closed.modify (·.set! i true)
              parts[i]!.body.close none
              index.set (i + 1)
              phase.set 0
              pending.set "\r\n".toUTF8
        let bytes ← pending.get
        if budget == 0 then return none
        let count := min budget bytes.size
        pending.set (bytes.extract count bytes.size)
        return some (.data (bytes.extract 0 count))
      close := fun reason => do
        for i in [:parts.size] do
          if !(← closed.get)[i]! then
            closed.modify (·.set! i true)
            try parts[i]!.body.close reason catch _ => pure () }
  catch error =>
    for part in parts do try part.body.close (some .failed) catch _ => pure ()
    throw error

end LeanWeave.Stream
