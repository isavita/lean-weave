import LeanWeave.Core

namespace LeanWeave.Body

structure Limits where
  maxBytes : Nat := 1024 * 1024
  maxFields : Nat := 128
  maxFieldBytes : Nat := 8192
  maxDepth : Nat := 32
  maxParts : Nat := 32
  maxPartHeaderBytes : Nat := 4096
  maxFileBytes : Nat := 512 * 1024
  deriving Repr

private def checkSize (bytes : ByteArray) (limits : Limits) : Except AppError Unit :=
  if bytes.size > limits.maxBytes then .error { status := 413, message := "body too large" } else .ok ()

def form (bytes : ByteArray) (limits : Limits := {}) : Except AppError (Array (String × String)) := do
  checkSize bytes limits
  let some text := String.fromUTF8? bytes | throw { status := 400, message := "form must be UTF-8" }
  (decodePairs text limits.maxFields limits.maxFieldBytes).mapError fun message => { status := 400, message }

/-- Depth and byte limits are checked before invoking the JSON parser. -/
def json (bytes : ByteArray) (limits : Limits := {}) : Except AppError Lean.Json := do
  checkSize bytes limits
  let some text := String.fromUTF8? bytes | throw { status := 400, message := "JSON must be UTF-8" }
  let mut depth := 0
  let mut inString := false
  let mut escaped := false
  let mut nodes := 0
  for b in bytes do
    if inString then
      if escaped then escaped := false
      else if b == 92 then escaped := true
      else if b == 34 then inString := false
    else if b == 34 then inString := true
    else if b == 123 || b == 91 then
      depth := depth + 1
      nodes := nodes + 1
      if depth > limits.maxDepth then throw { status := 413, message := "JSON nesting too deep" }
    else if b == 125 || b == 93 then depth := depth - 1
    else if b == 44 then nodes := nodes + 1
    if nodes > limits.maxFields * limits.maxFields then
      throw { status := 413, message := "too many JSON elements" }
  (Lean.Json.parse text).mapError fun _ => { status := 400, message := "invalid JSON" }

structure Part where
  name : String
  filename : Option String := none
  contentType : Option String := none
  bytes : ByteArray
  deriving Inhabited

private def findBytes (bytes needle : ByteArray) (start : Nat) : Option Nat := Id.run do
  for i in [start:bytes.size + 1] do
    if i + needle.size <= bytes.size then
      if (Array.range needle.size).all (fun n => bytes[i+n]! == needle[n]!) then return some i
  return none

private def findBoundary (bytes marker : ByteArray) (start : Nat) : Option Nat := Id.run do
  for i in [start:bytes.size + 1] do
    if i + marker.size + 2 <= bytes.size then
      if (Array.range marker.size).all (fun n => bytes[i+n]! == marker[n]!) then
        let tail := bytes.extract (i + marker.size) (i + marker.size + 2)
        if tail == "--".toUTF8 || tail == "\r\n".toUTF8 then return some i
  return none

private def quoted (text : String) : Option String := do
  if !text.startsWith "\"" || !text.endsWith "\"" || text.utf8ByteSize < 2 then none else
    let value := ((text.drop 1).toString.dropEnd 1).toString
    if value.contains '"' || value.contains '\\' || value.contains '\r' || value.contains '\n' then none else some value

/-- Bounded multipart/form-data. Payloads stay bytes, including binary uploads.
    No upload is written to a filename supplied by a client. Nested multipart is rejected. -/
def multipart (bytes : ByteArray) (boundary : String) (limits : Limits := {}) : Except AppError (Array Part) := do
  checkSize bytes limits
  if boundary.isEmpty || boundary.utf8ByteSize > 70 ||
      !boundary.toUTF8.data.all (fun b => b >= 33 && b <= 126 && b != 34) then
    throw { status := 400, message := "invalid multipart boundary" }
  let marker := ("--" ++ boundary).toUTF8
  if bytes.extract 0 marker.size != marker then throw { status := 400, message := "missing multipart boundary" }
  let mut cursor := marker.size
  let mut parts := #[]
  let mut finished := false
  for _ in [:limits.maxParts + 1] do
    if !finished then
      if bytes.extract cursor (cursor+2) == "--".toUTF8 then
        cursor := cursor + 2
        if bytes.extract cursor bytes.size != ByteArray.empty && bytes.extract cursor bytes.size != "\r\n".toUTF8 then
          throw { status := 400, message := "multipart epilogue not supported" }
        finished := true
      else
        if parts.size >= limits.maxParts then throw { status := 413, message := "too many multipart parts" }
        if bytes.extract cursor (cursor+2) != "\r\n".toUTF8 then throw { status := 400, message := "invalid multipart separator" }
        cursor := cursor + 2
        let some headEnd := findBytes bytes "\r\n\r\n".toUTF8 cursor
          | throw { status := 400, message := "incomplete multipart headers" }
        if headEnd - cursor > limits.maxPartHeaderBytes then throw { status := 413, message := "multipart headers too large" }
        let some head := String.fromUTF8? (bytes.extract cursor headEnd)
          | throw { status := 400, message := "invalid multipart headers" }
        let mut disposition : Option String := none
        let mut contentType : Option String := none
        for line in head.splitOn "\r\n" do
          let pieces := line.splitOn ":"
          let name := pieces.head!.toLower
          let value := (String.intercalate ":" pieces.tail!).trimAscii.toString
          if pieces.length < 2 then throw { status := 400, message := "invalid multipart header" }
          if name == "content-disposition" && disposition.isNone then disposition := some value
          else if name == "content-type" && contentType.isNone then contentType := some value
          else throw { status := 400, message := "duplicate or unsupported multipart header" }
        if contentType.any (fun v => v.toLower.startsWith "multipart/") then
          throw { status := 415, message := "nested multipart is unsupported" }
        let some dispositionValue := disposition | throw { status := 400, message := "missing content disposition" }
        let fields := dispositionValue.splitOn ";"
        if fields.head!.trimAscii.toString != "form-data" then throw { status := 400, message := "expected form-data disposition" }
        let mut name : Option String := none
        let mut filename : Option String := none
        for field in fields.tail! do
          let pair := field.trimAscii.toString.splitOn "="
          if pair.length != 2 then throw { status := 400, message := "invalid disposition parameter" }
          let some value := quoted pair[1]! | throw { status := 400, message := "expected quoted disposition value" }
          if pair[0]! == "name" && name.isNone then name := some value
          else if pair[0]! == "filename" && filename.isNone then filename := some value
          else throw { status := 400, message := "duplicate or unsupported disposition parameter" }
        let some fieldName := name | throw { status := 400, message := "missing multipart field name" }
        let bodyStart := headEnd + 4
        let some bodyEnd := findBoundary bytes ("\r\n".toUTF8 ++ marker) bodyStart
          | throw { status := 400, message := "unterminated multipart body" }
        let payload := bytes.extract bodyStart bodyEnd
        let maxSize := if filename.isSome then limits.maxFileBytes else limits.maxFieldBytes
        if payload.size > maxSize then throw { status := 413, message := "multipart part too large" }
        if fieldName.utf8ByteSize > limits.maxFieldBytes then throw { status := 413, message := "multipart name too large" }
        parts := parts.push { name := fieldName, filename, contentType, bytes := payload }
        cursor := bodyEnd + 2 + marker.size
  if !finished then throw { status := 400, message := "missing final multipart boundary" }
  return parts

end LeanWeave.Body
