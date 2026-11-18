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
  /-- Digits in the lexical/canonical mantissa or exponent field, including
      leading zeroes. Also bounds positive-exponent mantissa expansion. -/
  maxNumberDigits : Nat := 512
  /-- Absolute explicit decimal exponent. The default includes IEEE754 normal
      and subnormal spellings while bounding Lean's arbitrary-size expansion. -/
  maxExponentMagnitude : Nat := 1024
  deriving Repr

private def checkSize (bytes : ByteArray) (limits : Limits) : Except AppError Unit :=
  if bytes.size > limits.maxBytes then .error { status := 413, message := "body too large" } else .ok ()

def form (bytes : ByteArray) (limits : Limits := {}) : Except AppError (Array (String × String)) := do
  checkSize bytes limits
  let some text := String.fromUTF8? bytes | throw { status := 400, message := "form must be UTF-8" }
  (decodePairs text limits.maxFields limits.maxFieldBytes).mapError fun message => { status := 400, message }

/-- Numeric preflight only; grammar remains Lean.Json.parse's responsibility.
This also permits non-HTTP decoders to share the same allocation bounds. -/
def checkJsonNumbers (bytes : ByteArray) (limits : Limits := {}) : Except AppError Unit := do
  checkSize bytes limits
  if limits.maxNumberDigits == 0 || limits.maxNumberDigits > 4096 || limits.maxExponentMagnitude > 4096 then
    throw { status := 500, message := "invalid JSON numeric limits" }
  let mut inString := false
  let mut escaped := false
  let mut index := 0
  while index < bytes.size do
    let b := bytes[index]!
    if inString then
      if escaped then escaped := false
      else if b == 92 then escaped := true
      else if b == 34 then inString := false
      index := index + 1
    else if b == 34 then
      inString := true
      index := index + 1
    else if b == 45 || (b >= 48 && b <= 57) then
      let mut mantissaDigits := 0
      let mut exponentDigits := 0
      let mut exponent := 0
      let mut inExponent := false
      let mut exponentNegative := false
      let mut inFraction := false
      let mut fractionDigits := 0
      let mut significantDigits := 0
      -- Include malformed numeric punctuation too: rejecting its oversized
      -- prefix must happen before the parser can expand a valid prefix.
      while index < bytes.size && ((bytes[index]! >= 48 && bytes[index]! <= 57) ||
          [43,45,46,69,101].contains bytes[index]!.toNat) do
        let current := bytes[index]!
        if current == 69 || current == 101 then inExponent := true
        else if current == 45 && inExponent && exponentDigits == 0 then exponentNegative := true
        else if current == 46 && !inExponent then inFraction := true
        else if current >= 48 && current <= 57 then
          if inExponent then
            exponentDigits := exponentDigits + 1
            if exponentDigits > limits.maxNumberDigits then
              throw { status := 413, message := "JSON exponent digit limit exceeded" }
            exponent := exponent * 10 + current.toNat - 48
            if exponent > limits.maxExponentMagnitude then
              throw { status := 413, message := "JSON exponent magnitude limit exceeded" }
          else
            mantissaDigits := mantissaDigits + 1
            if inFraction then fractionDigits := fractionDigits + 1
            if current != 48 || significantDigits > 0 then significantDigits := significantDigits + 1
            if mantissaDigits > limits.maxNumberDigits then
              throw { status := 413, message := "JSON mantissa digit limit exceeded" }
        index := index + 1
      let padding := if exponentNegative then 0 else exponent - fractionDigits
      let denominator := if exponentNegative then fractionDigits + exponent else fractionDigits - exponent
      if mantissaDigits + padding > limits.maxNumberDigits then
        throw { status := 413, message := "JSON expanded mantissa digit limit exceeded" }
      if denominator > limits.maxExponentMagnitude then
        throw { status := 413, message := "JSON effective denominator exponent limit exceeded" }
      if significantDigits > 0 then
        let digits := significantDigits + padding
        -- Lean's toString pads tiny fractions to min(e, 9+digits(m)) places.
        -- Include that canonical lexical form so compress/reparse stays within
        -- the same numeric policy, even for long subnormal-like mantissas.
        let canonicalDigits := if denominator == 0 then digits else max digits (1 + min denominator (9 + digits))
        if canonicalDigits > limits.maxNumberDigits then
          throw { status := 413, message := "JSON canonical mantissa digit limit exceeded" }
    else index := index + 1

private partial def normalizeZeros : Lean.Json → Lean.Json
  | .num value => if value.mantissa == 0 then .num 0 else .num value
  | .arr values => .arr (values.map normalizeZeros)
  | .obj fields => Lean.Json.mkObj (fields.toArray.toList.map fun (key, value) => (key, normalizeZeros value))
  | value => value

/-- Byte, depth, element and numeric allocation limits are checked before
invoking Lean's JSON parser, which can otherwise expand a tiny `1eN` into 10^N. -/
def json (bytes : ByteArray) (limits : Limits := {}) : Except AppError Lean.Json := do
  checkSize bytes limits
  checkJsonNumbers bytes limits
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
  -- Lean4.35 can print zero with a large denominator as invalid `0.e-...`.
  -- Collapse only numeric zero; the bounded tree shape and nonzero values stay
  -- intact, and zero spellings become safe for response/storage round trips.
  normalizeZeros <$> (Lean.Json.parse text).mapError fun _ => { status := 400, message := "invalid JSON" }

structure Part where
  name : String
  filename : Option String := none
  contentType : Option String := none
  bytes : ByteArray
  deriving Inhabited

private def matchesPrefixAt (bytes needle : @& ByteArray) (offset : Nat) : Nat → Bool
  | 0 => true
  | i + 1 => bytes[offset + i]! == needle[i]! && matchesPrefixAt bytes needle offset i

@[inline] private def matchesAt (bytes needle : ByteArray) (offset : Nat) : Bool :=
  matchesPrefixAt bytes needle offset needle.size

@[inline] private def pairAt (bytes : ByteArray) (offset : Nat) (first second : UInt8) : Bool :=
  offset + 1 < bytes.size && bytes[offset]! == first && bytes[offset + 1]! == second

private def findBytes (bytes needle : ByteArray) (start last : Nat) : Option Nat := Id.run do
  if needle.size > bytes.size then return none
  let last := min last (bytes.size - needle.size)
  for offset in [start:last + 1] do
    if matchesAt bytes needle offset then return some offset
  return none

private def findBoundary (bytes marker : ByteArray) (start last : Nat) : Option Nat := Id.run do
  if marker.size + 2 > bytes.size then return none
  let last := min last (bytes.size - marker.size - 2)
  for offset in [start:last + 1] do
    if bytes[offset]! == 13 && matchesAt bytes marker offset then
      let tail := offset + marker.size
      if pairAt bytes tail 45 45 || pairAt bytes tail 13 10 then return some offset
  return none

private def quoted (text : String) : Option String := do
  if !text.startsWith "\"" || !text.endsWith "\"" || text.utf8ByteSize < 2 then none else
    let value := ((text.drop 1).toString.dropEnd 1).toString
    if value.contains '"' || value.contains '\\' || value.contains '\r' || value.contains '\n' then none else some value

private def dispositionFieldEnd (bytes : @& ByteArray) (offset : Nat) (inQuote : Bool) : Nat → Nat
  | 0 => offset
  | remaining + 1 =>
    let byte := bytes[offset]!
    if byte == 59 && !inQuote then offset
    else dispositionFieldEnd bytes (offset + 1) (if byte == 34 then !inQuote else inQuote) remaining

-- Header bytes were validated as UTF-8. ASCII delimiters always fall on character boundaries.
private def dispositionFields (text : String) : Array String := Id.run do
  let bytes := text.toUTF8
  let mut fields := #[]
  let mut start := 0
  while start <= bytes.size do
    let stop := dispositionFieldEnd bytes start false (bytes.size - start)
    fields := fields.push (String.fromUTF8! (bytes.extract start stop))
    start := stop + 1
  return fields

/-- Bounded multipart/form-data. Payloads stay bytes, including binary uploads.
    No upload is written to a filename supplied by a client. Nested multipart is rejected. -/
def multipart (bytes : ByteArray) (boundary : String) (limits : Limits := {}) : Except AppError (Array Part) := do
  checkSize bytes limits
  if boundary.isEmpty || boundary.utf8ByteSize > 70 ||
      !boundary.toUTF8.data.all (fun b => b >= 33 && b <= 126 && b != 34) then
    throw { status := 400, message := "invalid multipart boundary" }
  let marker := ("--" ++ boundary).toUTF8
  if bytes.size < marker.size || !matchesAt bytes marker 0 then
    throw { status := 400, message := "missing multipart boundary" }
  let delimiter := "\r\n".toUTF8 ++ marker
  let headerEnd := "\r\n\r\n".toUTF8
  let mut cursor := marker.size
  let mut parts := #[]
  let mut finished := false
  for _ in [:limits.maxParts + 1] do
    if !finished then
      if pairAt bytes cursor 45 45 then
        cursor := cursor + 2
        if cursor != bytes.size && !(cursor + 2 == bytes.size && pairAt bytes cursor 13 10) then
          throw { status := 400, message := "multipart epilogue not supported" }
        finished := true
        break
      else
        if parts.size >= limits.maxParts then throw { status := 413, message := "too many multipart parts" }
        if !pairAt bytes cursor 13 10 then throw { status := 400, message := "invalid multipart separator" }
        cursor := cursor + 2
        let lastHeaderEnd := cursor + limits.maxPartHeaderBytes
        let some headEnd := findBytes bytes headerEnd cursor lastHeaderEnd | do
          if lastHeaderEnd + headerEnd.size <= bytes.size then
            throw { status := 413, message := "multipart headers too large" }
          throw { status := 400, message := "incomplete multipart headers" }
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
        let fields := dispositionFields dispositionValue
        if fields[0]!.trimAscii.toString != "form-data" then throw { status := 400, message := "expected form-data disposition" }
        let mut name : Option String := none
        let mut filename : Option String := none
        for i in [1:fields.size] do
          let field := fields[i]!.trimAscii.toString
          let some equals := field.find? '=' | throw { status := 400, message := "invalid disposition parameter" }
          let key := (field.sliceTo equals).toString
          let some value := quoted (field.sliceFrom equals.next!).toString
            | throw { status := 400, message := "expected quoted disposition value" }
          if key == "name" && name.isNone then name := some value
          else if key == "filename" && filename.isNone then filename := some value
          else throw { status := 400, message := "duplicate or unsupported disposition parameter" }
        let some fieldName := name | throw { status := 400, message := "missing multipart field name" }
        let bodyStart := headEnd + headerEnd.size
        let maxSize := if filename.isSome then limits.maxFileBytes else limits.maxFieldBytes
        if fieldName.utf8ByteSize > limits.maxFieldBytes then throw { status := 413, message := "multipart name too large" }
        let lastBodyEnd := bodyStart + maxSize
        let some bodyEnd := findBoundary bytes delimiter bodyStart lastBodyEnd | do
          if lastBodyEnd + delimiter.size + 2 <= bytes.size then
            throw { status := 413, message := "multipart part too large" }
          throw { status := 400, message := "unterminated multipart body" }
        let payload := bytes.extract bodyStart bodyEnd
        parts := parts.push { name := fieldName, filename, contentType, bytes := payload }
        cursor := bodyEnd + 2 + marker.size
  if !finished then throw { status := 400, message := "missing final multipart boundary" }
  return parts

end LeanWeave.Body
