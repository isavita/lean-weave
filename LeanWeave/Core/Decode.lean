import LeanWeave.Core.Types

namespace LeanWeave

private def hex? (c : UInt8) : Option Nat :=
  let n := c.toNat
  if n >= 48 && n <= 57 then some (n - 48)
  else if n >= 65 && n <= 70 then some (n - 55)
  else if n >= 97 && n <= 102 then some (n - 87)
  else none

/-- Strict one-pass percent decoding. Form mode additionally decodes `+`. -/
def percentDecode (s : String) (form : Bool := false) : Except String String := do
  let bytes := s.toUTF8
  let mut out := ByteArray.empty
  let mut i := 0
  for _ in [:bytes.size] do
    if i < bytes.size then
      let c := bytes[i]!
      if c == 37 then
        if i + 2 >= bytes.size then throw "incomplete percent escape"
        let some a := hex? bytes[i+1]! | throw "invalid percent escape"
        let some b := hex? bytes[i+2]! | throw "invalid percent escape"
        let decoded := (a*16+b).toUInt8
        if decoded == 0 then throw "NUL is forbidden"
        out := out.push decoded
        i := i + 3
      else
        if c == 0 then throw "NUL is forbidden"
        out := out.push (if form && c == 43 then 32 else c)
        i := i + 1
  let some text := String.fromUTF8? out | throw "invalid UTF-8"
  return text

/-- Percent-encode a single path segment or query component. -/
def percentEncode (s : String) : String := Id.run do
  let digits := "0123456789ABCDEF".toUTF8
  let mut out := ByteArray.empty
  for b in s.toUTF8 do
    let n := b.toNat
    if (n >= 65 && n <= 90) || (n >= 97 && n <= 122) ||
        (n >= 48 && n <= 57) || [45,46,95,126].contains n then
      out := out.push b
    else
      out := out.push 37 |>.push digits[n/16]! |>.push digits[n%16]!
  return String.fromUTF8! out

/-- Bounded key-value decoding preserves duplicate fields rather than silently overwriting them. -/
def decodePairs (s : String) (maxFields : Nat := 128) (maxFieldBytes : Nat := 8192)
    (form : Bool := true) : Except String (Array (String × String)) := do
  if s.isEmpty then return #[]
  let parts := s.splitOn "&"
  if parts.length > maxFields then throw "too many fields"
  parts.toArray.mapM fun part => do
    if part.utf8ByteSize > maxFieldBytes then throw "field too large"
    let parts := part.splitOn "="
    let key ← percentDecode (parts.head!) form
    let value ← percentDecode (String.intercalate "=" parts.tail!) form
    return (key, value)

/-- Lowercase authority. Ports remain part of host matching; userinfo and delimiters are rejected. -/
def normalizeHost (host : String) : Except String String := do
  if host.isEmpty || host.toUTF8.data.any (fun c => c <= 32 || c >= 127 ||
      [47,92,64,44,35,63].contains c.toNat) then throw "invalid host"
  return host.toLower

/-- Validate an origin-form target and decode its path once, before dispatch. -/
def Context.fromRequest (request : LeanNet.HTTP1.Request) : Except AppError Context := do
  let fail := fun message => AppError.mk 400 message
  let some target := String.fromUTF8? request.target | throw (fail "invalid request target")
  if target.utf8ByteSize > 16384 then throw { status := 414, message := "URI Too Long" }
  let serverOptions := target == "*" && request.method == "OPTIONS"
  if !serverOptions && (!target.startsWith "/" || target.startsWith "//" || target.contains '#') then
    throw (fail "expected origin-form request target")
  let pieces := target.splitOn "?"
  let pathText := pieces.head!
  let mut path := #[]
  if !serverOptions && pathText != "/" then
    for segment in (pathText.drop 1).toString.splitOn "/" do
      let decoded ← (percentDecode segment).mapError fail
      if decoded.contains '/' || decoded.contains '\\' || decoded == "." || decoded == ".." then
        throw (fail "invalid path segment")
      path := path.push decoded
  let query ← (decodePairs (String.intercalate "?" pieces.tail!) 128 8192 false).mapError fail
  let hosts := request.headers.filter (fun h => h.name.toLower == "host")
  if hosts.size != 1 then throw (fail "exactly one Host header is required")
  let some hostText := String.fromUTF8? hosts[0]!.value | throw (fail "invalid host")
  let host ← (normalizeHost hostText).mapError fail
  return { request, path, query, host }

end LeanWeave
