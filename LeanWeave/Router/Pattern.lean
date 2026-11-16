import Std

namespace LeanWeave.Router

/-- Capture codecs have an explicit precedence: natural, integer, then text. -/
inductive CaptureType where
  | nat | int | text
  deriving BEq, Repr, Inhabited

/-- A pattern is segmented before decoding; captures consume exactly one segment. -/
inductive Segment where
  | literal (value : String)
  | capture (name : String) (type : CaptureType)
  | rest (name : String)
  deriving BEq, Repr, Inhabited

def Segment.rank : Segment → Nat
  | .literal _ => 4
  | .capture _ .nat => 3
  | .capture _ .int => 2
  | .capture _ .text => 1
  | .rest _ => 0

private def validName (name : String) : Bool :=
  !name.isEmpty && name.toList.all (fun c => c.isAlphanum || c == '_')

private def parseSegment (part : String) : Except String Segment := do
  if part.startsWith ":" then
    let spec := part.drop 1 |>.toString
    let pieces := spec.splitOn "<"
    match pieces with
    | [name] =>
      unless validName name do throw "invalid capture name"
      return .capture name .text
    | [name, type] =>
      unless validName name && type.endsWith ">" do throw "invalid typed capture"
      let codec := type.dropEnd 1 |>.toString
      let captureType ← match codec with
        | "nat" => pure CaptureType.nat
        | "int" => pure CaptureType.int
        | "text" => pure CaptureType.text
        | _ => throw s!"unknown capture type '{codec}' (expected nat, int, text)"
      return .capture name captureType
    | _ => throw "invalid capture syntax"
  else if part.startsWith "*" then
    let name := part.drop 1 |>.toString
    unless validName name do throw "invalid rest capture name"
    return .rest name
  else
    if part.contains '?' || part.contains '#' || part.contains '%' || part.contains '\\' ||
        part == "." || part == ".." || part.toUTF8.toList.contains 0 then
      throw "literal route segments must be nonempty decoded text without ?, #, or %"
    return .literal part

/-- Parse a literal route pattern. Trailing slashes are significant; rest captures
    must be final. Percent escapes belong to requests, never to route definitions. -/
def parsePattern (path : String) : Except String (Array Segment) := do
  unless path.startsWith "/" do throw "route paths must begin with /"
  if path == "/" then return #[]
  let parts := (path.drop 1 |>.toString).splitOn "/"
  let mut segments := #[]
  let mut names : Array String := #[]
  for (part, index) in parts.zipIdx do
    if part.isEmpty && index + 1 != parts.length then throw "empty interior route segment"
    let segment ← parseSegment part
    let name? := match segment with
      | .literal _ => none
      | .capture name _ | .rest name => some name
    if let some name := name? then
      if names.contains name then throw s!"duplicate capture '{name}'"
      names := names.push name
    if segments.any (fun s => match s with | .rest _ => true | _ => false) then
      throw "rest captures must be final"
    segments := segments.push segment
  return segments

def isNatText (value : String) : Bool :=
  !value.isEmpty && value.toList.all (fun c => c >= '0' && c <= '9')

def isIntText (value : String) : Bool :=
  isNatText value || (value.startsWith "-" && isNatText (value.drop 1 |>.toString))

private def segmentOverlaps (a b : Segment) : Bool :=
  match a, b with
  | .literal a, .literal b => a == b
  | .literal a, .capture _ .nat | .capture _ .nat, .literal a => isNatText a
  | .literal a, .capture _ .int | .capture _ .int, .literal a => isIntText a
  | _, _ => true

/-- Equal-precedence overlapping patterns are ambiguous; unequal precedence is
    resolved independently of declaration order. -/
def ambiguousPatterns (a b : Array Segment) : Bool :=
  a.size == b.size && (a.toList.zip b.toList).all (fun (x, y) =>
    x.rank == y.rank && segmentOverlaps x y)

def patternPrecedes (a b : Array Segment) : Bool := Id.run do
  for (x, y) in a.toList.zip b.toList do
    if x.rank != y.rank then return x.rank > y.rank
  -- An exact path wins over a rest capture matching its empty suffix.
  return a.size < b.size

private def hexDigit (n : Nat) : Char :=
  Char.ofNat (if n < 10 then 48 + n else 65 + n - 10)

/-- Encode one path segment; UTF-8 bytes, slashes, and percent signs are escaped. -/
def encodeSegment (value : String) : String := String.ofList <|
  value.toUTF8.toList.flatMap fun b =>
    let n := b.toNat
    if (n >= 65 && n <= 90) || (n >= 97 && n <= 122) ||
       (n >= 48 && n <= 57) || [45, 46, 95, 126].contains n then
      [Char.ofNat n]
    else ['%', hexDigit (n / 16), hexDigit (n % 16)]

end LeanWeave.Router
