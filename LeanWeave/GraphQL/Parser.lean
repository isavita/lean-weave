import Lean.Data.Json

namespace LeanWeave.GraphQL

inductive Value where
  | literal (value : Lean.Json)
  | floatValue (value : Lean.JsonNumber)
  | enumValue (name : String)
  | variable (name : String)
  | list (values : Array Value)
  | object (fields : Array (String × Value))
  deriving Inhabited, BEq

inductive TypeRef where
  | named (name : String)
  | list (element : TypeRef)
  | nonNull (type : TypeRef)
  deriving Inhabited, BEq

structure Directive where
  name : String
  arguments : Array (String × Value) := #[]
  deriving Inhabited, BEq

inductive Selection where
  | field (alias : Option String) (name : String) (arguments : Array (String × Value))
      (directives : Array Directive) (children : Array Selection)
  | spread (name : String) (directives : Array Directive)
  | inlineFragment (typeCondition : Option String) (directives : Array Directive) (children : Array Selection)
  deriving Inhabited, BEq

structure Variable where
  name : String
  type : TypeRef
  defaultValue : Option Value := none
  deriving Inhabited, BEq

inductive OperationKind where
  | query | mutation | subscription
  deriving Inhabited, BEq

structure Operation where
  kind : OperationKind := .query
  name : Option String := none
  variables : Array Variable := #[]
  directives : Array Directive := #[]
  selections : Array Selection := #[]
  deriving Inhabited, BEq

structure Fragment where
  name : String
  typeCondition : String
  directives : Array Directive := #[]
  selections : Array Selection
  deriving Inhabited, BEq

structure Document where
  operations : Array Operation := #[]
  fragments : Array Fragment := #[]
  deriving Inhabited, BEq

structure ParseLimits where
  maxBytes : Nat := 65536
  maxTokens : Nat := 8192
  maxDepth : Nat := 32
  maxSelections : Nat := 1024
  deriving Inhabited, Repr

private inductive Kind where
  | name | string | number | punctuation
  deriving Inhabited, BEq

private structure Token where
  kind : Kind
  text : String
  offset : Nat
  deriving Inhabited

private def nameStart (char : Char) : Bool :=
  char == '_' || (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z')
private def nameChar (char : Char) : Bool := nameStart char || (char >= '0' && char <= '9')
private def decimal (char : Char) : Bool := char >= '0' && char <= '9'
private def indentation (line : String) : Nat := (line.toList.takeWhile (fun c => c == ' ' || c == '\t')).length
private def blank (line : String) : Bool := line.toList.all (fun c => c == ' ' || c == '\t')

/-- GraphQL BlockStringValue: normalize lines, remove common indentation after
    the first line, then remove leading and trailing whitespace-only lines. -/
def blockStringValue (raw : String) : String := Id.run do
  let lines := ((raw.replace "\r\n" "\n").replace "\r" "\n").splitOn "\n" |>.toArray
  let mut common : Option Nat := none
  for i in [1:lines.size] do
    let line := lines[i]!
    unless blank line do common := some (min (common.getD (indentation line)) (indentation line))
  let lines := lines.mapIdx fun i line => if i == 0 then line else (line.drop (common.getD 0)).toString
  let mut first := 0
  let mut last := lines.size
  while first < last && blank lines[first]! do first := first + 1
  while last > first && blank lines[last-1]! do last := last - 1
  return String.intercalate "\n" (lines.extract first last).toList

private def hexDigit (c : Char) : Option Nat :=
  if c >= '0' && c <= '9' then some (c.toNat-48)
  else if c >= 'a' && c <= 'f' then some (c.toNat-87)
  else if c >= 'A' && c <= 'F' then some (c.toNat-55) else none

private def unicodeEscape (chars : Array Char) (start : Nat) : Except String (Char × Nat) := do
  let mut index := start
  let mut codepoint := 0
  if chars[index]? == some '{' then
    index := index + 1
    let mut count := 0
    while index < chars.size && chars[index]! != '}' do
      let some digit := hexDigit chars[index]! | throw "invalid GraphQL Unicode escape"
      codepoint := 16 * codepoint + digit
      index := index + 1
      count := count + 1
      if codepoint > 1114111 then throw "invalid GraphQL Unicode scalar"
    if count == 0 || chars[index]? != some '}' then throw "unterminated GraphQL Unicode escape"
    index := index + 1
  else
    for _ in [:4] do
      let some digit := chars[index]?.bind hexDigit | throw "invalid GraphQL Unicode escape"
      codepoint := 16 * codepoint + digit
      index := index + 1
    if codepoint >= 55296 && codepoint <= 56319 then
      if chars[index]? != some '\\' || chars[index+1]? != some 'u' then throw "unpaired GraphQL Unicode surrogate"
      index := index + 2
      let mut low := 0
      for _ in [:4] do
        let some digit := chars[index]?.bind hexDigit | throw "invalid GraphQL Unicode surrogate"
        low := 16 * low + digit
        index := index + 1
      unless low >= 56320 && low <= 57343 do throw "invalid GraphQL Unicode surrogate pair"
      codepoint := 65536 + (codepoint-55296)*1024 + low-56320
  if codepoint > 1114111 || (codepoint >= 55296 && codepoint <= 57343) then throw "invalid GraphQL Unicode scalar"
  return (Char.ofNat codepoint, index)

private def readString (chars : Array Char) (start : Nat) : Except String (String × Nat) := do
  let block := chars[start+1]? == some '"' && chars[start+2]? == some '"'
  let mut index := start + if block then 3 else 1
  let mut text := ""
  while index < chars.size do
    let char := chars[index]!
    if block then
      if char == '"' && chars[index+1]? == some '"' && chars[index+2]? == some '"' then
        return (blockStringValue text, index+3)
      if char == '\\' && chars[index+1]? == some '"' && chars[index+2]? == some '"' && chars[index+3]? == some '"' then
        text := text ++ "\"\"\""
        index := index + 4
      else
        text := text.push char
        index := index + 1
    else
      if char == '"' then return (text, index+1)
      if char == '\n' || char == '\r' then throw "line terminator in GraphQL string"
      if char != '\\' then text := text.push char; index := index+1
      else
        let some escaped := chars[index+1]? | throw "unterminated GraphQL string escape"
        index := index + 2
        match escaped with
        | 'u' =>
          let (char, next) ← unicodeEscape chars index
          text := text.push char
          index := next
        | _ =>
          let value ← match escaped with
            | '"' => pure '"' | '\\' => pure '\\' | '/' => pure '/'
            | 'b' => pure (Char.ofNat 8) | 'f' => pure (Char.ofNat 12)
            | 'n' => pure '\n' | 'r' => pure '\r' | 't' => pure '\t'
            | _ => throw "invalid GraphQL string escape"
          text := text.push value
  throw "unterminated GraphQL string"

private def readNumber (chars : Array Char) (start : Nat) : Except String (String × Nat) := do
  let mut index := start
  if chars[index]? == some '-' then index := index + 1
  let some first := chars[index]? | throw "truncated GraphQL number"
  unless decimal first do throw "invalid GraphQL number"
  index := index + 1
  if first == '0' then
    if chars[index]?.any decimal then throw "GraphQL number has a leading zero"
  else
    while chars[index]?.any decimal do index := index + 1
  if chars[index]? == some '.' then
    index := index + 1
    unless chars[index]?.any decimal do throw "GraphQL fraction requires digits"
    while chars[index]?.any decimal do index := index + 1
  if chars[index]? == some 'e' || chars[index]? == some 'E' then
    index := index + 1
    if chars[index]? == some '+' || chars[index]? == some '-' then index := index + 1
    unless chars[index]?.any decimal do throw "GraphQL exponent requires digits"
    while chars[index]?.any decimal do index := index + 1
  if chars[index]?.any nameStart || chars[index]? == some '.' then throw "invalid character after GraphQL number"
  if index-start > 256 then throw "GraphQL numeric token length limit exceeded"
  return (String.ofList (chars.extract start index).toList, index)

private def lex (query : String) (limits : ParseLimits) : Except String (Array Token) := do
  if query.utf8ByteSize > limits.maxBytes then throw "GraphQL query byte limit exceeded"
  let chars := query.toList.toArray
  let mut index := 0
  let mut tokens := #[]
  while index < chars.size do
    let start := index
    let char := chars[index]!
    if [' ', '\t', '\r', '\n', ',', Char.ofNat 65279].contains char then index := index+1
    else if char == '#' then
      while index < chars.size && chars[index]! != '\n' && chars[index]! != '\r' do index := index+1
    else
      let (kind, text, next) ← if nameStart char then do
          index := index+1
          while chars[index]?.any nameChar do index := index+1
          pure (.name, String.ofList (chars.extract start index).toList, index)
        else if char == '"' then do
          let (text, next) ← readString chars index
          pure (.string, text, next)
        else if decimal char || char == '-' then do
          let (text, next) ← readNumber chars index
          pure (.number, text, next)
        else if char == '.' && chars[index+1]? == some '.' && chars[index+2]? == some '.' then
          pure (.punctuation, "...", index+3)
        else if ['!', '$', '&', '(', ')', ':', '=', '@', '[', ']', '{', '|', '}'].contains char then
          pure (.punctuation, String.singleton char, index+1)
        else throw s!"invalid GraphQL character at {index}"
      index := next
      tokens := tokens.push ⟨kind, text, start⟩
      if tokens.size > limits.maxTokens then throw "GraphQL query token limit exceeded"
  return tokens

private structure State where
  tokens : Array Token
  index : Nat := 0
  selections : Nat := 0
  limits : ParseLimits
private abbrev Parser := StateT State (Except String)

private def peek : Parser (Option Token) := do return (← get).tokens[(← get).index]?
private def has (text : String) : Parser Bool := do
  return (← peek).any (fun token => token.kind != .string && token.text == text)
private def next : Parser Token := do
  let state ← get
  let some token := state.tokens[state.index]? | throw "unexpected end of GraphQL document"
  set { state with index := state.index+1 }
  return token
private def take (text : String) : Parser Bool := do
  if ← has text then let _ ← next; return true
  return false
private def expect (text : String) : Parser Unit := do
  unless ← take text do
    let position := (← peek).map (·.offset)
    throw s!"expected '{text}' in GraphQL document at {position}"
private def name : Parser String := do
  let token ← next
  unless token.kind == .name do throw s!"expected GraphQL name at {token.offset}"
  return token.text
private def depth (remaining : Nat) : Parser Nat := do
  if remaining == 0 then throw "GraphQL nesting depth limit exceeded"
  return remaining-1

private partial def value (fuel : Nat) (constant : Bool := false) : Parser Value := do
  let fuel ← depth fuel
  if ← take "$" then
    if constant then throw "GraphQL constant value cannot contain a variable"
    return .variable (← name)
  if ← take "[" then
    let mut values := #[]
    while !(← has "]") do values := values.push (← value fuel constant)
    expect "]"
    return .list values
  if ← take "{" then
    let mut fields := #[]
    while !(← has "}") do
      let key ← name
      if fields.any (fun pair => pair.1 == key) then throw ("duplicate GraphQL input object field '" ++ key ++ "'")
      expect ":"
      fields := fields.push (key, ← value fuel constant)
    expect "}"
    return .object fields
  let token ← next
  match token.kind with
  | .string => return .literal (.str token.text)
  | .number =>
    let number ← Lean.Json.parse token.text
    if token.text.contains '.' || token.text.contains 'e' || token.text.contains 'E' then
      let .num number := number | throw "invalid GraphQL numeric literal"
      return .floatValue number
    return .literal number
  | .name => return match token.text with
    | "true" => .literal (.bool true) | "false" => .literal (.bool false) | "null" => .literal .null
    | _ => .enumValue token.text
  | _ => throw s!"expected GraphQL value at {token.offset}"

private def arguments (fuel : Nat) : Parser (Array (String × Value)) := do
  if !(← take "(") then return #[]
  let mut result := #[]
  while !(← has ")") do
    let key ← name
    if result.any (fun pair => pair.1 == key) then throw ("duplicate GraphQL argument '" ++ key ++ "'")
    expect ":"
    result := result.push (key, ← value fuel)
  if result.isEmpty then throw "GraphQL argument list cannot be empty"
  expect ")"
  return result

private def directives (fuel : Nat) : Parser (Array Directive) := do
  let mut result := #[]
  while ← take "@" do result := result.push ⟨← name, ← arguments fuel⟩
  return result

private partial def typeRef (fuel : Nat) : Parser TypeRef := do
  let fuel ← depth fuel
  let type ← if ← take "[" then do
      let element ← typeRef fuel
      expect "]"
      pure (.list element)
    else TypeRef.named <$> name
  if ← take "!" then return .nonNull type
  return type

mutual
  private partial def selectionSet (fuel : Nat) : Parser (Array Selection) := do
    let fuel ← depth fuel
    expect "{"
    let mut result := #[]
    while !(← has "}") do
      let state ← get
      if state.selections >= state.limits.maxSelections then throw "GraphQL selection count limit exceeded"
      set { state with selections := state.selections+1 }
      result := result.push (← selection fuel)
    if result.isEmpty then throw "GraphQL selection set cannot be empty"
    expect "}"
    return result

  private partial def selection (fuel : Nat) : Parser Selection := do
    if ← take "..." then
      if ← take "on" then
        let condition ← name
        let rules ← directives fuel
        return .inlineFragment (some condition) rules (← selectionSet fuel)
      if (← has "@") || (← has "{") then
        let rules ← directives fuel
        return .inlineFragment none rules (← selectionSet fuel)
      let fragment ← name
      return .spread fragment (← directives fuel)
    let first ← name
    let (alias, field) ← if ← take ":" then do pure (some first, ← name) else pure (none, first)
    let args ← arguments fuel
    let rules ← directives fuel
    let children ← if ← has "{" then selectionSet fuel else pure #[]
    return .field alias field args rules children
end

private def variables (fuel : Nat) : Parser (Array Variable) := do
  if !(← take "(") then return #[]
  let mut result := #[]
  while !(← has ")") do
    if (← peek).any (·.kind == .string) then let _ ← next
    expect "$"
    let binding ← name
    if result.any (fun item => item.name == binding) then throw ("duplicate GraphQL variable '" ++ binding ++ "'")
    expect ":"
    let type ← typeRef fuel
    let fallback ← if ← take "=" then some <$> value fuel true else pure none
    if ← has "@" then throw "GraphQL variable-definition directives are not supported"
    result := result.push ⟨binding, type, fallback⟩
  if result.isEmpty then throw "GraphQL variable definitions cannot be empty"
  expect ")"
  return result

private def document : Parser Document := do
  let fuel := (← get).limits.maxDepth
  let mut result : Document := {}
  while (← peek).isSome do
    -- September2025 descriptions have no execution/validation meaning.
    let described ← if (← peek).any (·.kind == .string) then do let _ ← next; pure true else pure false
    if described && (← has "{") then throw "GraphQL shorthand operation cannot have a description"
    if ← has "{" then
      result := { result with operations := result.operations.push { selections := ← selectionSet fuel } }
    else if ← take "fragment" then
      let fragment ← name
      if fragment == "on" then throw "GraphQL fragment cannot be named 'on'"
      if result.fragments.any (·.name == fragment) then throw ("duplicate GraphQL fragment '" ++ fragment ++ "'")
      expect "on"
      let condition ← name
      let rules ← directives fuel
      let selections ← selectionSet fuel
      result := { result with fragments := result.fragments.push ⟨fragment, condition, rules, selections⟩ }
    else
      let operation ← name
      let kind ← match operation with
        | "query" => pure .query | "mutation" => pure .mutation | "subscription" => pure .subscription
        | _ => throw ("expected GraphQL operation or fragment, found '" ++ operation ++ "'")
      let operationName ← if (← peek).any (·.kind == .name) then some <$> name else pure none
      if operationName.isSome && result.operations.any (·.name == operationName) then throw "duplicate GraphQL operation name"
      let definitions ← variables fuel
      let rules ← directives fuel
      let selections ← selectionSet fuel
      result := { result with operations := result.operations.push ⟨kind, operationName, definitions, rules, selections⟩ }
  if result.operations.isEmpty && result.fragments.isEmpty then throw "GraphQL document cannot be empty"
  if result.operations.size > 1 && result.operations.any (·.name.isNone) then
    throw "anonymous GraphQL operation must be the only operation"
  return result

/-- Bounded native executable-document parser. Validation against an executable
schema is separate. Type-system documents and variable-definition directives
are rejected explicitly instead of being silently discarded. -/
def parse (query : String) (limits : ParseLimits := {}) : Except String Document := do
  if limits.maxBytes == 0 || limits.maxBytes > 16*1024*1024 || limits.maxTokens == 0 || limits.maxTokens > 1000000 ||
      limits.maxDepth == 0 || limits.maxDepth > 128 || limits.maxSelections == 0 || limits.maxSelections > 100000 then
    throw "invalid GraphQL parser limits"
  let tokens ← lex query limits
  return (← document.run ⟨tokens, 0, 0, limits⟩).1

end LeanWeave.GraphQL
