import LeanWeave.Protobuf.Scalar

namespace LeanWeave.Protobuf

inductive TypeRef where
  | scalar (type : ScalarType)
  | named (name : String)
  | map (key : ScalarType) (value : TypeRef)
  deriving Repr, BEq, Inhabited

inductive Label where
  | singular | optional | repeated
  deriving Repr, BEq, Inhabited

structure Field where
  name : String
  number : Nat
  type : TypeRef
  label : Label := .singular
  oneof : Option String := none
  packed : Option Bool := none
  deriving Repr, Inhabited

structure MessageDescriptor where
  name : String
  fields : Array Field
  oneofs : Array String := #[]
  reservedNumbers : Array (Nat × Nat) := #[]
  reservedNames : Array String := #[]
  deriving Repr, Inhabited

structure EnumDescriptor where
  name : String
  values : Array (String × Int)
  allowAlias : Bool := false
  reservedNumbers : Array (Int × Int) := #[]
  reservedNames : Array String := #[]
  deriving Repr, Inhabited

structure MethodDescriptor where
  name : String
  input : String
  output : String
  clientStreaming : Bool := false
  serverStreaming : Bool := false
  deriving Repr, Inhabited

structure ServiceDescriptor where
  name : String
  methods : Array MethodDescriptor
  deriving Repr, Inhabited

structure Descriptor where
  package : String := ""
  messages : Array MessageDescriptor := #[]
  enums : Array EnumDescriptor := #[]
  services : Array ServiceDescriptor := #[]
  imports : Array String := #[]
  deriving Repr, Inhabited

structure ParseLimits where
  maxBytes : Nat := 4 * 1024 * 1024
  maxTokens : Nat := 100000
  maxDeclarations : Nat := 4096
  maxFields : Nat := 4096
  maxNesting : Nat := 32
  deriving Repr, Inhabited

private structure Token where
  text : String
  quoted : Bool := false
  offset : Nat
  deriving Inhabited

private def identifier (name : String) : Bool :=
  let chars := name.toList
  !chars.isEmpty && (chars.head!.isAlpha || chars.head! == '_') &&
    chars.all (fun c => c.toNat < 128 && (c.isAlphanum || c == '_'))

private def digitValue (c : Char) : Option Nat :=
  if c >= '0' && c <= '9' then some (c.toNat - 48)
  else if c >= 'a' && c <= 'f' then some (c.toNat - 87)
  else if c >= 'A' && c <= 'F' then some (c.toNat - 55) else none

private def parseNatural (text : String) : Except String Nat := do
  let (base, chars) := if text.startsWith "0x" || text.startsWith "0X" then
      (16, (text.drop 2).toString.toList)
    else if text.length > 1 && text.startsWith "0" then (8, text.toList) else (10, text.toList)
  if chars.isEmpty then throw "expected protobuf integer"
  let mut value := 0
  for char in chars do
    let some digit := digitValue char | throw "invalid protobuf integer"
    if digit >= base then throw "invalid protobuf integer digit"
    value := value * base + digit
  return value

private def lex (source : String) (limits : ParseLimits) : Except String (Array Token) := do
  if source.utf8ByteSize > limits.maxBytes then throw "protobuf schema byte budget exceeded"
  let chars := source.toList.toArray
  let mut tokens := #[]
  let mut i := 0
  for _ in [:chars.size + 1] do
    if i >= chars.size then return tokens
    let char := chars[i]!
    if char.isWhitespace then i := i + 1
    else if char == '/' && chars[i+1]? == some '/' then
      i := i + 2
      for _ in [:chars.size] do
        if i < chars.size && chars[i]! != '\n' then i := i + 1
    else if char == '/' && chars[i+1]? == some '*' then
      i := i + 2
      let mut closed := false
      for _ in [:chars.size] do
        if !closed && i + 1 < chars.size then
          if chars[i]! == '*' && chars[i+1]! == '/' then i := i + 2; closed := true
          else i := i + 1
      unless closed do throw "unterminated protobuf comment"
    else if char == '"' || char == '\'' then
      let start := i
      let quote := char
      i := i + 1
      let mut value := ""
      let mut closed := false
      for _ in [:chars.size] do
        if !closed && i < chars.size then
          let char := chars[i]!
          i := i + 1
          if char == quote then closed := true
          else if char == '\n' || char == '\r' then throw "newline in protobuf string"
          else if char != '\\' then value := value.push char
          else
            if i >= chars.size then throw "unterminated protobuf escape"
            let escaped := chars[i]!
            i := i + 1
            let char ← match escaped with
              | 'n' => pure '\n' | 'r' => pure '\r' | 't' => pure '\t'
              | 'a' => pure (Char.ofNat 7) | 'b' => pure (Char.ofNat 8)
              | 'f' => pure (Char.ofNat 12) | 'v' => pure (Char.ofNat 11)
              | '\\' => pure '\\' | '\'' => pure '\'' | '"' => pure '"' | '?' => pure '?'
              | 'x' | 'X' => do
                let some a := chars[i]?.bind digitValue | throw "invalid protobuf hex escape"
                let some b := chars[i+1]?.bind digitValue | throw "invalid protobuf hex escape"
                i := i + 2
                pure (Char.ofNat (16*a+b))
              | 'u' | 'U' => do
                let digits := if escaped == 'u' then 4 else 8
                let mut codepoint := 0
                for _ in [:digits] do
                  let some digit := chars[i]?.bind digitValue | throw "invalid protobuf Unicode escape"
                  codepoint := 16 * codepoint + digit
                  i := i + 1
                if codepoint > 1114111 || (codepoint >= 55296 && codepoint <= 57343) then
                  throw "invalid protobuf Unicode scalar"
                pure (Char.ofNat codepoint)
              | c => do
                unless c >= '0' && c <= '7' do throw "unsupported protobuf string escape"
                let mut n := c.toNat - 48
                for _ in [:2] do
                  if let some next := chars[i]? then
                    if next >= '0' && next <= '7' then
                      n := 8*n + next.toNat - 48
                      i := i + 1
                if n > 255 then throw "protobuf octal escape exceeds byte"
                pure (Char.ofNat n)
            value := value.push char
      unless closed do throw "unterminated protobuf string"
      tokens := tokens.push ⟨value, true, start⟩
    else if char.isAlphanum || char == '_' then
      let start := i
      let mut value := ""
      for _ in [:chars.size] do
        if i < chars.size && (chars[i]!.isAlphanum || chars[i]! == '_') then
          value := value.push chars[i]!
          i := i + 1
      tokens := tokens.push ⟨value, false, start⟩
    else if ['{','}','[',']','(',')','<','>','=',';',',','.','-','+',':'].contains char then
      tokens := tokens.push ⟨String.singleton char, false, i⟩
      i := i + 1
    else throw s!"invalid protobuf schema character at {i}"
    if tokens.size > limits.maxTokens then throw "protobuf schema token budget exceeded"
  throw "protobuf lexer exceeded input budget"

private structure ParserState where
  tokens : Array Token
  cursor : Nat := 0
  descriptor : Descriptor := {}
  limits : ParseLimits

private abbrev Parser := StateT ParserState (Except String)

private def peek : Parser (Option Token) := do return (← get).tokens[(← get).cursor]?
private def take : Parser Token := do
  let state ← get
  let some token := state.tokens[state.cursor]? | throw "unexpected end of protobuf schema"
  set { state with cursor := state.cursor + 1 }
  return token
private def accept (text : String) : Parser Bool := do
  if (← peek).any (fun token => !token.quoted && token.text == text) then let _ ← take; return true
  return false
private def expect (text : String) : Parser Unit := do
  unless ← accept text do
    let found ← peek
    throw s!"expected '{text}' in protobuf schema near {found.map (·.offset)}"
private def ident : Parser String := do
  let token ← take
  if token.quoted || !identifier token.text then throw s!"expected protobuf identifier at {token.offset}"
  return token.text
private def natural : Parser Nat := do
  let token ← take
  if token.quoted then throw "expected protobuf integer"
  match parseNatural token.text with | .ok n => return n | .error error => throw error
private def integer : Parser Int := do
  let negative ← accept "-"
  let _ ← accept "+"
  let value ← natural
  return if negative then -(Int.ofNat value) else Int.ofNat value
private def qualified : Parser String := do
  let absolute ← accept "."
  let mut name ← ident
  for _ in [:128] do if ← accept "." then name := name ++ "." ++ (← ident)
  return (if absolute then "." else "") ++ name

private def skipStatement : Parser Unit := do
  let mut level := 0
  let budget := (← get).limits.maxTokens
  for _ in [:budget] do
    let token ← take
    if !token.quoted then
      if ["{", "[", "(", "<"].contains token.text then level := level + 1
      else if ["}", "]", ")", ">"].contains token.text then
        if level == 0 then throw "unbalanced protobuf option"
        level := level - 1
      else if token.text == ";" && level == 0 then return
  throw "protobuf option token budget exceeded"

private partial def typeRef : Parser TypeRef := do
  if ← accept "map" then
    expect "<"
    let key ← ident
    let some key := ScalarType.parse key | throw "invalid protobuf map key type"
    unless [.int32,.int64,.uint32,.uint64,.sint32,.sint64,.fixed32,.fixed64,.sfixed32,.sfixed64,.bool,.string].contains key do
      throw "invalid protobuf map key type"
    expect ","
    let value ← qualified
    expect ">"
    return .map key (match ScalarType.parse value with | some type => .scalar type | none => .named value)
  let name ← qualified
  return match ScalarType.parse name with | some type => .scalar type | none => .named name

private def fieldOptions : Parser (Option Bool) := do
  unless ← accept "[" do return none
  let mut packed := none
  let mut nested := 0
  let budget := (← get).limits.maxTokens
  for _ in [:budget] do
    if nested == 0 && (← accept "]") then return packed
    if nested == 0 && (← accept "packed") then
      expect "="
      let value ← take
      unless value.text == "true" || value.text == "false" do throw "packed requires a boolean"
      if packed.isSome then throw "duplicate packed option"
      packed := some (value.text == "true")
    else
      let token ← take
      if !token.quoted then
        if ["[", "(", "{"].contains token.text then nested := nested + 1
        else if ["]", ")", "}"].contains token.text then
          if nested == 0 then throw "unbalanced protobuf field option"
          nested := nested - 1
  throw "unterminated protobuf field options"

private def parseField (oneof : Option String := none) : Parser Field := do
  let label ← if ← accept "repeated" then pure Label.repeated
    else if ← accept "optional" then pure Label.optional
    else if ← accept "required" then throw "required fields are not available in proto3"
    else pure Label.singular
  if oneof.isSome && label != .singular then throw "oneof members cannot have a label"
  let type ← typeRef
  if oneof.isSome then if let .map .. := type then throw "oneof members cannot be maps"
  let name ← ident
  expect "="
  let number ← natural
  let packed ← fieldOptions
  expect ";"
  return { name, number, type, label := if let .map .. := type then .repeated else label, oneof, packed }

private def reserved : Parser (Array (Int × Int) × Array String) := do
  let mut numbers := #[]
  let mut names := #[]
  let budget := (← get).limits.maxFields
  for _ in [:budget] do
    if let some token ← peek then
      if token.quoted then names := names.push (← take).text
      else
        let lo ← integer
        let hi ← if ← accept "to" then
            if ← accept "max" then pure 536870911 else integer
          else pure lo
        if hi < lo then throw "invalid protobuf reserved range"
        numbers := numbers.push (lo, hi)
    if ← accept ";" then return (numbers, names)
    expect ","
  throw "protobuf reserved range budget exceeded"

private def addEnum (value : EnumDescriptor) : Parser Unit := do
  modify fun state => { state with descriptor := { state.descriptor with enums := state.descriptor.enums.push value } }

private def parseEnum (scope : String) : Parser Unit := do
  let localName ← ident
  let name := if scope.isEmpty then localName else scope ++ "." ++ localName
  expect "{"
  let mut value : EnumDescriptor := { name, values := #[] }
  let budget := (← get).limits.maxFields
  for _ in [:budget] do
    if ← accept "}" then addEnum value; return
    if ← accept ";" then continue
    if ← accept "option" then
      if ← accept "allow_alias" then
        expect "="
        let option ← take
        unless option.text == "true" || option.text == "false" do throw "allow_alias requires a boolean"
        value := { value with allowAlias := option.text == "true" }
        expect ";"
      else skipStatement
    else if ← accept "reserved" then
      let (numbers, names) ← reserved
      value := { value with reservedNumbers := value.reservedNumbers ++ numbers, reservedNames := value.reservedNames ++ names }
    else
      let field ← ident
      expect "="
      let number ← integer
      let _ ← fieldOptions
      expect ";"
      value := { value with values := value.values.push (field, number) }
  throw "protobuf enum field budget exceeded"

private partial def parseMessage (scope : String) (depth : Nat) : Parser Unit := do
  if depth == 0 then throw "protobuf message nesting budget exceeded"
  let localName ← ident
  let name := if scope.isEmpty then localName else scope ++ "." ++ localName
  expect "{"
  let mut message : MessageDescriptor := { name, fields := #[] }
  let budget := (← get).limits.maxFields
  for _ in [:budget] do
    if ← accept "}" then
      modify fun state => { state with descriptor := { state.descriptor with messages := state.descriptor.messages.push message } }
      return
    if ← accept ";" then continue
    if ← accept "message" then parseMessage name (depth - 1)
    else if ← accept "enum" then parseEnum name
    else if ← accept "option" then skipStatement
    else if ← accept "reserved" then
      let (numbers, names) ← reserved
      if numbers.any (fun (lo, hi) => lo < 0 || hi > 536870911) then throw "invalid message reserved range"
      message := { message with reservedNumbers := message.reservedNumbers ++ numbers.map (fun (lo, hi) => (lo.toNat, hi.toNat)), reservedNames := message.reservedNames ++ names }
    else if ← accept "oneof" then
      let group ← ident
      message := { message with oneofs := message.oneofs.push group }
      expect "{"
      let mut closed := false
      for _ in [:budget] do
        unless closed do
          if ← accept "}" then closed := true
          else if ← accept "option" then skipStatement
          else message := { message with fields := message.fields.push (← parseField (some group)) }
      unless closed do throw "unterminated protobuf oneof"
    else message := { message with fields := message.fields.push (← parseField) }
  throw "protobuf message field budget exceeded"

private def parseService : Parser Unit := do
  let name ← ident
  expect "{"
  let mut methods := #[]
  let budget := (← get).limits.maxFields
  for _ in [:budget] do
    if ← accept "}" then
      modify fun state => { state with descriptor := { state.descriptor with services := state.descriptor.services.push ⟨name, methods⟩ } }
      return
    if ← accept ";" then continue
    if ← accept "option" then skipStatement
    else
      expect "rpc"
      let method ← ident
      expect "("
      let clientStreaming ← accept "stream"
      let input ← qualified
      expect ")"
      expect "returns"
      expect "("
      let serverStreaming ← accept "stream"
      let output ← qualified
      expect ")"
      if ← accept "{" then
        let mut closed := false
        for _ in [:budget] do
          unless closed do
            if ← accept "}" then closed := true
            else expect "option"; skipStatement
        unless closed do throw "unterminated protobuf rpc options"
      else expect ";"
      methods := methods.push ⟨method, input, output, clientStreaming, serverStreaming⟩
  throw "protobuf service method budget exceeded"

private def resolveName (descriptor : Descriptor) (scope name : String) : Except String String := do
  let absolute := name.startsWith "."
  let name := if absolute then (name.drop 1).toString else name
  let name := if !descriptor.package.isEmpty && name.startsWith (descriptor.package ++ ".") then
      (name.drop (descriptor.package.length + 1)).toString else name
  let parts := scope.splitOn "."
  let mut scopes := if absolute then [""] else
    (List.range (parts.length + 1)).reverse.map (fun n => String.intercalate "." (parts.take n))
  if !scopes.contains "" then scopes := scopes ++ [""]
  for scope in scopes do
    let candidate := if scope.isEmpty then name else scope ++ "." ++ name
    if descriptor.messages.any (·.name == candidate) || descriptor.enums.any (·.name == candidate) then return candidate
  throw s!"unresolved protobuf type '{name}' in '{scope}'"

private partial def resolveRef (descriptor : Descriptor) (scope : String) : TypeRef → Except String TypeRef
  | .scalar type => pure (.scalar type)
  | .named name => return .named (← resolveName descriptor scope name)
  | .map key value => return .map key (← resolveRef descriptor scope value)

def Descriptor.validate (descriptor : Descriptor) (limits : ParseLimits := {}) : Except String Descriptor := do
  if descriptor.messages.size + descriptor.enums.size + descriptor.services.size > limits.maxDeclarations then
    throw "protobuf declaration budget exceeded"
  let mut names : Array String := #[]
  for name in descriptor.messages.map (·.name) ++ descriptor.enums.map (·.name) ++ descriptor.services.map (·.name) do
    if names.contains name then throw s!"duplicate protobuf declaration '{name}'"
    names := names.push name
  for enumeration in descriptor.enums do
    if enumeration.values.isEmpty || enumeration.values[0]!.2 != 0 then throw "proto3 enums must begin with value zero"
    let mut fieldNames := #[]
    let mut numbers := #[]
    for (name, number) in enumeration.values do
      if fieldNames.contains name then throw s!"duplicate protobuf enum name '{name}'"
      if !enumeration.allowAlias && numbers.contains number then throw "protobuf enum aliases require allow_alias"
      if number < -2147483648 || number > 2147483647 then throw "protobuf enum value exceeds int32"
      if enumeration.reservedNames.contains name || enumeration.reservedNumbers.any (fun (lo, hi) => lo <= number && number <= hi) then
        throw "protobuf enum uses a reserved name or number"
      fieldNames := fieldNames.push name
      numbers := numbers.push number
  let messages ← descriptor.messages.mapM fun message => do
    let mut fieldNames := #[]
    let mut numbers := #[]
    let mut groups := #[]
    for group in message.oneofs do
      if groups.contains group then throw "duplicate protobuf oneof name"
      groups := groups.push group
    let mut fields := #[]
    for field in message.fields do
      if field.number == 0 || field.number > 536870911 || (field.number >= 19000 && field.number <= 19999) then
        throw s!"invalid protobuf field number {field.number}"
      if fieldNames.contains field.name || numbers.contains field.number then throw "duplicate protobuf field name or number"
      if field.name == "__unknownFields" || field.name == "encodeWith" || field.name == "decodeWith" then
        throw "protobuf field collides with generated codec metadata"
      if message.reservedNames.contains field.name || message.reservedNumbers.any (fun (lo, hi) => lo <= field.number && field.number <= hi) then
        throw "protobuf field uses a reserved name or number"
      if message.oneofs.contains field.name then throw "protobuf field collides with oneof name"
      fieldNames := fieldNames.push field.name
      numbers := numbers.push field.number
      let type ← resolveRef descriptor message.name field.type
      if field.packed.isSome then
        let packable := match type with
          | .scalar type => type.packable
          | .named name => descriptor.enums.any (·.name == name)
          | _ => false
        unless field.label == .repeated && packable do throw "packed option requires repeated primitive/enum field"
      fields := fields.push { field with type }
    return { message with fields }
  let services ← descriptor.services.mapM fun service => do
    let mut names := #[]
    let mut methods := #[]
    for method in service.methods do
      if names.contains method.name then throw "duplicate protobuf rpc name"
      names := names.push method.name
      let input ← resolveName descriptor "" method.input
      let output ← resolveName descriptor "" method.output
      unless descriptor.messages.any (·.name == input) && descriptor.messages.any (·.name == output) do
        throw "protobuf rpc inputs and outputs must be messages"
      methods := methods.push { method with input, output }
    return { service with methods }
  return { descriptor with messages, services }

/-- Native bounded proto3 parser. Imports are recorded, and referenced types must
    be present in this descriptor; unsupported syntax is rejected explicitly. -/
def parseSchema (source : String) (limits : ParseLimits := {}) : Except String Descriptor := do
  let tokens ← lex source limits
  let parser : Parser Descriptor := do
    expect "syntax"
    expect "="
    let version ← take
    unless version.quoted && version.text == "proto3" do throw "only proto3 syntax is supported"
    expect ";"
    let mut seenPackage := false
    for _ in [:limits.maxDeclarations + 1] do
      if (← peek).isNone then return (← get).descriptor
      if ← accept ";" then continue
      if ← accept "package" then
        if seenPackage then throw "duplicate protobuf package"
        seenPackage := true
        let name ← qualified
        expect ";"
        modify fun state => { state with descriptor := { state.descriptor with package := name } }
      else if ← accept "import" then
        let _ ← accept "public"
        let _ ← accept "weak"
        let path ← take
        unless path.quoted do throw "protobuf import requires string path"
        expect ";"
        modify fun state => { state with descriptor := { state.descriptor with imports := state.descriptor.imports.push path.text } }
      else if ← accept "option" then skipStatement
      else if ← accept "message" then parseMessage "" limits.maxNesting
      else if ← accept "enum" then parseEnum ""
      else if ← accept "service" then parseService
      else throw s!"unexpected protobuf declaration '{(← take).text}'"
    throw "protobuf declaration budget exceeded"
  let descriptor := (← parser.run ⟨tokens, 0, {}, limits⟩).1
  descriptor.validate limits

end LeanWeave.Protobuf
