import LeanWeave.Core.Types

namespace LeanWeave.Html

/-- A URL whose scheme and control characters have been checked. Constructor is private. -/
structure SafeUrl where
  private mk ::
  value : String
  deriving Repr, BEq

/-- Permit local references and explicit HTTP(S), mailto and tel URLs. Protocol-relative
URLs, backslashes, ASCII whitespace/control characters and executable schemes fail closed. -/
def SafeUrl.parse (value : String) : Except String SafeUrl := do
  if value.toList.any (fun c => c.toNat ≤ 32 || c.toNat == 127 || c == '\\') then
    throw "URL contains whitespace, a control character or a backslash"
  if value.startsWith "//" then throw "protocol-relative URLs are not allowed"
  let firstPart := ((value.splitOn "/").head!).splitOn "?" |>.head! |>.splitOn "#" |>.head!
  if firstPart.contains ':' then
    let scheme := (firstPart.splitOn ":").head! |>.toLower
    unless ["http", "https", "mailto", "tel"].contains scheme do
      throw "URL scheme is not allowed"
  return ⟨value⟩

/-- A safe local root URL. -/
def SafeUrl.root : SafeUrl := ⟨"/"⟩

/-- Container tags. Executable and embedding elements are deliberately absent. -/
inductive Tag where
  | html | head | body | title | main | header | footer | nav | section | article | aside
  | div | span | p | a | h1 | h2 | h3 | h4 | h5 | h6 | ul | ol | li | dl | dt | dd
  | form | label | button | textarea | select | option | fieldset | legend
  | table | thead | tbody | tfoot | tr | th | td | caption
  | strong | em | small | code | pre | blockquote | time | details | summary
  deriving Repr, BEq, Inhabited

def Tag.asString : Tag → String
  | .html => "html" | .head => "head" | .body => "body" | .title => "title"
  | .main => "main" | .header => "header" | .footer => "footer" | .nav => "nav"
  | .section => "section" | .article => "article" | .aside => "aside"
  | .div => "div" | .span => "span" | .p => "p" | .a => "a"
  | .h1 => "h1" | .h2 => "h2" | .h3 => "h3" | .h4 => "h4" | .h5 => "h5" | .h6 => "h6"
  | .ul => "ul" | .ol => "ol" | .li => "li" | .dl => "dl" | .dt => "dt" | .dd => "dd"
  | .form => "form" | .label => "label" | .button => "button" | .textarea => "textarea"
  | .select => "select" | .option => "option" | .fieldset => "fieldset" | .legend => "legend"
  | .table => "table" | .thead => "thead" | .tbody => "tbody" | .tfoot => "tfoot"
  | .tr => "tr" | .th => "th" | .td => "td" | .caption => "caption"
  | .strong => "strong" | .em => "em" | .small => "small" | .code => "code" | .pre => "pre"
  | .blockquote => "blockquote" | .time => "time" | .details => "details" | .summary => "summary"

/-- Void tags have no children by construction. -/
inductive VoidTag where
  | input | img | br | hr | meta_ | link
  deriving Repr, BEq, Inhabited

def VoidTag.asString : VoidTag → String
  | .input => "input" | .img => "img" | .br => "br" | .hr => "hr" | .meta_ => "meta" | .link => "link"

inductive TextAttribute where
  | id | class_ | name | value | title | type_ | placeholder | autocomplete | role
  | ariaLabel | ariaDescribedby | ariaInvalid | lang | charset | content | rel | target
  | for_ | method | enctype | accept | alt | width | height | scope | datetime
  | min | max | minlength | maxlength | pattern | rows | cols | step
  deriving Repr, BEq

def TextAttribute.asString : TextAttribute → String
  | .id => "id" | .class_ => "class" | .name => "name" | .value => "value"
  | .title => "title" | .type_ => "type" | .placeholder => "placeholder"
  | .autocomplete => "autocomplete" | .role => "role" | .ariaLabel => "aria-label"
  | .ariaDescribedby => "aria-describedby" | .ariaInvalid => "aria-invalid"
  | .lang => "lang" | .charset => "charset" | .content => "content" | .rel => "rel"
  | .target => "target" | .for_ => "for" | .method => "method" | .enctype => "enctype"
  | .accept => "accept" | .alt => "alt" | .width => "width" | .height => "height"
  | .scope => "scope" | .datetime => "datetime" | .min => "min" | .max => "max"
  | .minlength => "minlength" | .maxlength => "maxlength" | .pattern => "pattern"
  | .rows => "rows" | .cols => "cols" | .step => "step"

inductive UrlAttribute where
  | href | src | action | formaction
  deriving Repr, BEq

def UrlAttribute.asString : UrlAttribute → String
  | .href => "href" | .src => "src" | .action => "action" | .formaction => "formaction"

inductive BoolAttribute where
  | disabled | checked | selected | required | multiple | readonly | autofocus | hidden | open_
  deriving Repr, BEq

def BoolAttribute.asString : BoolAttribute → String
  | .disabled => "disabled" | .checked => "checked" | .selected => "selected"
  | .required => "required" | .multiple => "multiple" | .readonly => "readonly"
  | .autofocus => "autofocus" | .hidden => "hidden" | .open_ => "open"

/-- Attribute names cannot contain event handlers, style, or arbitrary markup.
URL attributes require a validated SafeUrl rather than a String. -/
inductive Attribute where
  | text (attr : TextAttribute) (value : String)
  | url (name : UrlAttribute) (value : SafeUrl)
  | flag (name : BoolAttribute) (enabled : Bool := true)
  deriving Repr

/-- Append escaped UTF-8 directly to a byte builder. Text and attribute contexts
both escape quotes, so a fragment can safely be reused in either context. -/
def escapeInto (out : ByteArray) (value : String) : ByteArray :=
  value.toUTF8.foldl (fun out byte =>
    match byte.toNat with
      | 38 => out.append "&amp;".toUTF8 | 60 => out.append "&lt;".toUTF8 | 62 => out.append "&gt;".toUTF8
      | 34 => out.append "&quot;".toUTF8 | 39 => out.append "&#39;".toUTF8 | 0 => out.append "&#xfffd;".toUTF8
      | _ => out.push byte) out

/-- Rendered, safe HTML bytes. Construction is restricted to escaping builders
and the explicitly named raw escape hatch. -/
structure Html where
  private mk ::
  bytes : ByteArray

abbrev Component (α : Type) := α → Html
abbrev Layout := Html → Html

def text (value : String) : Html := ⟨escapeInto ByteArray.empty value⟩

def empty : Html := ⟨ByteArray.empty⟩

/-- Deliberately bypass all HTML escaping. Only use for audited, trusted markup. -/
def rawUnsafe (trusted : ByteArray) : Html := ⟨trusted⟩

def render (html : Html) : ByteArray := html.bytes

def append (left right : Html) : Html := ⟨left.bytes.append right.bytes⟩

def fragment (children : Array Html) : Html :=
  ⟨children.foldl (fun bytes child => bytes.append child.bytes) ByteArray.empty⟩

private def Attribute.renderInto (out : ByteArray) : Attribute → ByteArray
  | .text attr value =>
    ((escapeInto (out.append s!" {attr.asString}=\"".toUTF8) value).append "\"".toUTF8)
  | .url name value =>
    ((escapeInto (out.append s!" {name.asString}=\"".toUTF8) value.value).append "\"".toUTF8)
  | .flag name enabled => if enabled then out.append s!" {name.asString}".toUTF8 else out

def element (tag : Tag) (attributes : Array Attribute := #[]) (children : Array Html := #[]) : Html :=
  let start := attributes.foldl Attribute.renderInto s!"<{tag.asString}".toUTF8
  let body := children.foldl (fun bytes child => bytes.append child.bytes) (start.append ">".toUTF8)
  ⟨body.append s!"</{tag.asString}>".toUTF8⟩

def void (tag : VoidTag) (attributes : Array Attribute := #[]) : Html :=
  ⟨(attributes.foldl Attribute.renderInto s!"<{tag.asString}".toUTF8).append ">".toUTF8⟩

def toResponse (html : Html) (status : Nat := 200) : Response :=
  ({ status, body := html.bytes } : Response) |>.withHeader "content-type" "text/html; charset=utf-8"

end LeanWeave.Html
