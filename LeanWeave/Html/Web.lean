import LeanWeave.Html.Basic
import LeanWeave.Core.Decode

namespace LeanWeave.Html

/-- A field keeps the submitted value and all its validation failures. -/
structure Field where
  name : String
  label : String
  value : String := ""
  errors : Array String := #[]
  deriving Repr, Inhabited

/-- Reusable partial for field errors. Error strings are always escaped. -/
def fieldErrors (field : Field) : Html :=
  if field.errors.isEmpty then empty
  else element .ul #[.text .id (field.name ++ "-errors"), .text .class_ "field-errors"]
    (field.errors.map fun message => element .li #[] #[text message])

def hidden (name value : String) : Html :=
  void .input #[.text .type_ "hidden", .text .name name, .text .value value]

/-- Include the session-bound CSRF token; generation and checking belong to middleware. -/
def csrfField (token : String) : Html := hidden "_csrf" token

/-- A labelled text input with escaped submitted values and accessible validation errors. -/
def textField (field : Field) (type_ : String := "text") : Html :=
  let errorAttrs := if field.errors.isEmpty then #[] else
    #[Attribute.text .ariaInvalid "true", .text .ariaDescribedby (field.name ++ "-errors")]
  element .div #[.text .class_ "field"] #[
    element .label #[.text .for_ field.name] #[text field.label],
    void .input (#[.text .id field.name, .text .name field.name, .text .type_ type_,
      .text .value field.value] ++ errorAttrs),
    fieldErrors field]

inductive FormMethod where
  | get | post
  deriving Repr, BEq

/-- Mutation forms include an anti-CSRF field. Browsers submit GET or POST;
resource update/delete methods can be represented by an explicit _method field. -/
def form (action : SafeUrl) (method : FormMethod) (children : Array Html)
    (csrfToken : String := "") : Html :=
  let methodName := if method == .get then "get" else "post"
  let children := if method == .post then #[csrfField csrfToken] ++ children else children
  element .form #[.url .action action, .text .method methodName] children

/-- Page layout applied once around the supplied partial. -/
def page (title : String) (body : Html) (locale : String := "en") : Html :=
  append (rawUnsafe "<!doctype html>".toUTF8) <|
    element .html #[.text .lang locale] #[
      element .head #[] #[void .meta_ #[.text .charset "utf-8"], element .title #[] #[text title]],
      element .body #[] #[element .main #[] #[body]]]

inductive FlashKind where
  | info | success | error
  deriving Repr, BEq

structure Flash where
  kind : FlashKind := .info
  message : String
  deriving Repr

/-- Render a flash message, typically consumed once by the session layer. -/
def flash (message : Flash) : Html :=
  let className := match message.kind with | .info => "flash info" | .success => "flash success" | .error => "flash error"
  element .div #[.text .class_ className, .text .role "status"] #[text message.message]

/-- Redirects accept the same checked URL type as link/form attributes. -/
def redirect (location : SafeUrl) (status : Nat := 303) : Response :=
  (Response.text "" (if [301, 302, 303, 307, 308].contains status then status else 303)).withHeader
    "location" location.value

inductive Representation where
  | html | json
  deriving Repr, BEq

private def parseQuality (value : String) : Nat := Id.run do
  let value := value.trimAscii.toString
  if value == "0" then return 0
  if value == "1" then return 1000
  let parts := value.splitOn "."
  if parts.length != 2 then return 0
  let whole := parts[0]!
  let fraction := parts[1]!
  if fraction.length > 3 || !fraction.toList.all (fun c => c ≥ '0' && c ≤ '9') then return 0
  let digits := ((fraction ++ "000").toList.take 3)
  let amount := digits.foldl (fun n c => n * 10 + c.toNat - '0'.toNat) 0
  if whole == "0" then return amount
  if whole == "1" && amount == 0 then return 1000
  return 0

private def mediaRange (entry : String) : String × Nat := Id.run do
  let parts := entry.splitOn ";"
  let media := parts.head! |>.trimAscii.toString |>.toLower
  let mut quality := 1000
  for parameter in parts.tail! do
    let pair := parameter.splitOn "="
    if pair.length == 2 && pair[0]!.trimAscii.toString.toLower == "q" then
      quality := parseQuality pair[1]!
  return (media, quality)

private def mediaQuality (ranges : List (String × Nat)) (media type_ : String) : Nat :=
  match ranges.find? (fun p => p.1 == media) with
  | some p => p.2
  | none => match ranges.find? (fun p => p.1 == type_ ++ "/*") with
    | some p => p.2
    | none => (ranges.find? (fun p => p.1 == "*/*")).map Prod.snd |>.getD 0

/-- Select the supported representation using Accept quality and specificity.
No acceptable offered media type returns 406 at the response helper. -/
def negotiate (accept : Option String) : Option Representation :=
  match accept with
  | none => some .html
  | some value =>
    let ranges := (value.splitOn ",").map mediaRange
    let html := mediaQuality ranges "text/html" "text"
    let json := mediaQuality ranges "application/json" "application"
    if html == 0 && json == 0 then none
    else if json > html then some .json else some .html

/-- Add Vary: Accept on both outcomes, preserving explicitly supplied HTML bytes. -/
def negotiated (ctx : Context) (html : Html) (json : Lean.Json) (status : Nat := 200) : Response :=
  let response := match negotiate (ctx.header "accept") with
    | some .html => toResponse html status
    | some .json => Response.json json status
    | none => Response.text "Not Acceptable" 406
  response.withHeader "vary" "Accept"

/-- Content fingerprints are supplied by the asset build. They form an explicit
versioned URL and are never silently recomputed from timestamps. -/
structure Asset where
  path : String
  fingerprint : String
  deriving Repr

/-- Validate a build-produced fingerprint and append it as a URL query dimension.
For immutable caching, keep old fingerprints available for their advertised lifetime. -/
def Asset.url (asset : Asset) : Except String SafeUrl := do
  if !asset.path.startsWith "/" || asset.path.startsWith "//" ||
      asset.path.contains '?' || asset.path.contains '#' then
    throw "asset paths must be absolute local paths without query or fragment"
  if asset.fingerprint.isEmpty || !asset.fingerprint.toList.all (fun c =>
      (c ≥ 'a' && c ≤ 'z') || (c ≥ 'A' && c ≤ 'Z') || (c ≥ '0' && c ≤ '9') || c == '-' || c == '_') then
    throw "asset fingerprint must be a nonempty URL-safe identifier"
  SafeUrl.parse (asset.path ++ "?v=" ++ asset.fingerprint)

end LeanWeave.Html
