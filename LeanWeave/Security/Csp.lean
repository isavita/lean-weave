import LeanWeave.Security.Crypto
import LeanWeave.Html
import Lean

namespace LeanWeave.Security.Csp

inductive Source where
  | self | none | nonce | strictDynamic | https | data
  | host (value : String)
  | sha256 (digest : String)
  deriving Repr, BEq, Lean.ToExpr

inductive Directive where
  | defaultSrc | scriptSrc | styleSrc | imgSrc | connectSrc | fontSrc
  | frameSrc | frameAncestors | objectSrc | baseUri | formAction | mediaSrc | workerSrc
  deriving Repr, BEq, Lean.ToExpr

def Directive.name : Directive → String
  | .defaultSrc => "default-src" | .scriptSrc => "script-src" | .styleSrc => "style-src"
  | .imgSrc => "img-src" | .connectSrc => "connect-src" | .fontSrc => "font-src"
  | .frameSrc => "frame-src" | .frameAncestors => "frame-ancestors" | .objectSrc => "object-src"
  | .baseUri => "base-uri" | .formAction => "form-action" | .mediaSrc => "media-src" | .workerSrc => "worker-src"

def Directive.parse (name : String) : Except String Directive :=
  match name with
  | "default-src" => .ok .defaultSrc | "script-src" => .ok .scriptSrc | "style-src" => .ok .styleSrc
  | "img-src" => .ok .imgSrc | "connect-src" => .ok .connectSrc | "font-src" => .ok .fontSrc
  | "frame-src" => .ok .frameSrc | "frame-ancestors" => .ok .frameAncestors | "object-src" => .ok .objectSrc
  | "base-uri" => .ok .baseUri | "form-action" => .ok .formAction | "media-src" => .ok .mediaSrc | "worker-src" => .ok .workerSrc
  | _ => .error s!"unsupported CSP directive {name}"

structure Policy where
  directives : Array (Directive × Array Source)
  upgradeInsecureRequests : Bool := false
  reportOnly : Bool := false
  deriving Repr, BEq, Lean.ToExpr

private def validHost (source : String) : Bool := Id.run do
  if source.isEmpty || source.utf8ByteSize > 256 then return false
  let scheme := source.splitOn "://"
  if scheme.length > 2 then return false
  let value := if scheme.length == 2 then scheme[1]! else source
  if scheme.length == 2 && !["http", "https", "ws", "wss"].contains scheme[0]! then return false
  let parts := value.splitOn "/"
  let authority := parts.head!.splitOn ":"
  if authority.length > 2 then return false
  if authority.length == 2 && authority[1]! != "*" then
    let some port := authority[1]!.toNat? | return false
    if port == 0 || port > 65535 then return false
  let host := authority[0]!
  let host := if host.startsWith "*." then (host.drop 2).toString else host
  if host.isEmpty then return false
  if host != "*" then
    for label in host.splitOn "." do
      if label.isEmpty || label.utf8ByteSize > 63 || label.startsWith "-" || label.endsWith "-" then return false
      if !label.toUTF8.data.all (fun b => (b >= 65 && b <= 90) || (b >= 97 && b <= 122) ||
          (b >= 48 && b <= 57) || b == 45) then return false
  for path in parts.tail! do
    if !path.toUTF8.data.all (fun b => (b >= 65 && b <= 90) || (b >= 97 && b <= 122) ||
        (b >= 48 && b <= 57) || [45,46,95,126].contains b.toNat) then return false
  return true

def Policy.validate (policy : Policy) : Except String Unit := do
  if policy.directives.isEmpty || policy.directives.size > 16 then throw "invalid CSP directive count"
  let names := policy.directives.map (fun p => p.1.name)
  if names.size != names.toList.eraseDups.length then throw "duplicate CSP directive"
  unless names.contains "default-src" && names.contains "object-src" && names.contains "base-uri" do
    throw "CSP requires default-src, object-src and base-uri"
  for (directive,sources) in policy.directives do
    if sources.isEmpty || sources.size > 32 then throw "CSP source list must contain 1..32 entries"
    if sources.contains .none && sources.size != 1 then throw "CSP none must be the only source"
    for source in sources do
      match source with
      | .host host => unless validHost host do throw "invalid CSP host source"
      | .sha256 digest =>
        let bytes ← Crypto.unbase64url digest 32
        if bytes.size != 32 then throw "CSP SHA256 hash must contain 32 bytes"
      | .nonce =>
        unless directive == .scriptSrc || directive == .styleSrc do throw "nonce/dynamic source requires script-src or style-src"
      | .strictDynamic =>
        unless directive == .scriptSrc do throw "strict-dynamic requires script-src"
      | .data => unless directive == .imgSrc || directive == .fontSrc do throw "data URLs only permitted for images/fonts"
      | _ => pure ()

/-- Compile-time DSL uses 'nonce' as a per-request nonce marker, never a static nonce. -/
def Policy.parse (text : String) : Except String Policy := do
  let mut directives := #[]
  let mut upgrade := false
  for declaration in text.splitOn ";" do
    let words := (declaration.trimAscii.toString.splitOn " ").filter (· != "")
    if !words.isEmpty then
      if words == ["upgrade-insecure-requests"] then upgrade := true
      else
        let directive ← Directive.parse words.head!
        let mut sources := #[]
        for word in words.tail! do
          let source ← match word with
            | "'self'" => pure Source.self | "'none'" => pure Source.none | "'nonce'" => pure Source.nonce
            | "'strict-dynamic'" => pure Source.strictDynamic | "https:" => pure Source.https | "data:" => pure Source.data
            | _ =>
              if word.startsWith "'sha256-" && word.endsWith "'" then
                pure (Source.sha256 (((word.drop 8).toString.dropEnd 1).toString))
              else if word.contains '\'' then throw "unsupported CSP keyword; unsafe-inline/eval are not implicit escape hatches"
              else pure (Source.host word)
          sources := sources.push source
        directives := directives.push (directive,sources)
  let policy := { directives, upgradeInsecureRequests := upgrade }
  policy.validate
  return policy

private def renderSource (nonce : String) : Source → String
  | .self => "'self'" | .none => "'none'" | .nonce => "'nonce-" ++ nonce ++ "'"
  | .strictDynamic => "'strict-dynamic'" | .https => "https:" | .data => "data:"
  | .host value => value | .sha256 digest => "'sha256-" ++ digest ++ "'"

def Policy.render (policy : Policy) (nonce : String) : Except String String := do
  policy.validate
  if nonce.utf8ByteSize < 22 || !(Crypto.unbase64url nonce 64).isOk then throw "CSP nonce must contain at least 128 random bits"
  let values := policy.directives.toList.map fun (directive,sources) =>
    directive.name ++ " " ++ String.intercalate " " (sources.toList.map (renderSource nonce))
  return String.intercalate "; " (values ++ if policy.upgradeInsecureRequests then ["upgrade-insecure-requests"] else [])

def freshNonce : IO String := do return Crypto.base64url (← IO.getRandomBytes 32)

def nonce (ctx : Context) : Option String := ctx.attribute "csp.nonce"

/-- Nonce and header are minted together per response. Render text with typed HTML;
inline executable content still requires the explicit unsafe HTML escape hatch. -/
def middleware (policy : Policy) : Middleware := fun next ctx => do
  let value ← freshNonce
  let header ← match policy.render value with | .ok header => pure header | .error _ => throw { message := "invalid CSP configuration" }
  let attributes := (ctx.attributes.filter (fun p => p.1 != "csp.nonce")).push ("csp.nonce", value)
  let response ← next { ctx with attributes }
  return response.withHeader (if policy.reportOnly then "content-security-policy-report-only" else "content-security-policy") header

open Lean Elab Term in
elab "csp!" source:str : term => do
  match Policy.parse source.getString with
  | .error error => throwErrorAt source error
  | .ok policy => return Lean.toExpr policy

end LeanWeave.Security.Csp
