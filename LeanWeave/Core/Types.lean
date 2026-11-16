import LeanWeave.Core.AppM
import LeanNet.HTTP1.Request
import LeanNet.HTTP1.Response
import LeanNet.HTTP1.Encoding
import LeanWeave.Core.Stream
import Lean.Data.Json

namespace LeanWeave

inductive CaptureValue where
  | text (value : String)
  | nat (value : Nat)
  | int (value : Int)
  deriving Repr, BEq, Inhabited

/-- Owned request data only: sockets and borrowed reactor buffers never enter application context. -/
structure Context where
  request : LeanNet.HTTP1.Request
  /-- UTF-8 path segments, percent-decoded exactly once by `Context.fromRequest`. -/
  path : Array String := #[]
  query : Array (String × String) := #[]
  captures : Array (String × CaptureValue) := #[]
  routeName : String := ""
  host : String := ""
  tenant : String := ""
  locale : String := "en"
  requestId : String := ""
  authenticated : Bool := false
  sessionId : Option String := none
  attributes : Array (String × String) := #[]

/-- Complete owned response. lean-net owns framing and HEAD suppression. -/
structure Response where
  status : Nat := 200
  headers : Array LeanNet.HTTP1.Header := #[]
  body : ByteArray := ByteArray.empty
  /-- Optional worker-owned cooperative body; exclusive with `body`. -/
  stream : Option Stream.Body := none

abbrev Handler := Context → AppM Response
abbrev Middleware := Handler → Handler

def header? (headers : Array LeanNet.HTTP1.Header) (name : String) : Option String :=
  (headers.find? fun h => h.name.toLower == name.toLower).bind fun h => String.fromUTF8? h.value

def Context.header (ctx : Context) (name : String) : Option String := header? ctx.request.headers name

/-- Transport-established peer IP bytes; absent outside a native server request. -/
def Context.peer (ctx : Context) : Option ByteArray := ctx.request.transport.map (·.peer)

def Context.secure (ctx : Context) : Bool := ctx.request.transport.any (·.secure)

def Context.attribute (ctx : Context) (name : String) : Option String :=
  (ctx.attributes.find? fun p => p.1 == name).map Prod.snd

def Context.capture (ctx : Context) (name : String) : Option CaptureValue :=
  (ctx.captures.find? fun p => p.1 == name).map Prod.snd

def Context.queryValue (ctx : Context) (name : String) : Option String :=
  (ctx.query.find? fun p => p.1 == name).map Prod.snd

def Response.withHeader (r : Response) (name value : String) : Response :=
  { r with headers := (r.headers.filter (fun h => h.name.toLower != name.toLower)).push { name := name, value := value.toUTF8 } }

def Response.getHeader (r : Response) (name : String) : Option String := header? r.headers name

def Response.text (body : String) (status : Nat := 200) : Response :=
  ({ status, body := body.toUTF8 } : Response) |>.withHeader "content-type" "text/plain; charset=utf-8"

def Response.html (body : String) (status : Nat := 200) : Response :=
  ({ status, body := body.toUTF8 } : Response) |>.withHeader "content-type" "text/html; charset=utf-8"

def Response.json (body : Lean.Json) (status : Nat := 200) : Response :=
  ({ status, body := body.compress.toUTF8 } : Response) |>.withHeader "content-type" "application/json; charset=utf-8"

/-- Response metadata only. Use `toReply` to preserve cooperative bodies. -/
def Response.toNet (r : Response) : LeanNet.HTTP1.Response :=
  { status := r.status, headers := r.headers, body := r.body }

def Response.toReply (r : Response) : LeanNet.HTTP1.Reply :=
  { response := r.toNet, stream := r.stream }

def Response.streaming (body : Stream.Body) (status : Nat := 200)
    (headers : Array LeanNet.HTTP1.Header := #[]) : Response :=
  { status, headers, stream := some body }

def Response.ofError (e : AppError) : Response := Response.text e.message e.status

/-- First middleware in the array runs first; the composition is built once. -/
def pipeline (steps : Array Middleware) (handler : Handler) : Handler :=
  steps.foldr (fun step next => step next) handler

end LeanWeave
