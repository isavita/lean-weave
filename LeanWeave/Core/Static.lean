import LeanWeave.Core.Server
import LeanNet.HTTP.Static.Catalog

namespace LeanWeave.Server

/-- A literal URL namespace mounted onto one sealed lean-net file catalog.
Preparation happens before listen; every reactor binds its own file registration. -/
structure StaticConfig where
  root : System.FilePath
  «prefix» : String := "/assets/"
  catalog : LeanNet.HTTP.Static.Catalog.Config := {}

def StaticConfig.validate (assets : StaticConfig) : Except String Unit := do
  let path := assets.prefix
  if path.utf8ByteSize > 256 || path == "/" || !path.startsWith "/" || !path.endsWith "/" ||
      path.toUTF8.data.any (fun b => b <= 32 || b >= 127 || [37, 63, 35, 92].contains b.toNat) then
    throw "static prefix must be a literal ASCII path namespace ending in /"
  let segments := ((path.drop 1).toString.dropEnd 1).toString.splitOn "/"
  if segments.any (fun part => part.isEmpty || part == "." || part == "..") then
    throw "static prefix cannot contain empty or dot segments"

private def staticTarget (assets : StaticConfig) (target : ByteArray) : Bool :=
  let bytes := assets.prefix.toUTF8
  target.size >= bytes.size && target.extract 0 bytes.size == bytes

private def mountedTarget (assets : StaticConfig) (target : ByteArray) : ByteArray :=
  "/".toUTF8 ++ target.extract assets.prefix.utf8ByteSize target.size

/-- Preserve the complete native reply, including resident file regions and compressed
representations. Only directory redirects need their public mount restored. -/
private def mountedReply (assets : StaticConfig) (reply : LeanNet.HTTP1.Reply) : LeanNet.HTTP1.Reply :=
  { reply with response := { reply.response with headers := reply.response.headers.map fun header =>
    if header.name.toLower == "location" && header.value.extract 0 1 == "/".toUTF8 then
      { header with value := (assets.prefix.dropEnd 1).toString.toUTF8 ++ header.value }
    else header } }

/-- Combine once on the owner worker. Global application head checks run first on
original targets; catalog lookup decodes the mounted target exactly once. -/
def staticHandlers (assets : StaticConfig) (application static : LeanNet.HTTP1.Handlers) : LeanNet.HTTP1.Handlers := {
  application with
  onHeaders := fun head framing => do
    match ← application.onHeaders head framing with
    | .reject reply => return .reject reply
    | .upgrade reply pipe => return .upgrade reply pipe
    | .accept override =>
      if !staticTarget assets head.target then return .accept override
      let head := { head with target := mountedTarget assets head.target }
      match ← static.onHeaders head (override.getD framing) with
      | .accept nextOverride => return .accept (nextOverride.or override)
      | .reject reply => return .reject (mountedReply assets reply)
      | .upgrade reply pipe => return .upgrade (mountedReply assets reply) pipe
  onRequest := fun request => do
    if staticTarget assets request.target then
      return mountedReply assets (← static.onRequest { request with target := mountedTarget assets request.target })
    application.onRequest request
  onRequestDeferred := some fun request => do
    if staticTarget assets request.target then
      return .immediate (mountedReply assets (← static.onRequest { request with target := mountedTarget assets request.target }))
    match application.onRequestDeferred with
    | some run => run request
    | none => return .immediate (← application.onRequest request)
}

/-- Seal the catalog before listen and bracket application resources on each reactor.
No request callback opens/stats a file or copies its contents through Response.body. -/
def serveStaticManagedWithTransport (provider : LeanNet.HTTP.Transport.WithWorker)
    (assets : StaticConfig) (factory : Nat → (Handler → IO Unit) → IO Unit)
    (config : Config := {}) : IO Unit := do
  match config.validate *> assets.validate with
  | .error message => throw (IO.userError message)
  | .ok () => pure ()
  LeanNet.HTTP.Static.Catalog.withCatalog assets.root assets.catalog fun catalog =>
    LeanNet.HTTP.ReactorServer.serveWorkerFactory
      (fun worker maxClients reactor use => do
        let bound ← catalog.bind reactor
        let static ← bound.handlers
        provider worker maxClients reactor fun transport =>
          factory worker fun handler => use transport (staticHandlers assets (handlers handler config) static))
      config.net config.host config.port

/-- Mount a sealed native catalog while preserving per-worker streaming, realtime
and lifecycle callbacks constructed with the Application. -/
def serveStaticApplicationsWithTransport (provider : LeanNet.HTTP.Transport.WithWorker)
    (assets : StaticConfig) (factory : Nat → (Application → IO Unit) → IO Unit)
    (config : Config := {}) : IO Unit := do
  match config.validate *> assets.validate with
  | .error message => throw (IO.userError message)
  | .ok () => pure ()
  LeanNet.HTTP.Static.Catalog.withCatalog assets.root assets.catalog fun catalog =>
    LeanNet.HTTP.ReactorServer.serveWorkerFactory
      (fun worker maxClients reactor use => do
        let bound ← catalog.bind reactor
        let static ← bound.handlers
        provider worker maxClients reactor fun transport =>
          factory worker fun app => use transport (staticHandlers assets (applicationHandlers app config) static))
      config.net config.host config.port

def serveStaticApplications (assets : StaticConfig) (factory : Nat → (Application → IO Unit) → IO Unit)
    (config : Config := {}) : IO Unit :=
  serveStaticApplicationsWithTransport LeanNet.HTTP.Transport.withPlainWorker assets factory config

def serveStaticWithTransport (provider : LeanNet.HTTP.Transport.WithWorker)
    (assets : StaticConfig) (factory : Nat → IO Handler) (config : Config := {}) : IO Unit :=
  serveStaticManagedWithTransport provider assets
    (fun worker use => do use (← factory worker)) config

/-- Plaintext application with compressed, conditional static file serving. -/
def serveStatic (assets : StaticConfig) (factory : Nat → IO Handler) (config : Config := {}) : IO Unit :=
  serveStaticWithTransport LeanNet.HTTP.Transport.withPlainWorker assets factory config

end LeanWeave.Server
