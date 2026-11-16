import LeanWeave.Core.Types
import LeanWeave.Core.Decode
import LeanNet.HTTP.ReactorServer

namespace LeanWeave.Server

/-- Typed startup configuration; native transport, TLS/static composition and
shutdown remain lean-net responsibilities. -/
structure Config where
  host : String := "127.0.0.1"
  port : UInt16 := 8080
  net : LeanNet.HTTP.ReactorServerConfig := {}
  /-- Whole-handler deadline; each await may have a tighter deadline. -/
  appTimeoutMs : Nat := 30000
  /-- Fixed reservation charged to lean-net before a deferred execution enters
      its queue. Every await must fit this reservation. -/
  appRetainedBytes : Nat := 65536
  /-- Runs in declaration order on both protocols before decoded body intake. -/
  headerChecks : Array (LeanNet.HTTP1.RequestHead → Except AppError Unit) := #[]
  /-- Optional early response, e.g. CORS preflight; runs after headerChecks. -/
  onHeaders : LeanNet.HTTP1.RequestHead → IO (Except AppError (Option Response)) :=
    fun _ => pure (.ok none)

def Config.validate (config : Config) : Except String Unit := do
  if config.host.isEmpty || config.host.toList.any (fun c => c.isWhitespace || c == '\x00') then
    throw "WEAVE_HOST must be a nonempty host without whitespace"
  if config.port == 0 then throw "WEAVE_PORT must be between 1 and 65535"
  if config.appTimeoutMs == 0 || config.appTimeoutMs > 9223372036854775807 then
    throw "WEAVE_APP_TIMEOUT_MS must be positive and fit the native clock"
  if config.appRetainedBytes == 0 || config.appRetainedBytes > config.net.deferred.maxRetainedBytes then
    throw "application retained byte reservation must fit the worker deferred byte budget"
  config.net.deferred.validate

private def envNat (name : String) (fallback : Nat) : IO (Except String Nat) := do
  let some value ← IO.getEnv name | return .ok fallback
  return match value.toNat? with
    | some value => .ok value
    | none => .error s!"{name} must be an unsigned decimal integer"

/-- Read strict startup values; malformed settings never silently fall back. -/
def Config.fromEnv (defaults : Config := {}) : IO (Except String Config) := do
  let host := (← IO.getEnv "WEAVE_HOST").getD defaults.host
  let port ← envNat "WEAVE_PORT" defaults.port.toNat
  let workers ← envNat "WEAVE_WORKERS" defaults.net.workers
  let timeout ← envNat "WEAVE_APP_TIMEOUT_MS" defaults.appTimeoutMs
  let retained ← envNat "WEAVE_APP_RETAINED_BYTES" defaults.appRetainedBytes
  return do
    let port ← port
    if port == 0 || port > 65535 then throw "WEAVE_PORT must be between 1 and 65535"
    let workers ← workers
    if workers > 256 then throw "WEAVE_WORKERS must be at most 256"
    let config := { defaults with
      host
      port := port.toUInt16
      net := { defaults.net with workers }
      appTimeoutMs := ← timeout
      appRetainedBytes := ← retained }
    config.validate
    return config

private def reply (result : Except AppError Response) : LeanNet.HTTP1.Reply :=
  { response := (match result with | .ok response => response | .error error => Response.ofError error).toNet }

/-- HTTP/2's head rejection is bodyless. Normalize HTTP/1 early replies too. -/
private def headReply (result : Except AppError Response) : LeanNet.HTTP1.Reply :=
  let value := reply result
  { value with response := { value.response with body := ByteArray.empty } }

/-- Construct callbacks once on the owner worker. No tasks are created per
request, and no sockets or borrowed native views enter AppM. -/
def handlers (handler : Handler) (config : Config := {}) : LeanNet.HTTP1.Handlers := {
  onHeaders := fun head _ => do
    for check in config.headerChecks do
      if let .error error := check head then return .reject (headReply (.error error))
    match ← config.onHeaders head with
    | .error error => return .reject (headReply (.error error))
    | .ok (some response) => return .reject (headReply (.ok response))
    | .ok none => return .accept none
  onRequest := fun request => do
    match Context.fromRequest request with
    | .error error => return reply (.error error)
    | .ok context => return reply (← AppM.run (handler context))
  onRequestDeferred := some fun request => do
    match Context.fromRequest request with
    | .error error => return .immediate (reply (.error error))
    | .ok context =>
      let execution ← AppM.execute (handler context) config.appRetainedBytes
      if let some result ← execution.result.get then return .immediate (reply result)
      return .deferred {
        poll := do return (← execution.poll).map reply
        cancel := execution.cancel
        timeoutMs := config.appTimeoutMs
        retainedBytes := config.appRetainedBytes }
}

/-- Create independent application state in each reactor worker and inherit
lean-net admission, protocol parity, transport budgets and graceful shutdown. -/
def serveWithTransport (provider : LeanNet.HTTP.Transport.WithWorker)
    (factory : Nat → IO Handler) (config : Config := {}) : IO Unit := do
  match config.validate with
  | .error message => throw (IO.userError message)
  | .ok () => pure ()
  LeanNet.HTTP.ReactorServer.serveFactoryWithTransport provider
    (fun worker => do return handlers (← factory worker) config)
    config.net config.host config.port

/-- Bracket resources on their owning worker. Finalizers run after the reactor
    drains; use `try use handler finally pool.close` inside the factory. -/
def serveManagedWithTransport (provider : LeanNet.HTTP.Transport.WithWorker)
    (factory : Nat → (Handler → IO Unit) → IO Unit) (config : Config := {}) : IO Unit := do
  match config.validate with
  | .error message => throw (IO.userError message)
  | .ok () => pure ()
  LeanNet.HTTP.ReactorServer.serveWorkerFactory
    (fun worker maxClients reactor use =>
      provider worker maxClients reactor fun transport =>
        factory worker fun handler => use transport (handlers handler config))
    config.net config.host config.port

/-- Managed plaintext application lifecycle. -/
def serveManaged (factory : Nat → (Handler → IO Unit) → IO Unit) (config : Config := {}) : IO Unit :=
  serveManagedWithTransport LeanNet.HTTP.Transport.withPlainWorker factory config

/-- Plaintext listener using lean-net's native transport. -/
def serve (factory : Nat → IO Handler) (config : Config := {}) : IO Unit :=
  serveWithTransport LeanNet.HTTP.Transport.withPlainWorker factory config

end LeanWeave.Server
