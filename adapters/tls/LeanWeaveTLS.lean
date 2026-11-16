import LeanWeave.Core.Static
import LeanNetTLS.ReactorServer

namespace LeanWeave.TLS

abbrev Config := LeanNetTLS.Reactor.Config

private def configure (bundle : LeanNetTLS.Reactor.Bundle) (tls : Config)
    (config : Server.Config) (upstreamCA : Option String) : Server.Config :=
  let net := { config.net with gateway := config.net.gateway.map fun gateway =>
    if gateway.tlsProviderFactory.isSome then gateway else
      { gateway with tlsProviderFactory := some fun reactor => LeanNetTLS.GatewayProvider.new reactor upstreamCA } }
  let net := if tls.reload.isNone then net else { net with onReload := do
    net.onReload
    let _ ← bundle.requestReload
    pure () }
  { config with net }

private def withTransport (tls : Config) (config : Server.Config) (upstreamCA : Option String)
    (use : LeanNet.HTTP.Transport.WithWorker → Server.Config → IO Unit) : IO Unit := do
  match config.validate with
  | .error message => throw (IO.userError message)
  | .ok () => pure ()
  LeanNetTLS.Reactor.withBundle tls fun bundle =>
    use (fun _ maxClients reactor run => LeanNetTLS.Reactor.withWorker bundle tls reactor maxClients run)
      (configure bundle tls config upstreamCA)

/-- Optional TLS listener. ALPN, verified client certificates and reload remain
lean-net capabilities; application continuations stay on their reactor owner. -/
def serve (factory : Nat → IO Handler) (tls : Config)
    (config : Server.Config := { port := 8443 }) (upstreamCA : Option String := none) : IO Unit :=
  withTransport tls config upstreamCA fun transport config => Server.serveWithTransport transport factory config

/-- Worker-owned application resources are released after encrypted output drains. -/
def serveManaged (factory : Nat → (Handler → IO Unit) → IO Unit) (tls : Config)
    (config : Server.Config := { port := 8443 }) (upstreamCA : Option String := none) : IO Unit :=
  withTransport tls config upstreamCA fun transport config => Server.serveManagedWithTransport transport factory config

/-- Serve mounted immutable file regions through dedicated TLS workers, preserving
compression, validators, HEAD framing and application deferred replies. -/
def serveStatic (assets : Server.StaticConfig) (factory : Nat → IO Handler) (tls : Config)
    (config : Server.Config := { port := 8443 }) (upstreamCA : Option String := none) : IO Unit :=
  withTransport tls config upstreamCA fun transport config => Server.serveStaticWithTransport transport assets factory config

def serveStaticManaged (assets : Server.StaticConfig) (factory : Nat → (Handler → IO Unit) → IO Unit)
    (tls : Config) (config : Server.Config := { port := 8443 }) (upstreamCA : Option String := none) : IO Unit :=
  withTransport tls config upstreamCA fun transport config =>
    Server.serveStaticManagedWithTransport transport assets factory config

end LeanWeave.TLS
