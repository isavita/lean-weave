import LeanWeaveTLS
import LeanWeave.Test.StaticApp

def main (args : List String) : IO Unit := do
  let [root, certificate, key, portText] := args
    | throw (IO.userError "usage: weave-tls-fixture ROOT CERT KEY PORT")
  let some port := portText.toNat? | throw (IO.userError "invalid port")
  if port == 0 || port > 65535 then throw (IO.userError "invalid port")
  LeanWeave.TLS.serveStatic (LeanWeave.Test.StaticApp.assets root)
    (fun _ => pure LeanWeave.Test.StaticApp.application)
    { certificateChain := certificate, privateKey := key, handshakeTimeoutMs := 5000, shutdownTimeoutMs := 1000 }
    (LeanWeave.Test.StaticApp.config port.toUInt16)
