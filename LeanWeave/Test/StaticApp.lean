import LeanWeave.Core.Static

namespace LeanWeave.Test.StaticApp

def application : Handler := fun ctx => do
  if ctx.path == #["deferred"] then
    let start ← IO.monoMsNow
    return ← AppM.await (do
      if (← IO.monoMsNow) - start < 20 then return none
      return some (.ok (Response.text "deferred application"))) (fun _ => pure ()) 1000 16
  if ctx.path == #["dynamic"] then return Response.text "dynamic application"
  return Response.text "Not Found" 404

def config (port : UInt16) : Server.Config := {
  port
  headerChecks := #[fun head =>
    if (header? head.headers "x-deny").isSome then .error { status := 403, message := "denied before static" }
    else .ok ()]
  net := {
    workers := 2
    tracing := false
    shutdownDrainMs := 500
    server := { maxClients := 16 }
    native := {
      maxConnections := 16
      maxEvents := 32
      maxWrites := 64
      buffers4k := 64
      buffers16k := 4
      buffers64k := 2 } } }

def assets (root : System.FilePath) : Server.StaticConfig := {
  root
  catalog := {
    residentBytes := 1024 * 1024
    directoryIndex := false
    builder := { minCompressBytes := 32, maxFileBytes := 256 * 1024 } } }

end LeanWeave.Test.StaticApp
