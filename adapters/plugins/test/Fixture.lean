import LeanWeavePlugins

open LeanWeave

def main (args : List String) : IO Unit := do
  let [port,source] := args | throw (IO.userError "usage: weave-plugin-fixture PORT LIBRARY")
  let some port := port.toNat? | throw (IO.userError "invalid port")
  let config : Plugins.Config := { timeoutMs := 1000,streamIdleTimeoutMs := 2000,streamLifetimeMs := 5000 }
  Server.serveApplications (fun _ use => do
    let manager ← Plugin.Manager.create { maxGenerations := 2,maxInFlight := 2 }
    let starts ← IO.mkRef (#[] : Array String)
    let stops ← IO.mkRef (#[] : Array String)
    let loads ← IO.mkRef (0 : Nat)
    let load := fun path => do
      loads.modify (· + 1)
      let plugin ← Plugins.load path config
      return { plugin with
        onStart := do plugin.onStart; starts.modify (·.push plugin.version)
        onShutdown := do
          plugin.onShutdown
          stops.modify (·.push plugin.version)
          IO.println ("PLUGIN_STOPPED:" ++ plugin.version)
          (← IO.getStdout).flush }
    if let .error e ← manager.reload (load source) then throw (IO.userError e.message)
    let handler : Handler := fun context => do
      if context.path == #["admin","stats"] then
        let s ← manager.snapshot
        return Response.json (Lean.Json.mkObj [
          ("generations",Lean.toJson s.generations),("retired",Lean.toJson s.retired),("inFlight",Lean.toJson s.inFlight),
          ("active",Lean.toJson s.activeVersion),("starts",Lean.toJson (← starts.get)),("stops",Lean.toJson (← stops.get)),
          ("loads",Lean.toJson (← loads.get))])
      if context.path == #["admin","reload"] then
        let path := (context.queryValue "path").getD source
        match ← manager.reload (load path) with
        | .ok () => return Response.text "loaded"
        | .error error => return Response.ofError error
      if context.path == #["admin","unload"] then
        manager.unload
        return Response.text "unloaded"
      return ← manager.handler context
    try use (manager.application { handler,tickIntervalMs := 2 })
    finally
      unless ← manager.close do throw (IO.userError "plugin generation still leased at final shutdown")
  ) { port := port.toUInt16,appRetainedBytes := config.retainedBytes,
      net := { workers := 1,shutdownDrainMs := 150,deferred := { pollIntervalMs := 2 } } }
