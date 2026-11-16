import LeanWeave.Test.StaticApp

def main (args : List String) : IO Unit := do
  let [root, portText] := args | throw (IO.userError "usage: weave-static-fixture ROOT PORT")
  let some port := portText.toNat? | throw (IO.userError "invalid port")
  if port == 0 || port > 65535 then throw (IO.userError "invalid port")
  LeanWeave.Server.serveStatic (LeanWeave.Test.StaticApp.assets root)
    (fun _ => pure LeanWeave.Test.StaticApp.application) (LeanWeave.Test.StaticApp.config port.toUInt16)
