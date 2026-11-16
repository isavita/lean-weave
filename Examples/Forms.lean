import Examples.Apps.Forms

def main (args : List String) : IO Unit := do
  LeanWeave.Examples.run (← LeanWeave.Examples.Forms.make (← LeanWeave.Examples.demoPassword args)) args true
