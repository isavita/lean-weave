import Examples.Apps.Crud

def main (args : List String) : IO Unit := do LeanWeave.Examples.run (← LeanWeave.Examples.Crud.make) args true
