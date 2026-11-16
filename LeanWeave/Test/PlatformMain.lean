import LeanWeave.Test.PlatformSecurity
import LeanWeave.Test.PlatformOperations
import LeanWeave.Test.MacroHygiene

def main : IO Unit := do
  LeanWeave.Test.MacroHygiene.run
  LeanWeave.Test.PlatformSecurity.run
  LeanWeave.Test.PlatformOperations.run
