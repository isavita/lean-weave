import LeanWeave.Test.PlatformSecurity
import LeanWeave.Test.PlatformOperations

def main : IO Unit := do
  LeanWeave.Test.PlatformSecurity.run
  LeanWeave.Test.PlatformOperations.run
