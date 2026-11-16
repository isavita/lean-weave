import LeanWeave.Test.PlatformSecurity
import LeanWeave.Test.PlatformOperations
import LeanWeave.Test.MacroHygiene
import LeanWeave.Test.Generator
import LeanWeave.Test.BodyBounds
import LeanWeave.Test.JsonBoundarySecurity

def main : IO Unit := do
  LeanWeave.Test.BodyBounds.run
  LeanWeave.Test.JsonBoundarySecurity.run
  LeanWeave.Test.Generator.run
  LeanWeave.Test.MacroHygiene.run
  LeanWeave.Test.PlatformSecurity.run
  LeanWeave.Test.PlatformOperations.run
