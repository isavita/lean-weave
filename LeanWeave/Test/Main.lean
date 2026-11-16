import LeanWeave.Test.Security
import LeanWeave.Test.Router
import LeanWeave.Test.Html
import LeanWeave.Test.Execution

def main : IO Unit := do
  LeanWeave.Test.Router.run
  LeanWeave.Test.htmlTests
  LeanWeave.Test.Execution.run
  LeanWeave.Test.securityTests
  IO.println "LeanWeave acceptance passed"
