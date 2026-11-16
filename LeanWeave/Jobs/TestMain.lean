import LeanWeave.Jobs.Test
import LeanWeave.Jobs.CronTest
import LeanWeave.Workflow.Test

def main : IO Unit := do
  LeanWeave.Jobs.Test.run
  LeanWeave.Jobs.Cron.Test.run
  LeanWeave.Workflow.Test.run
