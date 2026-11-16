import LeanWeave.Jobs.Worker
import LeanWeave.Jobs.Cron
import LeanWeave.Core.Server

namespace LeanWeave.Jobs

/-- Mount a bounded local scheduler on lean-net's native reactor timer pulse.
Construct on the application owner. Submit must be nonblocking (e.g. Queue.enqueue);
database schedulers instead own a dedicated thread and native connection. -/
def Cron.Scheduler.application (scheduler : Cron.Scheduler) (app : Server.Application) : IO Server.Application := do
  let due ← IO.mkRef 0
  return { app with
    tickIntervalMs := min 100 app.tickIntervalMs
    onTick := fun now => do
      app.onTick now
      let wall ← wallMs
      if wall >= (← due.get) then
        let _ ← scheduler.tick wall
        due.set (← scheduler.nextDue wall)
    onDrain := fun deadline => do
      scheduler.stop
      app.onDrain deadline }

/-- Nonblocking application hooks share the server's exact monotonic cutoff.
Bracket serveApplications in withOwnedWorkers; joining happens after the native
reactor returns, never inside a socket or timer callback. -/
def Workers.application (workers : Workers) (app : Server.Application) : Server.Application :=
  { app with
    onDrain := fun deadline => do workers.beginDrainAt deadline; app.onDrain deadline
    isDrained := do return (← workers.snapshot).finished == workers.config.workers && (← app.isDrained)
    onShutdown := do workers.requestClose; app.onShutdown }

end LeanWeave.Jobs
