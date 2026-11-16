import LeanWeave
import LeanWeavePostgres

namespace LeanWeave.Integration.Health

/-- One bounded, cooperative probe belongs to each reactor owner. The shared
Health value contains booleans only; HTTP callbacks never contact PostgreSQL. -/
private structure Owner where
  pool : Postgres.Pool
  health : Operations.Health
  dependency : String
  reservation : Nat
  execution : IO.Ref (Option (AppM.Execution Unit))
  nextProbe : IO.Ref Nat
  published : IO.Ref (Option Bool)
  desired : IO.Ref (Option Bool)
  initialized : IO.Ref Bool
  draining : IO.Ref Bool
  drainUntil : IO.Ref Nat

private def Owner.stop (owner : Owner) : IO Unit := do
  owner.draining.set true
  let _ ← owner.health.beginDrain
  let pending ← owner.execution.get
  owner.execution.set none
  if let some execution := pending then execution.cancel .shutdown
  owner.pool.close

private def Owner.publish (owner : Owner) : IO Unit := do
  let desired ← owner.desired.get
  if let some ready := desired then
    if desired != (← owner.published.get) then
      if ← owner.health.dependency owner.dependency ready then owner.published.set desired
  if !(← owner.initialized.get) then
    if let some snapshot ← owner.health.snapshot then
      if snapshot.initialized then owner.initialized.set true
      else if snapshot.dependencies.all (fun dependency => !dependency.required || dependency.ready) then
        if ← owner.health.initialized then owner.initialized.set true

private def Owner.tick (owner : Owner) (now : Nat) : IO Unit := do
  if ← owner.draining.get then
    let _ ← owner.health.beginDrain
    return
  if let some execution ← owner.execution.get then
    let progress ← (try return .ok (← execution.poll)
      catch _ => pure (.error ()) : IO (Except Unit (Option (Except AppError Unit))))
    match progress with
    | .ok none => pure ()
    | .ok (some result) =>
      owner.execution.set none
      owner.desired.set (some result.isOk)
      owner.nextProbe.set (now + 40)
    | .error () =>
      owner.execution.set none
      execution.cancel .failed
      owner.desired.set (some false)
      owner.nextProbe.set (now + 40)
  owner.publish
  if (← owner.execution.get).isNone && now >= (← owner.nextProbe.get) then
    let action : AppM Unit := do
      let result ← owner.pool.query "SELECT 1"
      if result.rows != #[#[some "1"]] then
        throw { status := 503, message := "unexpected dependency probe result" }
    let execution ← AppM.execute action owner.reservation
    owner.execution.set (some execution)

private def Owner.create (worker : Nat) (health : Operations.Health)
    (inherited : Postgres.Config) : IO Owner := do
  let config := { inherited with
    maxConnections := 1
    maxWaiters := 1
    connectTimeoutMs := 250
    queryTimeoutMs := 1000
    maxRows := 1
    maxColumns := 1
    maxResultBytes := 4096
    maxQueryBytes := 256
    maxParameters := 1
    maxParameterBytes := 256 }
  return {
    pool := ← Postgres.Pool.create config
    health
    dependency := s!"postgres-{worker}"
    reservation := config.retainedBytes
    execution := ← IO.mkRef none
    nextProbe := ← IO.mkRef 0
    published := ← IO.mkRef none
    desired := ← IO.mkRef none
    initialized := ← IO.mkRef false
    draining := ← IO.mkRef false
    drainUntil := ← IO.mkRef 0 }

/-- Disposable integration mode. SIGTERM starts native drain, cancels the
owner's probe and closes its pool; the factory finalizer is an idempotent fallback.
The callbacks do not start another timeout or promise a hard filesystem deadline. -/
def run (port : UInt16) : IO Unit := do
  let config ← match ← Postgres.Config.fromEnv with
    | .ok config => pure config
    | .error message => throw (IO.userError message)
  let health ← Operations.Health.create #[⟨"postgres-0", false, true⟩, ⟨"postgres-1", false, true⟩]
  Server.serveApplications (fun worker use => do
    let owner ← Owner.create worker health config
    let probes := Operations.Health.middleware health (fun _ => pure (Response.text "Not Found" 404))
    try
      use {
        handler := fun ctx => do
          let response ← probes ctx
          let pending := (← owner.execution.get).isSome
          return response.withHeader "x-health-owner" (toString worker)
            |>.withHeader "x-health-probe-pending" (if pending then "1" else "0")
        tickIntervalMs := 5
        onTick := owner.tick
        onDrain := fun deadline => do
          -- A short test hold, capped by the existing absolute shutdown budget,
          -- lets the wire harness observe DB closure before process teardown.
          owner.drainUntil.set (min deadline ((← IO.monoMsNow) + 400))
          owner.stop
        isDrained := do return (← owner.execution.get).isNone && (← IO.monoMsNow) >= (← owner.drainUntil.get)
        onShutdown := owner.stop }
    finally owner.stop) {
      port
      net := { workers := 2, shutdownDrainMs := 1000 } }

end LeanWeave.Integration.Health
