import LeanWeavePostgres

open LeanWeave LeanWeave.Postgres

private def check (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw (IO.userError s!"postgres: {message}")

private def drive (execution : AppM.Execution α) (timeoutMs : Nat := 5000) : IO (Except AppError α) := do
  let started ← IO.monoMsNow
  repeat
    if let some result ← execution.poll then return result
    if (← IO.monoMsNow) - started > timeoutMs then
      execution.cancel .timeout
      throw (IO.userError "postgres test driver timed out")
    IO.sleep 1

private def runAsync (action : AppM α) : IO (Except AppError α) := do
  drive (← AppM.execute action (16 * 1024 * 1024))

private def require (result : Except AppError α) : IO α :=
  match result with
  | .ok value => pure value
  | .error error => throw (IO.userError s!"unexpected application error: {error.message}")

private def env (name fallback : String) : IO String := return (← IO.getEnv name).getD fallback

private def baseConfig (port : UInt16) : IO Config := do
  return {
    hostAddr := ← env "WEAVE_PG_HOSTADDR" "127.0.0.1"
    hostName := ← env "WEAVE_PG_HOST" "localhost"
    port
    database := ← env "WEAVE_PG_DATABASE" "postgres"
    user := ← env "WEAVE_PG_USER" "postgres"
    password := ← env "WEAVE_PG_PASSWORD" ""
    sslMode := ← env "WEAVE_PG_SSLMODE" "disable"
    sslRootCert := ← env "WEAVE_PG_SSLROOTCERT" ""
    maxConnections := 1, maxWaiters := 1, maxRows := 16,
    maxResultBytes := 8192, queryTimeoutMs := 1000 }

private def tests (config : Config) : IO Unit := do
  let pool ← Pool.create config
  try
    let injection := "'; DROP TABLE users; --"
    let result ← require (← runAsync (pool.query "SELECT $1::text AS value, $2::text AS optional" #[some injection, none]))
    check (result.columns == #["value", "optional"] && result.rows == #[#[some injection, none]]) "parameterized values and SQL NULL"
    let saved ← IO.mkRef (none : Option Connection)
    let _ ← require (← runAsync (pool.withConnection fun connection => do saved.set (some connection)))
    if let some connection ← saved.get then
      let expired ← runAsync (connection.query "SELECT 1")
      check expired.toOption.isNone "released lease rejects queries"
    let _ ← require (← runAsync (pool.query "CREATE TABLE IF NOT EXISTS leanweave_adapter_tx (id bigint PRIMARY KEY)"))
    let _ ← require (← runAsync (pool.query "DELETE FROM leanweave_adapter_tx WHERE id IN (9001, 9002)"))
    let rolledBack ← runAsync (pool.transaction fun connection => do
      let _ ← connection.query "INSERT INTO leanweave_adapter_tx(id) VALUES ($1::bigint)" #[some "9001"]
      throw ({ status := 422, message := "rollback marker" } : AppError)
      pure ())
    check (match rolledBack with | .error error => error.status == 422 | _ => false) "transaction preserves application errors"
    let absent ← require (← runAsync (pool.query "SELECT id::text FROM leanweave_adapter_tx ORDER BY id"))
    check absent.rows.isEmpty "transaction rolls back database writes"
    let committed ← require (← runAsync (pool.transaction fun connection => do
      let _ ← connection.query "INSERT INTO leanweave_adapter_tx(id) VALUES ($1::bigint)" #[some "9002"]
      connection.query "SELECT $1::text AS value, $2::text AS optional" #[some "committed", some "yes"]))
    check (committed.rows == #[#[some "committed", some "yes"]]) "transaction commits"
    let persisted ← require (← runAsync (pool.query "SELECT id::text FROM leanweave_adapter_tx ORDER BY id"))
    check (persisted.rows == #[#[some "9002"]]) "transaction persists committed writes"
    let _ ← require (← runAsync (pool.query "DELETE FROM leanweave_adapter_tx WHERE id IN (9001, 9002)"))
    let _ ← require (← runAsync (pool.migrate #[{ version := 1, statements := #["CREATE TABLE IF NOT EXISTS leanweave_adapter_test (id bigint PRIMARY KEY)"] }]))
    let changed ← runAsync (pool.migrate #[{ version := 1, statements := #["CREATE TABLE IF NOT EXISTS changed_migration (id bigint)"] }])
    check (match changed with | .error error => error.status == 500 | _ => false) "migration checksum rejects rewritten history"
    let _ ← require (← runAsync (pool.migrate #[{ version := 1, statements := #["CREATE TABLE IF NOT EXISTS leanweave_adapter_test (id bigint PRIMARY KEY)"] }]))
    let migrations ← require (← runAsync (pool.query "SELECT version::text FROM leanweave_schema_migrations ORDER BY version"))
    check (migrations.rows.any fun row => row == #[some "1"]) "migration version recorded"
    let held ← IO.mkRef false
    let first ← AppM.execute (pool.withConnection fun _ => do
      held.set true
      AppM.await (pure (none : Option (Except AppError Unit))) (fun _ => pure ()) 5000) (16 * 1024 * 1024)
    for _ in [:1000] do
      unless ← held.get do let _ ← first.poll; IO.sleep 1
    check (← held.get) "first lease acquired"
    let waiting ← AppM.execute (pool.query "SELECT 1") (16 * 1024 * 1024)
    let rejected ← AppM.execute (pool.query "SELECT 1") (16 * 1024 * 1024)
    check ((← rejected.poll).bind Except.toOption |>.isNone) "saturated waiter has no successful result"
    check (match ← rejected.poll with | some (.error error) => error.status == 503 | _ => false) "bounded waiter queue"
    waiting.cancel .disconnected
    first.cancel .disconnected
    let next ← require (← runAsync (pool.query "SELECT 1"))
    check (next.rows == #[#[some "1"]]) "cancelled waiters and leases release capacity"
    let startedQuery ← IO.mkRef false
    let cancelled ← AppM.execute (pool.withConnection fun connection => do
      startedQuery.set true
      connection.query "SELECT pg_sleep(1)") (16 * 1024 * 1024)
    for _ in [:1000] do
      unless ← startedQuery.get do let _ ← cancelled.poll; IO.sleep 1
    check (← startedQuery.get) "cancellable query started"
    cancelled.cancel .disconnected
    check (← cancelled.poll |>.map Option.isNone) "cancelled query cannot publish a late result"
    let afterCancel ← require (← runAsync (pool.query "SELECT 1"))
    check (afterCancel.rows == #[#[some "1"]]) "active query cancellation replaces connection"
  finally pool.close
  let bounded ← Pool.create { config with maxRows := 2 }
  try
    let oversized ← runAsync (bounded.query "SELECT generate_series(1, 3)::text AS value")
    check oversized.toOption.isNone "result row limit"
    let recovered ← require (← runAsync (bounded.query "SELECT 1"))
    check (recovered.rows == #[#[some "1"]]) "oversized result closes and replaces connection"
  finally bounded.close
  let small ← Pool.create { config with maxResultBytes := 1024 }
  try
    let oversized ← runAsync (small.query "SELECT $1::text AS value, $2::text AS optional"
      #[some (String.ofList (List.replicate 2000 'a')), none])
    check oversized.toOption.isNone "result byte limit"
    let recovered ← require (← runAsync (small.query "SELECT 1"))
    check (recovered.rows == #[#[some "1"]]) "oversized bytes replace connection"
  finally small.close
  let timed ← Pool.create { config with queryTimeoutMs := 50 }
  try
    let timeout ← runAsync (timed.query "SELECT pg_sleep(1)")
    check (match timeout with | .error error => error.status == 504 | _ => false) "query timeout"
    let recovered ← require (← runAsync (timed.query "SELECT 1"))
    check (recovered.rows == #[#[some "1"]]) "timeout replaces connection"
  finally timed.close

def main (args : List String) : IO Unit := do
  check (({ hostAddr := "db.example.com" } : Config).validate.toOption.isNone) "hostname cannot invoke blocking DNS"
  check (({ maxConnections := 0 } : Config).validate.toOption.isNone) "pool capacity is positive"
  check (({ sslMode := "prefer" } : Config).validate.toOption.isNone) "TLS downgrade mode rejected"
  let portText ← match args with | [port] => pure port | [] => env "WEAVE_PG_PORT" "5432" | _ => throw (IO.userError "usage: postgres-test [PORT]")
  let some port := portText.toNat? | throw (IO.userError "invalid port")
  if port == 0 || port > 65535 then throw (IO.userError "invalid port")
  let config ← baseConfig port.toUInt16
  if (← IO.getEnv "WEAVE_PG_EXPECT_CONNECT_FAILURE") == some "1" then
    let pool ← Pool.create config
    try
      let failed ← runAsync (pool.query "SELECT 1")
      check (match failed with | .error error => error.status == 503 | _ => false) "invalid TLS identity rejected"
    finally pool.close
    IO.println "PostgreSQL TLS hostname mismatch rejected"
    return
  tests config
  IO.println "PostgreSQL adapter tests passed"
