import LeanWeavePostgres
import LeanWeave.Core.Server

open LeanWeave

/-- A database pool belongs to its reactor worker and closes after that worker drains. -/
def main : IO Unit := do
  let pgConfig ← match ← Postgres.Config.fromEnv with
    | .ok config => pure config
    | .error message => throw (IO.userError message)
  let defaults : Server.Config := { appRetainedBytes := pgConfig.retainedBytes }
  let serverConfig ← match ← Server.Config.fromEnv defaults with
    | .ok config => pure config
    | .error message => throw (IO.userError message)
  if serverConfig.appRetainedBytes < pgConfig.retainedBytes then
    throw (IO.userError "WEAVE_APP_RETAINED_BYTES is below the PostgreSQL adapter reservation")
  Server.serveManaged (fun _ run => do
    let pool ← Postgres.Pool.create pgConfig
    try
      run fun ctx => do
        let value := (ctx.queryValue "value").getD "Hello, PostgreSQL"
        let result ← pool.query "SELECT $1::text AS value" #[some value]
        let text := (result.rows[0]?.bind (·[0]?.join)).getD ""
        return Response.json (Lean.Json.mkObj [("value", Lean.Json.str text)])
    finally pool.close) serverConfig
