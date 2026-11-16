import LeanWeavePostgres.Tenant

open LeanWeave

private def check (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw (IO.userError s!"tenant RLS: {message}")

private def need (result : Except ε α) : IO α :=
  match result with | .ok value => pure value | .error _ => throw (IO.userError "expected valid tenant test configuration")

private def run (action : AppM α) : IO (Except AppError α) := do
  let execution ← AppM.execute action (2 * 1024 * 1024)
  for _ in [:20000] do
    if let some value ← execution.poll then return value
    IO.sleep 1
  execution.cancel .timeout
  return .error { status := 504, message := "tenant test deadline" }

def main (arguments : List String) : IO Unit := do
  if arguments == ["ddl"] then
    let table ← need (Tenant.Sql.Table.parse "platform_tenant_items")
    IO.println (Lean.toJson (Postgres.Tenant.migrationStatements table)).compress
    return
  let config ← need (← Postgres.Config.fromEnv)
  let pool ← Postgres.Pool.create { config with maxConnections := 1 }
  try
    let alpha ← need (Tenant.Id.parse "alpha")
    let beta ← need (Tenant.Id.parse "beta")
    let table ← need (Tenant.Sql.Table.parse "platform_tenant_items")
    let id ← need (Tenant.Sql.Identifier.parse "id")
    let title ← need (Tenant.Sql.Identifier.parse "title")
    for (tenant,value) in [(alpha,"a"),(beta,"b")] do
      let query ← need (Tenant.Sql.insert tenant table #[(id,.natural 1),(title,.text value)] #[title])
      let result ← need (← run (Postgres.Tenant.withTenant pool tenant (fun scope => scope.execute query)))
      check (result.rows == #[#[some value]]) "scoped insert returned tenant value"
    for (tenant,value) in [(alpha,"a"),(beta,"b"),(alpha,"a"),(beta,"b")] do
      let query ← need (Tenant.Sql.select tenant table #[title])
      let result ← need (← run (Postgres.Tenant.withTenant pool tenant (fun scope => scope.execute query)))
      check (result.rows == #[#[some value]]) "pool reuse cannot leak cross-tenant data"
    let result ← need (← run (pool.query "SELECT count(*)::text FROM platform_tenant_items"))
    check (result.rows == #[#[some "0"]]) "RLS denies data without a transaction-local tenant"
    let forged ← run (pool.query "INSERT INTO platform_tenant_items(tenant_id,id,title) VALUES('alpha',2,'forged')")
    check (!forged.isOk) "WITH CHECK denies writes without scoped tenant"
    let injected ← need (Tenant.Sql.select alpha table #[title] #[{ column := title, value := .text "' OR true --" }])
    let result ← need (← run (Postgres.Tenant.withTenant pool alpha (fun scope => scope.execute injected)))
    check (result.rows.isEmpty) "SQL injection remains a parameter"
    let tenantColumn ← need (Tenant.Sql.Identifier.parse "tenant_id")
    check (!(Tenant.Sql.insert alpha table #[(tenantColumn,.text "beta")]).isOk) "cannot override inserted tenant"
    check (!(Tenant.Sql.update alpha table #[(tenantColumn,.text "beta")] #[{ column := id, value := .natural 1 }]).isOk) "cannot move rows across tenants"
    check (!(Tenant.Sql.Identifier.parse "title; DROP TABLE platform_tenant_items").isOk) "identifiers reject injection"
    -- Even a forged setting is constrained to a non-superuser, and switching it
    -- requires trusted application SQL unavailable through the scoped API.
    IO.println "tenant SQL and real PostgreSQL RLS isolation passed"
  finally pool.close
