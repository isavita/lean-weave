import LeanWeavePostgres.Pool
import LeanWeave.Tenant.Sql

namespace LeanWeave.Postgres.Tenant

abbrev Id := LeanWeave.Tenant.Id
abbrev Table := LeanWeave.Tenant.Sql.Table
abbrev Command := LeanWeave.Tenant.Sql.Command

/-- A tenant-indexed connection is valid only inside its transaction/lease. -/
structure Scoped (tenant : Id) where
  private mk ::
  private connection : Postgres.Connection

def Scoped.execute (scope : Scoped tenant) (command : Command tenant) : AppM Postgres.Result :=
  scope.connection.query command.statement command.parameters

/-- SQL syntax is trusted builder output; tenant values remain parameters. SET LOCAL
    is transaction scoped and cannot bleed into the next pool lease. -/
def withTenant (pool : Postgres.Pool) (tenant : Id) (use : Scoped tenant → AppM α) : AppM α :=
  pool.transaction fun connection => do
    let role ← connection.query "SELECT rolsuper, rolbypassrls FROM pg_roles WHERE rolname = current_user"
    unless role.rows == #[#[some "f", some "f"]] do
      throw { status := 500, message := "tenant pools must use a non-superuser role without BYPASSRLS" }
    let _ ← connection.query "SELECT set_config('leanweave.tenant_id', $1, true)" #[some tenant.value]
    use ⟨connection⟩

/-- Apply with schema-owner migrations, before starting least-privilege application
    pools. A restrictive boundary cannot be widened by an existing permissive policy. -/
def migrationStatements (table : Table) : Array String :=
  let name := table.quoted
  let column := table.tenantColumn.quoted
  let guard := s!"{column} = current_setting('leanweave.tenant_id', true)"
  #[s!"ALTER TABLE {name} ENABLE ROW LEVEL SECURITY",
    s!"ALTER TABLE {name} FORCE ROW LEVEL SECURITY",
    s!"CREATE POLICY leanweave_tenant_access ON {name} AS PERMISSIVE FOR ALL USING (true) WITH CHECK (true)",
    s!"CREATE POLICY leanweave_tenant_boundary ON {name} AS RESTRICTIVE FOR ALL USING ({guard}) WITH CHECK ({guard})"]

end LeanWeave.Postgres.Tenant
