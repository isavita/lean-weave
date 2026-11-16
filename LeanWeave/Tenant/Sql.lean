import LeanWeave.Tenant.Basic

namespace LeanWeave.Tenant.Sql

structure Identifier where
  private mk ::
  value : String
  deriving Repr, BEq

def Identifier.parse (value : String) : Except String Identifier := do
  if value.isEmpty || value.utf8ByteSize > 63 || !value.toUTF8.data.all (fun b =>
      (b >= 97 && b <= 122) || (b >= 65 && b <= 90) || (b >= 48 && b <= 57) || b == 95) ||
      (value.toUTF8[0]! >= 48 && value.toUTF8[0]! <= 57) then throw "SQL identifiers must be bounded ASCII names"
  return ⟨value⟩

def Identifier.quoted (name : Identifier) : String := "\"" ++ name.value ++ "\""

structure Table where
  schema : Identifier
  name : Identifier
  tenantColumn : Identifier

def Table.parse (name : String) (schema : String := "public") (tenantColumn : String := "tenant_id") : Except String Table := do
  return { schema := ← Identifier.parse schema, name := ← Identifier.parse name, tenantColumn := ← Identifier.parse tenantColumn }

def Table.quoted (table : Table) : String := table.schema.quoted ++ "." ++ table.name.quoted

inductive Value where
  | null
  | text (value : String)
  | natural (value : Nat)
  | integer (value : Int)
  | boolean (value : Bool)
  | json (value : Lean.Json)

private def Value.parameter : Value → Option String
  | .null => none | .text value => some value | .natural value => some (toString value)
  | .integer value => some (toString value) | .boolean value => some (if value then "true" else "false")
  | .json value => some value.compress

inductive Operator where
  | eq | ne | lt | le | gt | ge
  deriving Repr, BEq

private def Operator.sql : Operator → String
  | .eq => "=" | .ne => "<>" | .lt => "<" | .le => "<=" | .gt => ">" | .ge => ">="

structure Filter where
  column : Identifier
  operator : Operator := .eq
  value : Value

/-- Tenant-indexed commands cannot be executed under a different ScopedConnection.
    Construction is private; every builder injects the tenant parameter/predicate. -/
structure Command (tenant : Tenant.Id) where
  private mk ::
  statement : String
  parameters : Array (Option String)
  operation : String

private def validateParams (values : Array (Option String)) : Except String Unit := do
  if values.size > 128 || values.foldl (fun total v => total + (v.map String.utf8ByteSize).getD 0) 0 > 65536 then
    throw "tenant SQL parameter budget exceeded"
  if values.any (fun v => v.any (fun s => s.toUTF8.data.contains 0)) then throw "SQL text parameters cannot contain NUL"

private def projection (columns : Array Identifier) : Except String String := do
  if columns.isEmpty || columns.size > 64 then throw "expected 1..64 selected columns"
  if columns.size != (columns.toList.map (·.value)).eraseDups.length then throw "duplicate selected column"
  return String.intercalate ", " (columns.toList.map (·.quoted))

private def conditions (tenant : Tenant.Id) (table : Table) (filters : Array Filter)
    (initial : Array (Option String) := #[]) : Except String (String × Array (Option String)) := do
  if filters.size > 32 then throw "too many tenant query filters"
  let mut parameters := #[some tenant.value] ++ initial
  let mut predicates := #[table.tenantColumn.quoted ++ " = $1"]
  for filter in filters do
    if filter.column == table.tenantColumn then throw "tenant column is supplied by the verified scope"
    match filter.value with
    | .null =>
      if filter.operator != .eq && filter.operator != .ne then throw "NULL supports equality/inequality only"
      predicates := predicates.push (filter.column.quoted ++ if filter.operator == .eq then " IS NULL" else " IS NOT NULL")
    | value =>
      parameters := parameters.push value.parameter
      predicates := predicates.push s!"{filter.column.quoted} {filter.operator.sql} ${parameters.size}"
  validateParams parameters
  return (String.intercalate " AND " predicates.toList, parameters)

/-- Always `WHERE tenant_id=$1`; values never enter SQL syntax. -/
def select (tenant : Tenant.Id) (table : Table) (columns : Array Identifier)
    (filters : Array Filter := #[]) (limit : Nat := 100) (offset : Nat := 0)
    (orderBy : Option Identifier := none) : Except String (Command tenant) := do
  if limit == 0 || limit > 1000 || offset > 1000000 then throw "tenant query pagination budget exceeded"
  let selected ← projection columns
  let (whereClause, parameters) ← conditions tenant table filters
  let parameters := parameters.push (some (toString limit)) |>.push (some (toString offset))
  validateParams parameters
  let order := orderBy.map (fun column => " ORDER BY " ++ column.quoted) |>.getD ""
  return ⟨s!"SELECT {selected} FROM {table.quoted} WHERE {whereClause}{order} LIMIT ${parameters.size-1} OFFSET ${parameters.size}", parameters, "SELECT"⟩

private def fields (table : Table) (values : Array (Identifier × Value)) : Except String Unit := do
  if values.isEmpty || values.size > 64 then throw "expected 1..64 mutation columns"
  if values.any (fun p => p.1 == table.tenantColumn) then throw "tenant column cannot be overridden"
  if values.size != (values.toList.map (fun p => p.1.value)).eraseDups.length then throw "duplicate mutation column"

/-- Inserts inject the tenant column even when a request omits or forges it. -/
def insert (tenant : Tenant.Id) (table : Table) (values : Array (Identifier × Value))
    (returning : Array Identifier := #[]) : Except String (Command tenant) := do
  fields table values
  let parameters := #[some tenant.value] ++ values.map (fun p => p.2.parameter)
  validateParams parameters
  let columns := #[table.tenantColumn] ++ values.map Prod.fst
  let names := String.intercalate ", " (columns.toList.map (·.quoted))
  let placeholders := String.intercalate ", " ((List.range parameters.size).map (fun n => s!"${n+1}"))
  let returning ← if returning.isEmpty then pure "" else (projection returning).map (" RETURNING " ++ ·)
  return ⟨s!"INSERT INTO {table.quoted} ({names}) VALUES ({placeholders}){returning}", parameters, "INSERT"⟩

def update (tenant : Tenant.Id) (table : Table) (values : Array (Identifier × Value))
    (filters : Array Filter) (returning : Array Identifier := #[]) : Except String (Command tenant) := do
  fields table values
  if filters.isEmpty then throw "tenant updates require an explicit filter"
  let (whereClause, parameters) ← conditions tenant table filters (values.map (fun p => p.2.parameter))
  let assignments := String.intercalate ", " (values.toList.zipIdx.map (fun ((column,_),index) => s!"{column.quoted} = ${index+2}"))
  let returning ← if returning.isEmpty then pure "" else (projection returning).map (" RETURNING " ++ ·)
  return ⟨s!"UPDATE {table.quoted} SET {assignments} WHERE {whereClause}{returning}", parameters, "UPDATE"⟩

def delete (tenant : Tenant.Id) (table : Table) (filters : Array Filter)
    (returning : Array Identifier := #[]) : Except String (Command tenant) := do
  if filters.isEmpty then throw "tenant deletes require an explicit filter"
  let (whereClause, parameters) ← conditions tenant table filters
  let returning ← if returning.isEmpty then pure "" else (projection returning).map (" RETURNING " ++ ·)
  return ⟨s!"DELETE FROM {table.quoted} WHERE {whereClause}{returning}", parameters, "DELETE"⟩

/-- Schema-owner migration statements. A restrictive boundary cannot be bypassed
    by another permissive policy; FORCE also covers ordinary table owners. -/
def rlsStatements (table : Table) : Array String := #[
  s!"ALTER TABLE {table.quoted} ENABLE ROW LEVEL SECURITY",
  s!"ALTER TABLE {table.quoted} FORCE ROW LEVEL SECURITY",
  s!"CREATE POLICY leanweave_tenant_access ON {table.quoted} AS PERMISSIVE FOR ALL USING (true) WITH CHECK (true)",
  s!"CREATE POLICY leanweave_tenant_boundary ON {table.quoted} AS RESTRICTIVE FOR ALL USING ({table.tenantColumn.quoted} = current_setting('leanweave.tenant_id', true)) WITH CHECK ({table.tenantColumn.quoted} = current_setting('leanweave.tenant_id', true))"]

end LeanWeave.Tenant.Sql
