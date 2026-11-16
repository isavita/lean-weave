#!/usr/bin/env python3
"""Compile-time checks of the tenant-indexed SQL API (intentional failures)."""
from pathlib import Path
import subprocess,tempfile
root=Path(__file__).resolve().parents[1]
cases={
 'tenant-mismatch':('''import LeanWeavePostgres.Tenant
open LeanWeave
example (alpha beta : Tenant.Id) (scope : Postgres.Tenant.Scoped alpha)
    (command : Tenant.Sql.Command beta) : AppM Postgres.Result := scope.execute command
''','type mismatch'),
 'raw-connection':('''import LeanWeavePostgres.Tenant
open LeanWeave
example (alpha : Tenant.Id) (scope : Postgres.Tenant.Scoped alpha) := scope.connection
''','private'),
 'forged-command':('''import LeanWeavePostgres.Tenant
open LeanWeave
example (alpha : Tenant.Id) : Tenant.Sql.Command alpha :=
  { statement := "SELECT * FROM anything", parameters := #[], operation := .select }
''','private')}
with tempfile.TemporaryDirectory(prefix='weave-tenant-compile-') as temp:
 for name,(source,expected) in cases.items():
  file=Path(temp)/(name.replace('-','_')+'.lean');file.write_text(source)
  result=subprocess.run(['lake','env','lean',str(file)],cwd=root,text=True,capture_output=True,timeout=60)
  assert result.returncode!=0,name+' unexpectedly compiled'
  diagnostics=result.stdout+result.stderr
  assert expected.lower() in diagnostics.lower(),name+' failed for a different reason: '+diagnostics
  print('rejected:',name)
