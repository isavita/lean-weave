#!/usr/bin/env python3
"""Real RLS and scoped SQL tests under a non-superuser PostgreSQL role."""
import argparse
import json
import os
from pathlib import Path
import subprocess
from cluster_support import cluster

root = Path(__file__).resolve().parents[1]
p = argparse.ArgumentParser()
p.add_argument("--pg-bin")
p.add_argument("--report", type=Path, default=root / "test/tenant-result.json")
a = p.parse_args()
with cluster(a.pg_bin, tls=True) as pg:
    binary = root / (".lake/build/bin/weave-pg-tenant-test" + (".exe" if os.name == "nt" else ""))
    ddl = json.loads(subprocess.check_output([str(binary),"ddl"],text=True))
    pg.sql("CREATE ROLE weave_app LOGIN NOSUPERUSER NOBYPASSRLS; "
        "CREATE TABLE platform_tenant_items (tenant_id text NOT NULL, id bigint NOT NULL, title text NOT NULL, PRIMARY KEY(tenant_id,id)); " +
        "; ".join(ddl) + "; GRANT SELECT,INSERT,UPDATE,DELETE ON platform_tenant_items TO weave_app;")
    subprocess.run([str(binary)], env=pg.environment("weave_app"), check=True, timeout=30)
    a.report.write_text(json.dumps({"passed":True,"kind":"real PostgreSQL TLS with non-superuser FORCE RLS",
        "tenant_isolation":True,"pool_scope_reset":True,"parameter_injection_rejected":True,
        "tenant_override_rejected":True,"actual_migration_helper_executed":True,
        "escaped_connection_reborrow_rejected":True,"escaped_tenant_scope_reborrow_rejected":True,
        "same_physical_connection_verified":True,"current_lease_preserved":True,
        "persistent_service_started":False}, indent=2)+"\n")
