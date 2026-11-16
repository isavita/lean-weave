#!/usr/bin/env python3
"""Build and record source-bound native platform acceptance; optional real services."""
from pathlib import Path
import argparse,hashlib,json,os,platform,subprocess,sys
root=Path(__file__).resolve().parents[2]
p=argparse.ArgumentParser();p.add_argument('--postgres',action='store_true');p.add_argument('--http',action='store_true');p.add_argument('--external',action='store_true');a=p.parse_args()
commands=[]
def run(args,cwd=root):
 commands.append({'argv':list(map(str,args)),'cwd':str(cwd.relative_to(root)) if cwd!=root else '.'})
 subprocess.run(list(map(str,args)),cwd=cwd,check=True,timeout=600)
suffix='.exe' if os.name=='nt' else ''
run(['lake','build','weave-platform-test','weave-crypto-fixture'])
run([root/('.lake/build/bin/weave-platform-test'+suffix)])
run([sys.executable,'test/platform/crypto.py'])
run([sys.executable,'scripts/test_platform_compile.py'])
if a.postgres:
 run(['lake','build','weave-pg-tenant-test'],root/'adapters/postgres')
 run([sys.executable,'adapters/postgres/test/tenant_compile.py'])
 run([sys.executable,'adapters/postgres/test/tenant_cluster.py','--pg-bin',os.environ.get('WEAVE_PG_BIN') or subprocess.check_output(['pg_config','--bindir'],text=True).strip()])
if a.http:
 run(['lake','build','weave-http-test'],root/'adapters/http')
 run([sys.executable,'adapters/http/test/wire.py'])
if a.external:
 run(['lake','build','platform-integration'],root/'integration/platform')
 run([sys.executable,'test/platform/external.py'])
 if a.postgres:
  run([sys.executable,'test/platform/test_health_probes.py','-v'])
  run([sys.executable,'test/platform/health.py','--pg-bin',os.environ.get('WEAVE_PG_BIN') or subprocess.check_output(['pg_config','--bindir'],text=True).strip()])
files=[Path(__file__).resolve()]
for directory in ['LeanWeave/Security','LeanWeave/Tenant','LeanWeave/Feature','LeanWeave/Observability','LeanWeave/Operations','LeanWeave/Secrets']:
 files+=list((root/directory).glob('*.lean'))
files+=list((root/'LeanWeave/Test').glob('Platform*.lean'))
files+=[root/'LeanWeave/Stream.lean',root/'LeanWeave/Test/Generator.lean',root/'LeanWeave/Test/BodyBounds.lean',root/'LeanWeave/Test/JsonBoundarySecurity.lean',root/'LeanWeave/Core/Body.lean',root/'LeanWeave/Test/MacroHygiene.lean',root/'LeanWeave/Core/Server.lean',root/'LeanWeave/Core/Static.lean',root/'LeanWeave/Cache/Response.lean',root/'adapters/postgres/LeanWeavePostgres/Tenant.lean',root/'adapters/postgres/LeanWeavePostgres/Pool.lean']
if a.http or a.external:
 files+=list((root/'adapters/http/LeanWeaveHttp').glob('*.lean'))+[root/'adapters/http/c/http.c']
if a.external and a.postgres:
 files += [root/'test/platform/health.py',root/'test/platform/test_health_probes.py']
report={'passed':True,'platform':platform.platform(),'machine':platform.machine(),'lean':subprocess.check_output(['lean','--version'],text=True).strip(),
 'lean_net_rev':next(p.get('rev') for p in json.loads((root/'lake-manifest.json').read_text())['packages'] if 'lean-net' in p['name']),'postgres':a.postgres,'http':a.http,'official_external_services':a.external,
 'native_groups':['SHA256/HMAC/base64url independent reference','hostile JWT and verified tenant sources','tenant dynamic config/rate/branding','cache tenant/config/feature isolation and bypass','sticky flags/kill/audience/mutation audit','CSP nonce/header/DSL','trace sampling/provenance/redaction/idempotence/stream cancellation','real job child span','OTLP retry/drain','structured logs/sampling/redaction/file','health/startup/readiness','shared deadline cleanup','secret rotation/expiry','audit anchoring/tamper/locking/restart/async append'],
 'commands':commands,'source_sha256':{str(file.relative_to(root)):hashlib.sha256(file.read_bytes()).hexdigest() for file in sorted(files)}}
(root/'docs/platform/platform-result.json').write_text(json.dumps(report,indent=2)+'\n')
print('platform acceptance source manifest recorded')
