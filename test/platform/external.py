#!/usr/bin/env python3
"""Official Vault + OpenTelemetry Collector acceptance, isolated temporary services.
Set WEAVE_VAULT_BINARY and WEAVE_OTELCOL_BINARY to checksum-verified pinned tools.
"""
from pathlib import Path
import hashlib, json, os, re, signal, socket, subprocess, sys, tempfile, time
root=Path(__file__).resolve().parents[2]
sys.path.insert(0,str(root/'adapters/postgres/test'))
from cluster_support import cluster
binary=root/'integration/platform/.lake/build/bin/platform-integration'

def free_port():
    with socket.socket() as sock:
        sock.bind(('127.0.0.1',0)); return sock.getsockname()[1]
def wait_port(port, process):
    for _ in range(200):
        if process.poll() is not None: raise RuntimeError('fixture service exited: '+process.communicate()[0])
        try:
            with socket.create_connection(('127.0.0.1',port),.1): return
        except OSError: time.sleep(.025)
    raise RuntimeError('fixture service startup deadline')
def finish(process):
    if process.poll() is None: process.send_signal(signal.SIGTERM)
    try: return process.communicate(timeout=10)[0]
    except subprocess.TimeoutExpired:
        process.kill(); process.communicate(); raise

reports={}
with tempfile.TemporaryDirectory(prefix='leanweave-platform-') as temporary:
    temp=Path(temporary)
    port=free_port(); vault=os.environ['WEAVE_VAULT_BINARY']; token='leanweave-integration-root'
    certs=temp/'vault-certs';certs.mkdir()
    process=subprocess.Popen([vault,'server','-dev','-dev-tls','-dev-listen-address=127.0.0.1:'+str(port),'-dev-tls-cert-dir='+str(certs)],
        env={**os.environ,'VAULT_DEV_ROOT_TOKEN_ID':token},stdout=subprocess.PIPE,stderr=subprocess.STDOUT,text=True)
    try:
        wait_port(port,process)
        ca=certs/'vault-ca.pem'
        if not ca.exists(): ca=certs/'vault-cert.pem'
        assert ca.exists(),list(certs.iterdir())
        output=subprocess.run([str(binary),'vault'],env={**os.environ,'WEAVE_VAULT_PORT':str(port),'WEAVE_VAULT_CA':str(ca),'WEAVE_VAULT_TOKEN':token},capture_output=True,text=True,timeout=30)
        if output.returncode: raise RuntimeError(output.stdout+output.stderr)
        reports['vault']={'passed':True,'official_binary':subprocess.check_output([vault,'version'],text=True).splitlines()[0],
            'binary_sha256':hashlib.sha256(Path(vault).read_bytes()).hexdigest(),'tls_verified':True,'kv2_rotation':True,'bootstrap_token_rotation_and_old_revocation':True,'failed_refresh_expires':True,'no_persistent_service':True}
        print(output.stdout.strip())
    finally: finish(process)
    port=free_port(); collector=os.environ['WEAVE_OTELCOL_BINARY']
    config=temp/'otel.yaml'
    config.write_text('''receivers:
  otlp:
    protocols:
      http:
        endpoint: 127.0.0.1:'''+str(port)+'''
exporters:
  debug:
    verbosity: detailed
service:
  telemetry:
    metrics:
      level: none
  pipelines:
    traces:
      receivers: [otlp]
      exporters: [debug]
''')
    process=subprocess.Popen([collector,'--config='+str(config)],stdout=subprocess.PIPE,stderr=subprocess.STDOUT,text=True)
    try:
        wait_port(port,process)
        with cluster(os.environ.get('WEAVE_PG_BIN'),tls=True) as pg:
            output=subprocess.run([str(binary),'otel'],env={**pg.environment(),'WEAVE_OTLP_PORT':str(port)},capture_output=True,text=True,timeout=30)
            if output.returncode: raise RuntimeError(output.stdout+output.stderr)
        time.sleep(.2)
    finally: log=finish(process)
    assert process.returncode == 0,log
    names=re.findall(r'Name\s*:\s*(.+)',log)
    trace_ids=re.findall(r'Trace ID\s*:\s*([0-9a-f]{32})',log)
    parent_ids=re.findall(r'Parent ID\s*:\s*([0-9a-f]{16})',log)
    assert len(trace_ids)==6 and set(trace_ids)=={'4bf92f3577b34da6a3ce929d0e0e4736'},log
    assert names.count('postgres.query')==2 and names.count('HTTP POST')==2 and 'job integration' in names and 'integration.request' in names,log
    assert len(set(parent_ids))==3 and '00f067aa0ba902b7' in parent_ids,log
    assert 'parameter-is-never-exported' not in log
    reports['opentelemetry']={'passed':True,'official_binary':subprocess.check_output([collector,'--version'],text=True).strip(),
        'binary_sha256':hashlib.sha256(Path(collector).read_bytes()).hexdigest(),'transport':'native OTLP/HTTP JSON','spans_received':6,'span_names':names,'trace_id':trace_ids[0],
        'parent_contexts':len(set(parent_ids)),'automatic_real_postgres_queries':2,'automatic_real_http_calls':2,'actual_job_execution':True,
        'no_sql_parameters_exported':True,'no_persistent_service':True}
    print(output.stdout.strip())
reports['source_sha256']={name:hashlib.sha256((root/name).read_bytes()).hexdigest() for name in [
    'test/platform/external.py','integration/platform/Main.lean','integration/platform/lake-manifest.json',
    'LeanWeave/Core/AppM.lean','LeanWeave/Core/Body.lean','LeanWeave/Observability/Trace.lean',
    'LeanWeave/Observability/Export.lean','LeanWeave/Observability/Jobs.lean',
    'LeanWeave/Secrets/Manager.lean','adapters/postgres/LeanWeavePostgres/Pool.lean',
    'adapters/http/LeanWeaveHttp/Client.lean','adapters/http/LeanWeaveHttp/Otlp.lean',
    'adapters/http/LeanWeaveHttp/Vault.lean','adapters/http/c/http.c']}
reports['fixture_sha256']=hashlib.sha256(binary.read_bytes()).hexdigest()
(root/'docs/platform/external-result.json').write_text(json.dumps(reports,indent=2)+'\n')
print(json.dumps(reports))
