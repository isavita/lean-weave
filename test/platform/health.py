#!/usr/bin/env python3
"""Real owner-worker health probes over HTTP/1+HTTP/2 and isolated TLS PostgreSQL.
No service is installed or retained. The reported shutdown duration is observed;
this test makes no hard filesystem latency guarantee.
"""
from pathlib import Path
import argparse
import hashlib
import http.client
import json
import os
import platform
import signal
import socket
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / 'adapters/postgres/test'))
from cluster_support import cluster
NET = ROOT / '.lake/packages/lean-net'
if not NET.is_dir(): NET = ROOT.parent / 'lean-net-platform'
sys.path.insert(0, str(NET / 'test'))
from http2_test_support import H2Client


def free_port():
    with socket.socket() as sock:
        sock.bind(('127.0.0.1', 0))
        return sock.getsockname()[1]


def wait(condition, message, timeout=6):
    cutoff = time.monotonic() + timeout
    while time.monotonic() < cutoff:
        try:
            if condition(): return
        except (OSError, http.client.HTTPException): pass
        time.sleep(.01)
    raise AssertionError(message)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--pg-bin', default=os.environ.get('WEAVE_PG_BIN'))
    parser.add_argument('--report', type=Path, default=ROOT / 'docs/platform/health-result.json')
    args = parser.parse_args()
    binary = ROOT / 'integration/platform/.lake/build/bin' / ('platform-integration.exe' if os.name == 'nt' else 'platform-integration')
    role = 'weave_health_app'
    report = {}
    with cluster(args.pg_bin, tls=True) as pg, tempfile.TemporaryFile(mode='w+') as log:
        pg.sql(f'CREATE ROLE {role} NOLOGIN NOSUPERUSER NOBYPASSRLS;')
        port = free_port()
        process = subprocess.Popen([str(binary), 'health', str(port)], env=pg.environment(role), stdout=log, stderr=log,
            creationflags=subprocess.CREATE_NEW_PROCESS_GROUP if os.name == 'nt' else 0)
        paused = []
        peer = None
        next_stream = 1
        def live_process():
            if process.poll() is not None:
                log.seek(0)
                raise AssertionError(f'health process exited {process.returncode}: {log.read()}')
        def http1(probe, method='GET'):
            live_process()
            connection = http.client.HTTPConnection('127.0.0.1', port, timeout=2)
            try:
                connection.request(method, '/health/' + probe)
                response = connection.getresponse()
                return response.status, {key.lower(): value for key, value in response.getheaders()}, response.read()
            finally: connection.close()
        def http2(probe, method=b'GET'):
            nonlocal next_stream
            live_process()
            stream = next_stream
            next_stream += 2
            peer.request(stream, ('/health/' + probe).encode(), method=method)
            peer.until(lambda: stream in peer.ended or stream in peer.resets or bool(peer.goaways))
            assert stream in peer.ended and stream not in peer.resets, (peer.resets, peer.goaways)
            body = bytes(peer.bodies[stream])
            headers = dict(peer.headers[stream][0].headers)
            if method == b'HEAD':
                assert not any(frame.kind == 0 and frame.stream == stream for frame in peer.frames), 'HEAD emitted DATA'
            return int(headers[b':status']), headers, bytes(body)
        def backends():
            return int(pg.sql(f"SELECT count(*) FROM pg_stat_activity WHERE usename='{role}';").strip())
        def assert_phase(expected):
            for probe, expected_status in expected.items():
                status, headers, body = http1(probe)
                assert status == expected_status, (probe, status, body)
                assert json.loads(body) == {'probe': probe, 'ok': expected_status == 200}
                status, head, empty = http1(probe, 'HEAD')
                assert (status, empty) == (expected_status, b'')
                for field in ['content-type', 'content-length', 'cache-control']:
                    assert head[field] == headers[field], (probe, field)
                status, headers2, body2 = http2(probe)
                assert (status, body2) == (expected_status, body)
                status, head2, empty = http2(probe, b'HEAD')
                assert (status, empty) == (expected_status, b'')
                for field in [b'content-type', b'content-length', b'cache-control']:
                    assert head2[field] == headers2[field]
                assert int(head2[b'content-length']) == len(body)
        def all_http1(probe, expected):
            # Fresh connections traverse both reactor owners.
            return all(http1(probe)[0] == expected for _ in range(4))
        def stop():
            if process.poll() is None:
                process.send_signal(signal.CTRL_BREAK_EVENT if os.name == 'nt' else signal.SIGTERM)
            try: process.wait(5)
            except subprocess.TimeoutExpired:
                process.kill(); process.wait(); raise AssertionError('native health shutdown exceeded external test deadline')
        try:
            wait(lambda: http1('live')[0] == 200, 'native health listener startup deadline')
            peer = H2Client(port)
            assert_phase({'live': 200, 'startup': 503, 'ready': 503})
            assert backends() == 0
            pg.sql(f'ALTER ROLE {role} LOGIN;')
            wait(lambda: all_http1('ready', 200) and backends() == 2, 'both owner TLS probes failed to initialize')
            assert_phase({'live': 200, 'startup': 200, 'ready': 200})
            owners = {http1('live')[1]['x-health-owner'] for _ in range(6)}
            assert owners == {'0', '1'}, owners
            tls = int(pg.sql(f"SELECT count(*) FROM pg_stat_ssl s JOIN pg_stat_activity a USING(pid) WHERE a.usename='{role}' AND s.ssl;").strip())
            assert tls == 2, 'pool dependencies were not real TLS connections'
            assert pg.sql(f"SELECT rolsuper OR rolbypassrls FROM pg_roles WHERE rolname='{role}';").strip() == 'f'
            pg.sql(f"ALTER ROLE {role} NOLOGIN; SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE usename='{role}';")
            wait(lambda: all_http1('ready', 503) and backends() == 0, 'revoked/terminated dependency remained ready')
            assert_phase({'live': 200, 'startup': 200, 'ready': 503})
            pg.sql(f'ALTER ROLE {role} LOGIN;')
            wait(lambda: all_http1('ready', 200) and backends() == 2, 'dependency readiness did not recover')
            assert_phase({'live': 200, 'startup': 200, 'ready': 200})
            # Pause only disposable app backends; no admin query runs until they
            # are resumed. This makes both SELECT 1 executions genuinely pending.
            pending_owners = set()
            if os.name != 'nt':
                paused = [int(value) for value in pg.sql(f"SELECT pid FROM pg_stat_activity WHERE usename='{role}';").split()]
                assert len(paused) == 2
                for pid in paused: os.kill(pid, signal.SIGSTOP)
                def pending():
                    status, headers, _ = http1('live')
                    assert status == 200
                    if headers['x-health-probe-pending'] == '1': pending_owners.add(headers['x-health-owner'])
                    return pending_owners == {'0', '1'}
                wait(pending, 'owner probe executions did not become pending', timeout=.7)
                assert all_http1('startup', 200), 'startup regressed while a probe was pending'
            started = time.monotonic()
            process.send_signal(signal.CTRL_BREAK_EVENT if os.name == 'nt' else signal.SIGTERM)
            for pid in paused: os.kill(pid, signal.SIGCONT)
            paused = []
            wait(lambda: backends() == 0, 'pool backends survived graceful drain', timeout=3)
            assert process.poll() is None, 'DB closure was only observed after process exit'
            process.wait(5)
            elapsed = time.monotonic() - started
            assert process.returncode == 0, process.returncode
            assert backends() == 0
            report = {
                'passed': True, 'platform': platform.platform(), 'postgres_tls': 'verify-full localhost with numeric hostaddr',
                'owners': 2, 'connections_per_owner': 1, 'max_probe_executions_per_owner': 1,
                'probe_source': 'onTick SELECT 1; HTTP callbacks read snapshots only',
                'initial_live': 200, 'initial_startup': 503, 'initial_ready': 503,
                'all_three_probes_http1_http2': True, 'head_get_metadata_and_no_body_data': True,
                'revoke_login_and_terminate_backend_ready': 503, 'outage_live': 200, 'outage_startup': 200,
                'restored_login_recovers_ready': 200, 'startup_monotone_after_success': True,
                'pending_probe_owners_cancelled': sorted(pending_owners), 'pools_closed_before_process_exit': True,
                'backend_count_after_exit': 0, 'shutdown_signal': 'CTRL_BREAK_EVENT' if os.name == 'nt' else 'SIGTERM',
                'observed_shutdown_seconds': round(elapsed, 6), 'native_shutdown_budget_ms': 1000,
                'hard_filesystem_deadline_claimed': False, 'persistent_service_started': False,
            }
        finally:
            for pid in paused:
                try: os.kill(pid, signal.SIGCONT)
                except ProcessLookupError: pass
            if peer is not None: peer.close()
            stop()
            if process.returncode:
                log.seek(0)
                print(log.read(), file=sys.stderr)
    sources = [
        'integration/platform/Health.lean', 'integration/platform/Main.lean', 'integration/platform/lakefile.toml',
        'test/platform/health.py', 'LeanWeave/Operations/Health.lean', 'LeanWeave/Core/Server.lean',
        'LeanWeave/Core/AppM.lean', 'adapters/postgres/LeanWeavePostgres/Pool.lean',
        'adapters/postgres/test/cluster_support.py', 'integration/platform/lake-manifest.json', 'lake-manifest.json',
    ]
    report['source_sha256'] = {name: hashlib.sha256((ROOT / name).read_bytes()).hexdigest() for name in sources}
    report['fixture_sha256'] = hashlib.sha256(binary.read_bytes()).hexdigest()
    args.report.parent.mkdir(parents=True, exist_ok=True)
    args.report.write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report))


if __name__ == '__main__': main()
