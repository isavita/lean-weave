#!/usr/bin/env python3
"""Real owner-worker health probes over HTTP/1+HTTP/2 and isolated TLS PostgreSQL.
No service is installed or retained. The reported shutdown duration is observed;
this test makes no hard filesystem latency guarantee.
"""
from pathlib import Path
from collections import deque
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


def wait(condition, message, timeout=6, diagnostics=None):
    cutoff = time.monotonic() + timeout
    last_error = None
    while time.monotonic() < cutoff:
        try:
            if condition(): return
        except (OSError, http.client.HTTPException) as error:
            last_error = repr(error)
        time.sleep(.01)
    detail = {'last_transport_error': last_error}
    if diagnostics is not None:
        try: detail.update(diagnostics())
        except Exception as error: detail['diagnostics_error'] = repr(error)
    raise AssertionError(message + ': ' + json.dumps(detail, sort_keys=True))


class OwnerProbes:
    """Keep a verified HTTP/1 connection to each actual reactor owner.

    Linux SO_REUSEPORT hashes new TCP connections; a fixed sample size cannot
    establish coverage. Discovery is bounded, then every owner assertion uses
    its retained connection and checks the owner header again on each reply.
    """
    def __init__(self, port, live_process=lambda: None, connection_factory=None):
        self.port = port
        self.live_process = live_process
        self.connection_factory = connection_factory or (lambda: http.client.HTTPConnection('127.0.0.1', port, timeout=2))
        self.owners = {}
        self.history = deque(maxlen=128)
        self.sequence = 0
        self.discovery_requests = 0

    def _request(self, connection, probe, method, expected_owner=None):
        self.live_process()
        connection.request(method, '/health/' + probe)
        response = connection.getresponse()
        headers = {key.lower(): value for key, value in response.getheaders()}
        body = response.read()
        self.sequence += 1
        observation = {'sequence': self.sequence, 'probe': probe, 'method': method,
            'expected_owner': expected_owner, 'status': response.status, 'headers': headers,
            'body': body.decode('utf-8', errors='replace')}
        self.history.append(observation)
        if expected_owner is not None:
            assert headers.get('x-health-owner') == expected_owner, ('retained connection changed owner', observation)
        return response.status, headers, body

    def request(self, probe, method='GET', owner=None):
        connection = self.owners[owner] if owner is not None else self.connection_factory()
        try: return self._request(connection, probe, method, owner)
        finally:
            if owner is None: connection.close()

    def discover(self, expected=('0', '1'), timeout=6, max_connections=64):
        cutoff = time.monotonic() + timeout
        for _ in range(max_connections):
            if time.monotonic() >= cutoff: break
            connection = self.connection_factory()
            retained = False
            try:
                self.discovery_requests += 1
                status, headers, _ = self._request(connection, 'live', 'GET')
                owner = headers.get('x-health-owner')
                assert status == 200 and owner in expected, ('owner discovery reply', list(self.history))
                if owner not in self.owners:
                    self.owners[owner] = connection
                    retained = True
                if set(self.owners) == set(expected): return
            finally:
                if not retained: connection.close()
        raise AssertionError(('bounded owner discovery failed', sorted(self.owners), list(self.history)))

    def all_statuses(self, probe, expected):
        assert set(self.owners) == {'0', '1'}, 'discover both owners before asserting health'
        samples = [self.request(probe, owner=owner) for owner in ('0', '1')]
        return all(status == expected for status, _, _ in samples)

    def close(self):
        for connection in self.owners.values(): connection.close()
        self.owners.clear()


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
        probes = OwnerProbes(port, live_process)
        http1 = probes.request
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
        def backends(authenticated=False):
            # Failed authentication can briefly expose a role with NULL state.
            # A non-NULL state requires an authenticated backend. Shutdown still
            # requires every backend, including authentication attempts, to exit.
            state = ' AND state IS NOT NULL' if authenticated else ''
            return int(pg.sql(f"SELECT count(*) FROM pg_stat_activity WHERE usename='{role}'{state};").strip())
        def diagnostics():
            result = {'process_returncode': process.poll(), 'http1_observations': list(probes.history)}
            if paused:
                result['postgres'] = 'backend processes paused; SQL deferred until resume'
            else:
                result['postgres'] = json.loads(pg.sql(f"""
                    SELECT json_build_object(
                      'can_login', (SELECT rolcanlogin FROM pg_roles WHERE rolname='{role}'),
                      'backends', (SELECT COALESCE(json_agg(json_build_object(
                        'pid', pid, 'state', state, 'wait_event_type', wait_event_type,
                        'wait_event', wait_event, 'query', query)), '[]'::json)
                        FROM pg_stat_activity WHERE usename='{role}'));
                    """))
            log.flush()
            if hasattr(os, 'pread'):
                size = os.fstat(log.fileno()).st_size
                result['native_log_tail'] = os.pread(log.fileno(), min(size, 8192), max(0, size - 8192)).decode('utf-8', errors='replace')
            return result
        def wait_phase(condition, message, timeout=6):
            wait(condition, message, timeout, diagnostics)
        def assert_new_tls_login_denied():
            denied = subprocess.run([str(pg.binary_dir / 'psql'), '-X', '-w', '-h', 'localhost', '-p', str(pg.port),
                '-U', role, '-d', 'postgres', '-v', 'ON_ERROR_STOP=1', '-At', '-c', 'SELECT 1'],
                env=dict(os.environ, PGHOSTADDR='127.0.0.1', PGSSLMODE='verify-full',
                    PGSSLROOTCERT=str(pg.directory / 'data/server.crt'), PGCONNECT_TIMEOUT='2',
                    PGAPPNAME='weave-health-revocation-check'), capture_output=True, text=True, timeout=3)
            assert denied.returncode != 0 and 'not permitted to log in' in denied.stderr, (
                'fresh TLS login was not rejected after committed NOLOGIN', denied.returncode, denied.stdout, denied.stderr)
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
            return probes.all_statuses(probe, expected)
        def stop():
            if process.poll() is None:
                process.send_signal(signal.CTRL_BREAK_EVENT if os.name == 'nt' else signal.SIGTERM)
            try: process.wait(5)
            except subprocess.TimeoutExpired:
                process.kill(); process.wait(); raise AssertionError('native health shutdown exceeded external test deadline')
        try:
            wait_phase(lambda: http1('live')[0] == 200, 'native health listener startup deadline')
            probes.discover()
            peer = H2Client(port)
            assert_phase({'live': 200, 'startup': 503, 'ready': 503})
            assert backends(authenticated=True) == 0
            pg.sql(f'ALTER ROLE {role} LOGIN;')
            wait_phase(lambda: all_http1('ready', 200) and backends(authenticated=True) == 2, 'both owner TLS probes failed to initialize')
            assert_phase({'live': 200, 'startup': 200, 'ready': 200})
            assert {http1('live', owner=owner)[1]['x-health-owner'] for owner in ('0', '1')} == {'0', '1'}
            tls = int(pg.sql(f"SELECT count(*) FROM pg_stat_ssl s JOIN pg_stat_activity a USING(pid) WHERE a.usename='{role}' AND s.ssl;").strip())
            assert tls == 2, 'pool dependencies were not real TLS connections'
            assert pg.sql(f"SELECT rolsuper OR rolbypassrls FROM pg_roles WHERE rolname='{role}';").strip() == 'f'
            # One psql -c containing both statements runs in one transaction:
            # termination could let a replacement authenticate before NOLOGIN
            # commits. Establish the committed denial before touching the pools.
            pg.sql(f'ALTER ROLE {role} NOLOGIN;')
            assert pg.sql(f"SELECT rolcanlogin FROM pg_roles WHERE rolname='{role}';").strip() == 'f'
            assert_new_tls_login_denied()
            terminated = pg.sql(f"SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE usename='{role}' AND state IS NOT NULL;").split()
            assert terminated == ['t', 't'], ('expected both authenticated owner backends', terminated, diagnostics())
            wait_phase(lambda: all_http1('ready', 503) and backends(authenticated=True) == 0, 'revoked/terminated dependency remained ready')
            assert_phase({'live': 200, 'startup': 200, 'ready': 503})
            pg.sql(f'ALTER ROLE {role} LOGIN;')
            wait_phase(lambda: all_http1('ready', 200) and backends(authenticated=True) == 2, 'dependency readiness did not recover')
            assert_phase({'live': 200, 'startup': 200, 'ready': 200})
            # Pause only disposable app backends; no admin query runs until they
            # are resumed. This makes both SELECT 1 executions genuinely pending.
            pending_owners = set()
            if os.name != 'nt':
                paused = [int(value) for value in pg.sql(f"SELECT pid FROM pg_stat_activity WHERE usename='{role}';").split()]
                assert len(paused) == 2
                for pid in paused: os.kill(pid, signal.SIGSTOP)
                def pending():
                    pending_now = set()
                    for owner in ('0', '1'):
                        status, headers, _ = http1('live', owner=owner)
                        assert status == 200, diagnostics()
                        if headers['x-health-probe-pending'] == '1': pending_now.add(owner)
                    pending_owners.update(pending_now)
                    return pending_now == {'0', '1'}
                wait_phase(pending, 'owner probe executions did not become pending', timeout=.7)
                assert all_http1('startup', 200), ('startup regressed while a probe was pending', diagnostics())
                assert_phase({'live': 200, 'startup': 200})
            started = time.monotonic()
            process.send_signal(signal.CTRL_BREAK_EVENT if os.name == 'nt' else signal.SIGTERM)
            for pid in paused: os.kill(pid, signal.SIGCONT)
            paused = []
            wait_phase(lambda: backends() == 0, 'pool backends survived graceful drain', timeout=3)
            assert process.poll() is None, 'DB closure was only observed after process exit'
            process.wait(5)
            elapsed = time.monotonic() - started
            assert process.returncode == 0, process.returncode
            assert backends() == 0
            report = {
                'passed': True, 'platform': platform.platform(), 'postgres_tls': 'verify-full localhost with numeric hostaddr',
                'owners': 2, 'connections_per_owner': 1, 'max_probe_executions_per_owner': 1,
                'http1_assertions_use_verified_owner_connections': True,
                'owner_connection_discovery_requests': probes.discovery_requests,
                'probe_source': 'onTick SELECT 1; HTTP callbacks read snapshots only',
                'initial_live': 200, 'initial_startup': 503, 'initial_ready': 503,
                'all_three_probes_http1_http2': True, 'head_get_metadata_and_no_body_data': True,
                'revoke_login_and_terminate_backend_ready': 503, 'outage_live': 200, 'outage_startup': 200,
                'login_revocation_committed_before_termination': True, 'fresh_tls_login_denied_before_termination': True,
                'authenticated_backends_during_outage': 0,
                'restored_login_recovers_ready': 200, 'startup_monotone_after_success': True,
                'pending_probe_owners_cancelled': sorted(pending_owners), 'pools_closed_before_process_exit': True,
                'backend_count_after_exit': 0, 'shutdown_signal': 'CTRL_BREAK_EVENT' if os.name == 'nt' else 'SIGTERM',
                'observed_shutdown_seconds': round(elapsed, 6), 'native_shutdown_budget_ms': 1000,
                'hard_filesystem_deadline_claimed': False, 'persistent_service_started': False,
            }
        except Exception:
            try: print('health failure observations: ' + json.dumps(diagnostics(), sort_keys=True), file=sys.stderr)
            except Exception as error: print('health diagnostics failed: ' + repr(error), file=sys.stderr)
            raise
        finally:
            for pid in paused:
                try: os.kill(pid, signal.SIGCONT)
                except ProcessLookupError: pass
            probes.close()
            if peer is not None: peer.close()
            stop()
            if process.returncode:
                log.seek(0)
                print(log.read(), file=sys.stderr)
    sources = [
        'integration/platform/Health.lean', 'integration/platform/Main.lean', 'integration/platform/lakefile.toml',
        'test/platform/health.py', 'test/platform/test_health_probes.py',
        'LeanWeave/Operations/Health.lean', 'LeanWeave/Core/Server.lean',
        'LeanWeave/Core/AppM.lean', 'adapters/postgres/LeanWeavePostgres/Pool.lean',
        'adapters/postgres/test/cluster_support.py', 'integration/platform/lake-manifest.json', 'lake-manifest.json',
    ]
    report['source_sha256'] = {name: hashlib.sha256((ROOT / name).read_bytes()).hexdigest() for name in sources}
    report['fixture_sha256'] = hashlib.sha256(binary.read_bytes()).hexdigest()
    args.report.parent.mkdir(parents=True, exist_ok=True)
    args.report.write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report))


if __name__ == '__main__': main()
