#!/usr/bin/env python3
"""Two independent native LeanWeave nodes over an isolated real Redis server."""
import http.client
import json
import hashlib
import os
import platform
from pathlib import Path
import shutil
import signal
import socket
import subprocess
import sys
import tempfile
import time
import unittest
from urllib.parse import urlencode

ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT.parents[1]
NET = Path(os.environ.get('LEAN_NET_ROOT', SOURCE / '.lake/packages/lean-net'))
sys.path.insert(0, str(NET / 'test'))
from test_websocket import read_ws, websocket_frame
BINARY = ROOT / '.lake/build/bin' / ('weave-redis-pubsub-fixture.exe' if sys.platform == 'win32' else 'weave-redis-pubsub-fixture')


def free_port():
    with socket.socket() as sock:
        sock.bind(('127.0.0.1', 0))
        return sock.getsockname()[1]


class DistributedRealtime(unittest.TestCase):
    def setUp(self):
        redis = shutil.which('redis-server')
        self.assertIsNotNone(redis, 'install redis-server for real distributed acceptance')
        self.temp = tempfile.TemporaryDirectory(prefix='leanweave-pubsub-')
        self.addCleanup(self.temp.cleanup)
        self.logs = []
        self.processes = []
        self.addCleanup(self.stop)
        redis_port = free_port()
        self.start([redis, '--bind', '127.0.0.1', '--port', str(redis_port), '--save', '', '--appendonly', 'no', '--dir', self.temp.name])
        self.wait(lambda: self.port_open(redis_port))
        self.a, self.b = free_port(), free_port()
        self.start([str(BINARY), str(self.a), str(redis_port), 'node-a'])
        self.start([str(BINARY), str(self.b), str(redis_port), 'node-b'])
        self.wait(lambda: self.get(self.a, '/ready') == 'ready' and self.get(self.b, '/ready') == 'ready')

    def start(self, args):
        log = tempfile.TemporaryFile(mode='w+')
        self.logs.append(log)
        self.processes.append(subprocess.Popen(args, stdout=log, stderr=log))

    def stop(self):
        for process in reversed(self.processes):
            if process.poll() is None:
                process.send_signal(signal.SIGINT)
                try: process.wait(5)
                except subprocess.TimeoutExpired:
                    process.kill(); process.wait(); self.fail('bounded shutdown failed')
            self.assertEqual(process.returncode, 0)
        for log in self.logs: log.close()

    def port_open(self, port):
        try:
            socket.create_connection(('127.0.0.1', port), timeout=.1).close()
            return True
        except OSError: return False

    def wait(self, predicate, timeout=4):
        until = time.monotonic() + timeout
        while time.monotonic() < until:
            try:
                if predicate(): return
            except (OSError, http.client.HTTPException): pass
            for process, log in zip(self.processes, self.logs):
                if process.poll() is not None:
                    log.seek(0); self.fail(log.read())
            time.sleep(.01)
        self.fail('distributed condition timed out')

    def get(self, port, path, **query):
        connection = http.client.HTTPConnection('127.0.0.1', port, timeout=2)
        try:
            connection.request('GET', path + ('?' + urlencode(query) if query else ''))
            response = connection.getresponse()
            self.assertEqual(response.status, 200)
            return response.read().decode()
        finally: connection.close()

    def members(self, port):
        return sorted(json.loads(self.get(port, '/members')))

    def test_two_node_broadcast_uses_real_redis(self):
        connection = http.client.HTTPConnection('127.0.0.1', self.b, timeout=3)
        self.addCleanup(connection.close)
        connection.request('GET', '/events')
        response = connection.getresponse()
        self.assertEqual(response.status, 200)
        receipt = json.loads(self.get(self.a, '/broadcast', value='from-node-a'))
        self.assertEqual(receipt, {'queued': True, 'recipients': None})
        for _ in range(30):
            line = response.readline().decode()
            if line.startswith('data: '):
                message = json.loads(line[6:])
                self.assertEqual(message, {'topic': 'room', 'event': 'message', 'payload': 'from-node-a'})
                break
        else: self.fail('cross-node broadcast never arrived')
        stats = json.loads(self.get(self.b, '/stats'))
        self.assertGreater(stats['received'], 0)

    def test_presence_partition_rejoin_and_leave_do_not_resurrect(self):
        self.get(self.a, '/track', key='alice')
        self.get(self.b, '/track', key='bob')
        self.wait(lambda: self.members(self.a) == ['alice', 'bob'] and self.members(self.b) == ['alice', 'bob'])
        self.get(self.b, '/partition')
        self.wait(lambda: self.members(self.a) == ['alice'])
        self.get(self.a, '/untrack', key='alice')
        self.wait(lambda: self.get(self.b, '/ready') == 'ready')
        self.wait(lambda: self.members(self.a) == ['bob'] and self.members(self.b) == ['bob'])
        time.sleep(.15)
        self.assertEqual(self.members(self.a), ['bob'])
        self.assertEqual(self.members(self.b), ['bob'])
        self.assertGreaterEqual(json.loads(self.get(self.b, '/stats'))['reconnects'], 2)

    def test_two_node_channel_websocket_broadcast(self):
        with socket.create_connection(('127.0.0.1', self.b), timeout=3) as peer:
            peer.sendall((f'GET /socket HTTP/1.1\r\nHost: 127.0.0.1:{self.b}\r\nConnection: Upgrade\r\n'
                          'Upgrade: websocket\r\nSec-WebSocket-Version: 13\r\n'
                          'Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n'
                          f'Origin: http://127.0.0.1:{self.b}\r\n\r\n').encode())
            headers = b''
            while not headers.endswith(b'\r\n\r\n'):
                chunk = peer.recv(1)
                self.assertTrue(chunk, 'WebSocket handshake closed without headers')
                headers += chunk
                self.assertLess(len(headers), 16384)
            self.assertIn(b'101', headers.split(b'\r\n')[0])
            receipt = json.loads(self.get(self.a, '/broadcast', value='over-websocket'))
            self.assertEqual(receipt, {'queued': True, 'recipients': None})
            for _ in range(20):
                opcode, data = read_ws(peer)
                opcode &= 0x0f
                if opcode == 9:
                    peer.sendall(websocket_frame(10, data)); continue
                self.assertEqual(opcode, 1)
                self.assertEqual(json.loads(data), {'topic':'room','event':'message','payload':'over-websocket'})
                peer.sendall(websocket_frame(8, (1000).to_bytes(2, 'big')))
                break
            else: self.fail('remote WebSocket publication never arrived')


if __name__ == '__main__':
    result = unittest.main(exit=False).result
    if not result.wasSuccessful(): sys.exit(1)
    source = ROOT.parents[1]
    files = [source / 'LeanWeave/Realtime/Channel.lean', source / 'LeanWeave/Realtime/PubSub.lean',
             source / 'LeanWeave/Presence.lean', ROOT / 'LeanWeaveRedis/PubSub.lean',
             ROOT / 'test/PubSubFixture.lean', Path(__file__).resolve()]
    (ROOT / 'test/pubsub-presence-result.json').write_text(json.dumps({
        'passed': True, 'platform': platform.platform(), 'native_processes': 2,
        'tests': result.testsRun, 'actual_channel_broadcast_via_redis': True,
        'receipt_is_queue_admission_not_delivery_ack': True,
        'sse_remote_delivery_observed': True, 'presence_partition_rejoin_leave': True,
        'websocket_remote_delivery_observed': True,
        'persistent_service_started': False,
        'sources_sha256': {str(p.relative_to(source)): hashlib.sha256(p.read_bytes()).hexdigest() for p in files}
    }, indent=2) + '\n')
