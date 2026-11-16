#!/usr/bin/env python3
"""Two independent native LeanWeave nodes over an isolated real Redis server."""
import http.client
import json
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
        self.assertEqual(self.get(self.a, '/publish', value='from-node-a'), 'queued')
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


if __name__ == '__main__': unittest.main()
