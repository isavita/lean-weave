#!/usr/bin/env python3
"""Native SSE replay/keepalive, multipart and typed Live diffs over HTTP1/H2."""
import http.client
import json
import os
from pathlib import Path
import re
import signal
import socket
import subprocess
import sys
import tempfile
import time
import unittest

ROOT = Path(__file__).resolve().parents[1]
NET = Path(os.environ.get('LEAN_NET_ROOT', ROOT.parent / 'lean-net-platform'))
if not NET.is_dir(): NET = ROOT / '.lake/packages/lean-net'
sys.path.insert(0, str(NET / 'test'))
from http2_test_support import H2Client, frame
from test_websocket import read_ws, websocket_frame

BINARY = ROOT / '.lake/build/bin' / ('weave-realtime-fixture.exe' if sys.platform == 'win32' else 'weave-realtime-fixture')


def free_port():
    with socket.socket() as sock:
        sock.bind(('127.0.0.1', 0)); return sock.getsockname()[1]


class RealtimeWire(unittest.TestCase):
    def setUp(self):
        self.port = free_port()
        self.origin = f'http://127.0.0.1:{self.port}'
        self.log = tempfile.TemporaryFile(mode='w+')
        self.process = subprocess.Popen([str(BINARY), str(self.port)], stdout=self.log, stderr=self.log,
            creationflags=subprocess.CREATE_NEW_PROCESS_GROUP if sys.platform == 'win32' else 0)
        self.addCleanup(self.stop)
        for _ in range(200):
            if self.process.poll() is not None:
                self.log.seek(0); self.fail(self.log.read())
            try:
                self.get('/'); return
            except OSError: time.sleep(.01)
        self.fail('realtime startup timeout')

    def stop(self):
        if self.process.poll() is None:
            self.process.send_signal(signal.CTRL_BREAK_EVENT if sys.platform == 'win32' else signal.SIGINT)
            try: self.process.wait(5)
            except subprocess.TimeoutExpired:
                self.process.kill(); self.process.wait(); self.fail('realtime shutdown deadline')
        self.log.seek(0); log = self.log.read(); self.log.close()
        self.assertEqual(self.process.returncode, 0, log)

    def get(self, path, method='GET', body=None, headers=None):
        connection = http.client.HTTPConnection('127.0.0.1', self.port, timeout=3)
        try:
            connection.request(method, path, body, headers or {})
            response = connection.getresponse()
            return response.status, dict(response.getheaders()), response.read()
        finally: connection.close()

    def stream(self, path, headers=None):
        connection = http.client.HTTPConnection('127.0.0.1', self.port, timeout=3)
        self.addCleanup(connection.close)
        connection.request('GET', path, headers=headers or {})
        response = connection.getresponse()
        self.assertEqual(response.status, 200)
        return response

    def next_event(self, response):
        fields = {}
        for _ in range(100):
            line = response.readline().decode().rstrip('\r\n')
            if not line:
                if fields: return fields
                continue
            if line.startswith(':'): return {'comment': line}
            key, value = line.split(':', 1)
            fields[key] = value.lstrip()
        self.fail('SSE event bound exceeded')

    def test_sse_reconnect_last_event_id_replay_gap_and_keepalive(self):
        ids = [self.get('/publish?data=' + value)[2].decode() for value in ['one', 'two', 'three']]
        stream = self.stream('/events', {'Last-Event-ID': ids[0]})
        self.assertEqual(self.next_event(stream), {'retry': '50'})
        self.assertEqual(self.next_event(stream), {'id': ids[1], 'data': 'two'})
        self.assertEqual(self.next_event(stream), {'id': ids[2], 'data': 'three'})
        self.assertIn('keep-alive', self.next_event(stream)['comment'])
        self.get('/publish?data=four')
        gap = self.stream('/events', {'Last-Event-ID': ids[0]})
        self.next_event(gap)
        self.assertEqual(self.next_event(gap), {'event': 'reset', 'data': 'replay-gap'})
        self.assertEqual(self.next_event(gap)['data'], 'two')

    def test_sse_and_multipart_http2_parity(self):
        first = self.get('/publish?data=one')[2]
        self.get('/publish?data=two')
        peer = H2Client(self.port); self.addCleanup(peer.close)
        peer.request(1, b'/events', headers=[(b'last-event-id', first)])
        peer.until(lambda: b'data: two\n\n' in peer.bodies[1])
        self.assertNotIn(b'data: one', peer.bodies[1])
        peer.until(lambda: b': keep-alive' in peer.bodies[1])
        self.assertNotIn(1, peer.ended)
        peer.request(3, b'/multipart')
        expected = b'--weave\r\n\r\none\r\n--weave\r\n\r\ntwo\r\n--weave--\r\n'
        self.assertEqual(peer.response(3), expected)
        self.assertEqual(self.get('/multipart')[2], expected)
        peer.send(frame(3, stream=1, payload=(8).to_bytes(4, 'big')))

    def mount(self):
        status, headers, html = self.get('/')
        self.assertEqual(status, 200)
        cookie = headers['set-cookie'].split(';', 1)[0]
        csrf = re.search(rb'name="weave-csrf" content="([a-f0-9]+)"', html).group(1).decode()
        return cookie, csrf

    def websocket(self, cookie):
        peer = socket.create_connection(('127.0.0.1', self.port), timeout=3)
        self.addCleanup(peer.close)
        peer.sendall(('GET /live/socket HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: Upgrade\r\nUpgrade: websocket\r\nSec-WebSocket-Version: 13\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\nOrigin: ' + self.origin + '\r\nCookie: ' + cookie + '\r\n\r\n').encode())
        headers = b''
        while not headers.endswith(b'\r\n\r\n'): headers += peer.recv(1)
        self.assertIn(b'101', headers.split(b'\r\n')[0])
        return peer

    def test_live_typed_websocket_diffs_reconnect_and_csrf(self):
        cookie, csrf = self.mount()
        peer = self.websocket(cookie)
        initial = json.loads(read_ws(peer)[1])
        self.assertEqual(initial['type'], 'snapshot')
        self.assertEqual(len(initial['patches']), 2)
        peer.sendall(websocket_frame(1, json.dumps({'event': 'inc', 'csrf': csrf}).encode()))
        diff = json.loads(read_ws(peer)[1])
        self.assertEqual((diff['type'], diff['version']), ('patch', 1))
        self.assertEqual([patch['key'] for patch in diff['patches']], ['counter'])
        self.assertIn('id="count">1</p>', diff['patches'][0]['html'])
        peer.close()
        reconnect = self.websocket(cookie)
        restored = json.loads(read_ws(reconnect)[1])
        self.assertEqual((restored['type'], restored['version']), ('snapshot', 1))
        self.assertIn('id="count">1</p>', restored['patches'][0]['html'])
        status, _, _ = self.get('/live/event', 'POST', json.dumps({'event':'inc', 'csrf':'wrong'}),
            {'Cookie':cookie, 'Origin':self.origin, 'Content-Type':'application/json'})
        self.assertEqual(status, 403)
        status, _, _ = self.get('/live/event', 'POST', json.dumps({'event':'inc', 'csrf':csrf}),
            {'Cookie':cookie, 'Origin':'https://attacker.test', 'Content-Type':'application/json'})
        self.assertEqual(status, 403)

    def test_live_sse_post_fallback_and_http2_websocket(self):
        cookie, csrf = self.mount()
        stream = self.stream('/live/events', {'Cookie':cookie})
        initial = self.next_event(stream)
        self.assertEqual(json.loads(initial['data'])['type'], 'snapshot')
        status, _, _ = self.get('/live/event', 'POST', json.dumps({'event':'dec', 'csrf':csrf}),
            {'Cookie':cookie, 'Origin':self.origin, 'Content-Type':'application/json'})
        self.assertEqual(status, 204)
        diff = self.next_event(stream)
        self.assertIn('id="count">-1</p>', json.loads(diff['data'])['patches'][0]['html'])
        peer = H2Client(self.port); self.addCleanup(peer.close)
        peer.request(1, b'/live/socket', method=b'CONNECT', end_stream=False,
            headers=[(b':protocol',b'websocket'),(b'sec-websocket-version',b'13'),
                     (b'origin',self.origin.encode()),(b'cookie',cookie.encode())])
        peer.until(lambda: len(peer.bodies[1]) >= 4)
        payload = peer.bodies[1]
        self.assertEqual(payload[0], 0x81)
        start = 2 if payload[1] < 126 else 4
        size = payload[1] if payload[1] < 126 else int.from_bytes(payload[2:4], 'big')
        peer.until(lambda: len(peer.bodies[1]) >= start + size)
        snapshot = json.loads(peer.bodies[1][start:start+size])
        self.assertEqual(snapshot['version'], 1)


if __name__ == '__main__': unittest.main()
