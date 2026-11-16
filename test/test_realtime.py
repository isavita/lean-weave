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

    def mount(self, cookie=None):
        status, headers, html = self.get('/', headers={'Cookie':cookie} if cookie else None)
        self.assertEqual(status, 200)
        cookie = headers['set-cookie'].split(';', 1)[0]
        csrf = re.search(rb'name="weave-csrf" content="([a-f0-9]+)"', html).group(1).decode()
        view = re.search(rb'name="weave-view" content="([a-f0-9]+)"', html).group(1).decode()
        return cookie, csrf, view

    def websocket(self, cookie, view, expected=101):
        peer = socket.create_connection(('127.0.0.1', self.port), timeout=3)
        self.addCleanup(peer.close)
        peer.sendall(('GET /live/socket?view=' + view + ' HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: Upgrade\r\nUpgrade: websocket\r\nSec-WebSocket-Version: 13\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\nOrigin: ' + self.origin + '\r\nCookie: ' + cookie + '\r\n\r\n').encode())
        headers = b''
        while not headers.endswith(b'\r\n\r\n'): headers += peer.recv(1)
        self.assertIn(str(expected).encode(), headers.split(b'\r\n')[0])
        return peer

    def packet(self, peer):
        for _ in range(50):
            opcode, payload = read_ws(peer)
            opcode &= 0x0f
            if opcode == 9:
                peer.sendall(websocket_frame(10, payload))
            else:
                self.assertEqual(opcode, 1)
                return json.loads(payload)
        self.fail('Live packet read bound exceeded')

    def command(self, peer, csrf, event='inc'):
        peer.sendall(websocket_frame(1, json.dumps({'event':event,'csrf':csrf}).encode()))
        return self.packet(peer)

    def test_live_typed_websocket_diffs_reconnect_and_csrf(self):
        cookie, csrf, view = self.mount()
        peer = self.websocket(cookie, view)
        initial = json.loads(read_ws(peer)[1])
        self.assertEqual(initial['type'], 'snapshot')
        self.assertEqual(len(initial['patches']), 2)
        peer.sendall(websocket_frame(1, json.dumps({'event': 'inc', 'csrf': csrf}).encode()))
        diff = json.loads(read_ws(peer)[1])
        self.assertEqual((diff['type'], diff['version']), ('patch', 1))
        self.assertEqual([patch['key'] for patch in diff['patches']], ['counter'])
        self.assertIn('id="count">1</p>', diff['patches'][0]['html'])
        peer.close()
        reconnect = self.websocket(cookie, view)
        restored = json.loads(read_ws(reconnect)[1])
        self.assertEqual((restored['type'], restored['version']), ('snapshot', 1))
        self.assertIn('id="count">1</p>', restored['patches'][0]['html'])
        status, _, _ = self.get('/live/event?view=' + view, 'POST', json.dumps({'event':'inc', 'csrf':'wrong'}),
            {'Cookie':cookie, 'Origin':self.origin, 'Content-Type':'application/json'})
        self.assertEqual(status, 403)
        status, _, _ = self.get('/live/event?view=' + view, 'POST', json.dumps({'event':'inc', 'csrf':csrf}),
            {'Cookie':cookie, 'Origin':'https://attacker.test', 'Content-Type':'application/json'})
        self.assertEqual(status, 403)

    def test_live_sse_post_fallback_and_http2_websocket(self):
        cookie, csrf, view = self.mount()
        stream = self.stream('/live/events?view=' + view, {'Cookie':cookie})
        initial = self.next_event(stream)
        self.assertEqual(json.loads(initial['data'])['type'], 'snapshot')
        status, _, _ = self.get('/live/event?view=' + view, 'POST', json.dumps({'event':'dec', 'csrf':csrf}),
            {'Cookie':cookie, 'Origin':self.origin, 'Content-Type':'application/json'})
        self.assertEqual(status, 204)
        diff = self.next_event(stream)
        self.assertIn('id="count">-1</p>', json.loads(diff['data'])['patches'][0]['html'])
        peer = H2Client(self.port); self.addCleanup(peer.close)
        peer.request(1, ('/live/socket?view=' + view).encode(), method=b'CONNECT', end_stream=False,
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

    def test_two_views_share_owner_cookie_with_independent_state_and_fallback(self):
        owner, csrf_a, view_a = self.mount()
        first = self.websocket(owner, view_a)
        self.assertEqual(self.packet(first)['version'], 0)
        same_owner, csrf_b, view_b = self.mount(owner)
        self.assertEqual(owner, same_owner, 'new tab must preserve the owner cookie')
        self.assertNotEqual(view_a, view_b)
        self.assertNotEqual(csrf_a, csrf_b)
        second = self.websocket(same_owner, view_b)
        self.assertEqual(self.packet(second)['version'], 0)
        a = self.command(first, csrf_a)
        b = self.command(second, csrf_b)
        self.assertEqual((a['version'], b['version']), (1, 1))
        self.assertIn('id="count">1</p>', a['patches'][0]['html'])
        self.assertIn('id="count">1</p>', b['patches'][0]['html'])
        a = self.command(first, csrf_a, 'dec')
        self.assertEqual(a['version'], 2)
        self.assertIn('id="count">0</p>', a['patches'][0]['html'])
        # A per-view transport replacement must not supersede the other tab.
        fallback = self.stream('/live/events?view=' + view_a, {'Cookie':owner})
        restored = json.loads(self.next_event(fallback)['data'])
        self.assertEqual(restored['version'], 2)
        b = self.command(second, csrf_b)
        self.assertEqual(b['version'], 2)
        self.assertIn('id="count">2</p>', b['patches'][0]['html'])
        status, _, _ = self.get('/live/event?view=' + view_a, 'POST', json.dumps({'event':'inc','csrf':csrf_a}),
            {'Cookie':owner,'Origin':self.origin,'Content-Type':'application/json'})
        self.assertEqual(status, 204)
        patch = json.loads(self.next_event(fallback)['data'])
        self.assertEqual(patch['version'], 3)
        self.assertIn('id="count">1</p>', patch['patches'][0]['html'])

    def test_view_selector_is_not_a_credential_and_logout_revokes_all_owner_views(self):
        owner, csrf, view = self.mount()
        _, second_csrf, second_view = self.mount(owner)
        other_owner, other_csrf, other_view = self.mount()
        self.websocket(other_owner, view, expected=401)
        for selector, cookie, token, expected in [
                ('?view=' + view, other_owner, csrf, 401),
                ('?view=' + view, owner, other_csrf, 403),
                ('', owner, csrf, 401),
                ('?view=' + view + '&view=' + view, owner, csrf, 401)]:
            status, _, _ = self.get('/live/event' + selector, 'POST', json.dumps({'event':'inc','csrf':token}),
                {'Cookie':cookie,'Origin':self.origin,'Content-Type':'application/json'})
            self.assertEqual(status, expected)
        status, _, _ = self.get('/live/events?view=' + view, headers={'Cookie':other_owner})
        self.assertEqual(status, 401)
        status, _, _ = self.get('/live/invalidate', 'POST', headers={'Cookie':owner,'Origin':self.origin})
        self.assertEqual(status, 204)
        for selector, token in [(view,csrf),(second_view,second_csrf)]:
            status, _, _ = self.get('/live/event?view=' + selector, 'POST', json.dumps({'event':'inc','csrf':token}),
                {'Cookie':owner,'Origin':self.origin,'Content-Type':'application/json'})
            self.assertEqual(status, 401)
        self.assertEqual(self.get('/live/event?view=' + other_view, 'POST', json.dumps({'event':'inc','csrf':other_csrf}),
            {'Cookie':other_owner,'Origin':self.origin,'Content-Type':'application/json'})[0], 204)

    def test_per_owner_view_capacity_preserves_admitted_views(self):
        owner, csrf, view = self.mount()
        for _ in range(15):
            same_owner, _, _ = self.mount(owner)
            self.assertEqual(same_owner, owner)
        self.assertEqual(self.get('/', headers={'Cookie':owner})[0], 503)
        self.assertEqual(self.get('/live/event?view=' + view, 'POST', json.dumps({'event':'inc','csrf':csrf}),
            {'Cookie':owner,'Origin':self.origin,'Content-Type':'application/json'})[0], 204)
        self.assertEqual(self.get('/')[0], 200, 'one owner cannot exhaust another owner view allowance')


if __name__ == '__main__': unittest.main()
