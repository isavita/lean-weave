#!/usr/bin/env python3
"""Typed native GraphQL HTTP/H2 and graphql-transport-ws wire acceptance."""
import http.client
import json
import os
from pathlib import Path
import signal
import socket
import subprocess
import sys
import tempfile
import time
import unittest
from urllib.parse import urlencode

ROOT = Path(__file__).resolve().parents[1]
NET = Path(os.environ.get('LEAN_NET_ROOT', ROOT / '.lake/packages/lean-net'))
if not NET.is_dir(): NET = ROOT.parent / 'lean-net-platform'
sys.path.insert(0, str(NET / 'test'))
from http2_test_support import H2Client, frame
from test_websocket import read_ws, websocket_frame
BINARY = ROOT / '.lake/build/bin' / ('weave-graphql-fixture.exe' if sys.platform == 'win32' else 'weave-graphql-fixture')


def free_port():
    with socket.socket() as sock:
        sock.bind(('127.0.0.1', 0)); return sock.getsockname()[1]


class Peer:
    def __init__(self, test):
        self.sock = socket.create_connection(('127.0.0.1', test.port), timeout=3)
        test.addCleanup(self.close)
        self.sock.sendall(('GET /ws HTTP/1.1\r\nHost: localhost\r\nConnection: Upgrade\r\nUpgrade: websocket\r\nSec-WebSocket-Version: 13\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\nSec-WebSocket-Protocol: graphql-transport-ws\r\nOrigin: ' + test.origin + '\r\n\r\n').encode())
        headers = b''
        while not headers.endswith(b'\r\n\r\n'): headers += self.sock.recv(1)
        test.assertIn(b'101', headers.split(b'\r\n')[0])
        test.assertIn(b'sec-websocket-protocol: graphql-transport-ws', headers.lower())

    def close(self): self.sock.close()
    def send(self, value): self.sock.sendall(websocket_frame(1, json.dumps(value).encode()))
    def receive(self):
        while True:
            opcode, payload = read_ws(self.sock)
            if opcode & 15 == 9:
                self.sock.sendall(websocket_frame(10, payload)); continue
            if opcode & 15 == 8: return {'close': int.from_bytes(payload[:2], 'big')}
            assert opcode == 0x81, (opcode, payload)
            return json.loads(payload)
    def init(self, payload=None):
        self.send({'type':'connection_init', 'payload':payload or {}})
        assert self.receive() == {'type':'connection_ack'}
    def subscribe(self, id, query, variables=None):
        self.send({'id':id, 'type':'subscribe', 'payload':{'query':query,'variables':variables or {}}})


class GraphQLWire(unittest.TestCase):
    def setUp(self):
        self.port = free_port(); self.origin = f'http://127.0.0.1:{self.port}'
        self.log = tempfile.TemporaryFile(mode='w+')
        self.process = subprocess.Popen([str(BINARY), str(self.port)], stdout=self.log, stderr=self.log,
            creationflags=subprocess.CREATE_NEW_PROCESS_GROUP if sys.platform == 'win32' else 0)
        self.addCleanup(self.stop)
        for _ in range(200):
            if self.process.poll() is not None:
                self.log.seek(0); self.fail(self.log.read())
            try:
                if self.http('{counter{value}}')[0] == 200: return
            except OSError: time.sleep(.01)
        self.fail('GraphQL startup timeout')

    def stop(self):
        if self.process.poll() is None:
            self.process.send_signal(signal.CTRL_BREAK_EVENT if sys.platform == 'win32' else signal.SIGINT)
            try: self.process.wait(5)
            except subprocess.TimeoutExpired:
                self.process.kill(); self.process.wait(); self.fail('shutdown exceeded shared deadline')
        self.log.seek(0); log = self.log.read(); self.log.close()
        self.assertEqual(self.process.returncode, 0, log)

    def http(self, query, variables=None, method='POST', operation=None):
        body = {'query':query,'variables':variables or {}}
        if operation: body['operationName'] = operation
        connection = http.client.HTTPConnection('127.0.0.1', self.port, timeout=3)
        try:
            if method == 'GET': connection.request('GET', '/graphql?' + urlencode({'query':query}))
            else: connection.request('POST', '/graphql', json.dumps(body), {'Content-Type':'application/json'})
            response = connection.getresponse()
            return response.status, json.loads(response.read())
        finally: connection.close()

    def test_typed_queries_variables_fragments_mutations_validation_and_introspection(self):
        status, result = self.http('query Q{first:counter{...C} echo(text:"ok")} fragment C on Counter{value __typename}', {'n':1})
        self.assertEqual(status, 200); self.assertEqual(result['data']['first'], {'value':0,'__typename':'Counter'})
        self.assertEqual(result['data']['echo'], 'ok')
        status, result = self.http('mutation Add($n:Int!){one:increment(amount:$n){value} two:increment(amount:3){value}}', {'n':2})
        self.assertEqual(result['data'], {'one':{'value':2},'two':{'value':5}})
        status, result = self.http('mutation{increment(amount:1){value} unknown}')
        self.assertEqual(status, 400); self.assertIn('errors', result)
        self.assertEqual(self.http('{counter{value}}')[1]['data']['counter']['value'], 5)
        self.assertEqual(self.http('mutation{increment(amount:1){value}}', method='GET')[0], 405)
        for query in ['{unknown}', '{counter{bad}}', 'mutation{increment(amount:1.0){value}}', 'mutation{increment(amount:2147483648){value}}']:
            self.assertEqual(self.http(query)[0], 400)
        status, result = self.http('{__schema{queryType{name fields{name}}} __type(name:"Counter"){name kind fields{name type{kind name ofType{name}}}}}')
        self.assertEqual(status, 200); self.assertEqual(result['data']['__type']['kind'], 'OBJECT')
        self.assertEqual(result['data']['__schema']['queryType']['name'], 'Query')

    def test_http2_query_and_cooperative_resolver_match_http1(self):
        peer = H2Client(self.port); self.addCleanup(peer.close)
        body = json.dumps({'query':'{slow counter{value}}'}).encode()
        peer.request(1, b'/graphql', method=b'POST', end_stream=False,
            headers=[(b'content-type',b'application/json'),(b'content-length',str(len(body)).encode())])
        peer.send(frame(0, 1, 1, body))
        result = json.loads(peer.response(1))
        self.assertEqual(result['data'], {'slow':'done','counter':{'value':0}})
        self.assertEqual(self.http('{slow counter{value}}')[1], result)

    def test_subscriptions_next_ping_pong_complete_and_cleanup(self):
        peer = Peer(self); peer.init()
        peer.send({'type':'ping','payload':{'probe':1}})
        self.assertEqual(peer.receive(), {'type':'pong','payload':{'probe':1}})
        peer.subscribe('sub', 'subscription{ticks{value}}')
        peer.subscribe('probe', '{counter{value}}')
        self.assertEqual(peer.receive()['type'], 'next'); self.assertEqual(peer.receive()['type'], 'complete')
        self.http('mutation{increment(amount:4){value}}')
        next = peer.receive()
        self.assertEqual((next['type'],next['id'],next['payload']['data']['ticks']['value']), ('next','sub',4))
        peer.send({'id':'sub','type':'complete'})
        peer.subscribe('stats', '{cancelled}')
        self.assertEqual(peer.receive()['payload']['data']['cancelled'], 1)
        self.assertEqual(peer.receive()['type'], 'complete')

    def test_protocol_close_codes_and_operation_errors(self):
        timeout = Peer(self); self.assertEqual(timeout.receive(), {'close':4408})
        unauthorized = Peer(self); unauthorized.subscribe('a', '{counter{value}}')
        self.assertEqual(unauthorized.receive(), {'close':4401})
        denied = Peer(self); denied.send({'type':'connection_init','payload':{'token':'deny'}})
        self.assertEqual(denied.receive(), {'close':4403})
        repeated = Peer(self); repeated.init(); repeated.send({'type':'connection_init'})
        self.assertEqual(repeated.receive(), {'close':4429})
        bad = Peer(self); bad.send({'type':'unknown'}); self.assertEqual(bad.receive(), {'close':4400})
        duplicate = Peer(self); duplicate.init(); duplicate.subscribe('a','subscription{ticks{value}}'); duplicate.subscribe('a','subscription{ticks{value}}')
        self.assertEqual(duplicate.receive(), {'close':4409})
        invalid = Peer(self); invalid.init(); invalid.subscribe('bad','{unknown}')
        result = invalid.receive(); self.assertEqual((result['type'],result['id']), ('error','bad'))
        self.assertIsInstance(result['payload'], list)
        invalid.subscribe('ok','{counter{value}}')
        self.assertEqual(invalid.receive()['type'],'next'); self.assertEqual(invalid.receive()['type'],'complete')

    def test_cancel_awaiting_query_prevents_late_next_and_cleans_once(self):
        peer = Peer(self); peer.init()
        peer.subscribe('slow','{slow}')
        peer.send({'id':'slow','type':'complete'})
        time.sleep(.08)
        peer.subscribe('stats','{cancelled}')
        next = peer.receive()
        self.assertEqual((next['id'],next['payload']['data']['cancelled']), ('stats',1))
        self.assertEqual(peer.receive()['type'],'complete')
        peer.send({'id':'slow','type':'complete'})
        self.assertEqual(self.http('{cancelled}')[1]['data']['cancelled'], 1)

    def test_disconnect_cancels_subscription_once(self):
        peer = Peer(self); peer.init(); peer.subscribe('sub','subscription{ticks{value}}'); peer.subscribe('probe','{counter{value}}')
        peer.receive(); peer.receive(); peer.close()
        until = time.monotonic() + 2
        while time.monotonic() < until:
            if self.http('{cancelled}')[1]['data']['cancelled'] == 1: break
            time.sleep(.01)
        self.assertEqual(self.http('{cancelled}')[1]['data']['cancelled'],1)

    def test_http2_graphql_websocket_subprotocol_and_query(self):
        peer = H2Client(self.port); self.addCleanup(peer.close)
        peer.request(1,b'/ws',method=b'CONNECT',end_stream=False,headers=[
            (b':protocol',b'websocket'),(b'sec-websocket-version',b'13'),
            (b'sec-websocket-protocol',b'graphql-transport-ws'),(b'origin',self.origin.encode())])
        peer.until(lambda: bool(peer.headers[1]))
        self.assertIn((b'sec-websocket-protocol',b'graphql-transport-ws'),peer.headers[1][0].headers)
        offset = 0
        def receive():
            nonlocal offset
            while True:
                peer.until(lambda: len(peer.bodies[1]) >= offset + 2)
                data = peer.bodies[1]; opcode, size = data[offset], data[offset+1]; head = 2
                if size == 126:
                    peer.until(lambda: len(peer.bodies[1]) >= offset+4); size = int.from_bytes(data[offset+2:offset+4],'big'); head=4
                peer.until(lambda: len(peer.bodies[1]) >= offset+head+size)
                payload = bytes(data[offset+head:offset+head+size]); offset += head+size
                if opcode & 15 == 9:
                    peer.send(frame(0,0,1,websocket_frame(10,payload))); continue
                return json.loads(payload)
        peer.send(frame(0,0,1,websocket_frame(1,b'{"type":"connection_init"}')))
        self.assertEqual(receive(), {'type':'connection_ack'})
        peer.send(frame(0,0,1,websocket_frame(1,json.dumps({'type':'subscribe','id':'query','payload':{'query':'{counter{value}}'}}).encode())))
        self.assertEqual(receive()['payload']['data']['counter']['value'],0)
        self.assertEqual(receive()['type'],'complete')


if __name__ == '__main__': unittest.main()
