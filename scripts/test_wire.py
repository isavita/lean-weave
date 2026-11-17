#!/usr/bin/env python3
"""Live HTTP/1+HTTP/2 framework acceptance using lean-net's independent wire client."""
import json
import os
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
import signal
import socket
import struct
import subprocess
import sys
import tempfile
import threading
import time
import unittest

ROOT = Path(__file__).resolve().parents[1]
NET = Path(os.environ.get("LEAN_NET_PATH", ROOT / ".lake/packages/lean-net"))
if not (NET / "test/http2_test_support.py").exists():
    NET = ROOT.parent / "lean-net"
sys.path.insert(0, str(NET / "test"))
from http2_test_support import H2Client, frame
from test_echo import unused_port
from test_http1_reactor import Reader

BINARY = ROOT / (".lake/build/bin/weave-wire-fixture" + (".exe" if os.name == "nt" else ""))

class FrameworkWire(unittest.TestCase):
    def setUp(self):
        self.port = unused_port()
        self.log = tempfile.TemporaryFile(mode="w+")
        self.server = subprocess.Popen([str(BINARY), str(self.port)], stdout=self.log, stderr=self.log,
            creationflags=subprocess.CREATE_NEW_PROCESS_GROUP if os.name == "nt" else 0)
        self.addCleanup(self.log.close)
        self.addCleanup(self.stop)
        for _ in range(200):
            if self.server.poll() is not None:
                self.log.seek(0)
                self.fail(self.log.read())
            try:
                if self.h1(b"/fast")[0] == 200:
                    return
            except OSError:
                time.sleep(.01)
        self.fail("startup timeout")

    def stop(self):
        if self.server.poll() is None:
            self.server.send_signal(signal.CTRL_BREAK_EVENT if os.name == "nt" else signal.SIGINT)
            try:
                self.server.wait(timeout=5)
            except subprocess.TimeoutExpired:
                self.server.kill(); self.server.wait()
                self.fail("shutdown timeout")
        self.assertEqual(self.server.returncode, 0)

    def h1(self, path, method=b"GET", headers=(), body=b""):
        with socket.create_connection(("127.0.0.1", self.port), timeout=3) as peer:
            peer.sendall(method + b" " + path + b" HTTP/1.1\r\nHost: localhost\r\n" +
                b"".join(k+b": "+v+b"\r\n" for k,v in headers) +
                b"Content-Length: " + str(len(body)).encode() + b"\r\n\r\n" + body)
            response = Reader(peer).response(head=method == b"HEAD")
            return response.status, response.headers, response.body

    def h2(self, path, method=b"GET", headers=(), body=b""):
        client = H2Client(self.port)
        try:
            client.request(1, path, method=method, headers=headers, end_stream=not body)
            if body:
                client.send(frame(0, flags=1, stream=1, payload=body))
            client.until(lambda: 1 in client.ended or 1 in client.resets or bool(client.goaways))
            self.assertNotIn(1, client.resets)
            self.assertFalse(client.goaways)
            result = bytes(client.bodies[1])
            response = dict(client.headers[1][0].headers)
            return int(response.pop(b":status")), response, result
        finally:
            client.close()

    def test_routing_xss_csrf_and_errors_have_protocol_parity(self):
        for path, method, headers, body, expected in [
            (b"/hello/%3Cscript%3E", b"GET", [], b"", 200),
            (b"/hello/%FF", b"GET", [], b"", 400),
            (b"/missing", b"GET", [], b"", 404),
            (b"/fast", b"POST", [(b"origin", b"http://localhost")], b"", 405),
            (b"/fast", b"OPTIONS", [], b"", 204),
            (b"/protected", b"POST", [(b"origin", b"https://evil.test")], b"", 403),
            (b"/protected", b"POST", [(b"origin", b"http://localhost"), (b"content-type", b"application/x-www-form-urlencoded")], b"_csrf=forged", 403),
            (b"/protected", b"POST", [(b"origin", b"http://localhost"), (b"x-csrf-token", b"a"*64)], b"", 200),
        ]:
            with self.subTest(path=path, method=method):
                a = self.h1(path, method, headers, body)
                b = self.h2(path, method, headers, body)
                self.assertEqual((a[0], a[2]), (expected, b[2]))
                self.assertEqual(b[0], expected)
                if expected == 200 and path.startswith(b"/hello"):
                    self.assertIn(b"&lt;script&gt;", a[2]); self.assertNotIn(b"<script>", a[2])
                for name in (b"allow", b"content-type", b"x-content-type-options"):
                    self.assertEqual(a[1].get(name), b[1].get(name))

    def test_public_cache_isolation_head_and_conditional(self):
        for request in (self.h1, self.h2):
            a = request(b"/cache?q=a")
            b = request(b"/cache?q=a")
            self.assertEqual(a[2], b[2])
            self.assertNotEqual(a[2], request(b"/cache?q=b")[2])
            head = request(b"/cache?q=a", b"HEAD")
            self.assertEqual(head[0], 200); self.assertEqual(head[2], b"")
            self.assertEqual(int(head[1][b"content-length"]), len(a[2]))
            conditional = request(b"/cache?q=a", headers=[(b"if-none-match", a[1][b"etag"])])
            self.assertEqual((conditional[0], conditional[2]), (304, b""))
            private = request(b"/cache?q=a", headers=[(b"cookie", b"session=private")])
            self.assertNotEqual(a[2], private[2])

    def test_dynamic_vary_and_malformed_vary_on_both_protocols(self):
        for label, request in ((b"h1", self.h1), (b"h2", self.h2)):
            path = b"/cache-vary?protocol=" + label
            desktop = request(path, headers=[(b"x-device", b"desktop")])
            mobile = request(path, headers=[(b"x-device", b"mobile")])
            desktop_hit = request(path, headers=[(b"x-device", b"desktop")])
            self.assertEqual((desktop[0], mobile[0], desktop_hit[0]), (200, 200, 200))
            self.assertEqual(desktop[1].get(b"vary"), b"X-Device")
            self.assertEqual(desktop_hit[1].get(b"vary"), b"X-Device")
            self.assertEqual(desktop[2], desktop_hit[2])
            self.assertNotEqual(desktop[2], mobile[2])
            self.assertIn(b"device=desktop", desktop[2])
            self.assertIn(b"device=mobile", mobile[2])
            absent = request(path)
            empty = request(path, headers=[(b"x-device", b"")])
            self.assertNotEqual(absent[2], empty[2])
            self.assertEqual(absent[2], request(path)[2])
            self.assertEqual(empty[2], request(path, headers=[(b"x-device", b"")])[2])
            # Authorization and Cookie bypass both variants, even when Vary does not name them.
            for name, value in ((b"authorization", b"Bearer example"), (b"cookie", b"sid=example")):
                bypass = request(path, headers=[(b"x-device", b"desktop"), (name, value)])
                self.assertNotEqual(bypass[2], desktop[2])
            bad = b"/cache-malformed?protocol=" + label
            first = request(bad)
            second = request(bad)
            self.assertEqual(first[1].get(b"vary"), b"X-Device,")
            self.assertNotEqual(first[2], second[2])

    def test_ten_misses_coalesce_on_h1_and_h2(self):
        gate = threading.Barrier(10)
        def h1_miss():
            gate.wait(timeout=3)
            return self.h1(b"/cache-coalesce?protocol=h1")
        with ThreadPoolExecutor(max_workers=10) as pool:
            h1_results = list(pool.map(lambda _: h1_miss(), range(10)))
        self.assertEqual({result[0] for result in h1_results}, {200})
        self.assertEqual({result[2] for result in h1_results}, {b"calls=1"})
        client = H2Client(self.port)
        streams = range(1, 20, 2)
        try:
            for stream in streams:
                client.request(stream, b"/cache-coalesce?protocol=h2")
            client.until(lambda: all(stream in client.ended for stream in streams) or
                         bool(client.resets) or bool(client.goaways))
            self.assertFalse(client.resets)
            self.assertFalse(client.goaways)
            self.assertEqual({dict(client.headers[stream][0].headers)[b":status"] for stream in streams}, {b"200"})
            self.assertEqual({bytes(client.bodies[stream]) for stream in streams}, {b"calls=2"})
            client.request(21, b"/cache-coalesce?protocol=h2")
            self.assertEqual(client.response(21), b"calls=2")
        finally:
            client.close()

    def test_early_origin_check_before_body_on_both_protocols(self):
        with socket.create_connection(("127.0.0.1", self.port), timeout=3) as peer:
            peer.sendall(b"POST /protected HTTP/1.1\r\nHost: localhost\r\nOrigin: https://evil.test\r\nContent-Length: 100\r\nExpect: 100-continue\r\n\r\n")
            self.assertEqual(Reader(peer).response().status, 403)
        client = H2Client(self.port)
        try:
            client.request(1, b"/protected", method=b"POST", headers=[(b"origin", b"https://evil.test"), (b"content-length", b"100"), (b"expect", b"100-continue")], end_stream=False)
            client.until(lambda: 1 in client.ended)
            self.assertEqual(dict(client.headers[1][0].headers)[b":status"], b"403")
            self.assertFalse(any(dict(block.headers).get(b":status") == b"100" for block in client.headers[1]))
        finally:
            client.close()

    def test_appm_h1_order_h2_independence_and_cancellation(self):
        with socket.create_connection(("127.0.0.1", self.port), timeout=3) as peer:
            peer.sendall(b"GET /slow HTTP/1.1\r\nHost: localhost\r\n\r\nGET /fast HTTP/1.1\r\nHost: localhost\r\n\r\n")
            reader = Reader(peer)
            self.assertEqual(self.h1(b"/fast")[2], b"fast")
            self.assertEqual(reader.response().body, b"slow")
            self.assertEqual(reader.response().body, b"fast")
        client = H2Client(self.port)
        try:
            client.request(1, b"/slow"); client.request(3, b"/fast")
            self.assertEqual(client.response(3), b"fast"); self.assertNotIn(1, client.ended)
            client.send(frame(3, stream=1, payload=struct.pack(">I", 8)))
            client.ping_barrier()
            self.assertIn(b"cancelled=1", self.h1(b"/stats")[2])
        finally:
            client.close()

if __name__ == "__main__":
    unittest.main()
