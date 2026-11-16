#!/usr/bin/env python3
"""One native-client corpus over H1 plain/TLS and independent Python h2 TLS.

The H2 server advertises only h2, rejects another ALPN and stamps replies from
the actual codec. HTTP/1 fallback cannot satisfy the same negotiated-header test.
All listeners and certificates are disposable; no persistent service is started.
"""
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from importlib.metadata import version
from pathlib import Path
import hashlib
import json
import os
import platform
import select
import socketserver
import ssl
import subprocess
import sys
import tempfile
import threading
import time

import h2.config
import h2.connection
import h2.events
import h2.exceptions


class Records:
    def init_records(self):
        self.traces = []
        self.alpns = []
        self.lock = threading.Lock()

    def record(self, name, value):
        with self.lock:
            getattr(self, name).append(value)


class Server(Records, ThreadingHTTPServer):
    daemon_threads = True

    def __init__(self, address, handler):
        self.init_records()
        super().__init__(address, handler)

    def handle_error(self, *_): pass


class Handler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def log_message(self, *_): pass
    def do_POST(self): self.handle_request()
    def do_GET(self): self.handle_request()

    def handle_request(self):
        if isinstance(self.connection, ssl.SSLSocket):
            self.server.record("alpns", self.connection.selected_alpn_protocol())
        if self.path == "/slow": time.sleep(.5)
        if self.path == "/echo":
            self.server.record("traces", self.headers.get("traceparent"))
            data = self.rfile.read(int(self.headers.get("content-length", "0")))
        elif self.path == "/oversized": data = b"x" * 8192
        elif self.path == "/redirect": data = b""
        else: data = b"ok"
        try:
            self.send_response(302 if self.path == "/redirect" else 200)
            self.send_header("x-protocol", "http/1.1")
            if self.path == "/redirect": self.send_header("location", "/echo")
            if self.path == "/chunked":
                self.send_header("transfer-encoding", "chunked")
                self.end_headers()
                self.wfile.write(b"3\r\none\r\n3\r\ntwo\r\n0\r\nX-End: yes\r\n\r\n")
            else:
                self.send_header("content-length", str(len(data)))
                self.end_headers()
                self.wfile.write(data)
        except (BrokenPipeError, ConnectionResetError, ssl.SSLError): pass


class H2Server(Records, socketserver.ThreadingTCPServer):
    daemon_threads = True
    allow_reuse_address = True

    def __init__(self, context):
        self.init_records()
        self.context = context
        super().__init__(("127.0.0.1", 0), H2Handler)
        self.server_port = self.server_address[1]

    def handle_error(self, *_): pass


class H2Handler(socketserver.BaseRequestHandler):
    def handle(self):
        try:
            self.request.settimeout(2)
            with self.server.context.wrap_socket(self.request, server_side=True) as peer:
                selected = peer.selected_alpn_protocol()
                self.server.record("alpns", selected)
                if selected != "h2": return
                self.exchange(peer)
        except (OSError, ssl.SSLError, h2.exceptions.H2Error): pass

    def exchange(self, peer):
        connection = h2.connection.H2Connection(config=h2.config.H2Configuration(
            client_side=False, header_encoding="utf-8"))
        connection.initiate_connection()
        peer.sendall(connection.data_to_send())
        requests, pending = {}, {}
        cutoff = time.monotonic() + 5
        while time.monotonic() < cutoff:
            if peer.pending() or select.select([peer], [], [], .01)[0]:
                data = peer.recv(65536)
                if not data: return
                for event in connection.receive_data(data):
                    if isinstance(event, h2.events.RequestReceived):
                        requests[event.stream_id] = {"headers": dict(event.headers), "body": bytearray()}
                    elif isinstance(event, h2.events.DataReceived):
                        request = requests[event.stream_id]
                        request["body"].extend(event.data)
                        if len(request["body"]) > 1024 * 1024: return
                        connection.acknowledge_received_data(event.flow_controlled_length, event.stream_id)
                    elif isinstance(event, h2.events.StreamEnded):
                        request = requests.pop(event.stream_id)
                        path = request["headers"][":path"]
                        if path == "/echo": self.server.record("traces", request["headers"].get("traceparent"))
                        pending[event.stream_id] = (time.monotonic() + (.5 if path == "/slow" else 0), path, request["body"])
                    elif isinstance(event, h2.events.StreamReset):
                        requests.pop(event.stream_id, None)
                        pending.pop(event.stream_id, None)
                    elif isinstance(event, h2.events.ConnectionTerminated): return
            for stream, (due, path, body) in list(pending.items()):
                if time.monotonic() < due: continue
                del pending[stream]
                headers = [(":status", "302" if path == "/redirect" else "200"), ("x-protocol", "h2")]
                if path == "/redirect":
                    connection.send_headers(stream, headers + [("location", "/echo"), ("content-length", "0")], end_stream=True)
                elif path == "/chunked":
                    connection.send_headers(stream, headers)
                    connection.send_data(stream, b"one")
                    connection.send_data(stream, b"two")
                    connection.send_headers(stream, [("x-end", "yes")], end_stream=True)
                else:
                    data = bytes(body) if path == "/echo" else b"x" * 8192 if path == "/oversized" else b"ok"
                    connection.send_headers(stream, headers + [("content-length", str(len(data)))])
                    connection.send_data(stream, data, end_stream=True)
            output = connection.data_to_send()
            if output: peer.sendall(output)


def certificate(cert, key, name):
    subprocess.run(["openssl", "req", "-x509", "-newkey", "rsa:2048", "-nodes", "-days", "1",
        "-keyout", str(key), "-out", str(cert), "-subj", "/CN=" + name, "-addext", "subjectAltName=DNS:" + name],
        check=True, capture_output=True)


def main():
    root = Path(__file__).resolve().parents[1]
    executable = root / ".lake/build/bin" / ("weave-http-test.exe" if os.name == "nt" else "weave-http-test")
    windows_runtime = None
    unpackaged_backend = None
    if os.name == "nt":
        try:
            before = subprocess.run([str(executable), "--backend-info"], capture_output=True, text=True, timeout=5)
            unpackaged_backend = {"returncode": before.returncode, "description": before.stdout.strip(), "stderr": before.stderr.strip()}
        except (OSError, subprocess.TimeoutExpired) as error:
            unpackaged_backend = {"error": str(error)}
        print("UNPACKAGED native HTTP backend: " + json.dumps(unpackaged_backend), flush=True)
        packaged = subprocess.run([sys.executable, str(root / "scripts/stage_windows_runtime.py"), str(executable)],
            check=True, text=True, capture_output=True, timeout=60)
        windows_runtime = json.loads(packaged.stdout)
        print("STAGED native Windows libcurl runtime: " + json.dumps(windows_runtime), flush=True)
    results = []
    with tempfile.TemporaryDirectory(prefix="leanweave-http-") as temporary:
        temporary = Path(temporary)
        cert, key = temporary / "cert.pem", temporary / "key.pem"
        bad_ca = temporary / "unrelated.pem"
        certificate(cert, key, "localhost")
        certificate(bad_ca, temporary / "unrelated-key.pem", "unrelated.test")
        large_ca = temporary / "oversized.pem"
        large_ca.write_bytes(b"x" * (512 * 1024 + 1))
        for tls, http2 in [(False, False), (True, False), (True, True)]:
            expected = "h2" if http2 else "http/1.1"
            context = None
            if tls:
                context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
                context.load_cert_chain(cert, key)
                context.set_alpn_protocols([expected])
            if http2: server = H2Server(context)
            else:
                server = Server(("127.0.0.1", 0), Handler)
                if tls:
                    # Handshake in the request thread when its reader starts;
                    # canceled connects cannot pin the listener's accept loop.
                    server.socket = context.wrap_socket(server.socket, server_side=True, do_handshake_on_connect=False)
            thread = threading.Thread(target=server.serve_forever, daemon=True)
            thread.start()
            try:
                env = {**os.environ, "WEAVE_HTTP_PORT": str(server.server_port), "WEAVE_HTTP_TLS": str(int(tls)),
                       "WEAVE_HTTP_H2": str(int(http2)), "WEAVE_HTTP_CA": str(cert), "WEAVE_HTTP_BAD_CA": str(bad_ca),
                       "WEAVE_HTTP_LARGE_CA": str(large_ca), "WEAVE_HTTP_MISSING_CA": str(temporary / "missing.pem")}
                output = subprocess.run([str(executable)], env=env, capture_output=True, text=True, timeout=30)
                if output.returncode: raise RuntimeError(output.stdout + output.stderr)
                backend = next(line.removeprefix("HTTP_BACKEND ") for line in output.stderr.splitlines()
                               if line.startswith("HTTP_BACKEND "))
                print("ACTUAL native HTTP backend: " + backend, flush=True)
                if os.name == "nt":
                    assert "OpenSSL/" in backend, backend
                    module = Path(backend.split("; module=", 1)[1])
                    assert module.resolve().parent == executable.resolve().parent, "runtime DLL was not loaded from the packaged application directory"
                otlp = json.loads(output.stdout.splitlines()[0])
                spans = otlp["resourceSpans"][0]["scopeSpans"][0]["spans"]
                assert len(spans) == 6
                assert len(server.traces) == 1 and server.traces[0].startswith("00-4bf92f3577b34da6a3ce929d0e0e4736-")
                assert server.traces[0].endswith("-01")
                if tls: assert len(server.alpns) >= 3 and all(alpn == expected for alpn in server.alpns), server.alpns
                results.append({"tls": tls, "http2": http2, "protocol": expected, "alpn": expected if tls else None,
                    "passed": True, "automatic_spans": len(spans), "bounds_timeout_disconnect": True,
                    "chunked_or_h2_data_and_trailers": True, "negative_tls_hostname_and_trust": tls,
                    "active_operation_limit": True, "header_origin_injection": True,
                    "startup_ca_no_request_file_dependency": tls,"strict_ca_startup_bounds": tls,"explicit_system_trust_opt_in": tls,
                    "negotiation_asserted_by_header": True, "server_connections": len(server.alpns), "runtime_backend": backend})
                print(f"PASS native HTTP client {expected} {'TLS' if tls else 'plain'} bounded/trace/timeout/cancel/negotiation", flush=True)
            finally:
                server.shutdown()
                server.server_close()
                thread.join(timeout=3)
                assert not thread.is_alive(), "fixture listener did not stop"
    report = {"native": results, "platform": platform.platform(), "independent_h2": version("h2"), "windows_runtime": windows_runtime,
        "unpackaged_backend": unpackaged_backend,
        "sources_sha256": {str(path.relative_to(root)): hashlib.sha256(path.read_bytes()).hexdigest()
            for path in [root / "test/Main.lean", Path(__file__).resolve(), root / "LeanWeaveHttp/Client.lean", root / "c/http.c", root / "scripts/stage_windows_runtime.py"]}}
    (root / "test/wire-result.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(results))


if __name__ == "__main__": main()
