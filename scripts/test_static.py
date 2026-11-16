#!/usr/bin/env python3
"""Real mounted static/catalog integration over HTTP/1 + HTTP/2, optionally TLS.
Build weave-static-fixture; for --tls also build adapters/tls. Requires Python h2.
"""
from __future__ import annotations
import argparse
from contextlib import contextmanager
import gzip
import http.client
import os
from pathlib import Path
import socket
import ssl
import subprocess
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]


def request(port, protocol, method, path, headers=None, certificate=None):
    context = ssl.create_default_context(cafile=str(certificate)) if certificate else None
    if protocol == "HTTP/1":
        if context:
            context.set_alpn_protocols(["http/1.1"])
        client = (http.client.HTTPSConnection("127.0.0.1", port, timeout=5, context=context)
                  if context else http.client.HTTPConnection("127.0.0.1", port, timeout=5))
        try:
            client.request(method, path, headers=headers or {})
            response = client.getresponse()
            return response.status, {key.lower(): value for key, value in response.getheaders()}, response.read()
        finally:
            client.close()
    import h2.config
    import h2.connection
    import h2.events
    connection = h2.connection.H2Connection(h2.config.H2Configuration(header_encoding="utf-8"))
    connection.initiate_connection()
    connection.send_headers(1, [(":method", method), (":scheme", "https" if context else "http"),
                               (":authority", f"127.0.0.1:{port}"), (":path", path)] +
                            [(key.lower(), value) for key, value in (headers or {}).items()], end_stream=True)
    transport = socket.create_connection(("127.0.0.1", port), timeout=5)
    if context:
        context.set_alpn_protocols(["h2"])
        transport = context.wrap_socket(transport, server_hostname="127.0.0.1")
        assert transport.selected_alpn_protocol() == "h2"
    response_headers = {}
    body = bytearray()
    with transport:
        transport.sendall(connection.data_to_send())
        while True:
            incoming = transport.recv(65536)
            assert incoming, "connection closed before response"
            for event in connection.receive_data(incoming):
                if isinstance(event, h2.events.ResponseReceived):
                    response_headers.update(event.headers)
                elif isinstance(event, h2.events.DataReceived):
                    body.extend(event.data)
                    connection.acknowledge_received_data(event.flow_controlled_length, event.stream_id)
                elif isinstance(event, h2.events.StreamEnded):
                    return int(response_headers.pop(":status")), response_headers, bytes(body)
                elif isinstance(event, (h2.events.StreamReset, h2.events.ConnectionTerminated)):
                    raise AssertionError(event)
            pending = connection.data_to_send()
            if pending:
                transport.sendall(pending)


@contextmanager
def server(root, certificate=None, key=None):
    with socket.socket() as listener:
        listener.bind(("127.0.0.1", 0))
        port = listener.getsockname()[1]
    executable = (ROOT / "adapters/tls/.lake/build/bin/weave-tls-fixture" if certificate else
                  ROOT / ".lake/build/bin/weave-static-fixture")
    if os.name == "nt":
        executable = executable.with_suffix(".exe")
    arguments = [str(executable), str(root)]
    if certificate:
        arguments.extend([str(certificate), str(key)])
    arguments.append(str(port))
    child = subprocess.Popen(arguments, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
    try:
        deadline = time.monotonic() + 15
        while True:
            if child.poll() is not None:
                raise AssertionError(child.stderr.read().decode())
            try:
                with socket.create_connection(("127.0.0.1", port), timeout=.2):
                    break
            except OSError:
                assert time.monotonic() < deadline, "static server did not start"
                time.sleep(.05)
        yield port
    finally:
        child.terminate()
        try:
            child.wait(timeout=5)
        except subprocess.TimeoutExpired:
            child.kill()
            child.wait()
        child.stderr.close()


def check(port, protocol, payload, certificate):
    def get(method, path, headers=None):
        return request(port, protocol, method, path, headers, certificate)
    status, headers, body = get("GET", "/assets/asset.txt", {"accept-encoding": "identity"})
    assert status == 200 and body == payload and int(headers["content-length"]) == len(payload)
    tag = headers["etag"]
    status, head, body = get("HEAD", "/assets/asset.txt", {"accept-encoding": "identity"})
    assert status == 200 and body == b"" and head["content-length"] == str(len(payload))
    status, _, body = get("GET", "/assets/asset.txt", {"if-none-match": tag})
    assert status == 304 and body == b""
    status, headers, body = get("GET", "/assets/asset.txt", {"accept-encoding": "gzip"})
    assert status == 200 and headers["content-encoding"] == "gzip" and gzip.decompress(body) == payload
    assert headers["etag"] != tag
    status, headers, body = get("GET", "/assets/asset.txt", {"accept-encoding": "br"})
    assert status == 200 and headers["content-encoding"] == "br" and 0 < len(body) < len(payload)
    status, headers, _ = get("GET", "/assets/folder?x=1")
    assert status == 308 and headers["location"] == "/assets/folder/?x=1"
    assert get("GET", "/assets/folder/")[2] == b"directory index"
    assert get("GET", "/assets/missing")[0] == 404
    assert get("GET", "/assets/%2e%2e/asset.txt")[0] == 400
    assert get("GET", "/assets/asset.txt", {"x-deny": "yes"})[0] == 403
    assert get("GET", "/dynamic")[2] == b"dynamic application"
    assert get("GET", "/deferred")[2] == b"deferred application"


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--tls", action="store_true", help="also test the optional native TLS adapter")
    parser.add_argument("-v", "--verbose", action="store_true")
    options = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="weave-static-") as directory:
        directory = Path(directory)
        root = directory / "public"
        root.mkdir()
        payload = b"LeanWeave immutable native file region.\n" * 1000
        (root / "asset.txt").write_bytes(payload)
        (root / "folder").mkdir()
        (root / "folder/index.html").write_bytes(b"directory index")
        variants = [("plaintext", None, None)]
        if options.tls:
            certificate, key = directory / "cert.pem", directory / "key.pem"
            subprocess.run(["openssl", "req", "-x509", "-newkey", "rsa:2048", "-nodes", "-days", "1",
                            "-subj", "/CN=localhost", "-addext", "subjectAltName=DNS:localhost,IP:127.0.0.1",
                            "-keyout", str(key), "-out", str(certificate)], check=True,
                           stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            variants.append(("TLS (verified test certificate)", certificate, key))
        for name, certificate, key in variants:
            with server(root, certificate, key) as port:
                for protocol in ["HTTP/1", "HTTP/2"]:
                    check(port, protocol, payload, certificate)
                    print(f"PASS static {name}: {protocol} identity/gzip/br/HEAD/304/mount/global-check/deferred", flush=True)


if __name__ == "__main__":
    main()
