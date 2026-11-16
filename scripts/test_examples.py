#!/usr/bin/env python3
"""Boot all four apps and exercise real HTTP/1 + HTTP/2 security/data flows.

Build executables first. HTTP/2 uses the optional test dependency `h2` (pip install h2).
The default requires both protocols; --http1-only is an explicitly reduced check.
"""
from __future__ import annotations
import argparse
from contextlib import contextmanager
import http.client
import json
import os
from pathlib import Path
import re
import socket
import subprocess
import time
from urllib.parse import urlencode

ROOT = Path(__file__).resolve().parents[1]


def port() -> int:
    with socket.socket() as listener:
        listener.bind(("127.0.0.1", 0))
        return listener.getsockname()[1]


@contextmanager
def server(name: str):
    selected = port()
    executable = ROOT / ".lake/build/bin" / (name + (".exe" if os.name == "nt" else ""))
    environment = dict(os.environ, WEAVE_PORT=str(selected), WEAVE_WORKERS="1",
                       WEAVE_HOST="127.0.0.1", WEAVE_DEMO_PASSWORD="example-test-secret-2026")
    child = subprocess.Popen([str(executable)], env=environment, stdout=subprocess.DEVNULL,
                             stderr=subprocess.PIPE, start_new_session=os.name == "posix")
    try:
        deadline = time.monotonic() + 10
        while True:
            if child.poll() is not None:
                raise AssertionError(f"{name} exited: {child.stderr.read().decode()}")
            try:
                with socket.create_connection(("127.0.0.1", selected), timeout=.2):
                    break
            except OSError:
                if time.monotonic() > deadline:
                    raise AssertionError(f"{name} did not start")
                time.sleep(.05)
        yield selected
    finally:
        child.terminate()
        try:
            child.wait(timeout=5)
        except subprocess.TimeoutExpired:
            child.kill()
            child.wait()
        child.stderr.close()


def request_h1(port: int, method: str, path: str, headers=None, body=b""):
    connection = http.client.HTTPConnection("127.0.0.1", port, timeout=5)
    try:
        connection.request(method, path, body=body, headers=headers or {})
        response = connection.getresponse()
        return response.status, {k.lower(): v for k, v in response.getheaders()}, response.read()
    finally:
        connection.close()


def request_h2(port: int, method: str, path: str, headers=None, body=b""):
    import h2.connection
    import h2.config
    import h2.events
    connection = h2.connection.H2Connection(config=h2.config.H2Configuration(client_side=True,
                                                                          header_encoding="utf-8"))
    connection.initiate_connection()
    supplied = dict(headers or {})
    authority = supplied.pop("host", f"127.0.0.1:{port}")
    request_headers = [(":method", method), (":scheme", "http"), (":authority", authority), (":path", path)]
    request_headers.extend((key.lower(), value) for key, value in supplied.items())
    connection.send_headers(1, request_headers, end_stream=not body)
    if body:
        connection.send_data(1, body, end_stream=True)
    response_headers = {}
    content = bytearray()
    with socket.create_connection(("127.0.0.1", port), timeout=5) as transport:
        transport.sendall(connection.data_to_send())
        while True:
            data = transport.recv(65536)
            if not data:
                raise AssertionError("HTTP/2 connection closed before stream completion")
            for event in connection.receive_data(data):
                if isinstance(event, h2.events.ResponseReceived):
                    response_headers.update(event.headers)
                elif isinstance(event, h2.events.DataReceived):
                    content.extend(event.data)
                    connection.acknowledge_received_data(event.flow_controlled_length, event.stream_id)
                elif isinstance(event, h2.events.StreamEnded):
                    return int(response_headers.pop(":status")), response_headers, bytes(content)
                elif isinstance(event, (h2.events.StreamReset, h2.events.ConnectionTerminated)):
                    raise AssertionError(f"HTTP/2 failure: {event}")
            pending = connection.data_to_send()
            if pending:
                transport.sendall(pending)


def cookie(headers):
    return headers["set-cookie"].split(";", 1)[0]


def check_pages(request, selected):
    status, headers, body = request(selected, "GET", "/hello/%3Cscript%3E")
    assert status == 200 and b"&lt;script&gt;" in body and b"<script>" not in body
    get_length = headers.get("content-length")
    status, headers, body = request(selected, "HEAD", "/hello/%3Cscript%3E")
    assert status == 200 and not body and headers.get("content-length") == get_length
    status, headers, body = request(selected, "GET", "/hello/Lean", {"accept": "application/json"})
    assert status == 200 and json.loads(body)["greeting"] == "Hello Lean"
    assert request(selected, "GET", "/unknown")[0] == 404
    status, headers, _ = request(selected, "POST", "/about")
    assert status == 405 and headers["allow"] == "GET, HEAD, OPTIONS"
    assert request(selected, "OPTIONS", "/about")[0] == 204


def check_forms(request, selected):
    origin = f"http://127.0.0.1:{selected}"
    status, headers, body = request(selected, "GET", "/login")
    assert status == 200 and b'type="password"' in body
    assert "HttpOnly" in headers["set-cookie"] and "SameSite=Lax" in headers["set-cookie"]
    original_cookie = cookie(headers)
    token = re.search(rb'name="_csrf" value="([a-f0-9]{64})"', body).group(1).decode()
    common = {"origin": origin, "cookie": original_cookie, "content-type": "application/x-www-form-urlencoded"}
    payload = urlencode({"username": "demo", "password": "example-test-secret-2026"}).encode()
    status, rejected_headers, _ = request(selected, "POST", "/login", common, payload)
    assert status == 403 and "x-request-id" in rejected_headers and "content-security-policy" in rejected_headers
    payload = urlencode({"username": "demo", "password": "example-test-secret-2026", "_csrf": token}).encode()
    assert request(selected, "POST", "/login", dict(common, origin="https://evil.test"), payload)[0] == 403
    status, headers, _ = request(selected, "POST", "/login", common, payload)
    assert status == 303 and headers["location"] == "/account"
    rotated_cookie = cookie(headers)
    assert rotated_cookie != original_cookie
    assert request(selected, "GET", "/account", {"cookie": original_cookie})[0] == 401
    status, _, body = request(selected, "GET", "/account", {"cookie": rotated_cookie})
    assert status == 200 and b"Welcome demo" in body and b"session ID rotated" in body
    rotated_token = re.search(rb'name="_csrf" value="([a-f0-9]{64})"', body).group(1).decode()
    assert rotated_token != token
    status, _, second = request(selected, "GET", "/account", {"cookie": rotated_cookie})
    assert status == 200 and b"session ID rotated" not in second
    logout_headers = dict(common, cookie=rotated_cookie)
    assert request(selected, "POST", "/logout", logout_headers, urlencode({"_csrf": rotated_token}).encode())[0] == 303
    assert request(selected, "GET", "/account", {"cookie": rotated_cookie})[0] == 401


def check_crud(request, selected):
    origin = f"http://127.0.0.1:{selected}"
    status, headers, body = request(selected, "GET", "/csrf")
    assert status == 200
    csrf = json.loads(body)["csrf"]
    common = {"origin": origin, "cookie": cookie(headers), "x-csrf-token": csrf,
              "content-type": "application/json", "accept": "application/json"}
    assert request(selected, "POST", "/posts", dict(common, **{"x-csrf-token": "0" * 64}), b'{"title":"x"}')[0] == 403
    status, _, body = request(selected, "POST", "/posts", common, json.dumps({"title": "<script>title</script>"}).encode())
    assert status == 201
    post = json.loads(body)
    id = post["id"]
    status, _, html = request(selected, "GET", f"/posts/{id}", {"cookie": common["cookie"]})
    assert status == 200 and b"&lt;script&gt;title&lt;/script&gt;" in html and b"<script>" not in html
    status, _, body = request(selected, "PATCH", f"/posts/{id}", common, b'{"title":"updated","revision":1}')
    assert status == 200 and json.loads(body)["revision"] == 2
    assert request(selected, "PUT", f"/posts/{id}", common, b'{"title":"stale","revision":1}')[0] == 409
    assert request(selected, "POST", "/posts", common, json.dumps({"title": "x" * 257}).encode())[0] == 422
    assert request(selected, "DELETE", f"/posts/{id}", common)[0] == 204
    assert request(selected, "GET", f"/posts/{id}")[0] == 404


def check_dashboard(request, selected):
    status, headers, body = request(selected, "GET", "/?isolation=baseline")
    assert status == 200 and "etag" in headers and "public" in headers["cache-control"]
    first = body
    assert request(selected, "GET", "/?isolation=baseline")[2] == first
    assert request(selected, "GET", "/?isolation=other")[2] != first
    assert request(selected, "GET", "/?isolation=baseline", {"host": f"tenant.example:{selected}"})[2] != first
    assert request(selected, "GET", "/?isolation=baseline", {"authorization": "Bearer demo"})[2] != first
    assert request(selected, "GET", "/?isolation=baseline", {"cookie": "session=demo"})[2] != first
    assert request(selected, "GET", "/?isolation=baseline", {"if-none-match": headers["etag"]})[0] == 304
    status, head, empty = request(selected, "HEAD", "/?isolation=baseline")
    assert status == 200 and not empty and head.get("content-length") == str(len(first))
    status, _, body = request(selected, "GET", "/?isolation=baseline", {"accept": "application/json"})
    assert status == 200 and "generation" in json.loads(body)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--http1-only", action="store_true")
    parser.add_argument("-v", "--verbose", action="store_true", help="show each protocol result (also shown by default)")
    options = parser.parse_args()
    protocols = [("HTTP/1", request_h1)]
    if not options.http1_only:
        import h2  # fail explicitly when parity dependencies are missing
        protocols.append(("HTTP/2", request_h2))
    for name, check in [("weave-pages", check_pages), ("weave-forms", check_forms),
                        ("weave-crud", check_crud), ("weave-dashboard", check_dashboard)]:
        with server(name) as selected:
            for protocol, request in protocols:
                check(request, selected)
                print(f"PASS {name}: {protocol}", flush=True)


if __name__ == "__main__":
    main()
