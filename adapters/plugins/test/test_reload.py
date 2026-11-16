#!/usr/bin/env python3
"""Compile actual native plugins and hot-reload one live HTTP/1+HTTP/2 process."""
from __future__ import annotations
import argparse
from concurrent.futures import ThreadPoolExecutor
import hashlib
import http.client
from importlib.metadata import version as package_version
import json
import os
from pathlib import Path
import platform
import shutil
import signal
import socket
import subprocess
import sys
import tempfile
import time
from urllib.parse import quote

ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT.parents[1]
NET = Path(os.environ.get("LEAN_NET_ROOT", SOURCE.parent / "lean-net-platform"))
if not NET.is_dir(): NET = SOURCE / ".lake/packages/lean-net"
sys.path.insert(0, str(NET / "test"))
from http2_test_support import H2Client, frame

EXE = ROOT / ".lake/build/bin" / ("weave-plugin-fixture.exe" if os.name == "nt" else "weave-plugin-fixture")


def wait_for(predicate, message, seconds=5):
    cutoff = time.monotonic() + seconds
    while time.monotonic() < cutoff:
        if predicate(): return
        time.sleep(.01)
    raise AssertionError(message)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", type=Path)
    options = parser.parse_args()
    started = time.monotonic()
    with tempfile.TemporaryDirectory(prefix="weave-plugin-test-") as temporary:
        directory = Path(temporary)
        suffix = ".dll" if os.name == "nt" else ".dylib" if sys.platform == "darwin" else ".so"
        compiler = os.environ.get("CC", "gcc" if os.name == "nt" else "cc")
        shared = ["-dynamiclib"] if sys.platform == "darwin" else ["-shared"]
        images = {}
        for version, extra in [("v1", []), ("v2", []), ("v3", []), ("bad-start", ["-DFAIL_START=1"]), ("bad-abi", ["-DABI=999"])]:
            image = directory / (version + suffix)
            subprocess.run([compiler, *shared, "-std=c11", "-Wall", "-Wextra", "-Wpedantic", "-Werror", "-fPIC",
                "-I", str(ROOT / "include"), '-DVERSION="' + version + '"', *extra,
                str(ROOT / "test/plugin.c"), "-o", str(image)], check=True, timeout=30)
            images[version] = image
        live = directory / ("live" + suffix)
        shutil.copyfile(images["v1"], live)
        with socket.socket() as sock:
            sock.bind(("127.0.0.1", 0)); port = sock.getsockname()[1]

        def request(path="/", method="GET", body=None):
            connection = http.client.HTTPConnection("127.0.0.1", port, timeout=4)
            try:
                connection.request(method, path, body)
                response = connection.getresponse()
                return response.status, dict(response.getheaders()), response.read()
            finally: connection.close()

        def stats(): return json.loads(request("/admin/stats")[2])

        def replace(version):
            replacement = directory / ("next" + suffix)
            shutil.copyfile(images[version], replacement)
            os.replace(replacement, live)

        def reload(path=live): return request("/admin/reload?path=" + quote(str(path)))[0]

        with tempfile.TemporaryFile(mode="w+") as log:
            process = subprocess.Popen([str(EXE), str(port), str(live)], stdout=log, stderr=log,
                creationflags=subprocess.CREATE_NEW_PROCESS_GROUP if os.name == "nt" else 0)
            try:
                def ready():
                    if process.poll() is not None:
                        log.seek(0); raise AssertionError(log.read())
                    try: return request()[2] == b"v1"
                    except OSError: return False
                wait_for(ready, "native plugin server startup")
                pid = process.pid
                with ThreadPoolExecutor(max_workers=1) as executor:
                    old = executor.submit(request, "/slow")
                    wait_for(lambda: stats()["inFlight"] == 1, "old deferred request lease")
                    replace("v2")
                    assert reload() == 200
                    assert request()[2] == b"v2" and process.pid == pid
                    assert stats()["retired"] == 1 and "v1" not in stats()["stops"]
                    assert old.result(timeout=4)[2] == b"v1"
                wait_for(lambda: stats()["retired"] == 0, "old deferred generation finalizer")
                assert stats()["stops"].count("v1") == 1
                print("PASS native same-path dlopen reload: old deferred v1 completes while new v2 serves, one process", flush=True)

                connection = http.client.HTTPConnection("127.0.0.1", port, timeout=4)
                connection.request("GET", "/stream")
                stream = connection.getresponse()
                assert stream.readline() == b"v2-first\n"
                replace("v3"); assert reload() == 200
                current = stats()
                assert current["retired"] == 1 and current["active"] == "v3"
                assert reload(images["v1"]) == 503 and stats()["starts"] == current["starts"]
                assert stats()["loads"] == current["loads"], "capacity checked before native factory/dlopen"
                assert request()[2] == b"v3"
                assert stream.read() == b"v2-last\n"
                connection.close()
                wait_for(lambda: stats()["retired"] == 0, "stream lease release")
                assert stats()["stops"].count("v2") == 1
                print("PASS native stream leases + bounded retained generations + deferred unload", flush=True)

                for rejected in [images["bad-abi"], images["bad-start"], directory / "missing.so"]:
                    assert reload(rejected) == 503
                    assert request()[2] == b"v3" and stats()["active"] == "v3"
                assert request("/bytes")[2] == bytes([0, 255, 1, 128])
                assert request("/badheaders")[0] == 502
                assert request("/badcontrol")[0] == 502
                for path in ["/badnumber", "/badnegative"]:
                    rejected_at = time.monotonic()
                    assert request(path)[0] == 502
                    assert time.monotonic() - rejected_at < 2, "unbounded plugin JSON number parsing"
                assert request("/numericstring")[1].get("x-test") == "1e1000000000"
                assert request()[2] == b"v3", "numeric rejection must release plugin exchange and keep serving"
                print("PASS native plugin JSON numeric bounds: huge positive/negative exponent rejected, string control and recovery", flush=True)
                assert request("/badstatus")[0] == 502
                assert request("/badbody")[0] == 502
                assert request("/failpoll")[0] == 502
                assert request("/", "POST", b"x" * 70000)[0] == 413
                wait_for(lambda: stats()["inFlight"] == 0, "failed exchange leases cleaned")
                print("PASS native ABI/start/load failures preserve v3; input/output/header bounds and binary copies", flush=True)

                blocked = []
                try:
                    for _ in range(2):
                        connection = http.client.HTTPConnection("127.0.0.1", port, timeout=4)
                        connection.request("GET", "/forever")
                        response = connection.getresponse()
                        assert response.readline() == b"v3-first\n"
                        blocked.append((connection, response))
                    assert stats()["inFlight"] == 2 and request()[0] == 503
                finally:
                    for connection, response in blocked:
                        response.close(); connection.close()
                wait_for(lambda: stats()["inFlight"] == 0, "disconnect releases bounded request slots")
                print("PASS native in-flight bound + HTTP/1 disconnect cancellation releases both slots", flush=True)

                with ThreadPoolExecutor(max_workers=1) as executor:
                    pending = executor.submit(request, "/pending")
                    wait_for(lambda: stats()["inFlight"] == 1, "pending request lease")
                    replace("v1"); assert reload() == 200
                    assert request()[2] == b"v1"
                    assert pending.result(timeout=4)[0] == 504
                wait_for(lambda: stats()["retired"] == 0, "timeout generation release")
                assert "v3" in stats()["stops"]
                print("PASS native deferred timeout cancels exchange before retired code unload", flush=True)

                peer = H2Client(port)
                try:
                    peer.request(1, b"/forever")
                    peer.until(lambda: b"v1-first\n" in peer.bodies[1])
                    replace("v2"); assert reload() == 200
                    peer.request(3, b"/")
                    assert peer.response(3) == b"v2"
                    assert stats()["retired"] == 1
                    peer.send(frame(3, stream=1, payload=(8).to_bytes(4, "big")))
                    wait_for(lambda: stats()["retired"] == 0, "HTTP/2 RST releases old native code")
                finally: peer.close()
                print("PASS native HTTP/2 independent new generation stream + RST cancellation unload", flush=True)

                # HEAD must close a body whose bytes are suppressed by the server.
                assert request("/stream", "HEAD")[2] == b""
                wait_for(lambda: stats()["inFlight"] == 0, "HEAD body suppression release")
                assert request("/admin/unload")[0] == 200
                assert request()[0] == 503
                replace("v3"); assert reload() == 200

                disconnected = http.client.HTTPConnection("127.0.0.1", port, timeout=4)
                disconnected.request("GET", "/forever")
                response = disconnected.getresponse()
                assert response.readline() == b"v3-first\n"
                stopped = time.monotonic()
                process.send_signal(signal.CTRL_BREAK_EVENT if os.name == "nt" else signal.SIGTERM)
                process.wait(timeout=3)
                response.close(); disconnected.close()
                log.seek(0); output = log.read()
                assert process.returncode == 0, output
                assert "PLUGIN_STOPPED:v3" in output
                assert time.monotonic() - stopped < 2
                print("PASS native unload/reload, HEAD suppression and SIGTERM closes leased streaming generation", flush=True)
            finally:
                if process.poll() is None:
                    process.kill(); process.wait(timeout=5)
    if options.output:
        paths = [SOURCE / "LeanWeave/Plugin/Manager.lean", SOURCE / "LeanWeave/Core/Body.lean", *sorted((ROOT / "LeanWeavePlugins").glob("*.lean")),
                 ROOT / "c/plugins.c", ROOT / "include/leanweave_plugin.h", ROOT / "test/plugin.c",
                 ROOT / "test/Fixture.lean", Path(__file__).resolve()]
        options.output.parent.mkdir(parents=True, exist_ok=True)
        options.output.write_text(json.dumps({"passed": True,"platform": platform.platform(),
            "independent_clients": {name: package_version(name) for name in ["h2","hpack","hyperframe"]},
            "duration_seconds": round(time.monotonic()-started,3),"native_dynamic_loading": True,
            "same_process_same_path_reload": True,"old_deferred_request_completes": True,"stream_lease_until_close": True,
            "generation_bound": 2,"in_flight_bound": 2,"failed_reload_preserves_active": True,"http2_reset_unloads": True,
            "head_closes_stream": True,"sigterm_stream_shutdown": True,
            "header_json_numeric_bounds": True,
            "sources_sha256": {str(p.relative_to(SOURCE)): hashlib.sha256(p.read_bytes()).hexdigest() for p in paths}},indent=2)+"\n")


if __name__ == "__main__": main()
