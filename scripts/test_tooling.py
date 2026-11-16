#!/usr/bin/env python3
"""Generate/build/test/list a fresh application and verify successful + failed rebuilds.
Uses temporary directories and a loopback port. Run after `lake build weave`.
"""
from __future__ import annotations
import http.client
import os
from pathlib import Path
import signal
import socket
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[1]
WEAVE = ROOT / ".lake/build/bin" / ("weave.exe" if os.name == "nt" else "weave")


def response(port):
    client = http.client.HTTPConnection("127.0.0.1", port, timeout=.5)
    try:
        client.request("GET", "/")
        reply = client.getresponse()
        return reply.status, reply.read()
    except OSError:
        return None, b""
    finally:
        client.close()


def wait_for(predicate, message, seconds=30):
    deadline = time.monotonic() + seconds
    while time.monotonic() < deadline:
        if predicate():
            return
        time.sleep(.2)
    raise AssertionError(message)


def main():
    with tempfile.TemporaryDirectory(prefix="weave-smoke-") as temporary:
        temporary = Path(temporary)
        app = temporary / "app"
        subprocess.run([str(WEAVE), "new", str(app), "--path", str(ROOT)], check=True)
        original = (app / "App.lean").read_text()
        for command in ["build", "test", "routes"]:
            subprocess.run([str(WEAVE), command], cwd=app, check=True)
        # Existing directories are protected against scaffolder overwrites.
        assert subprocess.run([str(WEAVE), "new", str(app)], stdout=subprocess.DEVNULL,
                              stderr=subprocess.DEVNULL).returncode != 0
        assert (app / "App.lean").read_text() == original
        with socket.socket() as listener:
            listener.bind(("127.0.0.1", 0))
            port = listener.getsockname()[1]
        environment = dict(os.environ, WEAVE_PORT=str(port), WEAVE_HOST="127.0.0.1", WEAVE_WORKERS="1")
        log_path = temporary / "watch.log"
        with log_path.open("w") as log:
            watcher = subprocess.Popen([str(WEAVE), "dev"], cwd=app, env=environment,
                                       stdout=log, stderr=subprocess.STDOUT, start_new_session=os.name == "posix",
                                       creationflags=subprocess.CREATE_NEW_PROCESS_GROUP if os.name == "nt" else 0)
            try:
                wait_for(lambda: b"Hello, LeanWeave!" in response(port)[1], "initial dev server did not start")
                changed = original.replace("Hello, LeanWeave!", "Rebuilt, LeanWeave!")
                (app / "App.lean").write_text(changed)
                wait_for(lambda: b"Rebuilt, LeanWeave!" in response(port)[1], "successful build did not restart")
                (app / "App.lean").write_text(changed + "\nthis is invalid Lean syntax\n")
                wait_for(lambda: "build failed; application preserved" in log_path.read_text(), "failed build not observed")
                assert b"Rebuilt, LeanWeave!" in response(port)[1], "failed build killed the last good server"
                (app / "App.lean").write_text(original)
                wait_for(lambda: b"Hello, LeanWeave!" in response(port)[1], "recovery build did not restart")
            except BaseException:
                print(log_path.read_text(errors="replace"), file=sys.stderr)
                raise
            finally:
                if os.name == "posix":
                    os.killpg(watcher.pid, signal.SIGTERM)
                else:
                    watcher.send_signal(signal.CTRL_BREAK_EVENT)
                try:
                    watcher.wait(timeout=10)
                except subprocess.TimeoutExpired:
                    watcher.kill()
                    watcher.wait()
            wait_for(lambda: response(port)[0] is None, "development child was not stopped", seconds=10)
        print("PASS weave new/build/test/routes/dev + rebuild failure preservation + graceful stop")


if __name__ == "__main__":
    main()
