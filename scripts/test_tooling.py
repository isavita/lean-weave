#!/usr/bin/env python3
"""Generate/build/test/list a fresh application and verify successful + failed rebuilds.
Uses temporary directories and a loopback port. Run after `lake build weave`.
"""
from __future__ import annotations
from contextlib import contextmanager
import http.client
import os
from pathlib import Path
import shutil
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


@contextmanager
def watching(app, environment, log_path):
    with log_path.open("w", encoding="utf-8") as log:
        watcher = subprocess.Popen([str(WEAVE), "dev"], cwd=app, env=environment,
                                   stdout=log, stderr=subprocess.STDOUT, start_new_session=os.name == "posix",
                                   creationflags=subprocess.CREATE_NEW_PROCESS_GROUP if os.name == "nt" else 0)
        try:
            yield watcher
        except BaseException:
            print(log_path.read_text(encoding="utf-8", errors="replace"), file=sys.stderr)
            raise
        finally:
            if os.name == "posix":
                try:
                    os.killpg(watcher.pid, signal.SIGTERM)
                except ProcessLookupError:
                    pass
            elif watcher.poll() is None:
                watcher.send_signal(signal.CTRL_BREAK_EVENT)
            try:
                watcher.wait(timeout=10)
            except subprocess.TimeoutExpired:
                watcher.kill()
                watcher.wait()
                raise


def stop_launcher(watcher, app, port):
    shutdown = app / "test-shutdown"
    shutdown.unlink(missing_ok=True)
    watcher.terminate()
    watcher.wait(timeout=10)
    wait_for(shutdown.is_file, "PID-only launcher shutdown did not run the application cleanup", seconds=10)
    wait_for(lambda: response(port)[0] is None, "application still serves after launcher exit", seconds=10)


def process_exited(pid):
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return True
    return False


def stop_during_build(app, environment, port, temporary):
    # Make the rebuild stay busy until the watcher stops it. The initial build
    # still runs real Lake and starts the generated server normally.
    bins = temporary / "build-fixture"
    bins.mkdir()
    fake_lake = bins / "lake"
    fake_lake.write_text('''#!/bin/sh
if test -f "$WEAVE_TEST_HOLD_BUILD"; then
    printf '%s\n' "$$" > "$WEAVE_TEST_BUILD_PID"
    exec sleep 60
fi
exec "$WEAVE_TEST_REAL_LAKE" "$@"
''', encoding="utf-8")
    fake_lake.chmod(0o755)
    hold = temporary / "hold-build"
    build_pid = temporary / "build-pid"
    real_lake = shutil.which("lake", path=environment["PATH"])
    assert real_lake is not None
    environment = dict(environment, PATH=str(bins) + os.pathsep + environment["PATH"],
                       WEAVE_TEST_HOLD_BUILD=str(hold), WEAVE_TEST_BUILD_PID=str(build_pid),
                       WEAVE_TEST_REAL_LAKE=real_lake)
    with watching(app, environment, temporary / "build-watch.log") as watcher:
        wait_for(lambda: b"Hello, LeanWeave!" in response(port)[1], "build-stop fixture did not start")
        hold.touch()
        with (app / "App.lean").open("a", encoding="utf-8") as source:
            source.write("\n-- Trigger a deliberately slow rebuild.\n")
        wait_for(lambda: build_pid.is_file() and build_pid.read_text(encoding="utf-8").strip(), "rebuild did not start")
        pid = int(build_pid.read_text(encoding="utf-8"))
        assert b"Hello, LeanWeave!" in response(port)[1], "rebuild stopped the previous server"
        try:
            stop_launcher(watcher, app, port)
            wait_for(lambda: process_exited(pid), "build survived launcher exit", seconds=10)
        finally:
            # Prevent a failing regression from retaining the timed fixture.
            try:
                os.killpg(pid, signal.SIGTERM)
            except ProcessLookupError:
                pass


def main():
    with tempfile.TemporaryDirectory(prefix="weave-smoke-") as temporary:
        temporary = Path(temporary)
        app = temporary / "app"
        subprocess.run([str(WEAVE), "new", str(app), "--path", str(ROOT)], check=True)
        original = (app / "App.lean").read_text(encoding="utf-8")
        for command in ["build", "test", "routes"]:
            subprocess.run([str(WEAVE), command], cwd=app, check=True)
        # Existing directories are protected against scaffolder overwrites.
        assert subprocess.run([str(WEAVE), "new", str(app)], stdout=subprocess.DEVNULL,
                              stderr=subprocess.DEVNULL).returncode != 0
        assert (app / "App.lean").read_text(encoding="utf-8") == original
        main_file = app / "Main.lean"
        serve = "Server.serve (fun _ => pure routes.handler) config"
        main_source = main_file.read_text(encoding="utf-8")
        assert serve in main_source
        main_file.write_text(main_source.replace(serve,
            'Server.serve (fun _ => pure routes.handler) { config with\n'
            '      onShutdown := IO.FS.writeFile "test-shutdown" "drained" }'), encoding="utf-8")
        with socket.socket() as listener:
            listener.bind(("127.0.0.1", 0))
            port = listener.getsockname()[1]
        environment = dict(os.environ, WEAVE_PORT=str(port), WEAVE_HOST="127.0.0.1", WEAVE_WORKERS="1")
        log_path = temporary / "watch.log"
        with watching(app, environment, log_path) as watcher:
            wait_for(lambda: b"Hello, LeanWeave!" in response(port)[1], "initial dev server did not start")
            changed = original.replace("Hello, LeanWeave!", "Rebuilt, LeanWeave!")
            (app / "App.lean").write_text(changed, encoding="utf-8")
            wait_for(lambda: b"Rebuilt, LeanWeave!" in response(port)[1], "successful build did not restart")
            (app / "App.lean").write_text(changed + "\nthis is invalid Lean syntax\n", encoding="utf-8")
            wait_for(lambda: "build failed; application preserved" in log_path.read_text(encoding="utf-8", errors="replace"),
                     "failed build not observed")
            assert b"Rebuilt, LeanWeave!" in response(port)[1], "failed build killed the last good server"
            (app / "App.lean").write_text(original, encoding="utf-8")
            wait_for(lambda: b"Hello, LeanWeave!" in response(port)[1], "recovery build did not restart")
            stop_launcher(watcher, app, port)
        wait_for(lambda: response(port)[0] is None, "development child was not stopped", seconds=10)
        print("PASS PID-only launcher stop while serving")
        if os.name == "posix":
            stop_during_build(app, environment, port, temporary)
            print("PASS PID-only launcher stop while rebuilding")
        print("PASS weave new/build/test/routes/dev + rebuild failure preservation + graceful stop")


if __name__ == "__main__":
    main()
