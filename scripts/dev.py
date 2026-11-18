#!/usr/bin/env python3
"""Portable bounded Lean build/restart loop used by `weave dev`.

No shell evaluation: targets and arguments are passed directly to subprocesses.
A failed build preserves the running application. SIGINT/SIGTERM drain the child.
"""
from __future__ import annotations
import os
from pathlib import Path
import shutil
import signal
import subprocess
import sys
import threading
import time

MAX_FILES = 10_000
POLL_SECONDS = 0.5
BUILD_TIMEOUT_SECONDS = 600
DRAIN_SECONDS = 5
EXCLUDED = {".git", ".lake", "node_modules", ".venv", "__pycache__"}


def snapshot(root: Path) -> dict[str, tuple[int, int]]:
    result: dict[str, tuple[int, int]] = {}
    for directory, folders, files in os.walk(root):
        folders[:] = sorted(folder for folder in folders if folder not in EXCLUDED)
        for name in sorted(files):
            if not (name.endswith((".lean", ".toml")) or name == "lean-toolchain"):
                continue
            path = Path(directory) / name
            try:
                stat = path.stat()
            except FileNotFoundError:
                continue
            result[str(path.relative_to(root))] = (stat.st_mtime_ns, stat.st_size)
            if len(result) > MAX_FILES:
                raise RuntimeError(f"watch limit exceeded ({MAX_FILES} source files)")
    return result


def stop(child: subprocess.Popen | None) -> None:
    if child is None or child.poll() is not None:
        return
    if os.name == "posix":
        try:
            os.killpg(child.pid, signal.SIGTERM)
        except ProcessLookupError:
            pass
    else:
        try:
            child.send_signal(signal.CTRL_BREAK_EVENT)
        except (OSError, ValueError):
            child.terminate()
    try:
        child.wait(timeout=DRAIN_SECONDS)
    except subprocess.TimeoutExpired:
        if os.name == "posix":
            try:
                os.killpg(child.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
        else:
            child.kill()
        child.wait()


def executable(target: str) -> Path:
    return Path(".lake/build/bin") / (target + (".exe" if os.name == "nt" else ""))


def remove_snapshot(path: Path | None) -> None:
    if path is None:
        return
    # A stopped Windows process can briefly retain a loader/antivirus mapping.
    for attempt in range(20):
        try:
            path.unlink(missing_ok=True)
            return
        except PermissionError:
            if attempt == 19:
                raise
            time.sleep(.05)


def run(target: str, parent_pipe: bool = False) -> int:
    if not target or any(c not in "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_" for c in target):
        raise ValueError("target must be a simple Lake executable name")
    root = Path.cwd()
    child = None
    child_snapshot = None
    staged_snapshot = None
    generation = 0
    stopping = False

    def on_signal(_signum, _frame):
        nonlocal stopping
        stopping = True

    def watch_parent():
        nonlocal stopping
        try:
            while os.read(sys.stdin.fileno(), 1):
                pass
        except OSError:
            pass
        stopping = True

    signal.signal(signal.SIGINT, on_signal)
    signal.signal(signal.SIGTERM, on_signal)
    if os.name == "nt":
        signal.signal(signal.SIGBREAK, on_signal)
    if parent_pipe:
        threading.Thread(target=watch_parent, daemon=True).start()
    # Keep the launcher pipe exclusively for the watcher. Direct script calls
    # retain their existing inherited stdin for the build and application.
    child_stdin = subprocess.DEVNULL if parent_pipe else None
    child_group = dict(start_new_session=os.name == "posix",
                       creationflags=subprocess.CREATE_NEW_PROCESS_GROUP if os.name == "nt" else 0)
    previous = None
    try:
        while not stopping:
            current = snapshot(root)
            if current != previous:
                previous = current
                print(f"[weave] building {target}", flush=True)
                build = subprocess.Popen(["lake", "build", target], stdin=child_stdin, **child_group)
                deadline = time.monotonic() + BUILD_TIMEOUT_SECONDS
                try:
                    while not stopping and build.poll() is None and time.monotonic() < deadline:
                        time.sleep(.1)
                    if not stopping and build.poll() is None:
                        print("[weave] build timeout; application preserved", flush=True)
                        continue
                finally:
                    stop(build)
                if stopping:
                    break
                if build.returncode == 0:
                    path = executable(target)
                    if not path.is_file():
                        raise RuntimeError(f"build target is not an executable: {path}")
                    staged_snapshot = None
                    if os.name == "nt":
                        # Windows cannot relink an executable that is running.
                        # A sibling copy keeps the original build output unlocked
                        # and preserves the executable's DLL search directory.
                        generation += 1
                        staged_snapshot = path.with_name(f".weave-dev-{target}-{os.getpid()}-{generation}.exe")
                        shutil.copy2(path, staged_snapshot)
                    stop(child)
                    child = None
                    remove_snapshot(child_snapshot)
                    child_snapshot = staged_snapshot
                    staged_snapshot = None
                    print(f"[weave] starting {target}", flush=True)
                    if stopping:
                        break
                    child = subprocess.Popen([str((child_snapshot or path).resolve())],
                                             stdin=child_stdin, **child_group)
                else:
                    print("[weave] build failed; application preserved", flush=True)
            time.sleep(POLL_SECONDS)
    finally:
        try:
            stop(child)
        finally:
            try:
                remove_snapshot(child_snapshot)
            finally:
                remove_snapshot(staged_snapshot)
    return 0


if __name__ == "__main__":
    try:
        target = sys.argv[1] if len(sys.argv) > 1 else "app"
        raise SystemExit(run(target, parent_pipe=sys.argv[2:] == ["--parent-pipe"]))
    except (OSError, RuntimeError, ValueError) as error:
        print(f"weave dev: {error}", file=sys.stderr)
        raise SystemExit(1)
