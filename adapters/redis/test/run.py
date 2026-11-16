#!/usr/bin/env python3
"""Run Redis acceptance against a disposable local Redis (no daemon service)."""
from pathlib import Path
import shutil
import socket
import subprocess
import tempfile
import time
import sys

ROOT = Path(__file__).resolve().parents[1]

def free_port():
    with socket.socket() as peer:
        peer.bind(("127.0.0.1", 0))
        return peer.getsockname()[1]

def main():
    redis = shutil.which("redis-server")
    if not redis:
        raise SystemExit("redis-server is required (install Redis; no service needs to run)")
    port, refused = free_port(), free_port()
    with tempfile.TemporaryDirectory(prefix="weave-redis-") as directory:
        with open(Path(directory) / "redis.log", "w+") as log:
            server = subprocess.Popen([redis, "--bind", "127.0.0.1", "--port", str(port),
                "--save", "", "--appendonly", "no", "--dir", directory], stdout=log, stderr=log)
            try:
                for _ in range(200):
                    if server.poll() is not None:
                        log.seek(0)
                        raise RuntimeError(f"Redis startup failed: {log.read()}")
                    try:
                        with socket.create_connection(("127.0.0.1", port), timeout=.1):
                            break
                    except OSError:
                        time.sleep(.01)
                else:
                    raise RuntimeError("Redis startup timeout")
                binary = ROOT / (".lake/build/bin/weave-redis-test" + (".exe" if sys.platform == "win32" else ""))
                subprocess.run([str(binary), str(port), str(refused)], check=True, timeout=30)
            finally:
                server.terminate()
                try:
                    server.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    server.kill()
                    server.wait(timeout=5)

if __name__ == "__main__":
    main()
