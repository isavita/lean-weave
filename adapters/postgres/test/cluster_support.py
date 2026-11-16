"""Disposable localhost-only PostgreSQL test clusters; never starts a service."""
from contextlib import contextmanager
from dataclasses import dataclass
import os
from pathlib import Path
import shutil
import socket
import subprocess
import tempfile

@dataclass
class Cluster:
    directory: Path
    binary_dir: Path
    port: int
    tls: bool

    def environment(self, user="postgres", password="", **extra):
        result = dict(os.environ, WEAVE_PG_HOSTADDR="127.0.0.1", WEAVE_PG_HOST="localhost",
            WEAVE_PG_PORT=str(self.port), WEAVE_PG_DATABASE="postgres", WEAVE_PG_USER=user,
            WEAVE_PG_PASSWORD=password, WEAVE_PG_SSLMODE="verify-full" if self.tls else "disable")
        if self.tls:
            result["WEAVE_PG_SSLROOTCERT"] = str(self.directory / "data/server.crt")
        result.update(extra)
        return result

    def sql(self, statement):
        return subprocess.check_output([str(self.binary_dir / "psql"), "-X", "-h", "127.0.0.1", "-p", str(self.port),
            "-U", "postgres", "-d", "postgres", "-v", "ON_ERROR_STOP=1", "-At", "-c", statement],
            env=dict(os.environ, PGSSLMODE="disable", PGAPPNAME="weave-test-admin"), text=True)

@contextmanager
def cluster(binary_dir=None, tls=False):
    if binary_dir is None:
        binary_dir = subprocess.check_output(["pg_config", "--bindir"], text=True).strip()
    binary_dir = Path(binary_dir)
    if not (binary_dir / "postgres").is_file():
        raise RuntimeError("PostgreSQL server binaries are required for real-database tests")
    with tempfile.TemporaryDirectory(prefix="weave-platform-pg-") as temporary:
        directory = Path(temporary)
        data = directory / "data"
        with socket.socket() as listener:
            listener.bind(("127.0.0.1", 0)); port = listener.getsockname()[1]
        subprocess.run([str(binary_dir / "initdb"), "-D", str(data), "-U", "postgres", "-A", "trust", "--no-locale", "-E", "UTF8"],
            check=True, stdout=subprocess.DEVNULL)
        options = f"-h 127.0.0.1 -p {port} -k {directory}"
        if tls:
            subprocess.run(["openssl", "req", "-x509", "-newkey", "rsa:2048", "-nodes", "-days", "1", "-subj", "/CN=localhost",
                "-addext", "subjectAltName=DNS:localhost", "-addext", "basicConstraints=critical,CA:TRUE",
                "-keyout", str(data / "server.key"), "-out", str(data / "server.crt")],
                check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            (data / "server.key").chmod(0o600)
            options += " -c ssl=on"
        started = False
        try:
            subprocess.run([str(binary_dir / "pg_ctl"), "-D", str(data), "-l", str(directory / "server.log"), "-w", "-o", options, "start"],
                check=True, stdout=subprocess.DEVNULL)
            started = True
            yield Cluster(directory, binary_dir, port, tls)
        finally:
            if started:
                subprocess.run([str(binary_dir / "pg_ctl"), "-D", str(data), "-m", "immediate", "-w", "stop"],
                    check=True, stdout=subprocess.DEVNULL)
