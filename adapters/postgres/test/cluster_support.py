"""Disposable localhost-only PostgreSQL test clusters; never starts a service."""
from contextlib import contextmanager
from dataclasses import dataclass
import os
from pathlib import Path
import shutil
import socket
import subprocess
import sys
import tempfile


def _bounded_tail(stream, limit):
    """Read at most limit bytes even if the child/server produced a large log."""
    stream.seek(0, os.SEEK_END)
    size = stream.tell()
    stream.seek(max(0, size - limit))
    text = stream.read(limit).decode("utf-8", errors="replace")
    return (f"[last {limit} of {size} bytes]\n" if size > limit else "") + text


def _certificate_openssl(binary_dir):
    """Use the PostgreSQL-linked Homebrew provider, rather than Apple LibreSSL."""
    override = os.environ.get("WEAVE_TEST_OPENSSL")
    if override:
        executable = shutil.which(override)
        if executable is None:
            raise RuntimeError("WEAVE_TEST_OPENSSL must name an available OpenSSL executable")
        return Path(executable)
    if sys.platform == "darwin":
        linked = subprocess.check_output(["otool", "-L", str(binary_dir / "postgres")], text=True, timeout=10)
        for line in linked.splitlines()[1:]:
            library = line.strip().split(" (compatibility version", 1)[0]
            if Path(library).name.startswith("libssl."):
                executable = Path(library).parent.parent / "bin/openssl"
                if executable.is_file():
                    return executable
        raise RuntimeError("cannot locate PostgreSQL's OpenSSL tool; set WEAVE_TEST_OPENSSL explicitly")
    executable = shutil.which("openssl")
    if executable is None:
        raise RuntimeError("an OpenSSL executable is required for PostgreSQL TLS fixtures")
    return Path(executable)


def _certificate_diagnostics(binary_dir, data):
    """Only public metadata: never read or print server.key."""
    try:
        executable = _certificate_openssl(binary_dir)
        commands = [
            ("certificate OpenSSL provider", [str(executable), "version", "-a"], 4096),
            ("public certificate", [str(executable), "x509", "-in", str(data / "server.crt"),
                "-noout", "-subject", "-issuer", "-dates", "-sha256", "-fingerprint",
                "-ext", "subjectAltName,basicConstraints,keyUsage,extendedKeyUsage"], 8192),
        ]
        for label, command, limit in commands:
            print(f"--- {label}: {executable} (bounded) ---", file=sys.stderr)
            with tempfile.TemporaryFile() as output:
                try:
                    subprocess.run(command, check=True, stdout=output, stderr=output, timeout=10)
                except (subprocess.SubprocessError, OSError) as error:
                    print(f"diagnostic command failed: {error}", file=sys.stderr)
                print(_bounded_tail(output, limit), file=sys.stderr)
    except (subprocess.SubprocessError, OSError, RuntimeError) as error:
        print(f"certificate diagnostics unavailable: {error}", file=sys.stderr)
    sys.stderr.flush()


def create_tls_certificate(binary_dir, data):
    """Create one explicit extension profile, independent of global req config.

    LibreSSL can append a second basicConstraints when the host req config
    already enables v3_ca and -addext supplies it again. OpenSSL rejects that
    certificate as invalid. Keep each extension in this fixture-only section,
    use SHA256/RSA2048, and verify server purpose and localhost before startup.
    """
    executable = _certificate_openssl(binary_dir)
    config = data / "fixture-openssl.cnf"
    config.write_text("""[req]
prompt = no
distinguished_name = subject
x509_extensions = server_certificate
[subject]
CN = localhost
[server_certificate]
subjectAltName = DNS:localhost
basicConstraints = critical,CA:TRUE
keyUsage = critical,digitalSignature,keyEncipherment,keyCertSign
extendedKeyUsage = serverAuth
subjectKeyIdentifier = hash
""")
    commands = [
        [str(executable), "req", "-config", str(config), "-extensions", "server_certificate",
            "-x509", "-newkey", "rsa:2048", "-nodes", "-sha256", "-days", "1",
            "-keyout", str(data / "server.key"), "-out", str(data / "server.crt")],
        [str(executable), "verify", "-purpose", "sslserver", "-verify_hostname", "localhost",
            "-CAfile", str(data / "server.crt"), str(data / "server.crt")],
    ]
    for command in commands:
        with tempfile.TemporaryFile() as output:
            try:
                subprocess.run(command, check=True, stdout=output, stderr=output, timeout=30)
            except (subprocess.SubprocessError, OSError):
                print("--- certificate fixture command failed (bounded) ---", file=sys.stderr)
                print(_bounded_tail(output, 4096), file=sys.stderr)
                _certificate_diagnostics(binary_dir, data)
                raise
    (data / "server.key").chmod(0o600)
    _certificate_diagnostics(binary_dir, data)


def start_postgres(binary_dir, data, log, options):
    """Start one private cluster, exposing bounded logs before temp cleanup.

    pg_ctl's generic failure omits the server's actual bind/IPC/TLS error.
    Keep the original exception and emit only bounded diagnostic tails. The
    parent deadline also handles an inherited excessive PGCTLTIMEOUT. A server
    that started after pg_ctl timed out must not outlive its private directory.
    """
    command = [str(binary_dir / "pg_ctl"), "-D", str(data), "-l", str(log),
               "-w", "-o", options, "start"]
    with tempfile.TemporaryFile() as control:
        try:
            subprocess.run(command, check=True, stdout=control, stderr=control, timeout=75)
        except (subprocess.SubprocessError, OSError):
            print(f"PostgreSQL startup failed: data={data}, options={options[:4096]}", file=sys.stderr)
            print("--- pg_ctl output (bounded) ---", file=sys.stderr)
            print(_bounded_tail(control, 4096), file=sys.stderr)
            for label, path, limit in [("server log", log, 16384), ("postmaster.pid", data / "postmaster.pid", 1024)]:
                print(f"--- {label} (bounded) ---", file=sys.stderr)
                try:
                    with path.open("rb") as stream:
                        print(_bounded_tail(stream, limit), file=sys.stderr)
                except OSError as error:
                    print(f"unavailable: {error}", file=sys.stderr)
            if (data / "server.crt").is_file():
                _certificate_diagnostics(binary_dir, data)
            sys.stderr.flush()
            if (data / "postmaster.pid").is_file():
                with tempfile.TemporaryFile() as cleanup:
                    try:
                        subprocess.run([str(binary_dir / "pg_ctl"), "-D", str(data), "-m", "immediate", "-w", "stop"],
                                       check=True, stdout=cleanup, stderr=cleanup, timeout=15)
                    except (subprocess.SubprocessError, OSError):
                        print("--- failed-start cleanup output (bounded) ---", file=sys.stderr)
                        print(_bounded_tail(cleanup, 4096), file=sys.stderr, flush=True)
            raise

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
        binary_dir = os.environ.get("WEAVE_PG_BIN") or subprocess.check_output(["pg_config", "--bindir"], text=True).strip()
    binary_dir = Path(binary_dir)
    if not (binary_dir / ("postgres.exe" if os.name == "nt" else "postgres")).is_file():
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
            create_tls_certificate(binary_dir, data)
            options += " -c ssl=on"
        started = False
        try:
            start_postgres(binary_dir, data, directory / "server.log", options)
            started = True
            yield Cluster(directory, binary_dir, port, tls)
        finally:
            if started:
                subprocess.run([str(binary_dir / "pg_ctl"), "-D", str(data), "-m", "immediate", "-w", "stop"],
                    check=True, stdout=subprocess.DEVNULL)
