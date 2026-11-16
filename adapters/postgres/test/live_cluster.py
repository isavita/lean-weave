#!/usr/bin/env python3
"""Run real PostgreSQL acceptance inside a disposable localhost cluster."""
import argparse
import json
import os
import pathlib
import shutil
import socket
import subprocess
import tempfile
import time
import urllib.parse
import urllib.request


def wait_for_database_disconnects(binary_dir, port, timeout=5):
    """Require remote backend cleanup after local owner finalizers have run.

    Earlier cancelled pg_sleep requests use the same application_name. Their
    server backends can remain active until PostgreSQL observes the disconnect
    or its statement deadline, even after the client's descriptors are closed.
    Fresh psql sessions avoid reusing a cached statistics snapshot.
    """
    query = (
        "SELECT COALESCE(json_agg(json_build_object('pid', pid, 'state', state, "
        "'wait_event_type', wait_event_type, 'wait_event', wait_event)), '[]'::json) "
        "FROM pg_stat_activity WHERE application_name = 'lean-weave' AND pid <> pg_backend_pid()"
    )
    command = [str(binary_dir / "psql"), "-X", "-w", "-h", "127.0.0.1", "-p", str(port),
               "-U", "postgres", "-d", "postgres", "-At", "-c", query]
    environment = dict(os.environ, PGSSLMODE="disable", PGAPPNAME="leanweave-shutdown-observer")
    started = time.monotonic()
    deadline = started + timeout
    observations = 0
    initial = None
    pending = []
    while True:
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            raise AssertionError("managed worker drain left database backends after "
                                 f"{timeout}s: {json.dumps(pending)}")
        pending = json.loads(subprocess.check_output(command, env=environment, text=True, timeout=remaining))
        observations += 1
        if initial is None:
            initial = pending
        if not pending:
            return {"deadline_seconds": timeout, "observations": observations,
                    "elapsed_seconds": round(time.monotonic() - started, 6), "initial_backends": initial}
        time.sleep(min(.05, max(0, deadline - time.monotonic())))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--pg-bin", default=os.environ.get("WEAVE_PG_BIN", ""))
    parser.add_argument("--report", default="test/live-result.json")
    parser.add_argument("--tls", action="store_true", help="verify a disposable CA and localhost certificate")
    parser.add_argument("--example", action="store_true", help="test worker-local pools through the HTTP example and drain")
    args = parser.parse_args()
    binary_dir = pathlib.Path(args.pg_bin) if args.pg_bin else pathlib.Path(shutil.which("pg_config") or "pg_config").parent
    if not (binary_dir / "postgres").exists():
        raise SystemExit("set --pg-bin to a PostgreSQL server installation's bin directory")
    with tempfile.TemporaryDirectory(prefix="leanweave-pg-") as directory:
        base = pathlib.Path(directory)
        data = base / "data"
        log = base / "postgres.log"
        with socket.socket() as reservation:
            reservation.bind(("127.0.0.1", 0))
            port = reservation.getsockname()[1]
        subprocess.run([str(binary_dir / "initdb"), "-D", str(data), "-U", "postgres", "-A", "trust", "--no-locale", "-E", "UTF8"],
                       check=True, stdout=subprocess.DEVNULL)
        options = f"-h 127.0.0.1 -p {port} -k {directory}"
        if args.tls:
            subprocess.run(["openssl", "req", "-x509", "-newkey", "rsa:2048", "-nodes", "-days", "1",
                            "-subj", "/CN=localhost", "-addext", "subjectAltName=DNS:localhost",
                            "-addext", "basicConstraints=critical,CA:TRUE",
                            "-keyout", str(data / "server.key"), "-out", str(data / "server.crt")],
                           check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            (data / "server.key").chmod(0o600)
            options += " -c ssl=on"
        started = False
        try:
            subprocess.run([str(binary_dir / "pg_ctl"), "-D", str(data), "-l", str(log), "-w", "-o",
                            options, "start"], check=True, stdout=subprocess.DEVNULL)
            started = True
            environment = dict(os.environ, WEAVE_PG_HOSTADDR="127.0.0.1", WEAVE_PG_HOST="localhost",
                               WEAVE_PG_USER="postgres", WEAVE_PG_DATABASE="postgres", WEAVE_PG_SSLMODE="disable")
            if args.tls:
                environment.update(WEAVE_PG_SSLMODE="verify-full", WEAVE_PG_SSLROOTCERT=str(data / "server.crt"))
            subprocess.run([str(pathlib.Path(".lake/build/bin/postgres-test").resolve()), str(port)], check=True,
                           env=environment, timeout=30)
            if args.tls:
                invalid_identity = dict(environment, WEAVE_PG_HOST="wrong.example", WEAVE_PG_EXPECT_CONNECT_FAILURE="1")
                subprocess.run([str(pathlib.Path(".lake/build/bin/postgres-test").resolve()), str(port)], check=True,
                               env=invalid_identity, timeout=10)
            if args.example:
                with socket.socket() as reservation:
                    reservation.bind(("127.0.0.1", 0))
                    web_port = reservation.getsockname()[1]
                web_environment = dict(environment, WEAVE_PG_PORT=str(port), WEAVE_PORT=str(web_port), WEAVE_WORKERS="2")
                process = subprocess.Popen([str(pathlib.Path(".lake/build/bin/postgres-example").resolve())], env=web_environment,
                                           stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
                try:
                    value = "parameter '<tag>& €"
                    url = f"http://127.0.0.1:{web_port}/?" + urllib.parse.urlencode({"value": value}, quote_via=urllib.parse.quote)
                    deadline = time.monotonic() + 10
                    while True:
                        try:
                            with urllib.request.urlopen(url, timeout=2) as response:
                                assert json.load(response) == {"value": value}
                            break
                        except OSError:
                            if process.poll() is not None or time.monotonic() >= deadline:
                                raise
                            time.sleep(0.02)
                    for _ in range(16):
                        with urllib.request.urlopen(url, timeout=2) as response:
                            assert json.load(response) == {"value": value}
                finally:
                    process.terminate()
                    stdout, stderr = process.communicate(timeout=10)
                    if process.returncode != 0:
                        raise AssertionError(f"managed example shutdown failed ({process.returncode}): {stderr}\n{stdout}")
                shutdown_observation = wait_for_database_disconnects(binary_dir, port)
            version = subprocess.check_output([str(binary_dir / "postgres"), "--version"], text=True).strip()
            report = {"passed": True, "kind": "real PostgreSQL in disposable localhost cluster", "version": version,
                      "parameter_separation": True, "transaction_rollback_commit": True, "migration_idempotence": True,
                      "row_limit_reconnection": True, "result_bytes_limit_reconnection": True,
                      "timeout_reconnection": True, "bounded_waiters_cancellation": True,
                      "active_query_disconnect_cancellation": True,
                      "migration_checksum_drift_rejected": True, "tls_verified": args.tls,
                      "tls_hostname_mismatch_rejected": args.tls, "managed_http_pool_shutdown": args.example,
                      "persistent_service_started": False}
            if args.example:
                report["managed_shutdown_observation"] = shutdown_observation
            pathlib.Path(args.report).write_text(json.dumps(report, indent=2) + "\n")
            print(json.dumps(report, indent=2))
        finally:
            if started:
                subprocess.run([str(binary_dir / "pg_ctl"), "-D", str(data), "-w", "-m", "immediate", "stop"],
                               check=True, stdout=subprocess.DEVNULL)


if __name__ == "__main__":
    main()
