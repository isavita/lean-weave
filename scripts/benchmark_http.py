#!/usr/bin/env python3
"""Reproduce a concurrency-one loopback HTTP benchmark without rebuilding.

This measures the compiled application and a Python client together. It is not a
server saturation benchmark: client parsing, checksums and scheduling are included.
"""
import argparse
from datetime import datetime, timezone
import hashlib
import http.client
import importlib.metadata
import json
import math
import os
from pathlib import Path
import platform
import signal
import socket
import subprocess
import sys
import tempfile
import time

from h2.config import H2Configuration
from h2.connection import H2Connection
from h2.events import (ConnectionTerminated, DataReceived, RemoteSettingsChanged,
                       ResponseReceived, SettingsAcknowledged, StreamEnded, StreamReset)


ROOT = Path(__file__).resolve().parents[1]
MAX_BODY = 1024 * 1024


def bounded_integer(low, high):
    def parse(value):
        number = int(value)
        if not low <= number <= high:
            raise argparse.ArgumentTypeError(f"expected an integer from {low} to {high}")
        return number
    return parse


def unused_port():
    with socket.socket() as reservation:
        reservation.bind(("127.0.0.1", 0))
        return reservation.getsockname()[1]


class Http1:
    def __init__(self, port, target, timeout):
        self.target = target
        self.authority = f"localhost:{port}"
        self.connection = http.client.HTTPConnection("127.0.0.1", port, timeout=timeout)
        self.connection.connect()
        self.peer = self.connection.sock
        self.peer.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)

    def exchange(self):
        if self.connection.sock is not self.peer:
            raise RuntimeError("HTTP/1 connection was replaced during a keepalive sample")
        self.connection.request("GET", self.target, headers={"Host": self.authority, "Connection": "keep-alive"})
        response = self.connection.getresponse()
        if response.version != 11 or response.will_close:
            raise RuntimeError("expected a persistent HTTP/1.1 response")
        body = response.read(MAX_BODY + 1)
        if len(body) > MAX_BODY:
            raise RuntimeError("benchmark response exceeds one MiB")
        if self.connection.sock is not self.peer:
            raise RuntimeError("HTTP/1 sample unexpectedly reconnected")
        return response.status, body

    def close(self):
        self.connection.close()


class Http2:
    def __init__(self, port, target, timeout):
        self.target, self.authority, self.timeout = target, f"localhost:{port}", timeout
        self.peer = socket.create_connection(("127.0.0.1", port), timeout=timeout)
        self.peer.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
        self.connection = H2Connection(config=H2Configuration(client_side=True, header_encoding="utf-8"))
        self.stream = 1
        try:
            self.connection.initiate_connection()
            self.flush()
            settings = acknowledged = False
            deadline = time.monotonic() + timeout
            while not (settings and acknowledged):
                for event in self.receive(deadline):
                    settings |= isinstance(event, RemoteSettingsChanged)
                    acknowledged |= isinstance(event, SettingsAcknowledged)
                self.flush()
        except BaseException:
            self.close()
            raise

    def flush(self):
        outbound = self.connection.data_to_send()
        if outbound:
            self.peer.sendall(outbound)

    def receive(self, deadline):
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            raise TimeoutError("HTTP/2 exchange exceeded its deadline")
        self.peer.settimeout(remaining)
        data = self.peer.recv(65536)
        if not data:
            raise EOFError("HTTP/2 connection closed before stream completion")
        events = self.connection.receive_data(data)
        for event in events:
            if isinstance(event, (ConnectionTerminated, StreamReset)):
                raise RuntimeError(f"HTTP/2 connection/stream aborted: {event}")
        return events

    def exchange(self):
        stream = self.stream
        self.stream += 2
        self.connection.send_headers(stream, [(":method", "GET"), (":scheme", "http"),
            (":authority", self.authority), (":path", self.target)], end_stream=True)
        self.flush()
        status, ended, body = None, False, bytearray()
        deadline = time.monotonic() + self.timeout
        while not ended:
            for event in self.receive(deadline):
                if isinstance(event, ResponseReceived):
                    if event.stream_id != stream or status is not None:
                        raise RuntimeError("unexpected HTTP/2 response stream or duplicate headers")
                    statuses = [value for name, value in event.headers if name == ":status"]
                    if len(statuses) != 1:
                        raise RuntimeError("expected exactly one HTTP/2 :status")
                    status = int(statuses[0])
                elif isinstance(event, DataReceived):
                    if event.stream_id != stream:
                        raise RuntimeError("unexpected concurrent HTTP/2 stream")
                    body.extend(event.data)
                    if len(body) > MAX_BODY:
                        raise RuntimeError("benchmark response exceeds one MiB")
                    self.connection.acknowledge_received_data(event.flow_controlled_length, stream)
                elif isinstance(event, StreamEnded):
                    if event.stream_id != stream:
                        raise RuntimeError("unexpected completed HTTP/2 stream")
                    ended = True
            self.flush()
        if status is None:
            raise RuntimeError("HTTP/2 response ended without headers")
        return status, bytes(body)

    def close(self):
        self.peer.close()


def validate(response, expected):
    status, body = response
    digest = hashlib.sha256(body).digest()
    if status != 200 or (len(body), digest) != expected:
        raise RuntimeError(f"response validation failed: status={status}, bytes={len(body)}, sha256={digest.hex()}")
    return digest


def percentile(ordered, quantile):
    """Nearest-rank quantiles, documented in the output for reproducibility."""
    return ordered[max(0, math.ceil(quantile * len(ordered)) - 1)] / 1000


def measure(client, name, expected, iterations, warmup):
    try:
        validate(client.exchange(), expected)  # Always exclude connection/first-response setup.
        for _ in range(warmup):
            validate(client.exchange(), expected)
        latencies = []
        checksum = hashlib.sha256()
        started = time.perf_counter_ns()
        for _ in range(iterations):
            request_started = time.perf_counter_ns()
            digest = validate(client.exchange(), expected)
            checksum.update(digest)
            latencies.append(time.perf_counter_ns() - request_started)
        elapsed = time.perf_counter_ns() - started
        ordered = sorted(latencies)
        return {
            "protocol": name, "iterations": iterations, "warmup_requests": warmup, "priming_requests": 1,
            "connections": 1, "concurrency": 1, "status": 200, "validated_responses": iterations + warmup + 1,
            "elapsed_seconds": elapsed / 1e9, "requests_per_second": iterations * 1e9 / elapsed,
            "latency_us": {"min": ordered[0] / 1000, "p50": percentile(ordered, .50),
                "p95": percentile(ordered, .95), "p99": percentile(ordered, .99), "max": ordered[-1] / 1000},
            "response_bytes": expected[0], "response_sha256": expected[1].hex(),
            "measured_body_digest_chain_sha256": checksum.hexdigest(),
            "response_payload_bytes_per_second": expected[0] * iterations * 1e9 / elapsed,
        }
    finally:
        client.close()


def stop_server(server, log):
    if server.poll() is None:
        server.send_signal(signal.CTRL_BREAK_EVENT if os.name == "nt" else signal.SIGINT)
        try:
            server.wait(timeout=7)
        except subprocess.TimeoutExpired:
            server.kill()
            server.wait()
            raise RuntimeError("benchmark server failed to drain within seven seconds")
    if server.returncode != 0:
        log.seek(0)
        raise RuntimeError(f"benchmark server exited {server.returncode}: {log.read()[-8192:]}")


def command_output(command):
    return subprocess.check_output(command, cwd=ROOT, text=True).strip()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--iterations", type=bounded_integer(1, 100000), default=1000)
    parser.add_argument("--warmup", type=bounded_integer(0, 100000), default=100)
    parser.add_argument("--workers", type=bounded_integer(1, 256), default=1)
    parser.add_argument("--timeout", type=bounded_integer(1, 30), default=5)
    parser.add_argument("--app", choices=("pages", "wire"), default="pages")
    parser.add_argument("--binary", type=Path, help="existing executable; no build is performed")
    parser.add_argument("--target", help="ASCII origin-form path; defaults to / (pages) or /fast (wire)")
    parser.add_argument("--output", type=Path, default=ROOT / "benchmarks/http-results.json")
    args = parser.parse_args()
    target = args.target or ("/" if args.app == "pages" else "/fast")
    if not target.startswith("/") or not target.isascii() or any(char.isspace() for char in target):
        parser.error("target must be an ASCII origin-form path without whitespace")
    if args.app == "wire" and args.workers != 1:
        parser.error("wire fixture has one fixed worker; use pages to select workers")
    executable = args.binary or ROOT / (f".lake/build/bin/weave-{'pages' if args.app == 'pages' else 'wire-fixture'}" +
                                        (".exe" if os.name == "nt" else ""))
    executable = executable.resolve()
    if not executable.is_file():
        parser.error(f"missing executable {executable}; build it before running this script")
    port = unused_port()
    environment = dict(os.environ, WEAVE_HOST="127.0.0.1", WEAVE_PORT=str(port), WEAVE_WORKERS=str(args.workers),
                       WEAVE_APP_TIMEOUT_MS="30000", WEAVE_APP_RETAINED_BYTES="65536")
    command = [str(executable)] + ([str(port)] if args.app == "wire" else [])
    with tempfile.TemporaryFile(mode="w+") as log:
        server = subprocess.Popen(command, cwd=ROOT, env=environment, stdout=log, stderr=log,
                                  creationflags=subprocess.CREATE_NEW_PROCESS_GROUP if os.name == "nt" else 0)
        try:
            deadline = time.monotonic() + 10
            while True:
                if server.poll() is not None:
                    raise RuntimeError("benchmark server exited during startup")
                try:
                    client = Http1(port, target, args.timeout)
                    try:
                        status, body = client.exchange()
                    finally:
                        client.close()
                    if status != 200:
                        raise RuntimeError(f"benchmark target returned HTTP {status}")
                    expected = (len(body), hashlib.sha256(body).digest())
                    break
                except OSError:
                    if time.monotonic() >= deadline:
                        raise TimeoutError("benchmark server startup deadline exceeded")
                    time.sleep(.01)
            samples = [
                measure(Http1(port, target, args.timeout), "HTTP/1.1 keepalive", expected, args.iterations, args.warmup),
                measure(Http2(port, target, args.timeout), "HTTP/2 prior knowledge, sequential streams", expected,
                        args.iterations, args.warmup),
            ]
        finally:
            stop_server(server, log)
    processor = platform.processor()
    if sys.platform == "darwin":
        processor = command_output(["sysctl", "-n", "machdep.cpu.brand_string"])
    report = {
        "utc": datetime.now(timezone.utc).isoformat(),
        "command": " ".join(["lake", "env", str(Path(sys.executable).relative_to(ROOT)) if Path(sys.executable).is_relative_to(ROOT) else sys.executable,
                             "scripts/benchmark_http.py", "--iterations", str(args.iterations), "--warmup", str(args.warmup),
                             "--workers", str(args.workers), "--app", args.app]),
        "source_commit": command_output(["git", "rev-parse", "HEAD"]),
        "source_dirty": bool(command_output(["git", "status", "--porcelain"])),
        "binary": str(executable.relative_to(ROOT)) if executable.is_relative_to(ROOT) else str(executable),
        "binary_sha256": hashlib.sha256(executable.read_bytes()).hexdigest(),
        "lean": command_output(["lean", "--version"]),
        "host": {"platform": platform.platform(), "machine": platform.machine(), "processor": processor,
                 "logical_cpus": os.cpu_count(), "python": platform.python_version()},
        "client_dependencies": {name: importlib.metadata.version(name) for name in ("h2", "hpack", "hyperframe")},
        "server": {"application": args.app, "workers": args.workers, "address": "127.0.0.1", "port": port,
                   "target": target, "tls": False, "tcp_nodelay": True, "graceful_exit_code": server.returncode},
        "measurement": "Single Python client and server on loopback TCP, concurrency 1 per protocol, measured sequentially HTTP/1 then HTTP/2. Includes Python HTTP/HPACK parsing, flow-control ACKs, body SHA-256 validation, loop overhead and OS scheduling. Excludes TCP/settings setup, priming and warmup. No TLS, saturation, browser, remote-network, allocation or multi-client claim.",
        "percentiles": "Nearest-rank percentiles over per-request monotonic wall-clock nanoseconds, reported in microseconds.",
        "samples": samples,
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
