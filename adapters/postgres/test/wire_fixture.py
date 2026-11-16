#!/usr/bin/env python3
"""Exercise real libpq I/O against a deliberately small PostgreSQL v3 peer.

This verifies framing, parameter separation, async progress, pool bounds and
cancellation. It does not validate PostgreSQL SQL semantics: use a real server
with postgres-test for that acceptance layer.
"""
import argparse
import hashlib
import json
import pathlib
import socket
import struct
import subprocess
import threading
import time


def frame(tag, payload=b""):
    return tag.encode() + struct.pack("!I", len(payload) + 4) + payload


def receive(connection, size):
    result = b""
    while len(result) < size:
        chunk = connection.recv(size - len(result))
        if not chunk:
            raise EOFError
        result += chunk
    return result


def columns(names):
    return frame("T", struct.pack("!h", len(names)) + b"".join(
        name.encode() + b"\0" + struct.pack("!IhIhih", 0, 0, 25, -1, -1, 0)
        for name in names))


def data(values):
    return frame("D", struct.pack("!h", len(values)) + b"".join(
        struct.pack("!i", -1) if value is None else struct.pack("!I", len(value.encode())) + value.encode()
        for value in values))


class Fixture:
    def __init__(self):
        self.listener = socket.socket()
        self.listener.bind(("127.0.0.1", 0))
        self.listener.listen(16)
        self.port = self.listener.getsockname()[1]
        self.events = []
        self.versions = {}
        self.transaction_rows = []
        self.threads = []
        self.running = True
        self.acceptor = threading.Thread(target=self.accept, daemon=True)
        self.acceptor.start()

    def accept(self):
        while self.running:
            try:
                connection, _ = self.listener.accept()
            except OSError:
                return
            thread = threading.Thread(target=self.client, args=(connection,), daemon=True)
            self.threads.append(thread)
            thread.start()

    def client(self, connection):
        try:
            length = struct.unpack("!I", receive(connection, 4))[0]
            startup = receive(connection, length - 4)
            assert startup[:4] == struct.pack("!I", 196608), "TLS disabled fixture expects startup directly"
            connection.sendall(frame("R", struct.pack("!I", 0)) +
                frame("S", b"client_encoding\0UTF8\0") + frame("S", b"server_version\0" + b"17.5\0") +
                frame("S", b"integer_datetimes\0on\0") + frame("S", b"standard_conforming_strings\0on\0") +
                frame("K", struct.pack("!II", 123, 456)) + frame("Z", b"I"))
            query, params, state = "", [], b"I"
            transaction_rows = None
            while True:
                tag = receive(connection, 1).decode()
                length = struct.unpack("!I", receive(connection, 4))[0]
                body = receive(connection, length - 4)
                if tag == "X":
                    return
                if tag == "P":
                    query = body.split(b"\0", 2)[1].decode()
                elif tag == "B":
                    portal, statement, rest = body.split(b"\0", 2)
                    offset = 0
                    formats = struct.unpack_from("!h", rest, offset)[0]
                    offset += 2 + 2 * formats
                    count = struct.unpack_from("!h", rest, offset)[0]
                    offset += 2
                    params = []
                    for _ in range(count):
                        size = struct.unpack_from("!i", rest, offset)[0]
                        offset += 4
                        params.append(None if size == -1 else rest[offset:offset+size].decode())
                        if size >= 0:
                            offset += size
                elif tag == "S":
                    self.events.append({"query": query, "params": params})
                    response = frame("1") + frame("2")
                    rows, names, command = [], [], "SELECT 1"
                    if query.startswith("BEGIN"):
                        state, command = b"T", "BEGIN"
                        transaction_rows = list(self.transaction_rows)
                    elif query in ("COMMIT", "ROLLBACK"):
                        state, command = b"I", query
                        if query == "COMMIT" and transaction_rows is not None:
                            self.transaction_rows = transaction_rows
                        transaction_rows = None
                    elif query.startswith("CREATE TABLE"):
                        command = "CREATE TABLE"
                    elif query.startswith("INSERT INTO public.leanweave_schema_migrations"):
                        self.versions[params[0]] = hashlib.sha256(params[1].encode()).hexdigest()
                        command = "INSERT 0 1"
                    elif query.startswith("SELECT checksum = "):
                        names = ["matches"]
                        rows = [] if params[0] not in self.versions else [[
                            "t" if self.versions[params[0]] == hashlib.sha256(params[1].encode()).hexdigest() else "f"]]
                    elif query.startswith("INSERT INTO leanweave_adapter_tx"):
                        target = transaction_rows if transaction_rows is not None else self.transaction_rows
                        target.append(params[0])
                        command = "INSERT 0 1"
                    elif query.startswith("DELETE FROM leanweave_adapter_tx"):
                        self.transaction_rows = []
                        command = "DELETE 2"
                    elif query.startswith("SELECT id::text FROM leanweave_adapter_tx"):
                        names, rows = ["id"], [[value] for value in self.transaction_rows]
                    elif query.startswith("SELECT version::text"):
                        names, rows = ["version"], [[version] for version in self.versions]
                    elif query.startswith("SELECT pg_advisory_xact_lock"):
                        names, rows = ["pg_advisory_xact_lock"], [[""]]
                    elif query.startswith("SELECT $1::text"):
                        names, rows = ["value", "optional"], [params]
                    elif query.startswith("SELECT generate_series"):
                        names, rows = ["value"], [[str(n)] for n in range(1, 4)]
                    elif query == "SELECT pg_sleep(1)":
                        time.sleep(1)
                        names, rows = ["pg_sleep"], [[""]]
                    elif query == "SELECT 1":
                        names, rows = ["?column?"], [["1"]]
                    else:
                        raise AssertionError(f"fixture query not implemented: {query}")
                    if names:
                        response += columns(names) + b"".join(data(row) for row in rows)
                        command = f"SELECT {len(rows)}"
                    else:
                        response += frame("n")
                    response += frame("C", command.encode() + b"\0") + frame("Z", state)
                    connection.sendall(response)
        except (EOFError, ConnectionError, OSError):
            pass
        finally:
            connection.close()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--binary", default=".lake/build/bin/postgres-test")
    parser.add_argument("--report", default="test/wire-result.json")
    args = parser.parse_args()
    fixture = Fixture()
    try:
        result = subprocess.run([str(pathlib.Path(args.binary).resolve()), str(fixture.port)], timeout=20)
        assert result.returncode == 0, f"adapter test failed: {result.returncode}"
        queries = [event["query"] for event in fixture.events]
        assert "ROLLBACK" in queries and "COMMIT" in queries
        inserts = [query for query in queries if query.startswith("INSERT INTO public.leanweave_schema_migrations")]
        assert len(inserts) == 1, "migration is applied once"
        parameterized = next(event for event in fixture.events if event["query"].startswith("SELECT $1::text"))
        assert parameterized["params"] == ["'; DROP TABLE users; --", None]
        report = {"passed": True, "kind": "controlled PostgreSQL wire peer, real libpq", "queries": len(queries),
                  "parameter_separation": True, "transaction_rollback_commit": True, "migration_once": True,
                  "live_database_engine": False}
        pathlib.Path(args.report).write_text(json.dumps(report, indent=2) + "\n")
        print(json.dumps(report, indent=2))
    finally:
        fixture.running = False
        fixture.listener.close()


if __name__ == "__main__":
    main()
