# lean-weave-postgres

Optional PostgreSQL adapter. The main `lean-weave` package has no database dependency.
This package requires `pg_config`, libpq headers and libraries. It uses libpq's
asynchronous connection and extended-query APIs on the reactor's owning thread;
it creates no query tasks and performs no blocking DNS lookup.

```sh
# Ubuntu: apt-get install libpq-dev
# macOS: brew install libpq; export PATH="$(brew --prefix libpq)/bin:$PATH"
# MSYS2 MINGW64: pacman -S mingw-w64-x86_64-postgresql
lake build postgres-test postgres-example
python3 test/wire_fixture.py
```

On macOS the inherited lean-net build also needs Homebrew zlib and Brotli; set
`PKG_CONFIG_PATH="$(brew --prefix zlib)/lib/pkgconfig:$(brew --prefix brotli)/lib/pkgconfig"`.
On Windows put MinGW's `bin` directory (including libpq and its DLL dependencies)
on PATH. Windows uses `libpq.dll.a`; Linux/macOS use the native libpq shared library.

## Queries and worker lifecycle

```lean
import LeanWeavePostgres
import LeanWeave.Core.Server
open LeanWeave

def example : IO Unit := do
  let database : Postgres.Config := {
    hostAddr := "127.0.0.1", hostName := "db.example.com",
    database := "app", user := "app", sslRootCert := "/etc/app/database-ca.pem" }
  Server.serveManaged (fun _ run => do
    let pool ← Postgres.Pool.create database
    try
      run fun _ => do
        let result ← pool.query "SELECT $1::text AS value" #[some "user input"]
        return Response.text result.commandTag
    finally pool.close) { appRetainedBytes := database.retainedBytes }
```

Create one pool per reactor worker. `serveManaged` keeps the pool bracket open
until that worker has drained; `finally pool.close` releases all database sockets
on their owner. Native handles reject cross-thread calls. Do not retain a leased
`Connection` outside `withConnection`/`transaction`; a released lease rejects queries.

SQL and `Array (Option String)` parameters are sent separately. `none` is SQL NULL;
`some ""` is an empty string. Never concatenate untrusted values into SQL. Identifiers
and migration statements are trusted application code. One query is one PostgreSQL
statement; there is no implicit retry.

`Config.fromEnv` reads `WEAVE_PG_HOSTADDR`, `HOST`, `PORT`, `DATABASE`, `USER`,
`PASSWORD`, `SSLMODE`, `SSLROOTCERT`, `MAX_CONNECTIONS`, `MAX_WAITERS`,
`CONNECT_TIMEOUT_MS`, `QUERY_TIMEOUT_MS` and `MAX_RESULT_BYTES` (all with the
`WEAVE_PG_` prefix). `HOSTADDR` must be a numeric IP; `HOST` supplies the certificate
hostname without triggering DNS. The TLS default is `verify-full`. Use a trusted CA
bundle and the server's certificate hostname. `disable` is available for explicit
local/test configurations; automatic TLS downgrade modes are rejected.

## Bounds and cancellation

Pools cap connections and waiting leases. Full wait queues return 503. Connecting,
waiting and queries have deadlines; disconnect cancellation closes active database
sockets and frees leases. A server-side `statement_timeout` and
`idle_in_transaction_session_timeout` provide additional operation limits. Results
use libpq single-row mode and cap rows, columns and retained application bytes;
query and parameter sizes are bounded before sending.

Set the web server's application reservation to at least `Config.retainedBytes`,
and size lean-net's total deferred budget for the desired concurrency. HTTP request
memory is charged separately by lean-net. The adapter's result budget includes
conservative row/object overhead, rather than only cell text.

libpq can transiently buffer one oversized server row before the adapter can reject
it. The retained result limit is **not** a hard bound on libpq's internal allocations.
Select bounded columns/pages and enforce database value-size constraints where a
strict native memory ceiling matters. Polling is capped at 16 available results per
callback; result arrival is checked cooperatively without blocking the worker.

Cancellation drops the connection instead of using libpq's blocking cancel API.
PostgreSQL rolls back an open transaction when it observes the disconnect; the
server-side statement deadline also limits abandoned work. If a connection fails
while COMMIT or an autocommit mutation is in flight, commit outcome can be unknown;
applications needing safe retries should use idempotency keys.

## Transactions and migrations

`pool.transaction (fun connection => ...)` retains one lease, issues BEGIN, commits
on success, and rolls back application errors. Broken/cancelled connections are
dropped. Isolation choices are read committed, repeatable read and serializable.

`pool.migrate #[{ version := 1, statements := #["CREATE TABLE widgets (id bigint PRIMARY KEY)"] }]`
serializes runners with transaction-scoped advisory lock `549589292`. Versions and
SHA-256 statement checksums live in `public.leanweave_schema_migrations`; reusing an
applied version with different SQL fails. All pending DDL and metadata commit in one
transaction. Migration statements must support transactions (for example, use a
separate operational procedure for `CREATE INDEX CONCURRENTLY`). Run migrations
using a dedicated schema-owner role; application pools should use narrower DML roles.

## Acceptance

`test/wire_fixture.py` uses actual libpq against a controlled PostgreSQL v3 wire
peer to test framing, parameter separation, pool saturation, cancellation, and
timeout/result-limit recovery. It does not claim SQL-engine acceptance.

Run the same native tests against a real server with
`WEAVE_PG_USER=postgres WEAVE_PG_PASSWORD=... .lake/build/bin/postgres-test 5432`.
The test database must be disposable: tests create adapter tables and migrations.
An isolated cluster helper starts and stops a temporary localhost-only PostgreSQL
installation without enabling a persistent service:

```sh
python3 test/live_cluster.py --pg-bin /path/to/postgresql/bin --tls --example
```

The helper generates an ephemeral CA/server certificate, verifies TLS and rejects
a mismatched hostname. `--example` also runs a two-worker HTTP application and checks
that managed shutdown leaves no database connections. Committed JSON reports record
the locally tested layers.
