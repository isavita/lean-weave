# Runnable LeanWeave applications

Build from the repository root with Lean 4.35.0, zlib and Brotli development
libraries installed. On macOS, select Homebrew zlib before building:

```sh
export PKG_CONFIG_PATH="$(brew --prefix zlib)/lib/pkgconfig:${PKG_CONFIG_PATH:-}"
lake build weave weave-pages weave-forms weave-crud weave-dashboard
```

Each application accepts `--routes` and uses `WEAVE_HOST` (default `127.0.0.1`),
`WEAVE_PORT` (default `8080`), `WEAVE_WORKERS` (examples default to one),
`WEAVE_APP_TIMEOUT_MS` and `WEAVE_APP_RETAINED_BYTES`. Shutdown uses lean-net's
graceful drain.

| Application | Run | Demonstrates |
| --- | --- | --- |
| Pages | `lake exe weave-pages` | Compile-time named routes, typed captures, safe HTML, compiled templates, HTML/JSON negotiation, versioned stylesheet URL |
| Forms/login | `WEAVE_DEMO_PASSWORD='a-long-demo-secret' lake exe weave-forms` | Form validation, field errors, CSRF, session expiry, login ID/token rotation, authorization, flash consumption, logout |
| CRUD | `lake exe weave-crud` | Bounded shared state, create/read/update/delete, JSON and browser forms, CSRF, optimistic revision conflicts |
| Dashboard | `lake exe weave-dashboard` | Worker-local bounded response/fragment caches, isolation dimensions, ETags, conditional GET and HEAD metadata |

Use a separate port when running applications together. The login app has one
configured demo account, `demo`; it requires `WEAVE_DEMO_PASSWORD` of at least
12 characters and has no built-in password. Applications keep all data in memory.
The CRUD board is public shared sample data, with at most 256 posts and 256 bytes
per title. Updates require the last observed `revision`; stale writes return 409.

The HTTP form examples explicitly select a development cookie named `weave_dev`
with HttpOnly and SameSite=Lax. LeanWeave's session defaults use Secure and the
`__Host-weave` name. Use the default secure configuration with TLS in deployed
applications. Unsafe form/CRUD requests must carry an exact matching Origin;
for the default listener that is `http://127.0.0.1:8080`.

## CLI and development loop

The CLI creates a new Lake application using the selected local LeanWeave source
checkout. It never overwrites an existing directory.

```sh
lake exe weave new ../my-app --path "$PWD"
cd ../my-app
/path/to/lean-weave/.lake/build/bin/weave build
/path/to/lean-weave/.lake/build/bin/weave routes
/path/to/lean-weave/.lake/build/bin/weave test
/path/to/lean-weave/.lake/build/bin/weave dev
```

`WEAVE_PATH` can select the source checkout in place of `--path`. Generated
applications include `App.lean`, a server entry point and an application test.
Project commands accept an optional Lake executable target. In this repository:

```sh
lake exe weave routes weave-crud
lake exe weave dev weave-pages
```

`dev` requires Python 3. It watches at most 10,000 Lean/configuration files,
coalesces edits, allows one build at a time and gives builds a ten-minute limit.
It restarts after a successful build, keeps the last good server on build errors,
and allows five seconds for graceful child shutdown before forced termination.
POSIX process groups and Windows console process groups receive their appropriate
shutdown signals. The native application CI matrix targets Linux, macOS and Windows; the watcher
rebuild/restart check was verified locally on macOS.

## Real network acceptance

```sh
python3 -m venv .lake/test-venv
.lake/test-venv/bin/pip install -r scripts/requirements-test.txt
.lake/test-venv/bin/python scripts/test_examples.py
python3 scripts/test_tooling.py
```

The application checks boot all four native servers and run both real HTTP/1 and
HTTP/2 requests. They exercise XSS escaping, routing and HEAD/OPTIONS, session
rotation, CSRF rejection, logout, CRUD conflicts and cache host/query/auth/cookie
isolation. HTTP/2 tests require the `h2` package; `--http1-only` explicitly requests
a reduced run. The tooling check creates an application in a temporary directory,
builds/tests/lists it, edits its source, observes restart, preserves the running
server through a compile failure, recovers, and shuts down the watcher.

The dashboard caches are independent per worker. Values become stale until the
two-second response TTL or one-second fragment TTL expires. Restart discards them.
There is no cross-worker invalidation in this example; applications needing that
must broadcast invalidations or use the shared-store adapters described in the
framework documentation.
