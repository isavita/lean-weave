# Runnable LeanWeave applications

Install the [native prerequisites](../docs/getting-started.md#prerequisites)
and build from the repository root.

```sh
lake build weave weave-pages weave-forms weave-crud weave-dashboard
```

Each application accepts `--routes` and uses `WEAVE_HOST` (default `127.0.0.1`),
`WEAVE_PORT` (default `8080`), `WEAVE_WORKERS` (examples default to one),
`WEAVE_APP_TIMEOUT_MS` and `WEAVE_APP_RETAINED_BYTES`. Shutdown uses lean-net's
graceful drain.

| Application | Run | Try at `http://127.0.0.1:8080` |
| --- | --- | --- |
| [Pages](Apps/Pages.lean) | `lake exe weave-pages` | Open `/` for the layout and versioned stylesheet. Visit `/hello/Lean` for a typed route using a compiled template. |
| [Forms/login](Apps/Forms.lean) | `WEAVE_DEMO_PASSWORD='a-long-demo-secret' lake exe weave-forms` | Open `/login`, sign in as `demo` with the configured password, then sign out from `/account`. |
| [CRUD](Apps/Crud.lean) | `lake exe weave-crud` | Open `/posts`, create a post, then edit or delete it. Each save checks the matching revision. |
| [Dashboard](Apps/Dashboard.lean) | `lake exe weave-dashboard` | Refresh `/` within two seconds to see a reused response. Change the query string to see a new cache entry. |

The pages and dashboard examples also accept `Accept: application/json` at
`/hello/Lean` and `/`, respectively. For the dashboard, use `curl -I` with the
same query string and `Accept` header to see the cached representation's
headers. Pass its ETag as `If-None-Match` to receive a bodyless 304 while the
entry is fresh.

Use a separate port when running applications together. The login app has one
configured demo account, `demo`; it requires `WEAVE_DEMO_PASSWORD` of at least
12 characters and has no built-in password. Applications keep all data in memory.
The CRUD board is public shared sample data, with at most 256 posts and 256 bytes
per title. Updates require the last observed `revision`; stale writes return 409.

Keep the forms and CRUD examples on loopback. They use an HTTP development
cookie named `weave_dev` with HttpOnly and SameSite=Lax. Deployed applications
should use TLS and the default Secure `__Host-weave` cookie. For form and CRUD
mutations, the browser Origin must match the configured listener exactly. The
default is `http://127.0.0.1:8080`.

## CLI and development loop

Follow the [scaffold guide](../docs/getting-started.md#scaffold-a-project-with-weave)
for a separate Lake application. Project commands accept an optional Lake
executable target. To list routes or develop an example in this repository:

```sh
lake exe weave routes weave-crud
lake exe weave dev weave-pages
```

`dev` watches at most 10,000 Lean and configuration files. It runs one build at
a time with a ten-minute limit and allows five seconds for graceful child shutdown.

## Real network acceptance

Use Python 3.10 or newer for the pinned HTTP/2 test libraries.

```sh
python3 -m venv .lake/test-venv
.lake/test-venv/bin/python -m pip install --require-hashes -r scripts/requirements-test.txt
lake env .lake/test-venv/bin/python scripts/test_examples.py
lake env .lake/test-venv/bin/python scripts/test_tooling.py
```

On Windows use `.lake/test-venv/Scripts/python.exe`.

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
