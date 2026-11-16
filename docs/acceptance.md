# Acceptance and reproducible measurements

Run from the repository root with the documented native development libraries:

```sh
lake build LeanWeave weave weave-test weave-pages weave-forms weave-crud weave-dashboard weave-wire-fixture weave-bench
lake exe weave-test
python3 -m venv .lake/test-venv
.lake/test-venv/bin/python -m pip install --require-hashes -r scripts/requirements-test.txt
lake env .lake/test-venv/bin/python scripts/test_wire.py -v
lake env .lake/test-venv/bin/python scripts/test_examples.py -v
lake env .lake/test-venv/bin/python scripts/test_tooling.py
python3 scripts/benchmark.py --iterations 20000 --output benchmarks/results.json
```

On Windows use `.lake/test-venv/Scripts/python.exe`. `lake env` supplies the native
library search path. Python3.14 is used by CI and supports the pinned HTTP/2 test
libraries. HTTP/2 clients use independent hpack/h2 implementations; these checks
use real TCP, not browser emulation.

`weave-test` covers route precedence, host groups, typed reverses, compiler
rejections, single decoding, XSS and unsafe template failures, body limits,
validation, CSRF/auth, sessions, cache isolation/eviction/freshness/invalidation,
cooperative sequential awaiting, cancellation and startup validation.

`test_wire.py` covers the complete application adapter on HTTP/1 and HTTP/2:
status/header/body parity, early header rejection before 100 Continue or body
intake, HEAD lengths, conditional GET, cache bypass, HTTP/1 pipeline order,
HTTP/2 independent completion and stream cancellation. Native malformed protocol
messages may produce different transport errors (HTTP/1 status versus HTTP/2
RST/GOAWAY); those belong to lean-net's protocol acceptance suite.

`test_examples.py` covers all four apps over both protocols: pages/XSS/routes,
login/CSRF/session rotation/old-cookie rejection/flash/logout, bounded CRUD and
revision conflicts, cached dashboard isolation and validators. `test_tooling.py`
creates a fresh application, builds/tests/lists routes, observes a real
rebuild/restart, preserves the old process on a compilation error, recovers and
stops gracefully.

The optional adapter test directories contain real-server and controlled-wire
acceptance layers with explicit bounds, cancellation and TLS checks. PostgreSQL
reports distinguish a real SQL engine from the wire fixture. Redis tests use a
disposable local server. No persistent service is started by these scripts.

CI runs the core/application tests on Linux, macOS and Windows. Adapter CI runs
real PostgreSQL/Redis on Linux and macOS, plus Windows compilation and protocol
fixtures. Documentation builds with pinned doc-gen4 and publishes through GitHub
Pages. Check each workflow's current result before asserting platform acceptance.

## Allocation counts and benchmark scope

`benchmarks/results.json` records the exact source revision, dirty flag, toolchain,
platform, iteration count and checksums. Timing uses native monotonic nanoseconds.
Small-allocation counts use `IO.getNumHeartbeats`, Lean's thread-local count of
small memory allocations. They include loop and IO scaffolding; they exclude
large ByteArray/native allocator traffic, total allocated bytes and process RSS.
These counts must not be described as total allocations or memory consumption.

Samples warm up literal initialization and measure compiled HTML, full
path/query decode plus typed route/render, and fragment-cache hits. Each result
feeds a checksum so work remains observable. These are single-process core
microbenchmarks, not production throughput, TLS, HTTP/2 network latency or a
comparison against another framework. Run on controlled hardware and repeat
samples for comparisons; CI uploads measurements without flaky timing thresholds.

## Network microbenchmark

Build `weave-pages` using the command above, then run the existing executable:

```sh
lake env .lake/test-venv/bin/python scripts/benchmark_http.py --iterations 1000 --warmup 100 --output benchmarks/http-results.json
```

The script performs no build. It starts a one-worker pages application on an
ephemeral loopback port, validates every HTTP 200 response and body SHA-256, and
measures one persistent HTTP/1.1 connection followed by one cleartext HTTP/2
connection with sequential streams. Each protocol has one priming request and
the configured warmup, excluded from measurements. Iterations default to 1,000
and are capped at 100,000; `--workers`, `--warmup`, and `--app wire` permit explicit
variants. Clients use `TCP_NODELAY`; concurrency is one, without pipelining or
multiplexed load. Shutdown must drain successfully and exit zero.

`benchmarks/http-results.json` records requests/second, payload throughput,
nearest-rank p50/p95/p99 request latency, body and aggregate checksums, host and
client-library details, binary hash, source commit, and dirty flag. These are
end-to-end loopback measurements including Python parsing, HPACK, checksum work,
flow-control acknowledgments, and OS scheduling. They exclude connection/settings
setup and TLS, and do not establish saturation throughput, remote-network latency,
allocation counts, or comparisons with other frameworks. Repeat on controlled
hardware and a clean source revision when comparing changes.
