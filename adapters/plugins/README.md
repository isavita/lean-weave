# Native development plugins

`lean-weave-plugins` is an optional package over LeanWeave's owner-local
`LeanWeave.Plugin.Manager`. It loads real shared libraries using `dlopen` with
local symbols on Unix/macOS and restricted `LoadLibraryExW` search on Windows.
It does not use Lean's global plugin initializers, which intentionally remain
loaded for the lifetime of the Lean runtime.

## Loading and lifecycle

Create one manager on each application worker. Define native plugins against
[`include/leanweave_plugin.h`](include/leanweave_plugin.h), exporting
`leanweave_plugin_v1`. The versioned ABI exposes `on_start`, `on_request`, `poll`,
`cancel`, `release` and `on_shutdown`. All callbacks execute on the owner that
started the plugin. Native handles reject operations from another thread.
Callbacks must be bounded and nonblocking. Plugins are trusted native code and
must obey the ABI's allocation, ownership and callback contracts.

```lean
import LeanWeavePlugins
open LeanWeave

-- Run inside the Server.serveApplications owner factory.
def install (manager : Plugin.Manager) (image : System.FilePath) : IO (Except AppError Unit) :=
  manager.reload (Plugins.load image)
```

The manager checks generation capacity **before** executing the loader. The
loader copies the library to a private temporary generation directory and loads
that image, allowing the same source path to be rebuilt/replaced while earlier
versions are still mapped. Shared dependencies must resolve independently of
the copied image directory (for example through appropriate platform runpaths).
The copied image is bounded by `maxLibraryBytes` and is removed after unload.

A candidate's ABI, identity and start hook are checked before the active
generation changes. Failed loads/start hooks preserve the working generation;
partially initialized state is passed to `on_shutdown` for cleanup. Each retained
generation needs a distinct `(name, version)` identity. Reusing an already
retained identity is rejected. Reloads update one worker's manager; applications
with multiple workers must deliver their reload command to each owner.

`Manager.handler` leases the selected generation for a request. The lease
survives `AppM.await`, transfers to a streaming response, and is released on
completion, error, timeout, reset, disconnect, HEAD body suppression or stream
close. A retired generation's shutdown/unload runs only after its last lease
ends. The default bounds are four retained generations and 1024 concurrent
leases. When an old stream keeps the limit full, reload fails with 503 and keeps
the active generation serving.

`Manager.unload` retires the active generation; subsequent requests receive 503
until reload. `beginDrain` also stops admission permanently. `collect` and
`snapshot` are nonblocking owner operations. `Manager.application` composes
timer/drain/shutdown hooks with `Server.Application`; native cancellation closes
remaining streams at the common server cutoff. After the native reactor returns,
call `manager.close` in the owner factory finalizer. It returns false rather
than unloading code that is still leased. `test/Fixture.lean` shows the complete
bracket and demonstrates management endpoints for the fixture only; the library
does not mount file-loading HTTP endpoints automatically.

## Request and streaming ABI

The request callback receives method, target, headers JSON, decoded body and
correlation metadata as owned host bytes valid during that callback. It must copy
anything it retains; sockets and borrowed native receive views are never passed
to a plugin. ABI v1 requires a complete bounded decoded request body. Applications
that accept streaming uploads should handle those routes outside this adapter.

`poll` returns pending, one response head, stream data or stream finish. Header
JSON is an array of `[name,value]` pairs. The host validates names, control bytes,
header count and bytes, rejects plugin framing/hop headers, and owns final HTTP/1
and HTTP/2 framing. Complete bodies and chunks have `maxChunkBytes` bounds; larger
outputs use streaming. A stream's data cannot exceed the exact credit supplied
to its poll. Response buffers are copied into Lean `ByteArray` before returning
to the reactor; plugin buffers never remain borrowed across polls. ABI v1 does
not expose response trailers or custom protocol upgrades.

`Config` bounds library size, total request bytes, response headers, chunks and
timeouts. Configure `Server.Config.appRetainedBytes` to at least
`pluginConfig.retainedBytes` so pending exchange reservations fit the native
deferred budget. Native plugin heap allocations remain the trusted plugin's
responsibility; plugins must not retain global function pointers/callbacks after
shutdown or start untracked threads that outlive their generation.

## Reproducible native checks

Install the [core prerequisites](../../docs/getting-started.md#prerequisites)
and the [test dependencies](../../docs/acceptance.md), then run from the
repository root.

```sh
lake -d adapters/plugins build weave-plugin-fixture
lake env .lake/test-venv/bin/python adapters/plugins/test/test_reload.py --output .lake/test-output/plugins.json
```

Use a C11 compiler (`CC` overrides the default). Tests compile actual v1/v2/v3, invalid-ABI and
failed-start shared libraries. One native listener proves same-path replacement
without restart, concurrent old deferred/new responses, streaming generation
retention, reload backpressure before the loader runs, failed reload recovery,
binary copying, request/output/header limits, timeout cancellation, independent
HTTP/2 streams/reset, HEAD suppression, unload/reload and bounded SIGTERM exit.
The JSON evidence records platform, independent client versions and source
SHA256 hashes; results describe the platform actually run, not unrun platforms.
