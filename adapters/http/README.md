# Outbound HTTP adapter

Package `lean-weave-http`, import `LeanWeaveHttp`, namespace `LeanWeave.Http`.
It provides bounded libcurl requests, OTLP export, and Vault KV v2 integration.

Install the [core prerequisites](../../docs/getting-started.md#prerequisites)
and a libcurl development package with OpenSSL and HTTP/2 support. On Debian
or Ubuntu install `libcurl4-openssl-dev`. On macOS install `brew install curl`
and select its pkg-config files after selecting the core libraries.

```sh
export PKG_CONFIG_PATH="$(brew --prefix curl)/lib/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"
```

On Windows, use the MSYS2 OpenSSL build of `mingw-w64-x86_64-curl`. See the
[native library setup and deployment instructions](../../docs/platform/operations.md#secrets-and-native-outbound-http).

From the repository root, build the adapter with `lake -d adapters/http build`.
For an application using a local checkout, add this stanza to `lakefile.toml`
with the path adjusted to your checkout.

```toml
[[require]]
name = "lean-weave-http"
path = "../lean-weave/adapters/http"
```

The example below exposes a health check for a local service on port 9090.
It creates one client per reactor worker and closes it after the worker drains.

```lean
import LeanWeaveHttp
import LeanWeave.Core.Server

open LeanWeave

def main : IO Unit := do
  let upstream : Http.Config := {
    endpoint := {
      hostName := "localhost"
      hostAddr := "127.0.0.1"
      port := 9090
      secure := false
    }
    maxResponseBytes := 4096
  }
  Server.serveManaged (fun _ run => do
    let client ← Http.Client.create upstream
    try
      run fun _ => do
        let result ← client.request { path := "/health" }
        if result.status == 200 then return Response.text "upstream healthy"
        return Response.text "upstream unhealthy" 502
    finally client.close) { appRetainedBytes := upstream.retainedBytes }
```

Run `lake exe app` in your application, with a service returning 200 at
`http://127.0.0.1:9090/health`. Then `curl -fsS http://127.0.0.1:8080/` returns
`upstream healthy`.

For HTTPS, supply a startup PEM `caBundle`, the numeric `hostAddr` and the
certificate hostname as `hostName`. The client verifies the peer and hostname
and uses the configured destination for every request. Size `appRetainedBytes`
for the client and the worker's deferred budget for concurrent requests.

To run the loopback adapter acceptance from the repository root, install the
[test dependencies](../../docs/acceptance.md), then run:

```sh
lake -d adapters/http build weave-http-test
lake env .lake/test-venv/bin/python adapters/http/test/wire.py
```

The test uses OpenSSL to generate temporary certificates and checks HTTP/1,
HTTP/2, ALPN, CA and hostname verification, timeouts, bounds and cancellation.
It writes its local result to `adapters/http/test/wire-result.json`.
