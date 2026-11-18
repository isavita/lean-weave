# Optional native TLS adapter

Package `lean-weave-tls`, namespace `LeanWeave.TLS`. The core does not link
OpenSSL. This package wraps the matching pinned `lean-net-tls` revision and keeps
ALPN, mTLS, certificate reload, bounded TLS queues and encrypted file output in
lean-net's native TLS workers.

Keep the core and TLS adapter pins on the same lean-net commit when updating
dependencies. The TLS package resolves lean-net from its own parent checkout.

Install OpenSSL 1.1.1 or newer development libraries and the
[core prerequisites](../../docs/getting-started.md#prerequisites). The pinned
native implementation supports the OpenSSL 3.x and 4.x certificate identity
APIs. On macOS, select OpenSSL 3 and build from the repository root:

```sh
brew install openssl@3
export PKG_CONFIG_PATH="$(brew --prefix openssl@3)/lib/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"
lake -d adapters/tls build
```

For an application using a local checkout, add `lean-weave-tls` with
`path = "../lean-weave/adapters/tls"` to a `[[require]]` in `lakefile.toml`.
Save the following as `Main.lean` and supply your own `cert.pem` and `key.pem`.

```lean
import LeanWeaveTLS

open LeanWeave

def main : IO Unit :=
  TLS.serve (fun _ => pure (fun _ => pure (Response.text "Hello over TLS")))
    { certificateChain := "cert.pem", privateKey := "key.pem" }
```

The default listener is `127.0.0.1:8443`. If the certificate covers `localhost`,
verify the listener with `curl --cacert ca.pem https://localhost:8443/`, using
the corresponding CA certificate.

`serveStatic` mounts the catalog root at `/assets/`. Configure `«prefix»` to select
another literal namespace ending in `/`. The catalog is prepared before listen,
registered separately with every reactor, and stays alive until the workers drain.
File replies preserve their native resident regions, gzip/Brotli representation,
ETag, conditional GET and HEAD metadata. Directory redirects preserve the mount.
Global `Server.Config.headerChecks` and `onHeaders` run before catalog handling.

`serveManaged` and `serveStaticManaged` accept worker brackets for application
resources. They release those resources after server drain. `TLS.Config` is the
existing native TLS configuration; client certificates, reload settings and SNI
remain available. HTTPS gateway trust is installed unless an application supplied
its own TLS provider. Optional `upstreamCA` selects its CA bundle.

From the repository root, install the [test dependencies](../../docs/acceptance.md)
and run the core and TLS static acceptance:

```sh
lake build weave-static-fixture
lake -d adapters/tls build weave-tls-fixture
lake env .lake/test-venv/bin/python scripts/test_static.py --tls -v
```

The wire test creates a temporary self-signed certificate and explicitly trusts it
in the test client. Both HTTP/1 and HTTP/2 run over plaintext and TLS; HTTP/2 TLS
requires successful ALPN negotiation. It checks identity bytes, gzip decompression,
Brotli negotiation and transfer, representation ETags, HEAD/304, mounted redirects,
global early rejection and deferred dynamic application responses. These checks
were run on macOS; the CI matrix separately covers supported platforms.
