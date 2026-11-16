# Optional native TLS adapter

Package `lean-weave-tls`, namespace `LeanWeave.TLS`. The core does not link
OpenSSL. This package wraps the matching pinned `lean-net-tls` revision and keeps
ALPN, mTLS, certificate reload, bounded TLS queues and encrypted file output in
lean-net's native TLS workers.

Install OpenSSL 1.1.1 or 3.x development libraries, plus the core's zlib/Brotli
requirements. On macOS select OpenSSL 3 explicitly (the pinned lean-net native
code does not yet build with OpenSSL 4's new deprecation warnings):

```sh
export PKG_CONFIG_PATH="$(brew --prefix zlib)/lib/pkgconfig:$(brew --prefix openssl@3)/lib/pkgconfig:${PKG_CONFIG_PATH:-}"
cd adapters/tls
lake build
```

```lean
import LeanWeaveTLS

-- Application values:
-- factory : Nat → IO LeanWeave.Handler
-- tls : LeanWeave.TLS.Config := { certificateChain := "cert.pem", privateKey := "key.pem" }
-- LeanWeave.TLS.serve factory tls { port := 8443 }
-- LeanWeave.TLS.serveStatic { root := "public" } factory tls { port := 8443 }
```

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

From the repository root, after building the core static fixture and this package:

```sh
lake build weave-static-fixture
.lake/test-venv/bin/python scripts/test_static.py --tls -v
```

The wire test creates a temporary self-signed certificate and explicitly trusts it
in the test client. Both HTTP/1 and HTTP/2 run over plaintext and TLS; HTTP/2 TLS
requires successful ALPN negotiation. It checks identity bytes, gzip decompression,
Brotli negotiation and transfer, representation ETags, HEAD/304, mounted redirects,
global early rejection and deferred dynamic application responses. These checks
were run on macOS; the CI matrix separately covers supported platforms.
