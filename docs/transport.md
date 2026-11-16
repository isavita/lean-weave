# Reusing lean-net transport

Use `Server.serveStatic { root := "public" } factory config` for a native
`/assets/` catalog. `Server.StaticConfig` chooses the literal mount and catalog
budgets. Preparation happens before listen, files bind per reactor, and complete
lean-net Replies preserve file regions, gzip/Brotli, validators, HEAD lengths and
mounted directory redirects. Application global header checks run first.
`serveStaticManagedWithTransport` also brackets per-worker adapter resources.

The optional [TLS adapter](../adapters/tls/README.md) exports `LeanWeave.TLS.serve`,
`serveManaged`, `serveStatic` and `serveStaticManaged` using the same APIs with
lean-net TLS configuration. It retains ALPN, dedicated native TLS workers, mTLS,
certificate reload and bounded graceful draining. Use OpenSSL 1.1.1 or newer; the pinned native TLS implementation supports
OpenSSL 3.x and 4.x certificate identity APIs.

`Server.Config.net` is the existing lean-net `ReactorServerConfig`: admission,
connection/body/output budgets, HTTP/2 flow control, logging, metrics, rate limits
and graceful draining retain their transport semantics. `Server.serveWithTransport`
accepts a native transport provider; `Server.handlers` can also be used directly
inside lean-net's worker factories.

For TLS, add the optional `lean-net-tls` Lake package from the same lean-net
revision and its `tls` subdirectory, then:

```lean
import LeanWeave
import LeanNetTLS

-- `factory : Nat → IO LeanWeave.Handler` and `config : LeanWeave.Server.Config`
-- are application values; validate config before starting the listener.
-- LeanNetTLS.ReactorServer.serveFactory
--   (fun worker => do return LeanWeave.Server.handlers (← factory worker) config)
--   { certificateChain := "chain.pem", privateKey := "key.pem" }
--   config.net config.host config.port
```

This preserves ALPN, dedicated TLS workers, mTLS and certificate reload. Plaintext
application continuations remain on their reactor owner. Do not replace lean-net
with blocking TLS calls from AppM.

For static assets, prepare `LeanNet.HTTP.Static.Catalog.withCatalog` before
workers start, then use `HTTP.ReactorServer.serveWorkerFactory`. Inside its
factory bind `catalog.bind reactor`, obtain `bound.handlers`, and construct the
application handlers on the same reactor. Select the catalog handler for your
asset path namespace and the application handler otherwise. Delegate catalog
`onHeaders` and `onRequest` together; catalog replies include resident file
regions, compressed representations, validators and HEAD behavior that cannot be
reconstructed from Response.body alone. Use that same worker factory with
`LeanNetTLS.Reactor.withWorker` for encrypted file output.

Asset URLs can use `Html.Asset.url` and a build-produced fingerprint. Keep old
fingerprints available for their advertised cache lifetime; do not overwrite a
long-lived immutable asset URL with different bytes.
