# Execution and security contracts

The worker calls application callbacks synchronously. A callback cannot perform
blocking filesystem, network, process or sleep operations. Use the optional
nonblocking adapters or another bounded producer implementing `AppM.await`.

An execution has one outstanding suspension. It reserves its configured adapter
buffer allowance against lean-net's per-worker pending byte budget. lean-net also
charges retained request body/header bytes. Count, per-connection count, retained
bytes and deadlines are bounded independently. A full queue produces 503; expired
work produces 504. Cancel callbacks run once on disconnect, HTTP/2 RST, timeout,
shutdown, failure or overload. Terminal cancellation discards late continuations.
`ensuring` releases leases and resources on completion, error and cancellation.

HTTP/1 pauses application dispatch behind a deferred reply, preserving pipeline
order while the transport remains able to observe disconnects. Input retained
behind it remains subject to the native input bound. HTTP/2 continues dispatching
other streams and completes them independently. Output ordering, framing,
flow-control and borrowed-buffer lifetimes belong to lean-net. No socket or
borrowed input slice appears in Context.

A worker factory owns caches, metrics and adapter state. Do not capture those
references in tasks or share them with another worker. Sessions and CRUD examples
use explicit try-locks for shared state; contention fails fast instead of blocking.

Compile middleware once, in declared order. A typical browser pipeline is
`securityHeaders → requestId → errors → metrics → session → sessionAuth → csrf`.
Admin routes add `requireAuth` and `authorize`; APIs can use a token resolver and
exact-origin CORS. `Server.Config.headerChecks` can check hosts and origin before
body intake or 100 Continue. Early replies are bodyless on both protocols so
head-stage rejection semantics agree. CSRF validates a session-bound header/form token after
body decoding. It rejects missing sessions, repeated token headers and duplicate
form tokens. CORS validates one requested method and checks every field in the
preflight's requested header list.

Proxy headers are ignored unless an application supplies a trusted transport-peer
predicate. `Middleware.trustedProxyAddresses` compares an explicit list of 4/16-byte
IP addresses with `Context.peer`, minted by the native socket transport.
`Context.secure` and `request.transport` expose verified transport metadata, with
no socket handle. Never derive trust from a client-provided header or request
parameter. There is no automatic trust of loopback, X-Forwarded-For or Forwarded.

Authentication is an application resolver. The framework does not implement a
password database or invent a password hashing scheme. The login example uses an
explicitly configured demonstration secret; production credentials should use a
vetted authentication service or password library. Rotate the session ID and CSRF
token after login or privilege changes. Secure `__Host-` cookies are the default;
the local HTTP examples explicitly choose an insecure development cookie name.

HTML text and attributes escape delimiters. URL attributes require SafeUrl,
which rejects executable schemes, protocol-relative URLs, backslashes and ASCII
controls/whitespace. Components exclude executable tags and event/style
attributes. Raw HTML requires `Html.rawUnsafe`. Templates contain no runtime
markup parser. JSON uses Lean's serializer. Native framing/header validation
still applies to every response.

Cookie/session limits bound data and entry counts. Memory limits describe
serialized payload/key bytes, with separately bounded metadata counts; they are
not a hard process RSS cap. Incoming bodies are additionally bounded by lean-net
before application decoders run. Multipart payloads remain binary and filenames
are never used as filesystem paths.
