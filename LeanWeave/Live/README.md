# Stateful Live components

`Live.Component State Event` keeps typed state and event decoding on the server.
Rendering returns safe `Html.Html` regions with stable keys. Updates send only
changed regions; removed keys are explicit. Reconnect sends a full versioned
snapshot, and a browser gap requests a fresh stream. The small external browser
client uses WebSockets, Ping/Pong keepalives, and EventSource + same-origin POST
fallback. It preserves unchanged DOM regions.

Construct `Runtime.new` on its owning worker. **Use one worker for this endpoint,
or sticky routing to its owner.** State is worker-local and not migrated between
workers/nodes. A load balancer must preserve the opaque live cookie's owner; a
production deployment needing transparent failover should supply a shared,
versioned application state store rather than sharing IO.Ref across workers.

Mount through the application's authenticated browser pipeline. Runtime cookies
are random 256-bit IDs, HttpOnly, SameSite=Strict and Secure by default. HTTPS is
required by the default `__Host-` cookie. WebSocket handshakes and mutating POSTs
require the configured exact Origin; commands also require the per-session CSRF
token. An application must invalidate its Live session when authorization changes
or use an update callback that rechecks authorization for sensitive operations.
Fixture plaintext cookies are explicitly development-only.

Session count, state reservation, rendered HTML, region count, message bytes,
outbox count/bytes and lifetime are bounded. `stateBytes` must conservatively
account for all application state. A slow client receives a new full snapshot
when the diff queue fills. It cannot grow an unbounded pending diff log.
`Html.rawUnsafe` remains the explicit escape hatch; diff HTML must be rendered
through typed safe components. The browser inserts only server-authored HTML.

Run `lake build weave-realtime-fixture weave-stream-test`, then
`.lake/build/bin/weave-stream-test` and `python test/test_realtime.py -v`.
The fixture has an interactive counter, SSE replay endpoint, multipart stream and
nonce-CSP browser acceptance page. Start it with a loopback port, then use
Playwright CLI `open http://127.0.0.1:PORT --browser chromium` and
`run-code --filename=test/browser_realtime.cjs` for real browser evidence.
