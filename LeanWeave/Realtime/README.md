# Channels, PubSub and presence

A `Realtime.Broker` bounds subscribers, topics, queued message count and bytes.
Only immutable owned values cross workers. Shared access uses try-lock and never
blocks a reactor: contention rejects publishing/subscription admission or yields
pending while polling. Slow subscribers close explicitly on overflow. Polling
renews a finite subscription lease; closing under contention may be retried, and
expired abandoned slots are reclaimed on bounded maintenance operations.

`Channel.authorize` gates both WebSocket and SSE joins. It must inspect verified
application context. A channel does not trust a browser-supplied topic as an
identity. Typed `broadcast` uses ToJson; inbound WebSocket messages are bounded,
UTF-8/JSON validated and restricted to the joined topic. Consumers use `onPoll`
for asynchronous pushes. Ephemeral PubSub does not claim durable replay; use
`SSE.Store` for bounded Last-Event-ID replay and explicit replay-gap resets.

`Channel.broadcast` selects a `Publisher` while its WS/SSE subscriptions consume
the same local broker. The default publisher uses `Broker.publish` and returns a
`Publication` with the number of local subscriber queues that accepted the
message. A distributed publisher returns `recipients := none` when that count is
unknown. A successful receipt means queue admission; it never acknowledges remote
subscriber/application delivery. Name/byte bounds run before a custom publisher,
and transport rejection does not fall back to local fanout. Publishing is a
trusted server capability: authorize client commands before broadcasting them.

`Redis.PubSub.Adapter.channel authorize` supplies the Redis publisher and its
Redis-fed local broker. The same typed `channel.broadcast topic event value`
therefore crosses nodes; Redis echoes it to the publishing node too, preventing
an extra immediate local copy. `Adapter.publisher` can be supplied explicitly
when constructing a channel. Both hooks stay on the native adapter owner.

`Presence.Replica` is a dotted observed-remove CRDT with periodic full snapshots.
Concurrent state merges preserve unseen dots and remove observed deleted dots.
Track each connection separately with `trackConnection`; `list` returns metadata
per connection and `users` groups user keys. Untracking one socket preserves other
sockets. Lease expiry hides partitioned nodes. Rejoining survivors refresh their
dots, while stale snapshots cannot revive observed leaves. `Presence.Cluster`
bridges a broker topic and a bounded publish callback; call tick from onTick.

Actor IDs must be random/incarnation-unique and never reused after losing state.
The causal context is bounded and never silently evicted: reaching maxActors
rejects new actors, preserving removal knowledge. Reconfigure/reseed a cluster
through an explicit coordinated epoch change to retire causal history. This is
a finite-membership design, not unlimited churn with unsafe tombstone expiry.

The optional Redis adapter uses persistent nonblocking SUBSCRIBE and bounded
independent PUBLISH sockets, all owned by their worker. Redis PubSub is at-most-
once and loses transient broadcasts during disconnect. Periodic presence
snapshots provide anti-entropy after rejoin. Use separate ACL-protected Redis
namespaces for applications/tenants; this transport assumes trusted publishers.
The two-process native acceptance test starts an isolated real Redis server:
`lake -d adapters/redis build weave-redis-pubsub-fixture` then
`python adapters/redis/test/test_pubsub_presence.py -v`.
