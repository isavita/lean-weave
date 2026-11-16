# Typed executable GraphQL

Build fields with `Field.typed` or `Field.subscription` over Lean input/output
types implementing `Schema.HasSchema` and JSON codecs. `ExecutableSchema.make`
validates root/type names and conflicts once. Queries, ordered mutations and
cooperative subscriptions use the same typed schema for coercion, output checks,
introspection and documentation. Resolver code receives application Context.

The executable parser handles named/multiple operations, variables/defaults,
aliased nested selections, named/inline fragments, skip/include directives,
strict integer/float/enum literals, escaped Unicode and normalized block strings.
Preparation checks the selected operation before executing any resolver: unknown
fields/arguments, missing/incompatible variables, unused variables/fragments,
fragment cycles, conflicting response aliases and resource bounds fail early.
GraphQL Int values are checked as signed 32-bit values and Float values must be
finite doubles. String literals cannot masquerade as enum literals. Mutations
execute in source order; typed non-null failures propagate to the root result.

This first schema profile uses finite closed value objects, enums, scalars,
arrays and nullable wrappers from HasSchema. Nested fields project typed values;
they are not independent asynchronous field resolvers. Interfaces, unions,
recursive named schemas, custom executable directives, SDL schema construction,
APQ, defer/stream and federation are not implemented. Validation is performed for
the requested operation; clients should submit only operations intended for this
schema. This is an executable bounded profile, not a claim of complete GraphQL
spec conformance. Generated names for nested value/input types are stable and
visible through introspection.

`GraphQL.handler` accepts bounded JSON POSTs and query-only GETs. Mount it in the
application's authenticated API pipeline. A resolver may use AppM.await without
creating a request task. Configure Server.appRetainedBytes to cover the request
AST and accumulated output while awaiting; the native fixture reserves 1MiB.
Response byte, expansion, token, depth and subscription source limits are explicit.

`graphql-transport-ws` supports init/ack, subscribe, next/error/complete, ping/pong,
initialization timeout, exact Origin/subprotocol checks and cancellation. Supply
WebSocketConfig.authorize to verify connection_init credentials and return verified
Context. No authentication is inferred from untrusted payload fields. Operations
and outboxes have count/byte limits, round-robin polling and absolute deadlines.
Client completion, errors, timeout, disconnect and shutdown close sources once;
late await results cannot revive a cancelled operation. A connection's state and
all callbacks stay on their native reactor worker. Use acceptHTTP1/acceptHTTP2 in
Server.Application's upgrade selectors; normal HTTP/2 requests remain independent.

Validation:

```
lake build weave-graphql-test weave-graphql-fixture
.lake/build/bin/weave-graphql-test
python test/test_graphql.py -v
npm ci --prefix test/graphql-client --ignore-scripts --no-audit --no-fund
node test/graphql-client/acceptance.mjs
```

The wire tests exercise real HTTP/1+HTTP/2 and RFC8441 sockets, streaming updates,
protocol close codes and exactly-once cleanup. The pinned official GraphQL.js
client runs getIntrospectionQuery, buildClientSchema, assertValidSchema and query
validation; graphql-ws receives a real cross-connection mutation broadcast and
cancels it. See test/evidence/graphql-clients.json for the measured client versions.

References: [GraphQL September 2025](https://spec.graphql.org/September2025/),
[graphql-transport-ws protocol](https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md).
