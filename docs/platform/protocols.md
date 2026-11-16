# Typed APIs and RPCs

Import `LeanWeave.Schema`, `LeanWeave.OpenApi`, `LeanWeave.Protobuf`, and
`LeanWeave.Grpc` independently. The executable example is
`LeanWeave/Test/ProtocolFixture.lean` (`lake build weave-protocol-fixture`).

```lean
open LeanWeave
open scoped LeanWeave.Schema LeanWeave.OpenApi

weave_schema Greeting where {
  text : String checks { minLength 1, maxLength 100 },
  count : Nat checks { maximum 1000 }
}

def create (value : Greeting) (_ : Context) : AppM Greeting := pure value
weave_api api where {
  route "POST" "/greetings" as greetings.create accepts Greeting
    returns Greeting status 201 => create
}
```

The same declaration generates the Lean record, JSON codec, bounded validator,
route handler and OpenAPI metadata. Handler signatures, path captures and schema
constraints are checked during elaboration. The generated route table preserves
normal LeanWeave ambiguity rejection and reverse helpers under
`api.routes.greetings.createUrl`. `OpenApi.specHandler api config` validates and
precomputes an OpenAPI 3.1.1 JSON response; register it at `/openapi.json`.

The spec includes capture types, request/response component schemas, success and
validation statuses. `OpenApi.generate` rejects undocumented routes, component
collisions, missing security schemes and path templates that OpenAPI cannot
represent unambiguously. `Config.host` and `routeNames` select an API surface from
a larger host-scoped router. Rest captures use `x-leanweave-rest`; consumers must
understand this extension to expand multiple path segments. Security scheme
metadata documents middleware: applications still install their authentication
and authorization checks. Optional Swagger UI uses `swaggerPage` and
`swaggerBootstrap` with validated `Html.SafeUrl`s and separately hosted
Swagger UI distribution assets. Serve those through lean-net's static catalog;
no inline-script CSP exception is required.

## Native protobuf

```lean
open scoped LeanWeave.Protobuf LeanWeave.Grpc
protobuf_schema Chat := "syntax = \"proto3\"; package example;
message Message { string text = 1; }
service Chat { rpc Echo(Message) returns (Message);
               rpc Talk(stream Message) returns (stream Message); }"

def echo (message : Chat.Message) (_ : Context) : AppM Chat.Message := pure message
def talk (messages : Grpc.Source Chat.Message) (_ : Context) : AppM (Grpc.Source Chat.Message) :=
  pure messages

grpc_service service where {
  rpc Chat.Chat.Echo => echo,
  rpc Chat.Chat.Talk => talk
}
```

`protobuf_schema` parses proto3 natively during Lean elaboration; protoc is not a
runtime or build dependency. It emits typed recursive Lean structures, nested
messages/enums, codecs and method metadata. Supported fields include all scalar
wire types, packed/unpacked primitives, enum aliases and unknown numbers,
optional presence, oneof and maps. Unrecognized fields (including groups and
known numbers with unfamiliar wire types) survive decoding and re-encoding.
Singular embedded messages merge, oneof uses the last selected member and maps
use the last entry for a key. `Protobuf.encode` and `Protobuf.decode` enforce byte,
field, unknown-byte and nesting limits. Schemas have separate lexer/declaration
budgets. Invalid reserved/duplicate fields, missing referenced types, invalid map
keys and nonmessage RPC types fail at build time. The compiler currently accepts
one self-contained descriptor: imports are recorded, but referenced imported
types must be provided in that same schema string. Proto2, editions, custom
option interpretation and generated reflection are outside this proto3 API.
Generated records derive `Inhabited`; use `{ (default : Chat.Message) with text := "hi" }`
when constructing a subset of fields.

## RPC ownership and streaming

`Grpc.Method.unary`, `.serverStream`, `.clientStream`, and `.bidiStream` implement
the four RPC shapes. The `grpc_service` declaration obtains request/response types
and streaming flags from generated protobuf method metadata and rejects an
incorrect handler signature or duplicate method path.

`service.handler` serves native gRPC over HTTP/2 and gRPC-Web binary/text over
HTTP/1.1 or HTTP/2. Set `Server.Config.streamRequests := service.streamRequests`
so HTTP/2 handlers start at headers and consume owned request chunks as they
arrive. Client/bidi streams otherwise wait for the complete ordinary HTTP body.
Unary requests require exactly one message and request EOF. A bidi handler can
return a response stream before the request half-closes. HTTP/1 gRPC-Web supports
unary/server streaming; normal browser clients do not upload client streams.

`Grpc.Source α` is a worker-local producer. `poll` returns pending (`none`), one
message (`some (.ok (some value))`), EOF (`some (.ok none)`) or a terminal
`Status`. Poll and close callbacks must be nonblocking; no task, socket or
borrowed buffer is exposed. A returned source must declare its retained bytes.
`Source.next` awaits cooperatively inside `AppM`; custom producers should close
resources idempotently. `Source.ofArray` accepts an explicit reservation;
`Grpc.messages` additionally checks encoded size/count before creating a source.

Every input/output stream bounds message bytes, count and total bytes. Request
metadata, protobuf field count/depth, timeout and base64 carry are bounded too.
The response wrapper advances a producer only under native output credits and
splits every chunk to the exact supplied budget. gRPC-Web text decoding accepts
independently padded flush segments. Standard status/message trailers terminate
native gRPC; gRPC-Web carries the trailer block in a final flag-128 data frame.
Only identity message encoding is enabled; compressed messages or other encodings
return UNIMPLEMENTED. Configure HTTP body limits separately for the expected
cumulative upload size. Authentication belongs in the typed method or an RPC-aware
middleware wrapper; `AppError`s inside methods map to standard gRPC status codes,
with internal/availability/deadline errors sanitized.

Reserve `service.limits.reservation` (plus application state) through
`Server.Config.appRetainedBytes`, and configure `appTimeoutMs` and
`net.deferred.maxTimeoutMs` slightly above the RPC maximum. RPC deadlines apply
to both request intake and response production; output idle/lifetime deadlines
include a 1s margin to publish terminal deadline trailers. A disconnect/reset
cancels the owner and invokes its cleanup. Normal or logical RPC completion never
calls `IncomingBody.cancel`: native transport discards any remaining upload
without resetting the response. Native `Body.close none` means the final event
was admitted to bounded output, not that a peer acknowledged receipt.

## Reproducible interoperability checks

```sh
python3 -m venv /tmp/weave-protocols
/tmp/weave-protocols/bin/pip install -r scripts/requirements-protocols.txt
lake build weave-protocol-fixture
/tmp/weave-protocols/bin/python scripts/test_protocol_compile.py --report docs/platform/evidence/protocols-compile.json
/tmp/weave-protocols/bin/python scripts/test_protocols.py --report docs/platform/evidence/protocols-wire.json
/tmp/weave-protocols/bin/python scripts/test_grpc_web_browser.py --browser webkit --report docs/platform/evidence/grpc-web-browser.json
```

The wire suite uses official protoc/protobuf/grpcio and an independent h2 stack
against the compiled native executable. It checks all four shapes, a bidi reply
before half-close, a 480KB duplex upload, scalar boundaries, recursive/maps/oneof,
unknown-field preservation, malformed/oversized frames, deadlines, cancellation,
RPC errors, gRPC-Web over both HTTP versions, typed REST and the live spec endpoint.
The browser suite builds the official grpc-web and google-protobuf clients from
pinned npm dependencies, uses real XMLHttpRequest traffic and validates binary
and text unary/server streaming plus error trailers. It uses pinned Playwright
CLI automation; install its selected browser before running. Results identify the
actual browser and transport; loopback cleartext tests establish interoperability,
not TLS deployment or every protobuf conformance case.

Protocol references: [protobuf wire format](https://protobuf.dev/programming-guides/encoding/),
[gRPC over HTTP/2](https://github.com/grpc/grpc/blob/master/doc/PROTOCOL-HTTP2.md),
[gRPC-Web](https://github.com/grpc/grpc/blob/master/doc/PROTOCOL-WEB.md),
[OpenAPI 3.1.1](https://spec.openapis.org/oas/v3.1.1.html).
