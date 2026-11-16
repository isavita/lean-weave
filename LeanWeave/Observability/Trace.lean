import LeanWeave.Core
import LeanWeave.Security.Crypto
import LeanNet.HTTP.TraceContext

namespace LeanWeave.Observability

abbrev TraceContext := LeanNet.HTTP.TraceContext.Context

private def nonzero (bytes : ByteArray) : ByteArray :=
  if bytes.data.all (· == 0) then bytes.set! (bytes.size-1) 1 else bytes

def parseTraceparent (value : String) : Option TraceContext := LeanNet.HTTP.TraceContext.parse value.toUTF8

def formatTraceparent (value : TraceContext) : String := String.fromUTF8! value.format

def unixNanos : IO Nat := do return (← Std.Time.Timestamp.now).toNanosecondsSinceUnixEpoch.val.toNat

inductive AttributeValue where
  | text (value : String)
  | boolean (value : Bool)
  | integer (value : Int)
  deriving Repr, BEq

structure Attribute where
  key : String
  value : AttributeValue
  deriving Repr, BEq

inductive SpanKind where
  | internal | server | client | producer | consumer
  deriving Repr, BEq

private def SpanKind.code : SpanKind → Nat
  | .internal => 1 | .server => 2 | .client => 3 | .producer => 4 | .consumer => 5

inductive SpanStatus where
  | unset | ok | error
  deriving Repr, BEq

private def SpanStatus.code : SpanStatus → Nat
  | .unset => 0 | .ok => 1 | .error => 2

structure Span where
  context : TraceContext
  parentSpanId : Option ByteArray
  name : String
  kind : SpanKind
  startTimeUnixNano : Nat
  endTimeUnixNano : Nat
  attributes : Array Attribute := #[]
  status : SpanStatus := .unset
  errorType : String := ""

structure TraceConfig where
  serviceName : String := "lean-weave"
  serviceVersion : String := "0.2.0"
  maxPending : Nat := 1024
  maxBytes : Nat := 1024 * 1024
  maxActive : Nat := 1024
  maxAttributes : Nat := 64
  maxAttributeBytes : Nat := 1024
  /-- Head sampling for roots; a valid remote parent controls descendant sampling. -/
  sampleBasisPoints : Nat := 10000

private structure TraceState where
  pending : Array Span := #[]
  pendingBytes : Nat := 0
  active : Nat := 0
  dropped : Nat := 0
  sampleBasisPoints : Nat := 10000

/-- Worker-local SDK state. Exporters copy only bounded owned span data. -/
structure Tracer where
  config : TraceConfig
  private state : IO.Ref TraceState

structure TraceStats where
  active : Nat
  pending : Nat
  pendingBytes : Nat
  dropped : Nat
  deriving Repr, BEq

def Tracer.create (config : TraceConfig := {}) : IO Tracer := do
  if config.serviceName.isEmpty || config.serviceName.utf8ByteSize > 128 || config.serviceVersion.utf8ByteSize > 128 ||
      config.maxPending == 0 || config.maxBytes == 0 || config.maxActive == 0 || config.maxAttributes > 128 ||
      config.maxAttributeBytes == 0 || config.sampleBasisPoints > 10000 then
    throw (IO.userError "invalid tracing configuration")
  return { config, state := ← IO.mkRef { sampleBasisPoints := config.sampleBasisPoints } }

def Tracer.stats (tracer : Tracer) : IO TraceStats := do
  let state ← tracer.state.get
  return { active := state.active, pending := state.pending.size, pendingBytes := state.pendingBytes, dropped := state.dropped }

/-- Reload sampling without rebuilding application middleware. -/
def Tracer.sampling (tracer : Tracer) (basisPoints : Nat) : IO Unit := do
  if basisPoints > 10000 then throw (IO.userError "sampling rate exceeds 10000 basis points")
  tracer.state.modify fun state => { state with sampleBasisPoints := basisPoints }

private def protectedKey (key : String) : Bool :=
  let key := key.toLower
  ["authorization", "cookie", "password", "secret", "token", "body", "sql.parameters"].any (fun name =>
    key == name || key.endsWith ("." ++ name) || key.endsWith ("_" ++ name))

/-- UTF-8-safe byte truncation: a bound never splits a code point. -/
def boundedText (text : String) (maxBytes : Nat) : String := Id.run do
  let bytes := text.toUTF8
  let mut stop := min maxBytes bytes.size
  for _ in [:3] do
    if stop > 0 && stop < bytes.size && (bytes[stop]! &&& 0xc0) == 0x80 then stop := stop - 1
  return String.fromUTF8! (bytes.extract 0 stop)

private def sanitize (config : TraceConfig) (attributes : Array Attribute) : Array Attribute :=
  (attributes.extract 0 config.maxAttributes).map fun attr =>
    { key := boundedText attr.key 128
      value := if protectedKey attr.key then .text "[REDACTED]" else match attr.value with
        | .text value => .text (boundedText value config.maxAttributeBytes)
        | .integer value =>
          if value < -9223372036854775808 || value > 9223372036854775807 then .text "[OUT_OF_RANGE_INT64]" else .integer value
        | value => value }

private def spanBytes (span : Span) : Nat :=
  256 + span.name.utf8ByteSize + span.attributes.foldl (fun n attr => n + attr.key.utf8ByteSize +
    match attr.value with | .text value => value.utf8ByteSize | _ => 16) 0

structure SpanHandle where
  context : TraceContext
  /-- Terminal and idempotent: completion, cancellation and exception cleanup share this hook. -/
  finish : SpanStatus → Array Attribute → String → IO Unit

/-- Begins a real span before work starts. Incoming context must be valid W3C;
invalid context starts a new trace instead of being echoed or exported. -/
def Tracer.start (tracer : Tracer) (name : String) (parent : Option String := none)
    (kind : SpanKind := .internal) (attributes : Array Attribute := #[])
    (localRoot : Bool := false) : IO SpanHandle := do
  let parentContext := parent.bind parseTraceparent
  let traceId ← match parentContext with
    | some parent => pure parent.traceId
    | none => nonzero <$> IO.getRandomBytes 16
  let spanId := nonzero (← IO.getRandomBytes 8)
  let state ← tracer.state.get
  let rootBucket := (spanId.data.foldl (fun (n : UInt64) byte => n * 256 + byte.toUInt64) 0).toNat % 10000
  let sampled : Bool := match parentContext with
    | some parent => if localRoot then rootBucket < state.sampleBasisPoints else parent.flags &&& 1 == 1
    | none => rootBucket < state.sampleBasisPoints
  let context : TraceContext := { traceId, parentId := spanId, flags := if sampled then 1 else 0 }
  let startTime ← unixNanos
  let startedMono ← IO.monoNanosNow
  let accepted := sampled && state.active < tracer.config.maxActive
  tracer.state.set { state with active := state.active + (if accepted then 1 else 0), dropped := state.dropped + (if sampled && !accepted then 1 else 0) }
  let done ← IO.mkRef false
  return { context, finish := fun status finalAttributes errorType => do
    unless ← done.get do
      done.set true
      if !accepted then return
      let elapsed := (← IO.monoNanosNow) - startedMono
      let span : Span := {
        context, parentSpanId := if localRoot then none else parentContext.map (·.parentId), name := (name.take 256).toString, kind,
        startTimeUnixNano := startTime, endTimeUnixNano := startTime + elapsed,
        attributes := sanitize tracer.config (attributes ++ finalAttributes), status, errorType := (errorType.take 128).toString }
      let bytes := spanBytes span
      tracer.state.modify fun state =>
        if state.pending.size < tracer.config.maxPending && bytes <= tracer.config.maxBytes - min state.pendingBytes tracer.config.maxBytes then
          { state with active := state.active - 1, pending := state.pending.push span, pendingBytes := state.pendingBytes + bytes }
        else { state with active := state.active - 1, dropped := state.dropped + 1 } }

/-- Remove a bounded batch; exporter owns retry retention until it is accepted/dropped. -/
def Tracer.take (tracer : Tracer) (maximum : Nat := 64) : IO (Array Span) := do
  let state ← tracer.state.get
  let count := min maximum state.pending.size
  let batch := state.pending.extract 0 count
  let bytes := batch.foldl (fun n span => n + spanBytes span) 0
  tracer.state.set { state with pending := state.pending.extract count state.pending.size, pendingBytes := state.pendingBytes - bytes }
  return batch

def traceparent (ctx : Context) : Option String :=
  (ctx.attribute "otel.traceparent").or (do
    let values := ctx.request.headers.filter (·.name.toLower == "traceparent")
    if values.size != 1 then none else String.fromUTF8? values[0]!.value)

/-- Request spans propagate an immutable child context through this request's
continuation, never through thread-local state that can mix concurrent streams. -/
def Tracer.middleware (tracer : Tracer) : Middleware := fun next ctx => do
  let name := ctx.request.method ++ (if ctx.routeName.isEmpty then "" else " " ++ ctx.routeName)
  let span ← tracer.start name (traceparent ctx) .server #[
    { key := "http.request.method", value := .text ctx.request.method },
    { key := "http.route", value := .text ctx.routeName }, { key := "tenant.id", value := .text ctx.tenant },
    { key := "request.id", value := .text ctx.requestId }] ctx.request.traceGenerated
  let streaming ← IO.mkRef false
  let parent := formatTraceparent span.context
  let attributes := (ctx.attributes.filter (fun p => p.1 != "otel.traceparent")).push ("otel.traceparent", parent)
  (do
    try
      let response ← next { ctx with attributes }
      let attrs : Array Attribute := #[{ key := "http.response.status_code", value := .integer (Int.ofNat response.status) }]
      if let some source := response.stream then
        let source := { source with close := fun reason => do
          try source.close reason finally
            span.finish (if reason.isSome || response.status >= 500 then .error else .unset) attrs
              (if reason.isSome then "stream.cancelled" else "") }
        streaming.set true
        return ({ response with stream := some source }).withHeader "traceparent" parent
      span.finish (if response.status >= 500 then .error else .unset) attrs ""
      return response.withHeader "traceparent" parent
    catch error =>
      span.finish .error #[{ key := "http.response.status_code", value := .integer (Int.ofNat error.status) }] "application.error"
      throw error).ensuring (do if !(← streaming.get) then span.finish .error #[] "request.aborted")

private def Attribute.toOtlp (attr : Attribute) : Lean.Json :=
  let value := match attr.value with
    | .text value => Lean.Json.mkObj [("stringValue", Lean.toJson value)]
    | .boolean value => Lean.Json.mkObj [("boolValue", Lean.toJson value)]
    | .integer value => Lean.Json.mkObj [("intValue", Lean.toJson (toString value))]
  Lean.Json.mkObj [("key", Lean.toJson attr.key), ("value", value)]

def Span.toOtlp (span : Span) : Lean.Json :=
  Lean.Json.mkObj ([
    ("traceId", Lean.toJson (Security.Crypto.hex span.context.traceId)),
    ("spanId", Lean.toJson (Security.Crypto.hex span.context.parentId)),
    ("name", Lean.toJson span.name), ("kind", Lean.toJson span.kind.code),
    ("startTimeUnixNano", Lean.toJson (toString span.startTimeUnixNano)),
    ("endTimeUnixNano", Lean.toJson (toString span.endTimeUnixNano)),
    ("attributes", .arr (span.attributes.map Attribute.toOtlp)),
    ("status", Lean.Json.mkObj [("code", Lean.toJson span.status.code), ("message", Lean.toJson span.errorType)])] ++
    (span.parentSpanId.toList.map fun id => ("parentSpanId", Lean.toJson (Security.Crypto.hex id))))

/-- OTLP/HTTP JSON mapping uses hexadecimal IDs and decimal-string nanosecond/int64 values. -/
def Tracer.otlp (tracer : Tracer) (spans : Array Span) : Lean.Json :=
  Lean.Json.mkObj [("resourceSpans", .arr #[Lean.Json.mkObj [
    ("resource", Lean.Json.mkObj [("attributes", .arr #[
      { key := "service.name", value := .text tracer.config.serviceName : Attribute }.toOtlp,
      { key := "service.version", value := .text tracer.config.serviceVersion : Attribute }.toOtlp])]),
    ("scopeSpans", .arr #[Lean.Json.mkObj [
      ("scope", Lean.Json.mkObj [("name", Lean.toJson "LeanWeave"), ("version", Lean.toJson "0.2.0")]),
      ("spans", .arr (spans.map Span.toOtlp))]])]] )]

end LeanWeave.Observability
