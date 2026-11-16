import LeanWeave.Grpc.Source

namespace LeanWeave.Grpc

private def decodeMessage [Protobuf.Message α] (limits : Limits) (bytes : ByteArray) : Except Status α :=
  (Protobuf.decode α bytes limits.protobuf).mapError fun error => ⟨.invalidArgument, error⟩

private def encodeMessage [Protobuf.Message α] (limits : Limits) (value : α) : Except Status ByteArray :=
  (Protobuf.encode value limits.protobuf).mapError fun _ => ⟨.internal, "RPC response encoding failed"⟩

/-- A registered RPC has a fully-qualified protobuf path and one streaming shape. -/
structure Method where
  path : String
  clientStreaming : Bool := false
  serverStreaming : Bool := false
  invoke : Context → Limits → Source ByteArray → AppM (Source ByteArray)

private def requireSingle [Protobuf.Message α] (source : Source ByteArray) (limits : Limits) :
    AppM (Except Status α) := do
  let first ← source.next limits.maxTimeoutMs
  match first with
  | .error error => return .error error
  | .ok none => return .error ⟨.invalidArgument, "unary RPC requires one request message"⟩
  | .ok (some bytes) =>
    let last ← source.next limits.maxTimeoutMs
    return do
      let last ← last
      if last.isSome then throw ⟨.invalidArgument, "unary RPC received multiple messages"⟩
      decodeMessage limits bytes

private def singleton (value : Except Status ByteArray) : IO (Source ByteArray) := do
  match value with
  | .error error => return Source.failed error
  | .ok bytes => Source.ofArray #[bytes] bytes.size

def Method.unary [Protobuf.Message α] [Protobuf.Message β] (path : String)
    (handler : α → Context → AppM β) : Method := {
  path
  invoke := fun ctx limits source => do
    match ← requireSingle (α := α) source limits with
    | .error error => return Source.failed error
    | .ok value => singleton (encodeMessage limits (← handler value ctx)) }

def Method.serverStream [Protobuf.Message α] [Protobuf.Message β] (path : String)
    (handler : α → Context → AppM (Source β)) : Method := {
  path, serverStreaming := true
  invoke := fun ctx limits source => do
    match ← requireSingle (α := α) source limits with
    | .error error => return Source.failed error
    | .ok value => return (← handler value ctx).mapResult (encodeMessage limits) }

def Method.clientStream [Protobuf.Message α] [Protobuf.Message β] (path : String)
    (handler : Source α → Context → AppM β) : Method := {
  path, clientStreaming := true
  invoke := fun ctx limits source => do
    singleton (encodeMessage limits (← handler (source.mapResult (decodeMessage limits)) ctx)) }

def Method.bidiStream [Protobuf.Message α] [Protobuf.Message β] (path : String)
    (handler : Source α → Context → AppM (Source β)) : Method := {
  path, clientStreaming := true, serverStreaming := true
  invoke := fun ctx limits source => do
    return (← handler (source.mapResult (decodeMessage limits)) ctx).mapResult (encodeMessage limits) }

/-- Encode a finite server stream eagerly to count its retained bytes exactly. -/
def messages [Protobuf.Message α] (values : Array α) (limits : Limits := {}) : Except String (IO (Source α)) := do
  if values.size > limits.maxMessages then throw "RPC stream contains too many messages"
  let mut bytes := 0
  for value in values do
    let encoded ← Protobuf.encode value limits.protobuf
    bytes := bytes + encoded.size
    if bytes > limits.maxTotalBytes then throw "RPC stream exceeds its retained-byte bound"
  -- Typed values may have larger representations; reserve codec field metadata too.
  return Source.ofArray values (bytes * 4 + values.size * limits.maxProtoFields * 16)

structure Service where
  methods : Array Method
  limits : Limits := {}

private structure OutputState where
  pending : ByteArray := ByteArray.empty
  count : Nat := 0
  total : Nat := 0
  terminal : Option Status := none
  trailerSent : Bool := false
  closed : Bool := false

/-- No message advances until the transport grants output credit. Each emitted
chunk fits the exact byte budget supplied by HTTP/2 flow control. -/
def output (source : Source ByteArray) (request : Source ByteArray) (mode : Mode)
    (limits : Limits) (deadline : Nat) : IO Stream.Body := do
  let state ← IO.mkRef ({} : OutputState)
  let finish := fun (reason : Option Stream.CancelReason) => do
    let current ← state.get
    unless current.closed do
      state.set { current with pending := ByteArray.empty, closed := true }
      let reason := reason.orElse fun _ =>
        if current.terminal.any (·.code == .deadlineExceeded) then some .timeout else none
      try source.close reason finally request.close reason
  return {
    retainedBytes := source.retainedBytes + request.retainedBytes + limits.maxMessageBytes * 3 + 2048
    idleTimeoutMs := limits.maxTimeoutMs + 1000
    lifetimeMs := limits.maxTimeoutMs + 1000
    declaredTrailers := if mode == .native then #["grpc-status", "grpc-message"] else #[]
    close := finish
    poll := fun budget => do
      let mut current ← state.get
      if current.closed then return none
      if current.pending.isEmpty && current.terminal.isNone && budget > 0 then
        if (← IO.monoMsNow) >= deadline then
          current := { current with terminal := some ⟨.deadlineExceeded, "RPC deadline exceeded"⟩ }
        else
          let event ← try source.poll catch _ => pure (some (.error ⟨.internal, "Internal RPC error"⟩))
          match event with
          | none => return none
          | some (.error status) => current := { current with terminal := some status }
          | some (.ok none) => current := { current with terminal := some {} }
          | some (.ok (some message)) =>
            if message.size > limits.maxMessageBytes || current.count >= limits.maxMessages ||
                message.size > limits.maxTotalBytes - current.total then
              current := { current with terminal := some ⟨.resourceExhausted, "RPC response limit exceeded"⟩ }
            else
              let encoded := frame message
              current := { current with
                pending := if mode == .webText then base64 encoded else encoded
                count := current.count + 1
                total := current.total + message.size }
      if current.pending.isEmpty then
        if let some status := current.terminal then
          if mode == .native then
            state.set current
            return some (.finish (statusHeaders status))
          if current.trailerSent then return some (.finish #[])
          let trailers := webTrailers status
          current := { current with
            pending := if mode == .webText then base64 trailers else trailers
            trailerSent := true }
      if current.pending.isEmpty || budget == 0 then state.set current; return none
      let size := min budget current.pending.size
      let bytes := current.pending.extract 0 size
      state.set { current with pending := current.pending.extract size current.pending.size }
      return some (.data bytes) }

/-- Catch application IO failures during immediate work and later cooperative
polls, preserving RPC framing instead of leaking a plain HTTP error body. -/
private def protect (action : AppM (Source ByteArray)) : AppM (Source ByteArray) :=
  ⟨fun runtime done => do
    let complete := fun (result : Except AppError (Source ByteArray)) =>
      done (.ok (match result with | .ok source => source | .error error => Source.failed (Status.ofAppError error)))
    let failure := complete (.error {})
    let wrapped : AppRuntime := { suspend := fun pending => runtime.suspend {
      pending with poll := try pending.poll catch _ => do
        try pending.cancel .failed finally failure
        return true } }
    try action.start wrapped complete catch _ => failure⟩

private def headerSingleton (ctx : Context) (name : String) : Except Status (Option String) := do
  let values := ctx.request.headers.filter (fun value => value.name.toLower == name)
  if values.size > 1 then throw ⟨.invalidArgument, "duplicate " ++ name⟩
  return ctx.header name

def Service.validate (service : Service) : Except String Unit := do
  service.limits.validate
  let mut paths : Array String := #[]
  for method in service.methods do
    unless method.path.startsWith "/" && (method.path.splitOn "/").length == 3 &&
        !method.path.toList.any (fun c => c.isWhitespace || c == '\x00' || c == '?' || c == '#') do
      throw "RPC method path must be /package.Service/Method"
    if paths.contains method.path then throw ("duplicate RPC method: " ++ method.path)
    paths := paths.push method.path

/-- Select gRPC requests at HTTP/2 HEADERS, before their upload completes. -/
def Service.streamRequests (_service : Service) (head : LeanNet.HTTP1.RequestHead) : IO Bool :=
  pure (head.method == "POST" && ((header? head.headers "content-type").bind mode?).isSome)

private def setup (service : Service) (ctx : Context) : Except Status (Mode × Nat × Option Method) := do
  if let .error _ := service.validate then throw ⟨.internal, "invalid RPC service configuration"⟩
  let mode ← match (← headerSingleton ctx "content-type").bind mode? with
    | some mode => pure mode | none => throw ⟨.invalidArgument, "unsupported RPC content-type"⟩
  let metadata := ctx.request.headers.foldl (fun bytes header => bytes + header.name.utf8ByteSize + header.value.size + 32) 0
  if metadata > service.limits.maxMetadataBytes then throw ⟨.resourceExhausted, "RPC metadata limit exceeded"⟩
  if ctx.request.method != "POST" then throw ⟨.unimplemented, "RPC requires POST"⟩
  if mode == .native && ctx.request.version != "HTTP/2" then throw ⟨.unimplemented, "native gRPC requires HTTP/2"⟩
  if mode == .native && (← headerSingleton ctx "te").map String.toLower != some "trailers" then
    throw ⟨.invalidArgument, "native gRPC requires TE trailers"⟩
  if let some encoding ← headerSingleton ctx "grpc-encoding" then
    if encoding.toLower != "identity" then throw ⟨.unimplemented, "RPC message compression is not enabled"⟩
  let timeout ← timeoutMs (← headerSingleton ctx "grpc-timeout") service.limits.maxTimeoutMs
  let path := String.fromUTF8! ctx.request.target
  return (mode, timeout, service.methods.find? (·.path == path))

/-- Actual native gRPC and gRPC-Web endpoint. Register once and compose ordinary
middleware around it; authentication errors become standard gRPC status trailers. -/
def Service.handler (service : Service) : Handler := fun ctx => do
  let fallbackMode := ((ctx.header "content-type").bind mode?).getD .native
  let prepared := setup service ctx
  let (mode, timeout, method, error) := match prepared with
    | .ok (mode, timeout, method) => (mode, timeout, method, none)
    | .error error => (fallbackMode, service.limits.maxTimeoutMs, none, some error)
  let deadline := (← IO.monoMsNow) + timeout
  let request ← input ctx mode service.limits deadline
  let response ← match error, method with
    | some error, _ => pure (Source.failed error)
    | none, none => pure (Source.failed ⟨.unimplemented, "RPC method is not implemented"⟩)
    | none, some method => protect (method.invoke ctx service.limits request)
  let body ← output response request mode service.limits deadline
  return (Response.streaming body).withHeader "content-type" mode.contentType
    |>.withHeader "grpc-accept-encoding" "identity"
    |>.withHeader "cache-control" "no-store"
    |>.withHeader "x-content-type-options" "nosniff"

end LeanWeave.Grpc
