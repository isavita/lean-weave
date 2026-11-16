import LeanWeave.GraphQL.HTTP
import LeanWeave.Core.Decode
import LeanNet.WebSocket.Server

namespace LeanWeave.GraphQL

structure WebSocketConfig where
  origin : String
  limits : Limits := {}
  initTimeoutMs : Nat := 5000
  heartbeatMs : Nat := 15000
  subscriptionLifetimeMs : Nat := 3600000
  maxOperations : Nat := 8
  operationRetainedBytes : Nat := 1048576
  maxQueuedMessages : Nat := 32
  maxQueuedBytes : Nat := 262144
  maxMessageBytes : Nat := 65536
  /-- Verify connection_init credentials and return verified application context.
      No policy is configured by default, so initialization is denied. -/
  authorize : Context → Lean.Json → IO (Except String Context) :=
    fun _ _ => pure (.error "GraphQL WebSocket authorization policy required")

private structure Active where
  id : String
  source : Source Result

private structure SocketState where
  context : Context
  initialized : Bool := false
  operations : Array Active := #[]
  queue : Array ByteArray := #[]
  queuedBytes : Nat := 0
  closing : Option Nat := none
  closed : Bool := false
  cursor : Nat := 0
  lastOutput : Nat := 0

private def closeSource (source : Source α) (reason : Option Stream.CancelReason) : IO Unit :=
  try source.close reason catch _ => pure ()

private def wire (type : String) (id : Option String := none) (payload : Option Lean.Json := none) : ByteArray :=
  (Lean.Json.mkObj ([("type", .str type)] ++ id.toList.map ("id", .str ·) ++ payload.toList.map ("payload", ·))).compress.toUTF8

private def enqueue (state : IO.Ref SocketState) (config : WebSocketConfig) (bytes : ByteArray) : IO Unit := do
  let current ← state.get
  if bytes.size > config.maxMessageBytes || current.queue.size >= config.maxQueuedMessages ||
      bytes.size > config.maxQueuedBytes - current.queuedBytes then
    state.set { current with closing := some 1013 }
  else state.set { current with queue := current.queue.push bytes, queuedBytes := current.queuedBytes + bytes.size }

private def remove (state : IO.Ref SocketState) (id : String) (reason : Option Stream.CancelReason) : IO Unit := do
  let current ← state.get
  let selected := current.operations.filter (·.id == id)
  state.set { current with operations := current.operations.filter (·.id != id) }
  for active in selected do closeSource active.source reason

private def shutdown (state : IO.Ref SocketState) (reason : Option Stream.CancelReason) : IO Unit := do
  let current ← state.get
  state.set { current with operations := #[], queue := #[], queuedBytes := 0, closed := true }
  for active in current.operations do closeSource active.source reason

private def single (result : Result) : IO (Source Result) := do
  let sent ← IO.mkRef false
  return {
    retainedBytes := (Lean.toJson result).compress.utf8ByteSize
    poll := do
      if ← sent.get then return some (.ok none)
      sent.set true
      return some (.ok (some result))
    close := fun _ => sent.set true }

private def start (schema : ExecutableSchema) (request : Request) (ctx : Context)
    (config : WebSocketConfig) : IO (Except String (Source Result)) := do
  let prepared := prepare schema request config.limits
  if let .error message := prepared then return .error message
  let .ok prepared := prepared | return .error "Invalid operation"
  let action : AppM (Source Result) := if prepared.kind == .subscription then subscribe prepared ctx config.limits
    else do return ← single (← execute prepared ctx config.limits)
  let execution ← AppM.execute action config.operationRetainedBytes
  let current ← IO.mkRef (none : Option (Source Result))
  let closed ← IO.mkRef false
  let started ← IO.monoMsNow
  let timeout := if prepared.kind == .subscription then config.subscriptionLifetimeMs else config.limits.operationTimeoutMs
  let close := fun reason => do
    if !(← closed.get) then
      closed.set true
      execution.cancel (reason.getD .failed)
      if let some source ← current.get then closeSource source reason
      current.set none
  return .ok {
    retainedBytes := config.operationRetainedBytes
    close
    poll := do
      if ← closed.get then return some (.ok none)
      if (← IO.monoMsNow) - started >= timeout then
        close (some .timeout)
        return some (.error ⟨"Operation deadline exceeded", #[], "TIMEOUT"⟩)
      if (← current.get).isNone then
        match ← execution.poll with
        | none => return none
        | some (.error error) =>
          return some (.error ⟨if error.status >= 500 then "Internal server error" else error.message, #[], "RESOLVER_ERROR"⟩)
        | some (.ok source) =>
          if source.retainedBytes > config.operationRetainedBytes then
            closeSource source (some .overloaded)
            return some (.error ⟨"Operation source exceeds byte bound", #[], "OVERLOADED"⟩)
          current.set (some source)
      let some source ← current.get | return none
      try source.poll catch _ => return some (.error ⟨"Subscription failed", #[], "INTERNAL_ERROR"⟩) }

/-- graphql-transport-ws protocol. All operation polls and cleanup run on the
owner worker. No per-subscription tasks or borrowed receive buffers are retained. -/
def webSocketCallbacks (schema : ExecutableSchema) (ctx : Context) (config : WebSocketConfig) : IO LeanNet.WebSocket.Callbacks := do
  if config.origin.isEmpty || config.initTimeoutMs == 0 || config.heartbeatMs == 0 ||
      config.subscriptionLifetimeMs == 0 || config.limits.operationTimeoutMs == 0 ||
      config.maxOperations == 0 || config.maxOperations > 128 || config.operationRetainedBytes == 0 ||
      config.maxMessageBytes == 0 || config.maxMessageBytes > 1048576 ||
      config.maxQueuedMessages == 0 || config.maxQueuedBytes < config.maxMessageBytes then
    throw (IO.userError "invalid bounded GraphQL WebSocket configuration")
  let now ← IO.monoMsNow
  let deadline := now + config.initTimeoutMs
  let state ← IO.mkRef ({ context := ctx, lastOutput := now } : SocketState)
  return {
    onMessage := fun opcode bytes => do
      let current ← state.get
      if current.closed || current.closing.isSome then return none
      if opcode != .text then state.modify (fun s => { s with closing := some 4400 }); return none
      let .ok json := Body.json bytes { maxBytes := config.maxMessageBytes } | do
        state.modify (fun s => { s with closing := some 4400 }); return none
      let .ok type := json.getObjValAs? String "type" | do
        state.modify (fun s => { s with closing := some 4400 }); return none
      match type with
      | "connection_init" =>
        if current.initialized then state.modify (fun s => { s with closing := some 4429 }); return none
        if (← IO.monoMsNow) >= deadline then state.modify (fun s => { s with closing := some 4408 }); return none
        let payload := (json.getObjVal? "payload").toOption.getD (.mkObj [])
        if payload != .null && !(payload.getObj?).isOk then
          state.modify (fun s => { s with closing := some 4400 }); return none
        match ← config.authorize current.context payload with
        | .error _ => state.modify (fun s => { s with closing := some 4403 })
        | .ok context =>
          state.modify (fun s => { s with initialized := true, context })
          enqueue state config (wire "connection_ack")
      | "ping" =>
        let payload := (json.getObjVal? "payload").toOption
        if payload.any (fun value => value != .null && !(value.getObj?).isOk) then
          state.modify (fun s => { s with closing := some 4400 }); return none
        enqueue state config (wire "pong" none payload)
      | "pong" => pure ()
      | "complete" =>
        let .ok id := json.getObjValAs? String "id" | do
          state.modify (fun s => { s with closing := some 4400 }); return none
        remove state id none
      | "subscribe" =>
        if !current.initialized then state.modify (fun s => { s with closing := some 4401 }); return none
        let .ok id := json.getObjValAs? String "id" | do
          state.modify (fun s => { s with closing := some 4400 }); return none
        if id.isEmpty || id.utf8ByteSize > 128 then state.modify (fun s => { s with closing := some 4400 }); return none
        if current.operations.any (·.id == id) then state.modify (fun s => { s with closing := some 4409 }); return none
        if current.operations.size >= config.maxOperations then state.modify (fun s => { s with closing := some 1013 }); return none
        let request := json.getObjVal? "payload" >>= Request.decode
        match request with
        | .error _ => enqueue state config (wire "error" (some id) (some (Lean.toJson (#[Error.mk "Invalid operation payload" #[] "GRAPHQL_VALIDATION_FAILED"]))))
        | .ok request =>
          match ← start schema request current.context config with
          | .error message => enqueue state config (wire "error" (some id) (some (Lean.toJson (#[Error.mk message #[] "GRAPHQL_VALIDATION_FAILED"]))))
          | .ok source => state.modify fun s => { s with operations := s.operations.push ⟨id, source⟩ }
      | _ => state.modify (fun s => { s with closing := some 4400 })
      return none
    onPoll := do
      let current ← state.get
      if current.closed then return none
      let now ← IO.monoMsNow
      let code := current.closing.orElse fun _ => if !current.initialized && now >= deadline then some 4408 else none
      if let some code := code then
        shutdown state (some .failed)
        return some { opcode := .close, payload := ByteArray.mk #[(code / 256).toUInt8, (code % 256).toUInt8] }
      if current.queue.isEmpty && !current.operations.isEmpty then
        let index := current.cursor % current.operations.size
        let some active := current.operations[index]? | return none
        state.modify fun s => { s with cursor := index + 1 }
        match ← active.source.poll with
        | none => pure ()
        | some (.error error) =>
          remove state active.id (some .failed)
          enqueue state config (wire "error" (some active.id) (some (Lean.toJson #[error])))
        | some (.ok none) =>
          remove state active.id none
          enqueue state config (wire "complete" (some active.id))
        | some (.ok (some result)) => enqueue state config (wire "next" (some active.id) (some (Lean.toJson result)))
      let current ← state.get
      if let some bytes := current.queue[0]? then
        state.set { current with queue := current.queue.extract 1 current.queue.size, queuedBytes := current.queuedBytes - bytes.size, lastOutput := now }
        return some { opcode := .text, payload := bytes }
      if now - current.lastOutput >= config.heartbeatMs then
        state.modify (fun s => { s with lastOutput := now })
        return some { opcode := .ping, payload := ByteArray.empty }
      return none
    onClosed := fun info => shutdown state (some (if info.code == some 1001 then .shutdown else .disconnected)) }

private def policy (config : WebSocketConfig) : LeanNet.WebSocket.Handshake.Policy := {
  originAllowed := fun value => value == some config.origin.toUTF8
  subprotocols := #["graphql-transport-ws"]
  requireSubprotocol := true }

def acceptHTTP1 (schema : ExecutableSchema) (head : LeanNet.HTTP1.RequestHead) (config : WebSocketConfig) : IO LeanNet.HTTP1.HeaderResult := do
  if let .error rejection := LeanNet.WebSocket.Handshake.acceptHTTP1 head (policy config) then
    return .reject { response := { status := rejection.status } }
  let .ok ctx := Context.fromRequest { method := head.method, version := head.version, target := head.target, headers := head.headers, body := ByteArray.empty }
    | return .reject { response := { status := 400 } }
  LeanNet.WebSocket.Server.acceptHTTP1 head (policy config) (← webSocketCallbacks schema ctx config)

def acceptHTTP2 (schema : ExecutableSchema) (head : LeanNet.HTTP2.RequestHead) (config : WebSocketConfig) : IO (Option (LeanNet.WebSocket.Handshake.Policy × LeanNet.WebSocket.Callbacks)) := do
  if !(LeanNet.WebSocket.Handshake.acceptHTTP2 head head.protocol (policy config)).isOk then return none
  let .ok ctx := Context.fromRequest { method := head.method, version := "HTTP/2", target := head.target, headers := head.headers, body := ByteArray.empty }
    | return none
  return some (policy config, ← webSocketCallbacks schema ctx config)

end LeanWeave.GraphQL
