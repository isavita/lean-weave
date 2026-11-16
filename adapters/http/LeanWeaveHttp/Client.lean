import LeanWeaveHttp.Native
import LeanWeave.Observability.Trace
import Std.Net.Addr

namespace LeanWeave.Http

/-- Numeric connection address avoids DNS on a reactor worker; hostName remains
the TLS/SNI identity. Resolve service discovery before creating each client. -/
structure Endpoint where
  hostName : String
  hostAddr : String
  port : UInt16 := 443
  secure : Bool := true
  /-- Read once at client startup. When supplied, this bundle takes precedence
      over systemTrust and request callbacks never reopen its path. -/
  caBundle : String := ""
  /-- Explicitly permit platform/default trust-store work when no bundle is
      configured. This can perform OS/file work during a TLS handshake. -/
  systemTrust : Bool := false
  preferHttp2 : Bool := false

structure Config where
  endpoint : Endpoint
  maxActive : Nat := 16
  maxRequestBytes : Nat := 256 * 1024
  maxResponseBytes : Nat := 256 * 1024
  maxHeaderBytes : Nat := 16384
  maxCABytes : Nat := 512 * 1024
  timeoutMs : Nat := 5000

private def clean (text : String) : Bool := !text.toUTF8.data.any (fun b => b < 32 || b == 127)

def Config.validate (config : Config) : Except String Unit := do
  let endpoint := config.endpoint
  if endpoint.hostName.isEmpty || endpoint.hostName.utf8ByteSize > 253 ||
      !endpoint.hostName.toUTF8.data.all (fun b => (b >= 65 && b <= 90) || (b >= 97 && b <= 122) ||
        (b >= 48 && b <= 57) || b == 45 || b == 46) || endpoint.port == 0 then throw "invalid HTTP service identity"
  if (Std.Net.IPv4Addr.ofString endpoint.hostAddr).isNone && (Std.Net.IPv6Addr.ofString endpoint.hostAddr).isNone then
    throw "HTTP hostAddr must be a numeric IP address"
  if endpoint.secure && !endpoint.systemTrust && endpoint.caBundle.isEmpty then
    throw "secure HTTP requires a startup CA bundle or explicit systemTrust"
  if config.maxActive == 0 || config.maxActive > 1024 || config.maxRequestBytes > 16 * 1024 * 1024 ||
      config.maxResponseBytes == 0 || config.maxResponseBytes > 64 * 1024 * 1024 || config.maxHeaderBytes < 1024 ||
      config.maxHeaderBytes > 1024 * 1024 || config.timeoutMs == 0 || config.timeoutMs > 86400000 ||
      endpoint.caBundle.utf8ByteSize > 4096 || !clean endpoint.caBundle ||
      config.maxCABytes == 0 || config.maxCABytes > 4 * 1024 * 1024 then throw "invalid HTTP client bounds"

structure Request where
  method : String := "GET"
  path : String := "/"
  headers : Array (String × String) := #[]
  body : ByteArray := ByteArray.empty

structure Result where
  status : Nat
  headers : Array (String × String)
  body : ByteArray

def Result.header (result : Result) (name : String) : Option String :=
  (result.headers.find? (·.1 == name.toLower)).map (·.2)

private structure Active where
  id : Nat
  native : Native.Handle

structure Client where
  config : Config
  private tracer : Option Observability.Tracer
  /-- Shared original bundle. Each active native transfer separately accounts
      for the CURL_BLOB_COPY bytes in Config.retainedBytes. -/
  private caBlob : ByteArray
  private active : IO.Ref (Array Active)
  private nextId : IO.Ref Nat
  private closed : IO.Ref Bool

/-- Startup-only construction on the future owner: bounded CA file reads and
    native TLS capability checks complete before request callbacks use the client. -/
def Client.create (config : Config) (tracer : Option Observability.Tracer := none) : IO Client := do
  match config.validate with | .error message => throw (IO.userError message) | .ok () => pure ()
  Native.init
  let caBlob ← if config.endpoint.secure && !config.endpoint.caBundle.isEmpty then do
      Native.validateCABlob
      let bytes ← IO.FS.withFile config.endpoint.caBundle .read fun file => do
        let mut bytes := ByteArray.empty
        -- The extra byte distinguishes an exact-sized bundle from overflow;
        -- short file reads are retried without ever exceeding this allowance.
        while bytes.size <= config.maxCABytes do
          let chunk ← file.read (min 65536 (config.maxCABytes + 1 - bytes.size)).toUSize
          if chunk.isEmpty then break
          bytes := bytes ++ chunk
        return bytes
      if bytes.isEmpty || bytes.size > config.maxCABytes then
        throw (IO.userError "HTTP CA bundle is empty or exceeds maxCABytes")
      pure bytes
    else pure ByteArray.empty
  return { config, tracer, caBlob, active := ← IO.mkRef #[], nextId := ← IO.mkRef 0, closed := ← IO.mkRef false }

def Client.close (client : Client) : IO Unit := do
  client.closed.set true
  for active in ← client.active.get do Native.close active.native
  client.active.set #[]

def Client.isIdle (client : Client) : IO Bool := do return (← client.active.get).isEmpty

def Config.retainedBytes (config : Config) : Nat :=
  2 * config.maxRequestBytes + 2 * config.maxResponseBytes + 2 * config.maxHeaderBytes + 16384 +
    (if config.endpoint.secure && !config.endpoint.caBundle.isEmpty then config.maxCABytes else 0)

private def io (action : IO α) : AppM α := do
  try return ← action catch _ => throw { status := 503, message := "HTTP client operation failed" }

private def validateRequest (config : Config) (request : Request) : Except AppError Unit := do
  if request.method.isEmpty || request.method.utf8ByteSize > 16 || !request.method.toUTF8.data.all (fun b => b >= 65 && b <= 90) then
    throw { status := 400, message := "invalid HTTP method" }
  if !request.path.startsWith "/" || request.path.startsWith "//" || request.path.utf8ByteSize > 8192 ||
      !request.path.toUTF8.data.all (fun b => b >= 33 && b <= 126 && b != 35 && b != 92) then
    throw { status := 400, message := "invalid HTTP path" }
  if request.body.size > config.maxRequestBytes || request.headers.size > 128 then
    throw { status := 413, message := "HTTP request bounds exceeded" }
  let mut bytes := 0
  for (name,value) in request.headers do
    if name.isEmpty || !name.toUTF8.data.all (fun b => ((b >= 65 && b <= 90) || (b >= 97 && b <= 122) || (b >= 48 && b <= 57) || "!#$%&'*+-.^_`|~".toUTF8.data.contains b)) || !clean value ||
        ["host", "content-length", "transfer-encoding", "connection", "expect", "upgrade", "proxy-authorization"].contains name.toLower then
      throw { status := 400, message := "invalid HTTP request header" }
    bytes := bytes + name.utf8ByteSize + value.utf8ByteSize + 4
  if bytes > config.maxHeaderBytes then throw { status := 413, message := "HTTP request headers too large" }

private def parseHeaders (bytes : ByteArray) : Array (String × String) := Id.run do
  let text := String.fromUTF8! bytes
  let mut headers := #[]
  for line in text.splitOn "\r\n" do
    if line.startsWith "HTTP/" then headers := #[]
    else if let some split := line.toList.findIdx? (· == ':') then
      headers := headers.push ((line.take split).toString.toLower, (line.drop (split+1)).toString.trimAscii.toString)
  return headers

/-- Bounded native multi-interface I/O. poll/close stay on the invoking worker;
no task, proxy lookup, synchronous DNS, redirect or shell command is involved.
An explicit Context propagates the correct trace under concurrent HTTP/2 work. -/
def Client.request (client : Client) (request : Request) (context : Option Context := none) : AppM Result := do
  match validateRequest client.config request with | .error error => throw error | .ok () => pure ()
  if (← io client.closed.get) || (← io client.active.get).size >= client.config.maxActive then
    throw { status := 503, message := "HTTP client capacity unavailable" }
  let span ← match client.tracer with
    | some tracer => some <$> io (tracer.start ("HTTP " ++ request.method) (context.bind Observability.traceparent) .client #[
      { key := "http.request.method", value := .text request.method },
      { key := "server.address", value := .text client.config.endpoint.hostName },
      { key := "server.port", value := .integer (Int.ofNat client.config.endpoint.port.toNat) }])
    | none => pure none
  let terminal ← io (IO.mkRef false)
  let finish := fun status attrs errorType => do
    unless ← terminal.get do
      terminal.set true
      if let some span := span then span.finish status attrs errorType
  (do
    let endpoint := client.config.endpoint
    let url := (if endpoint.secure then "https://" else "http://") ++ endpoint.hostName ++ ":" ++ toString endpoint.port ++ request.path
    let address := if endpoint.hostAddr.contains ':' then "[" ++ endpoint.hostAddr ++ "]" else endpoint.hostAddr
    let resolve := endpoint.hostName ++ ":" ++ toString endpoint.port ++ ":" ++ address
    let headers : Array (String × String) := match span with
      | some span => (request.headers.filter (fun (p : String × String) => p.1.toLower != "traceparent")).push ("traceparent", Observability.formatTraceparent span.context)
      | none => request.headers
    let trust := if !client.caBlob.isEmpty then "blob" else if endpoint.systemTrust then "system" else "plain"
    let native ← io (Native.start #[url, request.method, resolve, trust, if endpoint.preferHttp2 then "2" else "1"]
      (headers.map fun (name,value) => name ++ ": " ++ value) request.body client.caBlob client.config.timeoutMs.toUSize
      client.config.maxResponseBytes.toUSize client.config.maxHeaderBytes.toUSize)
    let id ← io client.nextId.get
    io (client.nextId.modify (· + 1))
    io (client.active.modify (·.push { id, native }))
    let cleanup := do
      Native.close native
      client.active.modify (·.filter fun active => active.id != id)
    (do
      let result ← AppM.await
        (do
          try return (← Native.poll native).map fun result => .ok result
          catch _ => return some (.error { status := 502, message := "HTTP transport, certificate or response limits failed" }))
        (fun _ => cleanup) client.config.timeoutMs client.config.retainedBytes
      finish (if result.status >= 500 then .error else .unset)
        #[{ key := "http.response.status_code", value := .integer (Int.ofNat result.status) }] ""
      return ({ status := result.status, headers := parseHeaders result.headers, body := result.body } : Result)
    ).ensuring cleanup
  ).ensuring (finish .error #[] "http.cancelled_or_failed")

end LeanWeave.Http
