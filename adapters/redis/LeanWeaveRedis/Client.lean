import LeanWeave.Core.AppM
import LeanWeaveRedis.Protocol
import LeanWeaveRedis.Native

namespace LeanWeave.Redis

/-- One client is constructed inside each application's worker factory. -/
structure Config where
  host : String := "127.0.0.1"
  port : UInt16 := 6379
  maxConcurrent : Nat := 16
  timeoutMs : Nat := 1000
  limits : Limits := {}
  /-- Prefix is an application/environment namespace, never shared by default
      across unrelated applications. Tenant/Vary dimensions belong in keys. -/
  namespacePrefix : String := "lean-weave:"

structure Client where
  private mk ::
  private config : Config
  private active : IO.Ref Nat

/-- Validate all allocation bounds before accepting commands. Numeric address
syntax is checked with inet_pton by the nonblocking native transport, with no DNS. -/
def Client.new (config : Config := {}) : IO Client := do
  if config.host.isEmpty || config.port == 0 || config.maxConcurrent == 0 ||
      config.timeoutMs == 0 || config.limits.maxRequestBytes == 0 ||
      config.limits.maxResponseBytes == 0 || config.limits.maxElements == 0 ||
      config.limits.maxDepth == 0 || config.limits.maxLineBytes == 0 ||
      config.limits.maxBulkBytes > config.limits.maxResponseBytes ||
      config.limits.maxRequestBytes > 16 * 1024 * 1024 ||
      config.limits.maxResponseBytes > 16 * 1024 * 1024 ||
      config.limits.maxElements > 4096 || config.limits.maxDepth > 64 then
    throw (IO.userError "invalid bounded Redis configuration")
  Native.init
  return ⟨config, ← IO.mkRef 0⟩

/-- Reservation required by an AppM execution using this client. Include query,
response accumulation, parsed bulk values and bounded value-node overhead. -/
def Client.retainedBytes (client : Client) : Nat :=
  client.config.limits.maxRequestBytes + 3 * client.config.limits.maxResponseBytes +
    client.config.limits.maxElements * 64

/-- Number of live owner-worker command sockets, useful for admission metrics. -/
def Client.pending (client : Client) : IO Nat := client.active.get

/-- Bounded independent command connection. Nonblocking connect/send/receive
are polled by AppM on the owner worker. Cleanup closes on completion, exception,
timeout, overload or disconnect; late replies cannot resurrect the execution. -/
def Client.command (client : Client) (arguments : Array ByteArray) : AppM Value := do
  let query ← match encode arguments client.config.limits with
    | .ok query => pure query
    | .error message => throw { status := 503, message }
  if (← client.active.get) >= client.config.maxConcurrent then
    throw { status := 503, message := "Redis command capacity exhausted" }
  client.active.modify (· + 1)
  let opened ← (try return .ok (← Native.start client.config.host client.config.port query)
    catch error => pure (.error error.toString) : IO (Except String Native.Handle))
  let handle ← match opened with
    | .ok handle => pure handle
    | .error message =>
      client.active.modify (· - 1)
      throw { status := 503, message }
  let received ← IO.mkRef ByteArray.empty
  let close := do
    try Native.close handle finally client.active.modify (· - 1)
  AppM.ensuring (AppM.await (do
    let previous ← received.get
    if previous.size >= client.config.limits.maxResponseBytes then
      return some (.error { status := 503, message := "Redis response byte budget exhausted" })
    let read ← (try return .ok (← Native.poll handle (client.config.limits.maxResponseBytes - previous.size).toUSize)
      catch error => pure (.error error.toString) : IO (Except String ByteArray))
    let chunk ← match read with
      | .ok chunk => pure chunk
      | .error message => return some (.error { status := 503, message })
    if chunk.isEmpty then return none
    let bytes := previous ++ chunk
    received.set bytes
    match parse bytes client.config.limits with
    | .error message => return some (.error { status := 503, message })
    | .ok value => return value.map Except.ok)
    (fun _ => pure ()) client.config.timeoutMs client.retainedBytes) close

private def key (client : Client) (value : String) : ByteArray :=
  (client.config.namespacePrefix ++ "value:" ++ value).toUTF8

def Client.get (client : Client) (cacheKey : String) : AppM (Option ByteArray) := do
  match ← client.command #["GET".toUTF8, key client cacheKey] with
  | .bulk value => return value
  | .error message => throw { status := 503, message := "Redis GET: " ++ message }
  | _ => throw { status := 503, message := "Redis GET returned an unexpected reply" }

/-- SET with mandatory finite TTL, atomically replacing the value at the primary. -/
def Client.set (client : Client) (cacheKey : String) (value : ByteArray) (ttlMs : Nat) : AppM Unit := do
  if ttlMs == 0 || ttlMs > 9223372036854775807 then
    throw { status := 500, message := "Redis cache TTL must be positive and fit signed 64 bits" }
  match ← client.command #["SET".toUTF8, key client cacheKey, value, "PX".toUTF8, (toString ttlMs).toUTF8] with
  | .simple "OK" => return ()
  | .error message => throw { status := 503, message := "Redis SET: " ++ message }
  | _ => throw { status := 503, message := "Redis SET returned an unexpected reply" }

def Client.delete (client : Client) (cacheKey : String) : AppM Bool := do
  let script := "local n=redis.call('DEL',KEYS[1]);redis.call('DEL',KEYS[2]);return n"
  let leaseKey := (client.config.namespacePrefix ++ "lease:" ++ cacheKey).toUTF8
  match ← client.command #["EVAL".toUTF8, script.toUTF8, "2".toUTF8, key client cacheKey, leaseKey] with
  | .integer count => return count > 0
  | .error message => throw { status := 503, message := "Redis DEL: " ++ message }
  | _ => throw { status := 503, message := "Redis DEL returned an unexpected reply" }

/-- Attempt a distributed fill lease with finite TTL (at most one minute).
Use a fresh unpredictable owner token per attempt; tokens must have at least
32 bytes. A failed acquisition returns false and never waits on the worker. -/
def Client.acquire (client : Client) (cacheKey owner : String) (leaseMs : Nat := 5000) : AppM Bool := do
  if owner.utf8ByteSize < 32 || owner.utf8ByteSize > 256 || leaseMs == 0 || leaseMs > 60000 then
    throw { status := 500, message := "Redis lease requires a 32..256 byte token and a 1..60000ms TTL" }
  let leaseKey := (client.config.namespacePrefix ++ "lease:" ++ cacheKey).toUTF8
  match ← client.command #["SET".toUTF8, leaseKey, owner.toUTF8, "NX".toUTF8, "PX".toUTF8, (toString leaseMs).toUTF8] with
  | .simple "OK" => return true
  | .bulk none => return false
  | .error message => throw { status := 503, message := "Redis lease: " ++ message }
  | _ => throw { status := 503, message := "Redis lease returned an unexpected reply" }

/-- Compare-and-delete atomically at Redis. A stale producer cannot delete a
new owner's lease after its original lease expired. -/
def Client.release (client : Client) (cacheKey owner : String) : AppM Bool := do
  let script := "if redis.call('GET',KEYS[1]) == ARGV[1] then return redis.call('DEL',KEYS[1]) else return 0 end"
  let leaseKey := (client.config.namespacePrefix ++ "lease:" ++ cacheKey).toUTF8
  match ← client.command #["EVAL".toUTF8, script.toUTF8, "1".toUTF8, leaseKey, owner.toUTF8] with
  | .integer count => return count > 0
  | .error message => throw { status := 503, message := "Redis lease release: " ++ message }
  | _ => throw { status := 503, message := "Redis lease release returned an unexpected reply" }

private def publishArguments (client : Client) (cacheKey owner : String) (value : ByteArray) (ttlMs : Nat) : Array ByteArray :=
  let script := "if redis.call('GET',KEYS[1]) == ARGV[1] then redis.call('SET',KEYS[2],ARGV[2],'PX',ARGV[3]);redis.call('DEL',KEYS[1]);return 1 else return 0 end"
  let leaseKey := (client.config.namespacePrefix ++ "lease:" ++ cacheKey).toUTF8
  #["EVAL".toUTF8, script.toUTF8, "2".toUTF8, leaseKey, key client cacheKey,
    owner.toUTF8, value, (toString ttlMs).toUTF8]

/-- Check the exact encoded fill command before deciding to cache a response. -/
def Client.canPublish (client : Client) (cacheKey owner : String) (value : ByteArray) (ttlMs : Nat) : Bool :=
  (encode (publishArguments client cacheKey owner value ttlMs) client.config.limits).isOk

/-- Atomically publish only while the fill token still owns the lease, then
release it. Expired/replaced or explicitly invalidated fills return false. -/
def Client.publish (client : Client) (cacheKey owner : String) (value : ByteArray) (ttlMs : Nat) : AppM Bool := do
  if ttlMs == 0 || ttlMs > 9223372036854775807 then
    throw { status := 500, message := "Redis publish TTL must be positive and fit signed 64 bits" }
  match ← client.command (publishArguments client cacheKey owner value ttlMs) with
  | .integer count => return count > 0
  | .error message => throw { status := 503, message := "Redis fill publish: " ++ message }
  | _ => throw { status := 503, message := "Redis fill publish returned an unexpected reply" }

end LeanWeave.Redis
