import LeanWeavePostgres.Native
import Std.Net.Addr
import LeanWeave.Observability.Trace

namespace LeanWeave.Postgres

/-- Numeric hostaddr prevents blocking DNS in PQconnectStart/PQconnectPoll.
    hostName is only used for PostgreSQL TLS certificate verification. -/
structure Config where
  hostAddr : String := "127.0.0.1"
  hostName : String := "localhost"
  port : UInt16 := 5432
  database : String := "postgres"
  user : String := "postgres"
  password : String := ""
  sslMode : String := "verify-full"
  /-- PEM CA bundle. Empty uses libpq's default trust-file lookup. -/
  sslRootCert : String := ""
  maxConnections : Nat := 4
  maxWaiters : Nat := 64
  connectTimeoutMs : Nat := 5000
  queryTimeoutMs : Nat := 30000
  maxQueryBytes : Nat := 65536
  maxParameters : Nat := 128
  maxParameterBytes : Nat := 64 * 1024
  maxRows : Nat := 1000
  maxResultBytes : Nat := 256 * 1024
  maxColumns : Nat := 128

private def validNumericAddress (address : String) : Bool :=
  (Std.Net.IPv4Addr.ofString address).isSome || (Std.Net.IPv6Addr.ofString address).isSome

def Config.validate (config : Config) : Except String Unit := do
  if !validNumericAddress config.hostAddr then throw "PostgreSQL hostAddr must be a numeric IP address"
  if config.port == 0 then throw "PostgreSQL port must be positive"
  unless ["disable", "require", "verify-ca", "verify-full"].contains config.sslMode do
    throw "unsupported PostgreSQL TLS mode"
  for value in #[config.hostAddr, config.hostName, config.database, config.user, config.password, config.sslRootCert] do
    if value.toUTF8.toList.contains 0 || value.utf8ByteSize > 8192 then
      throw "PostgreSQL connection option contains NUL or exceeds 8192 bytes"
  if config.sslMode == "verify-full" && config.hostName.isEmpty then throw "verify-full requires a TLS hostname"
  if config.maxConnections == 0 || config.maxConnections > 1024 || config.maxWaiters > 65536 then
    throw "invalid PostgreSQL pool bounds"
  if config.connectTimeoutMs == 0 || config.queryTimeoutMs == 0 ||
      config.connectTimeoutMs > 86400000 || config.queryTimeoutMs > 86400000 ||
      config.maxResultBytes < 1024 || config.maxResultBytes > 256 * 1024 * 1024 ||
      config.maxRows == 0 || config.maxRows > 1000000 || config.maxColumns == 0 || config.maxColumns > 1024 ||
      config.maxQueryBytes == 0 || config.maxQueryBytes > 1024 * 1024 || config.maxParameters > 65535 ||
      config.maxParameterBytes == 0 || config.maxParameterBytes > 16 * 1024 * 1024 then
    throw "PostgreSQL operation limits must be positive and bounded"

/-- Required application adapter reservation, excluding the HTTP request itself. -/
def Config.retainedBytes (config : Config) : Nat :=
  max 8192 (config.maxResultBytes + config.maxParameterBytes + config.maxQueryBytes + config.maxParameters * 64 + 256)

/-- Read connection settings without logging credentials. Numeric pool and
    timeout settings are strict; malformed environment values fail startup. -/
def Config.fromEnv (defaults : Config := {}) : IO (Except String Config) := do
  let text := fun key fallback => do return (← IO.getEnv ("WEAVE_PG_" ++ key)).getD fallback
  let number := fun key fallback => do
    let value ← text key (toString fallback)
    return match value.toNat? with
      | some value => Except.ok value
      | none => Except.error s!"WEAVE_PG_{key} must be an unsigned decimal integer"
  let hostAddr ← text "HOSTADDR" defaults.hostAddr
  let hostName ← text "HOST" defaults.hostName
  let database ← text "DATABASE" defaults.database
  let user ← text "USER" defaults.user
  let password ← text "PASSWORD" defaults.password
  let sslMode ← text "SSLMODE" defaults.sslMode
  let sslRootCert ← text "SSLROOTCERT" defaults.sslRootCert
  let port ← number "PORT" defaults.port.toNat
  let maxConnections ← number "MAX_CONNECTIONS" defaults.maxConnections
  let maxWaiters ← number "MAX_WAITERS" defaults.maxWaiters
  let connectTimeoutMs ← number "CONNECT_TIMEOUT_MS" defaults.connectTimeoutMs
  let queryTimeoutMs ← number "QUERY_TIMEOUT_MS" defaults.queryTimeoutMs
  let maxResultBytes ← number "MAX_RESULT_BYTES" defaults.maxResultBytes
  return do
    let port ← port
    if port == 0 || port > 65535 then throw "WEAVE_PG_PORT must be from 1 to 65535"
    let config := { defaults with
      hostAddr, hostName, database, user, password, sslMode, sslRootCert,
      port := port.toUInt16, maxConnections := ← maxConnections, maxWaiters := ← maxWaiters,
      connectTimeoutMs := ← connectTimeoutMs, queryTimeoutMs := ← queryTimeoutMs, maxResultBytes := ← maxResultBytes }
    config.validate
    return config

private def Config.options (config : Config) : Array String := #[
  config.hostAddr, config.hostName, toString config.port, config.database, config.user,
  config.password, config.sslMode, "lean-weave",
  s!"-c statement_timeout={config.queryTimeoutMs} -c idle_in_transaction_session_timeout={config.queryTimeoutMs}",
  "UTF8", config.sslRootCert]

/-- A lease is valid only within `Pool.withConnection` or `Pool.transaction`.
    The native handle rejects operations from a different worker thread. -/
structure Connection where
  private mk ::
  private native : Native.Handle
  private config : Config
  private leased : IO.Ref Bool
  /-- Physical-slot epoch plus this acquisition's immutable snapshot. An escaped
      lease cannot become valid when the physical connection is borrowed again. -/
  private epoch : IO.Ref Nat
  private leaseEpoch : Nat
  private connected : IO.Ref Bool
  private failed : IO.Ref Bool
  private trace : Option (Observability.Tracer × Option String) := none

/-- Worker-local fixed-capacity pool. The waiting queue stores only resumable
    leases; saturation fails immediately with 503, without allocating a task. -/
structure Pool where
  private mk ::
  private config : Config
  private connections : IO.Ref (Array Connection)
  private waiting : IO.Ref Nat
  private closed : IO.Ref Bool
  private tracer : Option Observability.Tracer := none
  private parentTrace : Option String := none

private def attemptIO (action : IO α) : IO (Except AppError α) := do
  try return .ok (← action)
  catch error => return .error { status := 503, message := error.toString }

private def databaseIO (action : IO α) : AppM α := do
  match ← attemptIO action with
  | .ok value => return value
  | .error error => throw error

def Pool.create (config : Config := {}) (tracer : Option Observability.Tracer := none) : IO Pool := do
  match config.validate with
  | .error message => throw (IO.userError message)
  | .ok _ => return { config, connections := ← IO.mkRef #[], waiting := ← IO.mkRef 0, closed := ← IO.mkRef false, tracer }

private def Connection.closePhysical (connection : Connection) : IO Unit := do
  connection.failed.set true
  connection.leased.set false
  connection.connected.set false
  Native.close connection.native

private def Connection.isCurrent (connection : Connection) : IO Bool := do
  return (← connection.leased.get) && !(← connection.failed.get) &&
    (← connection.epoch.get) == connection.leaseEpoch

private def Connection.close (connection : Connection) : IO Unit := do
  if ← connection.isCurrent then connection.closePhysical

/-- Immediate, idempotent closure cancels leases and releases every database socket. -/
def Pool.close (pool : Pool) : IO Unit := do
  pool.closed.set true
  let connections ← pool.connections.get
  pool.connections.set #[]
  for connection in connections do connection.closePhysical

private def Pool.tryAcquire (pool : Pool) : IO (Option Connection) := do
  if ← pool.closed.get then throw (IO.userError "PostgreSQL pool is closed")
  let mut live : Array Connection := #[]
  let mut available : Option Connection := none
  for connection in ← pool.connections.get do
    unless ← connection.failed.get do
      if available.isNone && !(← connection.leased.get) then
        let previous ← connection.epoch.get
        if previous >= 9223372036854775807 then connection.closePhysical
        else
          let epoch := previous + 1
          connection.epoch.set epoch
          connection.leased.set true
          available := some { connection with leaseEpoch := epoch }
      unless ← connection.failed.get do live := live.push connection
  pool.connections.set live
  if available.isSome then return available
  if live.size >= pool.config.maxConnections then return none
  let native ← Native.start pool.config.options
  let connection := Connection.mk native pool.config (← IO.mkRef true) (← IO.mkRef 1) 1
    (← IO.mkRef false) (← IO.mkRef false) none
  pool.connections.set (live.push connection)
  return some connection

private def Pool.acquire (pool : Pool) : AppM Connection := do
  if let some connection ← databaseIO pool.tryAcquire then return connection
  if (← databaseIO pool.waiting.get) >= pool.config.maxWaiters then
    throw { status := 503, message := "PostgreSQL pool wait queue is full" }
  databaseIO (pool.waiting.modify (· + 1))
  let released ← databaseIO (IO.mkRef false)
  let release := do
    unless ← released.get do
      released.set true
      pool.waiting.modify (· - 1)
  AppM.ensuring (AppM.await
    (do
      try
        match ← pool.tryAcquire with
        | none => return none
        | some connection => return some (.ok connection)
      catch error => return some (.error { status := 503, message := error.toString }))
    (fun _ => release) pool.config.connectTimeoutMs 128) release

private def Connection.ensureConnected (connection : Connection) : AppM Unit := do
  unless ← databaseIO connection.isCurrent do
    throw { status := 503, message := "PostgreSQL lease is no longer active" }
  unless ← databaseIO connection.connected.get do
    AppM.await
      (do
        try
          unless ← connection.isCurrent do
            return some (.error { status := 503, message := "PostgreSQL lease is no longer active" })
          if ← Native.connectPoll connection.native then
            connection.connected.set true
            return some (.ok ())
          return none
        catch error =>
          connection.close
          return some (.error { status := 503, message := error.toString }))
      (fun _ => connection.close) connection.config.connectTimeoutMs 8192

private def Connection.release (connection : Connection) : IO Unit := do
  unless ← connection.isCurrent do return
  let healthy ← Native.healthy connection.native
  unless healthy do connection.closePhysical
  connection.leased.set false

/-- Run a bounded operation under a lease. Completion, failure, timeout and
    disconnect cancellation all release the lease; an open transaction is closed. -/
def Pool.withConnection (pool : Pool) (use : Connection → AppM α) : AppM α := do
  let connection ← pool.acquire
  let connection := { connection with trace := pool.tracer.map (fun tracer => (tracer, pool.parentTrace)) }
  AppM.ensuring (do connection.ensureConnected; use connection) connection.release

/-- Clone only tracing context while sharing owner-local pool state. Construct
per request; concurrent leases never depend on a mutable thread-local parent. -/
def Pool.withTraceContext (pool : Pool) (context : Context) : Pool :=
  { pool with parentTrace := Observability.traceparent context }

/-- Extended-query protocol separates SQL and parameter bytes. SQL NULL is `none`.
    Results use single-row mode and enforce row, column and retained-byte bounds. -/
def Connection.query (connection : Connection) (sql : String)
    (params : Array (Option String) := #[]) : AppM Result := do
  unless ← databaseIO connection.isCurrent do
    throw { status := 503, message := "PostgreSQL lease is no longer active" }
  if sql.isEmpty || sql.utf8ByteSize > connection.config.maxQueryBytes || sql.toUTF8.toList.contains 0 then
    throw { status := 400, message := "invalid or oversized SQL statement" }
  if params.size > connection.config.maxParameters then
    throw { status := 400, message := "too many SQL parameters" }
  let mut parameterBytes := 0
  for param in params do
    if let some value := param then
      if value.toUTF8.toList.contains 0 then throw { status := 400, message := "SQL text parameter contains NUL" }
      parameterBytes := parameterBytes + value.utf8ByteSize
  if parameterBytes > connection.config.maxParameterBytes then
    throw { status := 413, message := "SQL parameter budget exceeded" }
  let span ← match connection.trace with
    | some (tracer, parent) => some <$> databaseIO (tracer.start "postgres.query" parent .client #[
      { key := "db.system.name", value := .text "postgresql" },
      { key := "server.address", value := .text connection.config.hostName }])
    | none => pure none
  let finish := fun status code => do
    if let some span := span then span.finish status #[] code
  (do
    try
      databaseIO (Native.send connection.native sql params connection.config.maxRows.toUSize
        connection.config.maxResultBytes.toUSize connection.config.maxColumns.toUSize)
      let result ← AppM.await
        (do
          unless ← connection.isCurrent do
            return some (.error { status := 503, message := "PostgreSQL lease is no longer active" })
          try return (← Native.poll connection.native).map Except.ok
          catch error =>
            connection.close
            let status := if (error.toString.splitOn "57014").length > 1 then 504 else 503
            return some (.error { status, message := error.toString }))
        (fun _ => connection.close) connection.config.queryTimeoutMs
        (connection.config.maxResultBytes + parameterBytes + sql.utf8ByteSize + params.size * 64 + 256)
      finish .unset ""
      return result
    catch error =>
      finish .error "database.error"
      throw error).ensuring (finish .error "database.cancelled_or_failed")

def Pool.query (pool : Pool) (sql : String) (params : Array (Option String) := #[]) : AppM Result :=
  pool.withConnection fun connection => connection.query sql params

inductive Isolation where
  | readCommitted | repeatableRead | serializable
  deriving BEq, Repr

/-- Transactions retain one lease. Application failures issue ROLLBACK; a cancelled
    or broken connection is dropped, so PostgreSQL rolls back on disconnect. -/
def Pool.transaction (pool : Pool) (use : Connection → AppM α)
    (isolation : Isolation := .readCommitted) : AppM α :=
  pool.withConnection fun connection => do
    let mode := match isolation with
      | .readCommitted => "READ COMMITTED"
      | .repeatableRead => "REPEATABLE READ"
      | .serializable => "SERIALIZABLE"
    let _ ← connection.query ("BEGIN ISOLATION LEVEL " ++ mode)
    try
      let value ← use connection
      let _ ← connection.query "COMMIT"
      return value
    catch error =>
      try let _ ← connection.query "ROLLBACK"; pure () catch _ => pure ()
      throw error

structure Migration where
  version : Nat
  /-- Trusted application DDL; each entry is one PostgreSQL statement. -/
  statements : Array String

/-- Serialize migration runners with a transaction-scoped advisory lock. Each
    migration and its SHA-256 checksum commit atomically. Reusing an applied
    version with different statements fails instead of silently changing history. -/
def Pool.migrate (pool : Pool) (migrations : Array Migration) : AppM Unit := do
  let ordered := migrations.qsort (fun a b => a.version < b.version)
  let mut previous : Option Nat := none
  for migration in ordered do
    if previous == some migration.version then throw { status := 500, message := "duplicate migration version" }
    previous := some migration.version
  pool.transaction fun connection => do
    let _ ← connection.query "SELECT pg_advisory_xact_lock(549589292)"
    let _ ← connection.query "CREATE TABLE IF NOT EXISTS public.leanweave_schema_migrations (version bigint PRIMARY KEY, checksum text NOT NULL)"
    for migration in ordered do
      if migration.version > 9223372036854775807 then throw { message := "migration version exceeds bigint" }
      let payload := String.join (migration.statements.toList.map fun statement => s!"{statement.utf8ByteSize}:{statement}")
      let params := #[some (toString migration.version), some payload]
      let applied ← connection.query "SELECT checksum = encode(sha256(convert_to($2::text, 'UTF8')), 'hex') AS matches FROM public.leanweave_schema_migrations WHERE version = $1::bigint" params
      if applied.rows.isEmpty then
        for statement in migration.statements do let _ ← connection.query statement
        let _ ← connection.query "INSERT INTO public.leanweave_schema_migrations(version, checksum) VALUES ($1::bigint, encode(sha256(convert_to($2::text, 'UTF8')), 'hex'))" params
      else if applied.rows != #[#[some "t"]] then
        throw { status := 500, message := s!"migration {migration.version} checksum mismatch" }

end LeanWeave.Postgres
