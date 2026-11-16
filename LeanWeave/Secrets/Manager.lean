import LeanWeave.Core
import LeanWeave.Operations.Runtime

namespace LeanWeave.Secrets

/-- Opaque secret bytes have no Repr/ToJson. All plaintext exists only in memory.
Callbacks must not log or persist these bytes. Lean immutable storage does not
promise physical zeroization of every copy after collection. -/
structure Secret where
  private mk ::
  private bytes : ByteArray
  version : String
  expiresAtMs : Nat

/-- Bootstrap a key supplied by an environment/OS secret source at startup. -/
def Secret.bootstrap (bytes : ByteArray) (expiresAtMs : Nat := 18446744073709551615) : Except String Secret := do
  if bytes.isEmpty || bytes.size > 65536 then throw "bootstrap secret size invalid"
  return ⟨bytes, "bootstrap", expiresAtMs⟩

def Secret.use (secret : Secret) (nowMs : Nat) (f : ByteArray → α) : Except AppError α :=
  if nowMs >= secret.expiresAtMs then .error { status := 503, message := "secret lease expired" } else .ok (f secret.bytes)

structure Loaded where
  value : ByteArray
  version : String
  ttlMs : Nat

abbrev Backend := String → AppM Loaded

structure Binding where
  name : String
  source : String

structure Config where
  maxSecrets : Nat := 128
  maxSecretBytes : Nat := 16384
  maxActive : Nat := 4
  maxTtlMs : Nat := 3600000
  operationTimeoutMs : Nat := 5000
  retainedBytes : Nat := 2 * 1024 * 1024
  retryMs : Nat := 1000

private structure Entry where
  binding : Binding
  current : Option Secret := none
  nextRefreshMs : Nat := 0
  operation : Option (AppM.Execution Loaded) := none
  operationStartedMs : Nat := 0
  failures : Nat := 0

private structure State where
  entries : Array Entry
  draining : Bool := false
  closed : Bool := false
  deadline : Option Nat := none

structure Stats where
  total : Nat
  valid : Nat
  refreshing : Nat
  failures : Nat
  closed : Bool
  deriving Repr, BEq

/-- Owner-worker rotating provider. Timer pulses drive bounded deferred loads;
there is no blocking network request, plaintext file or task per lookup. -/
structure Manager where
  config : Config
  private backend : Backend
  private state : IO.Ref State

def Manager.create (bindings : Array Binding) (backend : Backend) (config : Config := {}) : IO Manager := do
  if config.maxSecrets == 0 || config.maxSecrets > 1024 || config.maxSecretBytes == 0 || config.maxSecretBytes > 65536 ||
      config.maxActive == 0 || config.maxActive > 64 || config.maxTtlMs < 100 || config.maxTtlMs > 86400000 ||
      config.operationTimeoutMs == 0 || config.operationTimeoutMs > 60000 || config.retainedBytes < config.maxSecretBytes ||
      config.retryMs == 0 || config.retryMs > 60000 || bindings.size > config.maxSecrets ||
      bindings.any (fun b => b.name.isEmpty || b.name.utf8ByteSize > 128 || b.source.isEmpty || b.source.utf8ByteSize > 1024) ||
      bindings.size != (bindings.toList.map (·.name)).eraseDups.length then throw (IO.userError "invalid secret provider bounds")
  return { config, backend, state := ← IO.mkRef { entries := bindings.map fun binding => { binding } } }

def Manager.get (manager : Manager) (name : String) : IO (Except AppError Secret) := do
  let state ← manager.state.get
  if state.closed then return .error { status := 503, message := "secret provider closed" }
  let now ← IO.monoMsNow
  match (state.entries.find? (·.binding.name == name)).bind (·.current) with
  | some secret =>
    if now < secret.expiresAtMs then return .ok secret
    else return .error { status := 503, message := "secret lease expired" }
  | none => return .error { status := 503, message := "secret unavailable" }

def Manager.stats (manager : Manager) : IO Stats := do
  let s ← manager.state.get
  let now ← IO.monoMsNow
  return {
    total := s.entries.size, valid := (s.entries.filter (fun e => e.current.any (now < ·.expiresAtMs))).size
    refreshing := (s.entries.filter (·.operation.isSome)).size, failures := s.entries.foldl (fun n e => n + e.failures) 0, closed := s.closed }

def Manager.close (manager : Manager) : IO Unit := do
  let s ← manager.state.get
  if s.closed then return
  manager.state.set { s with closed := true, entries := #[] }
  for entry in s.entries do
    if let some operation := entry.operation then operation.cancel .shutdown

/-- Old version remains available only until its original expiry. Failed refresh
never extends a lease. Valid new versions atomically replace the owner snapshot. -/
def Manager.tick (manager : Manager) (nowMs : Nat) : IO Unit := do
  let state ← manager.state.get
  if state.closed then return
  if state.deadline.any (nowMs >= ·) then manager.close; return
  let mut entries := state.entries
  let mut active := (entries.filter (·.operation.isSome)).size
  for index in [:entries.size] do
    let some entry := entries[index]? | continue
    if let some operation := entry.operation then
      let timedOut := nowMs - entry.operationStartedMs >= manager.config.operationTimeoutMs
      let result ← if timedOut then do
          operation.cancel .timeout
          pure (some (.error { status := 504, message := "secret refresh timed out" }))
        else try operation.poll catch _ => pure (some (.error { status := 503, message := "secret refresh failed" }))
      if let some result := result then
        active := active - 1
        let next := { entry with operation := none, nextRefreshMs := nowMs + manager.config.retryMs }
        let next := match result with
          | .ok loaded =>
            if loaded.value.isEmpty || loaded.value.size > manager.config.maxSecretBytes || loaded.version.utf8ByteSize > 128 ||
                loaded.ttlMs < 100 || loaded.ttlMs > manager.config.maxTtlMs then { next with failures := next.failures + 1 }
            else { next with current := some ⟨loaded.value, loaded.version, nowMs + loaded.ttlMs⟩, nextRefreshMs := nowMs + loaded.ttlMs * 2 / 3 }
          | .error _ => { next with failures := next.failures + 1 }
        entries := entries.set! index next
    else if !state.draining && active < manager.config.maxActive && nowMs >= entry.nextRefreshMs then
      let operation ← try some <$> AppM.execute (manager.backend entry.binding.source) manager.config.retainedBytes catch _ => pure none
      entries := entries.set! index { entry with
        operation, operationStartedMs := nowMs, nextRefreshMs := nowMs + manager.config.retryMs
        failures := entry.failures + (if operation.isNone then 1 else 0) }
      if operation.isSome then active := active + 1
  manager.state.modify fun s => { s with entries }

def Manager.beginDrain (manager : Manager) (deadline : Nat) : IO Unit :=
  manager.state.modify fun s => { s with draining := true, deadline := some deadline }

def Manager.isDrained (manager : Manager) : IO Bool := do
  return (← manager.state.get).entries.all (·.operation.isNone)

def Manager.participant (manager : Manager) : Operations.Participant := {
  name := "secrets"
  onTick := manager.tick
  onDrain := manager.beginDrain
  isDrained := manager.isDrained
  close := manager.close }

end LeanWeave.Secrets
