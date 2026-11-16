import LeanWeave.Tenant.Config

namespace LeanWeave.Tenant.ConfigTest

private def check (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw (IO.userError ("tenant config audit: " ++ message))

private def need (result : Except ε α) : IO α := match result with
  | .ok value => pure value
  | .error _ => throw (IO.userError "expected successful tenant config result")

private def variant (store : Store) (id : Id) : IO String := do
  let ctx : Context := {
    tenant := id.value
    request := { method := "GET", target := "/".toUTF8, headers := #[], body := ByteArray.empty } }
  let response ← need (← (configured store (fun ctx =>
    pure (Response.text ((ctx.attribute "tenant.config.variant").getD "missing"))) ctx).run)
  return String.fromUTF8! response.body

private def change (record : Security.Audit.Record) (key : String) : String :=
  ((record.event.changes.find? (·.1 == key)).map Prod.snd).getD "missing"

def run : IO Unit := do
  let alpha ← need (Id.parse "alpha")
  let beta ← need (Id.parse "beta")
  let initial : Config := { flags := #[("new-ui", false)], brandName := "Alpha" }
  let updated : Config := { initial with flags := #[("new-ui", true)] }
  let store ← Store.new 4 2
  let _ ← need (← store.set alpha initial "admin:alice")
  let old ← need (← store.get alpha)
  let _ ← need (← store.set alpha updated "admin:bob")
  let current ← need (← store.get alpha)
  check (current.map (·.revision) == some 2) "reload advances revision"
  check (!(← store.set beta initial).isOk && !(← store.remove alpha).isOk) "audit count exhaustion rejects mutations"
  check ((← need (← store.get alpha)) == current && (← need (← store.get beta)).isNone) "saturation preserves existing snapshots"
  let records ← need (← store.audit)
  check (records.size == 2) "set/update each append one record"
  let some a := records[0]? | throw (IO.userError "missing first audit")
  let some b := records[1]? | throw (IO.userError "missing second audit")
  check (a.event.action == "tenant.config.set" && a.event.actor == "admin:alice" && b.event.actor == "admin:bob") "actor/action metadata"
  check (a.event.tenant == "alpha" && a.event.target == "alpha" && a.event.timeUnixNano > 0) "tenant and wall-clock metadata"
  check (change a "before" == "null" && change a "after" == (Lean.toJson old).compress) "first complete snapshot"
  check (change b "before" == (Lean.toJson old).compress && change b "after" == (Lean.toJson current).compress) "update before/after snapshots"
  check ((Security.Audit.verify (records.map (·.json.compress)) {} (some b.checkpoint)).isOk) "verified hash chain"
  check (!(← store.acknowledgeAudit {}).isOk) "unknown persistence checkpoint rejected"
  let _ ← need (← store.acknowledgeAudit a.checkpoint)
  let _ ← need (← store.remove alpha "admin:carol")
  check ((← need (← store.get alpha)).isNone) "audited removal applied"
  let remaining ← need (← store.audit)
  let some removed := remaining[1]? | throw (IO.userError "missing removal audit")
  check (remaining.size == 2 && removed.sequence == 3) "ack frees prefix without resetting chain"
  check (removed.event.action == "tenant.config.remove" && removed.event.actor == "admin:carol") "remove metadata"
  check (change removed "before" == (Lean.toJson current).compress && change removed "after" == "null") "remove snapshots"
  check ((Security.Audit.verify (remaining.map (·.json.compress)) a.checkpoint (some removed.checkpoint)).isOk) "chain continues across acknowledgement"
  let _ ← need (← store.acknowledgeAudit removed.checkpoint)
  let _ ← need (← store.set alpha initial)
  let recreated ← need (← store.get alpha)
  check (recreated.map (·.revision) == some 3) "remove/recreate cannot reuse cached revision"
  let last ← need (← store.audit)
  let some finalRecord := last[0]? | throw (IO.userError "missing recreated audit")
  check (last.size == 1 && finalRecord.sequence == 4 && finalRecord.event.actor == "system") "default actor and persistent chain checkpoint"
  let _ ← need (← store.acknowledgeAudit finalRecord.checkpoint)
  let _ ← need (← store.set beta initial)
  check ((← need (← store.get beta)).map (·.revision) == some 4) "revision watermark advances across tenants"
  let firstStore ← Store.new
  let secondStore ← Store.new
  let sameStore ← Store.new
  let _ ← need (← firstStore.set alpha initial)
  let _ ← need (← secondStore.set alpha { initial with brandName := "Changed after restart" })
  let _ ← need (← sameStore.set alpha initial)
  let firstVariant ← variant firstStore alpha
  check (firstVariant.utf8ByteSize == 64 && firstVariant != (← variant secondStore alpha)) "different settings cannot share restarted-store cache variant"
  check (firstVariant == (← variant sameStore alpha)) "canonical identical effective configuration shares variant"
  let byteStore ← Store.new 2 10 (a.line.utf8ByteSize + 16)
  let _ ← need (← byteStore.set alpha initial "admin:alice")
  check (!(← byteStore.set alpha updated "admin:bob").isOk) "audit bytes exhaustion rejects update"
  check ((← need (← byteStore.get alpha)) == old && (← need (← byteStore.audit)).size == 1) "byte saturation preserves snapshot and chain"
  check (!(← byteStore.remove alpha).isOk) "audit bytes exhaustion rejects removal"
  let snapshotTooLarge : Config := { flags := (List.range 40).toArray.map fun n =>
    (String.ofList (List.replicate 100 'x') ++ toString n, true) }
  check (!snapshotTooLarge.validate.isOk) "serialized snapshot bounded to 4096 bytes"
  let mut boundary : Option Config := none
  for width in [100:129] do
    let candidate : Config := {
      revision := 9
      brandName := "b"
      flags := (List.range 29).toArray.map fun n =>
        (String.ofList (List.replicate width 'x') ++ toString n, false) }
    let bytes := (Lean.toJson candidate).compress.utf8ByteSize
    if bytes <= 4096 && 4096 - bytes < 256 then
      boundary := some { candidate with brandName := String.ofList (List.replicate (4097 - bytes) 'b') }
  let some snapshot := boundary | throw (IO.userError "missing bounded snapshot fixture")
  check ((Lean.toJson snapshot).compress.utf8ByteSize == 4096 && snapshot.validate.isOk) "exact-size snapshot accepted"
  let boundaryStore ← Store.new
  let _ ← need (← boundaryStore.set alpha snapshot)
  check (!(← boundaryStore.set alpha snapshot).isOk) "effective revision cannot expand snapshot past 4096 bytes"
  check ((← need (← boundaryStore.get alpha)) == some snapshot &&
    (← need (← boundaryStore.audit)).size == 1) "oversized effective revision leaves snapshot and audit unchanged"
  let badActor ← firstStore.set alpha updated ""
  check (!badActor.isOk && (← need (← firstStore.get alpha)) == old) "invalid audit metadata cannot mutate config"
  let invalidRevision := { initial with revision := 9223372036854775808 }
  check (!invalidRevision.validate.isOk) "revision must fit UInt63"
  let exhausted ← Store.new
  let _ ← need (← exhausted.set alpha { initial with revision := 9223372036854775807 })
  check (!(← exhausted.set beta initial).isOk) "revision generation never wraps"
  IO.println "PASS tenant config audit: atomic history, metadata, chain/ack, count/bytes, revisions and cache variants"

end LeanWeave.Tenant.ConfigTest
