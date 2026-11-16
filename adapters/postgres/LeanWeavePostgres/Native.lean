import LeanWeave.Core

namespace LeanWeave.Postgres

/-- Text-protocol values. `none` is SQL NULL, distinct from the empty string. -/
abbrev Row := Array (Option String)

structure Result where
  columns : Array String
  rows : Array Row
  commandTag : String
  deriving Repr, Inhabited

namespace Native

private opaque HandleImpl : NonemptyType
def Handle : Type := HandleImpl.type
instance : Nonempty Handle := HandleImpl.property

@[extern "weave_pg_start"] opaque start (options : @& Array String) : IO Handle
@[extern "weave_pg_connect_poll"] opaque connectPoll (handle : @& Handle) : IO Bool
@[extern "weave_pg_send"] opaque send (handle : @& Handle) (sql : @& String)
  (params : @& Array (Option String)) (maxRows maxBytes maxColumns : USize) : IO Unit
@[extern "weave_pg_poll"] opaque poll (handle : @& Handle) : IO (Option Result)
@[extern "weave_pg_healthy"] opaque healthy (handle : @& Handle) : IO Bool
@[extern "weave_pg_close"] opaque close (handle : @& Handle) : IO Unit

end Native
end LeanWeave.Postgres
