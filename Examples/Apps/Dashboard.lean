import Examples.Common
import LeanWeave.Cache

namespace LeanWeave.Examples.Dashboard

private def dashboard (fragments : Cache.Store) (generations : IO.Ref Nat) : Handler := fun ctx => do
  generations.modify (· + 1)
  let generation ← generations.get
  let fragment ← Cache.fragment fragments (Cache.key #["dashboard-card", ctx.host, ctx.tenant, ctx.locale]) 1000 do
    let count ← generations.get
    pure (Html.render ((html! "<p>Fragment generation {{count}}</p>") (toString count)))
  let page := Html.page "Cached dashboard" (Html.fragment #[
    (html! "<h1>Cached dashboard</h1><p>Response generation {{generation}}</p>") (toString generation),
    Html.rawUnsafe fragment,
    Html.element .p #[] #[Html.text "Public responses expire after two seconds. Try If-None-Match or HEAD."]])
  pure <| (Html.negotiated ctx page (Lean.Json.mkObj [("generation", Lean.toJson generation),
    ("worker", Lean.toJson ((ctx.attribute "worker").getD "0"))])).withHeader "cache-control" "public, max-age=2"

/-- Both caches are worker-owned and bounded. A restart/expiry drops local values;
no cross-worker coherence is claimed. Host, query, locale and Vary separate entries. -/
def factory : Factory := fun worker => do
  let responses ← Cache.ResponseStore.new {
    maxEntries := 128
    maxBytes := 256 * 1024
    maxEntryBytes := 32 * 1024
  }
  let fragments ← Cache.Store.new { maxEntries := 64, maxBytes := 64 * 1024, maxEntryBytes := 8192 }
  let generations ← IO.mkRef 0
  let addWorker : Middleware := fun next ctx => next { ctx with attributes := ctx.attributes.push ("worker", toString worker) }
  router #[
    ("GET", "/", "dashboard", pipeline #[addWorker, Cache.middleware responses {
      ttlMs := 2000
      admit := some (fun ctx => pure (.cache ctx "public:dashboard"))
    }] (dashboard fragments generations))]

end LeanWeave.Examples.Dashboard
