import LeanWeave

open LeanWeave
open scoped LeanWeave.Router

private def showPage (id : Nat) : Handler := fun _ =>
  pure (Html.toResponse (Html.page "Benchmark" (Html.text (toString id))))

weave_routes benchRoutes where {
  route "GET" "/pages/:id<nat>" as pages.show => showPage
}

private def card : String → Html.Html := html! "<article><h2>{{name}}</h2><p>Precomputed static content.</p></article>"

@[noinline] private def renderCard (n : Nat) : Nat :=
  (Html.render (card ("Item <" ++ toString n ++ ">"))).size

private def sampleBenchmark (name : String) (count : Nat) (action : Nat → IO Nat) : IO Unit := do
  -- Initialize lazy literals and JIT-independent native state before the sample.
  let _ ← action 0
  let allocationStart ← IO.getNumHeartbeats
  let start ← IO.monoNanosNow
  let mut checksum := 0
  for i in [:count] do checksum := checksum + (← action i)
  let elapsed := (← IO.monoNanosNow) - start
  let allocations := (← IO.getNumHeartbeats) - allocationStart
  IO.println <| (Lean.Json.mkObj [
    ("name", Lean.toJson name), ("iterations", Lean.toJson count),
    ("nanoseconds", Lean.toJson elapsed), ("small_allocations", Lean.toJson allocations),
    ("allocation_scope", Lean.toJson ("Lean thread small allocations, including loop and IO scaffolding; excludes large and native allocations" : String)),
    ("checksum", Lean.toJson checksum)]).compress

def main (args : List String) : IO Unit := do
  let count := (args.head?.bind String.toNat?).getD 20000
  if count == 0 || count > 10000000 then throw (IO.userError "iterations must be 1..10000000")
  sampleBenchmark "compiled_html" count (fun i => pure (renderCard i))
  sampleBenchmark "decode_route_render" count fun i => do
    let request : LeanNet.HTTP1.Request := { method := "GET", target := s!"/pages/{i}?q=lean".toUTF8, headers := #[{ name := "host", value := "benchmark.test".toUTF8 }], body := ByteArray.empty }
    match Context.fromRequest request with
    | .error error => throw (IO.userError error.message)
    | .ok context =>
      match ← (benchRoutes.handler context).run with
      | .error error => throw (IO.userError error.message)
      | .ok response => return response.body.size
  let store ← Cache.Store.new
  let _ ← store.put "hot" (Html.render (card "cached")) 600000 (← IO.monoMsNow)
  sampleBenchmark "fragment_cache_hit" count fun _ => do
    return ((← store.get "hot" (← IO.monoMsNow)).map (·.size)).getD 0

  let responseStore ← Cache.ResponseStore.new
  let cacheKey := Cache.CacheKey.ofParts #["benchmark", "response-store-hit"]
  let body := (String.ofList (List.replicate 1024 'x')).toUTF8
  let entry : Cache.CacheEntry := {
    status := 200
    headers := #[{ name := "cache-control", value := "public, max-age=3600".toUTF8 }]
    body
    ttlMs := 3600000
    storedAt := ← IO.monoMsNow
    baseKey := cacheKey
    varyNames := #[]
    varyFingerprint := cacheKey
  }
  if !(← responseStore.storeChecked cacheKey entry) then
    throw (IO.userError "could not seed response cache benchmark")
  sampleBenchmark "response_store_lookup_hit" count fun _ => do
    let some cached ← responseStore.lookup cacheKey
      | throw (IO.userError "response cache lookup missed during hit benchmark")
    return cached.body.size

  let request : LeanNet.HTTP1.Request := {
    method := "GET"
    target := "/cached?q=lean".toUTF8
    headers := #[{ name := "host", value := "benchmark.test".toUTF8 }]
    body := ByteArray.empty
  }
  let context ← match Context.fromRequest request with
    | .error error => throw (IO.userError error.message)
    | .ok context => pure { context with routeName := "benchmark.cached" }
  let middlewareStore ← Cache.ResponseStore.new
  let upstreamCalls ← IO.mkRef (0 : Nat)
  let cachedHandler := Cache.middleware middlewareStore {
    ttlMs := 3600000
    admit := some (fun ctx => pure (.cache ctx "public:benchmark"))
  } (fun _ => do
    upstreamCalls.modify (· + 1)
    pure (({ body } : Response).withHeader "cache-control" "public, max-age=3600"))
  let runCached : IO Nat := do
    match ← (cachedHandler context).run with
    | .error error => throw (IO.userError error.message)
    | .ok response => return response.body.size
  if (← runCached) != body.size then
    throw (IO.userError "could not seed response middleware benchmark")
  sampleBenchmark "response_middleware_hit" count fun _ => runCached
  if (← upstreamCalls.get) != 1 then
    throw (IO.userError "response middleware benchmark reached upstream on a cache hit")
