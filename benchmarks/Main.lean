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
