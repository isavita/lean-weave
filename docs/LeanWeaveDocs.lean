import LeanWeave
import LeanWeave.Test

/-!
# LeanWeave

A typed, cooperative Lean web framework over lean-net. Start with
`LeanWeave.Server.Config` and `LeanWeave.Server.serve`. Build named routing tables
with `weave_routes`, checked HTML with `html!`, and compile ordered middleware
once using `LeanWeave.pipeline`.

Application continuations, cache stores and adapters remain on their owner
worker. `LeanWeave.AppM.await` suspends without creating per-request threads.
See the repository README and runnable `Examples` for complete applications.
-/
