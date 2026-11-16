import LeanWeave
import LeanWeave.Test
import LeanWeavePostgres
import LeanWeaveRedis
import LeanWeaveTLS

/-!
# LeanWeave

A typed, cooperative Lean web framework over lean-net. Start with
`LeanWeave.Server.Config` and `LeanWeave.Server.serve`. Build named routing tables
with `weave_routes`, checked HTML with `html!`, and compile ordered middleware
once using `LeanWeave.pipeline`.

Application continuations, cache stores and adapters remain on their owner
worker. `LeanWeave.AppM.await` suspends without creating per-request threads.
The optional adapters are documented here too: `LeanWeave.Postgres.Pool` provides
bounded PostgreSQL queries, transactions and migrations; `LeanWeave.Redis.Client`
and its cache helpers provide deferred Redis operations; `LeanWeave.TLS.serve`
and `LeanWeave.TLS.serveStatic` preserve native TLS and compressed static output.
Applications only link these adapters when their packages are explicitly required.
See the repository README and runnable `Examples` for complete applications.
-/
