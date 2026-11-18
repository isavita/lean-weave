# Getting started with LeanWeave

## Prerequisites

Install [elan](https://github.com/leanprover/elan) and the native development
libraries used by lean-net. Use Lean `v4.35.0` for the revision below.

On Debian or Ubuntu:

```sh
sudo apt-get install build-essential pkg-config libgmp-dev libuv1-dev libssl-dev zlib1g-dev libbrotli-dev
export LEAN_CC=cc
```

On macOS with Command Line Tools installed:

```sh
brew install zlib brotli pkgconf
export PKG_CONFIG_PATH="$(brew --prefix zlib)/lib/pkgconfig:$(brew --prefix brotli)/lib/pkgconfig${PKG_CONFIG_PATH:+:$PKG_CONFIG_PATH}"
```

On Windows, install `mingw-w64-x86_64-gcc`, `mingw-w64-x86_64-pkgconf`,
`mingw-w64-x86_64-zlib` and `mingw-w64-x86_64-brotli` in MSYS2 MINGW64.
Put its `mingw64/bin` on `PATH` and set `PKG_CONFIG_PATH` to `mingw64/lib/pkgconfig`.
See the [CI workflow](../.github/workflows/ci.yml) for the exact setup tested.

## Add LeanWeave to a Lake project

Create an empty folder and save the following three files. In an existing
project, add only the `[[require]]` stanza and verify its `lean-toolchain`
matches. Lake fetches the pinned lean-net dependency transitively.

`lean-toolchain`

```text
leanprover/lean4:v4.35.0
```

`lakefile.toml`

```toml
name = "hello-weave"
defaultTargets = ["app"]

[[require]]
name = "lean-weave"
git = "https://github.com/isavita/lean-weave.git"
rev = "03227a868e16c9fbf70a1e6ca0a48fcb6ddcc1eb"

[[lean_exe]]
name = "app"
root = "Main"
```

`Main.lean`

```lean
import LeanWeave

open LeanWeave
open scoped LeanWeave.Router

private def hello (name : String) : Handler := fun _ =>
  pure (Html.toResponse (Html.page "Greeting" (Html.text s!"Hello, {name}!")))

weave_routes routes where {
  route "GET" "/hello/:name<text>" as hello => hello
}

def main : IO Unit :=
  Server.serve (fun _ => pure routes.handler)
```

From the project folder, run `lake update` and `lake exe app`. In another
terminal, run `curl -fsS http://127.0.0.1:8080/hello/Lean` to see `Hello, Lean!`
in the HTML. Stop the server with Ctrl+C. Commit `lake-manifest.json` to keep
the resolved dependency revisions reproducible.

This entry point uses the default `Server.Config`. To read environment settings
at startup, call `Server.Config.fromEnv` and handle its validation errors, as
the scaffold does.

For a local source checkout, replace `git` and `rev` with `path = "../lean-weave"`
in the dependency stanza. The optional adapter guides use this setup so the
framework and its adapters come from the same checkout. Run `lake update`
after changing a dependency.

## Scaffold a project with weave

From a local LeanWeave source checkout:

```sh
lake build weave
weave_bin="$PWD/.lake/build/bin/weave"
"$weave_bin" new ../my-app --path "$PWD"
cd ../my-app
"$weave_bin" build
"$weave_bin" routes
"$weave_bin" test
"$weave_bin" dev
```

`new` requires a new directory. It records the selected local checkout in
`lakefile.toml` and creates the route table in `App.lean`, the server in
`Main.lean` and a smoke test in `Test.lean`. `WEAVE_PATH` can select the checkout
in place of `--path`. On Windows the native executable is `weave.exe`.

`dev` requires `python3` on `PATH`. Open `http://127.0.0.1:8080/` while it runs.
Save an edit to `App.lean` and refresh after the rebuild. A compilation error
keeps the previous server running. Stopping the launcher cancels an active
build and lets the running application drain. The launcher reserves standard
input for this supervision. `WEAVE_PORT` selects a different port.
