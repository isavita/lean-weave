import Lean
-- Retain lean-net's native consumer link dependencies for system-compiler builds.
import LeanNet.HTTP1.Request

namespace LeanWeave.Tooling

private def usage : String :=
  "LeanWeave\n  weave new DIRECTORY [--path LEAN_WEAVE]\n  weave dev [TARGET=app]\n  weave routes [TARGET=app]\n  weave test [TARGET=app-test]\n  weave build [TARGET=app]\n\nRun project commands from its Lake package directory. Generated projects use a\nlocal LeanWeave dependency; --path or WEAVE_PATH selects its source checkout.\n"

private def process (cmd : String) (args : Array String) : IO UInt32 := do
  let child ← IO.Process.spawn { cmd, args }
  child.wait

private def sourceRoot : IO System.FilePath := do
  if let some path ← IO.getEnv "WEAVE_PATH" then
    let path ← IO.FS.realPath path
    unless ← (path / "LeanWeave.lean").pathExists do
      throw (IO.userError "WEAVE_PATH must point at the LeanWeave source checkout")
    return path
  let mut path := (← IO.appPath).parent.getD "."
  for _ in [:5] do
    if ← (path / "LeanWeave.lean").pathExists then return ← IO.FS.realPath path
    path := path.parent.getD path
  let cwd ← IO.currentDir
  if ← (cwd / "LeanWeave.lean").pathExists then return cwd
  throw (IO.userError "cannot find LeanWeave sources; set WEAVE_PATH or use new --path")

private def scaffoldApp : String := "import LeanWeave\n\nopen LeanWeave\nopen scoped LeanWeave.Router\n\nprivate def home : Handler := fun _ =>\n  pure (Html.toResponse (Html.page \"Welcome\" (Html.element .h1 #[] #[Html.text \"Hello, LeanWeave!\"])))\n\nweave_routes routes where {\n  route \"GET\" \"/\" as home => home\n}\n\n"

private def scaffoldMain : String := "import App\n\nopen LeanWeave\n\ndef main (args : List String) : IO Unit := do\n  if args == [\"--routes\"] then IO.println routes.listing\n  else\n    let config ← match ← Server.Config.fromEnv with\n      | .ok config => pure config\n      | .error message => throw (IO.userError message)\n    Server.serve (fun _ => pure routes.handler) config\n"

private def scaffoldTest : String := "import App\n\ndef main : IO Unit := do\n  let ctx ← match LeanWeave.Context.fromRequest { method := \"GET\", target := \"/\".toUTF8, headers := #[{ name := \"host\", value := \"localhost\".toUTF8 }], body := ByteArray.empty } with\n    | .ok ctx => pure ctx\n    | .error error => throw (IO.userError error.message)\n  match ← LeanWeave.AppM.run (routes.handler ctx) with\n  | .ok response => unless response.status == 200 do throw (IO.userError \"home failed\")\n  | .error error => throw (IO.userError error.message)\n  IO.println \"application tests passed\"\n"

private def validName (name : String) : Bool :=
  !name.isEmpty && name.toList.all (fun c =>
    (c ≥ 'a' && c ≤ 'z') || (c ≥ '0' && c ≤ '9') || c == '-' || c == '_')

private def create (directory : String) (explicitPath : Option String) : IO UInt32 := do
  let destination := System.FilePath.mk directory
  if ← destination.pathExists then throw (IO.userError "destination already exists; choose a new directory")
  let name := destination.fileName.getD ""
  unless validName name do throw (IO.userError "application directory must use lowercase letters, digits, '-' or '_'")
  let source ← match explicitPath with
    | none => sourceRoot
    | some path => IO.FS.realPath path
  unless ← (source / "LeanWeave.lean").pathExists do throw (IO.userError "--path must contain LeanWeave.lean")
  let pathLiteral := Lean.Json.compress (Lean.Json.str source.toString)
  let lakefile := s!"name = \"{name}\"\nversion = \"0.1.0\"\ndefaultTargets = [\"app\"]\n\n[[require]]\nname = \"lean-weave\"\npath = {pathLiteral}\n\n[[lean_lib]]\nname = \"App\"\n\n[[lean_exe]]\nname = \"app\"\nroot = \"Main\"\n\n[[lean_exe]]\nname = \"app-test\"\nroot = \"Test\"\n"
  let toolchain ← IO.FS.readFile (source / "lean-toolchain")
  IO.FS.createDirAll destination
  for (path, content) in #[("lakefile.toml", lakefile), ("lean-toolchain", toolchain),
      ("App.lean", scaffoldApp), ("Main.lean", scaffoldMain), ("Test.lean", scaffoldTest), (".gitignore", ".lake/\n"),
      ("README.md", s!"# {name}\n\nRun `weave build`, `weave routes`, `weave test`, or `weave dev` from this directory.\nConfiguration: WEAVE_HOST, WEAVE_PORT and WEAVE_WORKERS. Default listener: http://127.0.0.1:8080.\nThe generated Lake dependency points to the selected local LeanWeave checkout.\n")] do
    IO.FS.writeFile (destination / path) content
  IO.println s!"Created {directory}. Run weave build, weave test, or weave dev from that directory."
  return 0

/-- The bounded portable watcher is bundled into the executable. -/
private def devProgram : String := include_str "scripts" / "dev.py"

/-- Command line entry point. Arguments are never interpolated into a shell. -/
def run (args : List String) : IO UInt32 := do
  match args with
  | [] | ["help"] | ["--help"] | ["-h"] => IO.print usage; return 0
  | ["new", directory] => create directory none
  | ["new", directory, "--path", path] => create directory (some path)
  | ["build"] => process "lake" #["build", "app"]
  | ["build", target] => process "lake" #["build", target]
  | ["test"] => process "lake" #["exe", "app-test"]
  | ["test", target] => process "lake" #["exe", target]
  | ["routes"] => process "lake" #["exe", "app", "--routes"]
  | ["routes", target] => process "lake" #["exe", target, "--routes"]
  | ["dev"] => process "python3" #["-c", devProgram, "app"]
  | ["dev", target] => process "python3" #["-c", devProgram, target]
  | _ => IO.eprint usage; return 2

end LeanWeave.Tooling

def main (args : List String) : IO UInt32 := do
  try LeanWeave.Tooling.run args catch error => do
    IO.eprintln s!"weave: {error}"
    return 1
