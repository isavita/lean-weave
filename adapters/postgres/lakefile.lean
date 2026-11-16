import Lake
open Lake DSL

private def pgPath (option : String) : IO System.FilePath := do
  let output ← IO.Process.output { cmd := "pg_config", args := #[option] }
  if output.exitCode != 0 then throw (IO.userError "PostgreSQL libpq development files and pg_config are required")
  return System.FilePath.mk output.stdout.trimAscii.toString

package «lean-weave-postgres» where
  version := v!"0.2.0"
  moreLinkObjs := if System.Platform.isWindows then #[`@/«libpq-import»] else #[]
  moreLinkLibs := if System.Platform.isWindows then #[] else #[`@/«libpq-shared»]

require «lean-weave» from "../.."

target «libpq-import» _pkg : System.FilePath := do
  inputBinFile ((← pgPath "--libdir") / "libpq.dll.a")

target «libpq-shared» pkg : Dynlib := do
  let directory ← pgPath "--libdir"
  let name := if System.Platform.isWindows then "libpq.dll" else if System.Platform.isOSX then "libpq.dylib" else "libpq.so"
  let file ← inputBinFile (directory / name)
  file.mapM fun path => do
    if System.Platform.isOSX || System.Platform.isWindows then return { path, name := "pq" }
    -- Avoid exposing a global system -L directory ahead of Lean's bundled CRT.
    IO.FS.createDirAll pkg.sharedLibDir
    for entry in ← directory.readDir do
      if entry.fileName == "libpq.so" || entry.fileName.startsWith "libpq.so." then
        proc { cmd := "ln", args := #["-sfn", entry.path.toString, (pkg.sharedLibDir / entry.fileName).toString] }
    return { path := pkg.sharedLibDir / "libpq.so", name := "pq" }

@[default_target]
lean_lib LeanWeavePostgres

extern_lib weave_postgres pkg := do
  let source ← inputTextFile (pkg.dir / "c" / "postgres.c")
  let includes := #["-isystem", (← getLeanIncludeDir).toString, "-I", (← pgPath "--includedir").toString]
  let compiler := if System.Platform.isWindows then "gcc" else "cc"
  let object ← buildO (pkg.buildDir / "c" / "postgres.o") source includes
    #["-std=c11", "-Wall", "-Wextra", "-Wpedantic", "-Werror", "-O2", "-fPIC"] compiler
  buildStaticLib (pkg.staticLibDir / nameToStaticLib "weave_postgres") #[object]

lean_exe «postgres-test» where
  root := `test.Main

lean_exe «postgres-example» where
  root := `Example

lean_exe «weave-pg-tenant-test» where
  root := `test.TenantMain

lean_exe «weave-pg-jobs-test» where
  root := `test.JobsMain
