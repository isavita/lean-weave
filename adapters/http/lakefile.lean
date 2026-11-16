import Lake
open Lake DSL

private def curlDirectory : IO System.FilePath := do
  let output ← IO.Process.output { cmd := "pkg-config", args := #["--variable=libdir", "libcurl"] }
  if output.exitCode != 0 then throw (IO.userError "libcurl development files and pkg-config are required")
  return System.FilePath.mk output.stdout.trimAscii.toString

package «lean-weave-http» where
  version := v!"0.2.0"
  moreLinkObjs := if System.Platform.isWindows then #[`@/«curl-import»] else #[]
  moreLinkLibs := if System.Platform.isWindows then #[] else #[`@/«curl-shared»]

require «lean-weave» from "../.."

target «curl-import» _pkg : System.FilePath := do
  inputBinFile ((← curlDirectory) / "libcurl.dll.a")

target «curl-shared» pkg : Dynlib := do
  let directory ← curlDirectory
  let name := if System.Platform.isOSX then "libcurl.dylib" else "libcurl.so"
  let location := directory / name
  unless ← location.pathExists do error "libcurl DSO missing; configure pkg-config for a native libcurl installation"
  let file ← inputBinFile location
  file.mapM fun path => do
    if System.Platform.isOSX then return { path, name := "curl" }
    IO.FS.createDirAll pkg.sharedLibDir
    for entry in ← directory.readDir do
      if entry.fileName == "libcurl.so" || entry.fileName.startsWith "libcurl.so." then
        proc { cmd := "ln", args := #["-sfn", entry.path.toString, (pkg.sharedLibDir / entry.fileName).toString] }
    return { path := pkg.sharedLibDir / "libcurl.so", name := "curl" }

@[default_target]
lean_lib LeanWeaveHttp

extern_lib weave_http pkg := do
  let source ← inputTextFile (pkg.dir / "c" / "http.c")
  let output ← IO.Process.output { cmd := "pkg-config", args := #["--cflags", "libcurl"] }
  if output.exitCode != 0 then error "pkg-config --cflags libcurl failed"
  let includes := #["-isystem", (← getLeanIncludeDir).toString] ++ (output.stdout.trimAscii.toString.splitOn " ").toArray.filter (· != "")
  let object ← buildO (pkg.buildDir / "c" / "http.o") source includes
    #["-std=c11", "-Wall", "-Wextra", "-Wpedantic", "-Werror", "-O2", "-fPIC"] (if System.Platform.isWindows then "gcc" else "cc")
  buildStaticLib (pkg.staticLibDir / nameToStaticLib "weave_http") #[object]

lean_exe «weave-http-test» where
  root := `test.Main
