import Lake
open Lake DSL

package «lean-weave-redis» where
  version := v!"0.1.0"

require «lean-weave» from "../.."

@[default_target]
lean_lib LeanWeaveRedis

extern_lib weave_redis (pkg : NPackage __name__) := do
  let source ← inputTextFile (pkg.dir / "c" / "redis.c")
  let includeDir ← getLeanIncludeDir
  let compiler := if System.Platform.isWindows then "gcc" else "cc"
  let object ← buildO (pkg.buildDir / "c" / "redis.o") source
    #["-isystem", includeDir.toString]
    #["-std=c11", "-Wall", "-Wextra", "-Wpedantic", "-Werror"] compiler
  buildStaticLib (pkg.staticLibDir / nameToStaticLib "weave_redis") #[object]

lean_exe «weave-redis-test» where
  root := `test.Main
