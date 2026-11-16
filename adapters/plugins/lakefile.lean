import Lake
open Lake DSL

package «lean-weave-plugins» where
  version := v!"0.2.0"
  moreLinkArgs := if System.Platform.isWindows || System.Platform.isOSX then #[] else #["-ldl"]

require «lean-weave» from "../.."

@[default_target]
lean_lib LeanWeavePlugins

extern_lib weave_plugins pkg := do
  let source ← inputTextFile (pkg.dir / "c" / "plugins.c")
  let header ← inputTextFile (pkg.dir / "include" / "leanweave_plugin.h")
  let input := source.zipWith (fun src _ => src) header
  let compiler := if System.Platform.isWindows then "gcc" else "cc"
  let object ← buildO (pkg.buildDir / "c" / "plugins.o") input
    #["-isystem",(← getLeanIncludeDir).toString,"-I",(pkg.dir / "include").toString]
    #["-std=c11","-Wall","-Wextra","-Wpedantic","-Werror","-O2","-fPIC"] compiler
  buildStaticLib (pkg.staticLibDir / nameToStaticLib "weave_plugins") #[object]

lean_exe «weave-plugin-fixture» where
  root := `test.Fixture
