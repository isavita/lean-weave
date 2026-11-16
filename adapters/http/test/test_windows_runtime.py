#!/usr/bin/env python3
"""Exercise runtime packaging with actual PE imports, without executing PE code."""
from pathlib import Path
import importlib.util
import os
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location("windows_runtime", ROOT / "scripts/stage_windows_runtime.py")
runtime = importlib.util.module_from_spec(spec)
spec.loader.exec_module(runtime)
CC = shutil.which("gcc" if os.name == "nt" else "x86_64-w64-mingw32-gcc")
OBJDUMP = shutil.which("objdump" if os.name == "nt" else "x86_64-w64-mingw32-objdump")


@unittest.skipUnless(CC and OBJDUMP, "requires native Windows GCC or MinGW cross compiler")
class RuntimePackaging(unittest.TestCase):
    def test_dependency_closure_and_missing_dependency(self):
        with tempfile.TemporaryDirectory(prefix="weave-pe-runtime-") as temporary:
            directory = Path(temporary)
            binary = directory / "native"
            application = directory / "application"
            binary.mkdir()
            application.mkdir()
            dependency = directory / "dependency.c"
            dependency.write_text("__declspec(dllexport) int dependency(void) { return 7; }\n")
            curl = directory / "curl.c"
            curl.write_text("__declspec(dllimport) int dependency(void);\n"
                            "__declspec(dllexport) int curl_version_info(void) { return dependency(); }\n")
            main = directory / "main.c"
            main.write_text("__declspec(dllimport) int curl_version_info(void);\n"
                            "int mainCRTStartup(void) { return curl_version_info(); }\n")

            def compile(*arguments):
                subprocess.run([CC, "-nostdlib", *map(str, arguments)], check=True,
                               capture_output=True, text=True, timeout=30)

            compile("-shared", "-Wl,--entry,0", dependency, "-o", binary / "dependency.dll",
                    "-Wl,--out-implib," + str(binary / "libdependency.dll.a"))
            compile("-shared", "-Wl,--entry,0", curl, "-L" + str(binary), "-ldependency",
                    "-o", binary / "libcurl-4.dll", "-Wl,--out-implib," + str(binary / "libcurl.dll.a"))
            executable = application / "app.exe"
            compile("-Wl,--entry,mainCRTStartup", main, "-L" + str(binary), "-lcurl", "-o", executable)
            report = runtime.stage(executable, binary, Path(OBJDUMP))
            self.assertEqual({item["name"] for item in report["libraries"]}, {"libcurl-4.dll", "dependency.dll"})
            for name in ["libcurl-4.dll", "dependency.dll"]:
                self.assertEqual((application / name).read_bytes(), (binary / name).read_bytes())
                (application / name).unlink()
            (binary / "dependency.dll").unlink()
            with self.assertRaisesRegex(RuntimeError, "dependency missing"):
                runtime.stage(executable, binary, Path(OBJDUMP))
            self.assertFalse((application / "libcurl-4.dll").exists(), "incomplete closure must fail before staging")


if __name__ == "__main__":
    unittest.main(verbosity=2)
