"""Build the API reference with the repository's pinned doc-gen4 package."""

import os
from pathlib import Path
import subprocess
import sys
import tempfile


ROOT = Path(__file__).resolve().parents[1]


def build(env):
    env.setdefault("LEAN_NUM_THREADS", "2")
    env.setdefault("DISABLE_EQUATIONS", "1")
    subprocess.run(["lake", "-d", "docs", "build", "LeanWeaveDocs:docs"],
                   cwd=ROOT, env=env, check=True)
    subprocess.run([sys.executable, "docs/prepare_site.py"], cwd=ROOT, check=True)


def main():
    env = os.environ.copy()
    if sys.platform != "linux":
        build(env)
        return
    prefix = Path(subprocess.check_output(
        ["lean", "--print-prefix"], cwd=ROOT, text=True).strip())
    # doc-gen4's executable does not import LeanWeave, so it needs its own
    # matching C++ runtime search directory when using the host C compiler.
    with tempfile.TemporaryDirectory(prefix="lean-weave-doc-cxx-") as temporary:
        directory = Path(temporary)
        for name in ("libc++.a", "libc++abi.a"):
            source = prefix / "lib" / name
            if not source.is_file():
                raise FileNotFoundError(f"Lean toolchain archive is missing: {source}")
            (directory / name).symlink_to(source)
        env.setdefault("LEAN_CC", "cc")
        paths = [str(directory)]
        if env.get("LIBRARY_PATH"):
            paths.append(env["LIBRARY_PATH"])
        env["LIBRARY_PATH"] = os.pathsep.join(paths)
        build(env)


if __name__ == "__main__":
    main()
