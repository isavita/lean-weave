#!/usr/bin/env python3
"""Stage the selected native libcurl dependency closure beside a Windows app.

Windows searches the application directory before PATH, including toolchain
directories prepended by `lake env`. Use one coherent MSYS2 runtime instead of
silently loading an unrelated curl DLL from another installed development tool.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess


def imports(image: Path, objdump: Path) -> list[str]:
    output = subprocess.run([str(objdump), "-p", str(image)], check=True,
                            capture_output=True, text=True, timeout=15).stdout
    return re.findall(r"^[ \t]*DLL Name:[ \t]*(.+?)[ \t]*$", output, re.MULTILINE)


def stage(executable: Path, binary_directory: Path, objdump: Path) -> dict:
    executable = executable.resolve(strict=True)
    binary_directory = binary_directory.resolve(strict=True)
    roots = [name for name in imports(executable, objdump)
             if name.lower().startswith("libcurl") and name.lower().endswith(".dll")]
    if not roots:
        raise RuntimeError("application must import the selected native libcurl DLL")
    pending = list(roots)
    planned = {}
    total = 0
    system = Path(os.environ.get("SystemRoot", "C:/Windows")) / "System32"
    while pending:
        name = pending.pop()
        if Path(name).name != name or "/" in name or "\\" in name:
            raise RuntimeError("invalid DLL import name")
        key = name.lower()
        if key in planned:
            continue
        source = binary_directory / name
        if not source.is_file():
            if key.startswith(("api-ms-win-", "ext-ms-win-")) or (system / name).is_file():
                continue
            raise RuntimeError(f"native dependency missing from selected runtime: {name}")
        size = source.stat().st_size
        total += size
        if len(planned) >= 64 or size > 32 * 1024 * 1024 or total > 256 * 1024 * 1024:
            raise RuntimeError("native runtime packaging bound exceeded")
        dependencies = imports(source, objdump)
        planned[key] = (source, dependencies)
        pending.extend(dependencies)
    # Resolve the whole closure before replacing any destination DLL. Package
    # only while the application is stopped; the loader can otherwise retain
    # an earlier image even after a file has been replaced.
    records = []
    for key, (source, dependencies) in sorted(planned.items()):
        destination = executable.parent / source.name
        if destination.resolve() != source:
            shutil.copy2(source, destination)
        records.append({"name": key, "source": str(source), "destination": str(destination),
                        "sha256": hashlib.sha256(destination.read_bytes()).hexdigest(),
                        "imports": dependencies})
    return {"application": str(executable), "runtime": str(binary_directory),
            "libraries": records, "bytes": total}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("executable", type=Path)
    parser.add_argument("--bin", type=Path, help="selected MSYS2 mingw64/bin; default from libcurl pkg-config")
    parser.add_argument("--objdump", type=Path, help="PE import inspector; default to selected runtime's objdump.exe")
    args = parser.parse_args()
    binary_directory = args.bin
    if binary_directory is None:
        prefix = subprocess.check_output(["pkg-config", "--variable=prefix", "libcurl"], text=True, timeout=15).strip()
        if not prefix:
            raise RuntimeError("libcurl pkg-config prefix is empty")
        binary_directory = Path(prefix) / "bin"
    objdump = args.objdump or binary_directory / "objdump.exe"
    print(json.dumps(stage(args.executable, binary_directory, objdump)))


if __name__ == "__main__":
    main()
