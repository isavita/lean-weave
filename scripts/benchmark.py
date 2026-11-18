#!/usr/bin/env python3
"""Record native core timings and Lean thread-local small-allocation counts."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import subprocess
from datetime import datetime, timezone

root = Path(__file__).resolve().parents[1]
p = argparse.ArgumentParser()
p.add_argument("--iterations", type=int, default=20000)
p.add_argument("--output", type=Path, default=root / "benchmarks/results.json")
p.add_argument("--platform", action="store_true", help="sample advanced application platform primitives")
a = p.parse_args()
target = "weave-platform-bench" if a.platform else "weave-bench"
subprocess.run(["lake", "build", target], cwd=root, check=True)
binary = root / (".lake/build/bin/" + target + (".exe" if os.name == "nt" else ""))
result = subprocess.run([str(binary), str(a.iterations)], cwd=root, text=True, capture_output=True, check=True)
sources = (["benchmarks/Platform.lean", "LeanWeave/Tenant/Basic.lean", "LeanWeave/Feature/Basic.lean",
    "LeanWeave/Security/Crypto.lean", "LeanWeave/Security/Audit.lean", "LeanWeave/Observability/Trace.lean",
    "LeanWeave/Observability/Logging.lean", "LeanWeave/Realtime/PubSub.lean"] if a.platform else
    ["benchmarks/Main.lean", "LeanWeave/Core/AppM.lean", "LeanWeave/Core/Body.lean", "LeanWeave/Core/Decode.lean",
     "LeanWeave/Core/Types.lean", "LeanWeave/Router/Runtime.lean", "LeanWeave/Html/Basic.lean",
     "LeanWeave/Cache/Store.lean", "LeanWeave/Cache/Response.lean", "LeanWeave/Cache/ResponseStore.lean",
     "LeanWeave/Security/Crypto.lean"])
sources += ["scripts/benchmark.py", "lake-manifest.json"]
report = {
    "utc": datetime.now(timezone.utc).isoformat(),
    "platform": platform.platform(), "machine": platform.machine(), "processor": platform.processor(),
    "lean": subprocess.check_output(["lean", "--version"], cwd=root, text=True).strip(),
    "commit": subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=root, text=True).strip(),
    "dirty": bool(subprocess.check_output(["git", "status", "--porcelain"], cwd=root, text=True).strip()),
    "source_sha256": {name: hashlib.sha256((root / name).read_bytes()).hexdigest() for name in sources},
    "binary_sha256": hashlib.sha256(binary.read_bytes()).hexdigest(),
    "measurement": "Native wall time and IO.getNumHeartbeats thread-local small allocation deltas; excludes large byte buffers, native malloc, RSS, sockets and TLS. Counts include loop and IO scaffolding.",
    "samples": [json.loads(line) for line in result.stdout.splitlines() if line.strip()],
}
for sample in report["samples"]:
    sample["ns_per_iteration"] = sample["nanoseconds"] / sample["iterations"]
    sample["small_allocations_per_iteration"] = sample["small_allocations"] / sample["iterations"]
a.output.parent.mkdir(parents=True, exist_ok=True)
a.output.write_text(json.dumps(report, indent=2) + "\n")
print(json.dumps(report, indent=2))
