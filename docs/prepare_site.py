"""Expose the generated overview as the GitHub Pages entry point."""
from pathlib import Path
import json
import subprocess

root = Path(__file__).resolve().parents[1]
site = Path(__file__).parent / ".lake/build/doc"
html = (site / "LeanWeaveDocs.html").read_text()
(site / "index.html").write_text(html.replace("<title>LeanWeaveDocs</title>", "<title>LeanWeave API</title>"))
(site / ".nojekyll").touch()
manifest = json.loads((Path(__file__).parent / "lake-manifest.json").read_text())
(site / "build-info.json").write_text(json.dumps({
    "repository": "https://github.com/isavita/lean-weave",
    "revision": subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=root, text=True).strip(),
    "lean_net_revision": next(package["rev"] for package in manifest["packages"]
                              if package["name"].strip("«»") == "lean-net"),
}, indent=2) + "\n")
