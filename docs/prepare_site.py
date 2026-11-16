"""Expose the generated overview as the GitHub Pages entry point."""
from pathlib import Path
site = Path(__file__).parent / ".lake/build/doc"
html = (site / "LeanWeaveDocs.html").read_text()
(site / "index.html").write_text(html.replace("<title>LeanWeaveDocs</title>", "<title>LeanWeave API</title>"))
(site / ".nojekyll").touch()
