"""Source/binary bindings shared by the real protocol acceptance runners."""
from pathlib import Path
import datetime
import hashlib
import json
import os
import platform
import subprocess

ROOT = Path(__file__).resolve().parents[1]


def source_receipt(executable=None):
    sources = set()
    for directory in ['Core', 'Router', 'Schema', 'OpenApi', 'Protobuf', 'Grpc']:
        sources.update((ROOT / 'LeanWeave' / directory).glob('*.lean'))
    for name in [
        'LeanWeave/Core.lean', 'LeanWeave/Router.lean', 'LeanWeave/Schema.lean',
        'LeanWeave/OpenApi.lean', 'LeanWeave/Protobuf.lean', 'LeanWeave/Grpc.lean',
        'LeanWeave/Test/ProtocolFixture.lean', 'LeanWeave/Test/Protocols.lean',
        'scripts/protocol_evidence.py', 'scripts/test_protocol_compile.py',
        'scripts/test_protocols.py', 'scripts/test_grpc_web_browser.py',
        'scripts/protocol-browser/browser.js', 'scripts/protocol-browser/package.json',
        'scripts/protocol-browser/package-lock.json', 'lean-toolchain',
        'lakefile.toml', 'lake-manifest.json',
    ]:
        sources.add(ROOT / name)
    sources = sorted(path for path in sources if path.is_file())
    env = dict(os.environ, GIT_OPTIONAL_LOCKS='0')
    manifest = json.loads((ROOT / 'lake-manifest.json').read_text())
    receipt = {
        'sourceCommit': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, env=env, text=True).strip(),
        'sourceDirty': bool(subprocess.check_output(['git', 'status', '--porcelain'], cwd=ROOT, env=env, text=True).strip()),
        'source_sha256': {str(path.relative_to(ROOT)): hashlib.sha256(path.read_bytes()).hexdigest() for path in sources},
        'lean_net_revision': next(package.get('rev') for package in manifest['packages'] if 'lean-net' in package['name']),
        'host': platform.platform(),
        'recordedAt': datetime.datetime.now(datetime.timezone.utc).isoformat(),
    }
    if executable is not None:
        receipt['fixture_sha256'] = hashlib.sha256(Path(executable).read_bytes()).hexdigest()
    return receipt
