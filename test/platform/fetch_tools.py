#!/usr/bin/env python3
"""Fetch pinned test-only official binaries, verifying vendor SHA256 manifests."""
from pathlib import Path
import argparse, hashlib, platform, subprocess, tarfile, zipfile
p=argparse.ArgumentParser();p.add_argument('--directory',type=Path,default=Path.home()/'.cache/leanweave-test-tools');a=p.parse_args()
os_name={'Darwin':'darwin','Linux':'linux','Windows':'windows'}[platform.system()]
arch={'arm64':'arm64','aarch64':'arm64','x86_64':'amd64','AMD64':'amd64'}[platform.machine()]
suffix='.exe' if os_name=='windows' else ''
for tool,version in [('vault','1.20.0'),('otelcol','0.131.0')]:
    archive_ext='zip' if tool=='vault' or os_name=='windows' else 'tar.gz'
    filename=f'{tool}_{version}_{os_name}_{arch}.{archive_ext}'
    if tool=='vault':
        base=f'https://releases.hashicorp.com/vault/{version}/';manifest=f'vault_{version}_SHA256SUMS'
    else:
        base=f'https://github.com/open-telemetry/opentelemetry-collector-releases/releases/download/v{version}/';manifest='opentelemetry-collector-releases_otelcol_checksums.txt'
    directory=a.directory/f'{tool}-{version}';directory.mkdir(parents=True,exist_ok=True)
    for name in [filename,manifest]:
        file=directory/name
        if not file.exists(): subprocess.run(['curl','--fail','--location','--silent','--show-error','--output',str(file),base+name],check=True)
    expected=next(line.split()[0] for line in (directory/manifest).read_text().splitlines() if line.split()[-1].lstrip('*')==filename)
    archive=directory/filename
    assert hashlib.sha256(archive.read_bytes()).hexdigest()==expected,'vendor checksum mismatch'
    binary=directory/(tool+suffix)
    if archive_ext=='zip':
        with zipfile.ZipFile(archive) as z: binary.write_bytes(z.read(tool+suffix))
    else:
        with tarfile.open(archive) as t:
            member=next(m for m in t.getmembers() if Path(m.name).name==tool+suffix)
            binary.write_bytes(t.extractfile(member).read())
    binary.chmod(0o755)
    print(f'{tool}: {binary}\narchive SHA256: {expected}')
