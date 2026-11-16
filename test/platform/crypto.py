#!/usr/bin/env python3
"""Independent native SHA256/HMAC/base64url checks against Python's implementations."""
from pathlib import Path
import base64, hashlib, hmac, json, random, subprocess
root=Path(__file__).resolve().parents[2]
rng=random.Random(0xC0DEC0DE)
sizes=[0,1,2,3,31,32,55,56,57,63,64,65,127,128,129,255,256,511,512,1023,1024,4096,65536]
sizes += [rng.randrange(0,8193) for _ in range(96)]
vectors=[]
for i,size in enumerate(sizes):
    message=bytes(rng.getrandbits(8) for _ in range(size))
    key=bytes(rng.getrandbits(8) for _ in range([0,1,20,32,63,64,65,128,256][i%9]))
    b64=lambda data:base64.urlsafe_b64encode(data).decode().rstrip('=')
    vectors.append((message,key,{'message':b64(message),'key':b64(key)}))
output=subprocess.run([str(root/'.lake/build/bin/weave-crypto-fixture')],input=''.join(json.dumps(v[2])+'\n' for v in vectors),capture_output=True,text=True,timeout=120,check=True)
lines=output.stdout.splitlines()
assert len(lines)==len(vectors)
for (message,key,wire),line in zip(vectors,lines):
    result=json.loads(line)
    assert result['sha256']==hashlib.sha256(message).hexdigest()
    assert result['hmac']==hmac.new(key,message,hashlib.sha256).hexdigest()
    assert result['base64url']==wire['message']
report={'native':True,'passed':True,'independent_reference':'Python hashlib/hmac/base64','vectors':len(vectors),'seed':'0xC0DEC0DE','message_bytes':[min(sizes),max(sizes)],'key_lengths':[0,1,20,32,63,64,65,128,256], 'source_sha256':hashlib.sha256((root/'LeanWeave/Security/Crypto.lean').read_bytes()).hexdigest()}
(root/'docs/platform/crypto-result.json').write_text(json.dumps(report,indent=2)+'\n')
print(json.dumps(report))
