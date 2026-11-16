#!/usr/bin/env python3
"""Compile-time rejection of security policies and forged verification capabilities."""
from pathlib import Path
import json,subprocess,tempfile
root=Path(__file__).resolve().parents[1]
prelude='''import LeanWeave.Security.Policy
import LeanWeave.Security.Csp
import LeanWeave.Security.Jwt
open LeanWeave
open LeanWeave.Security
'''
cases=[
 ('CSP unsafe inline','def bad := csp! "default-src \'self\'; script-src \'unsafe-inline\'; object-src \'none\'; base-uri \'self\'"','unsafe-inline'),
 ('CSP unsafe scheme','def bad := csp! "default-src \'self\'; connect-src javascript:alert; object-src \'none\'; base-uri \'self\'"','invalid CSP host'),
 ('CSP duplicate directive','def bad := csp! "default-src \'self\'; default-src \'none\'; object-src \'none\'; base-uri \'self\'"','duplicate CSP'),
 ('CSP misplaced nonce','def bad := csp! "default-src \'self\'; img-src \'nonce\'; object-src \'none\'; base-uri \'self\'"','requires script-src'),
 ('CSP required boundary','def bad := csp! "default-src \'self\'"','requires default-src'),
 ('authorization empty group','def bad := auth_policy! any []','require 1..64'),
 ('authorization empty role','def bad := auth_policy! role ""','invalid authorization'),
 ('authorization invalid tenant','def bad := auth_policy! tenant "Alpha/other"','tenant'),
 ('forged JWT claims','def bad : Jwt.VerifiedClaims := { value := .null, subject := "alice", expires := 100 }','private'),
]
with tempfile.TemporaryDirectory(prefix='weave-platform-compile-') as temporary:
 def compile(source):
  path=Path(temporary)/'Check.lean';path.write_text(prelude+source)
  result=subprocess.run(['lake','env','lean',str(path)],cwd=root,capture_output=True,text=True,timeout=60)
  return result.returncode,result.stdout+result.stderr
 code,out=compile('def good := auth_policy! all [authenticated, any [role "admin", permission "report.read"], ! role "suspended", tenant "alpha"]')
 assert code==0,out
 for name,source,expected in cases:
  code,out=compile(source)
  assert code!=0 and expected.lower() in out.lower(),name+': '+out
  print('rejected:',name)
report={'passed':True,'valid':1,'rejected':[name for name,_,_ in cases]}
(root/'docs/platform/evidence/platform-compile.json').write_text(json.dumps(report,indent=2)+'\n')
