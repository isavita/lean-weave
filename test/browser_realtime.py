#!/usr/bin/env python3
"""Reproducible real-browser Live/CSP acceptance via Playwright CLI."""
from pathlib import Path
import argparse,hashlib,json,os,shlex,signal,socket,subprocess,tempfile,time,uuid
root=Path(__file__).resolve().parents[1]
p=argparse.ArgumentParser();p.add_argument('--cli',default='npx --yes --package @playwright/cli@0.1.24 playwright-cli');p.add_argument('--browser',default='chromium');p.add_argument('--output',type=Path,default=root/'output/playwright/realtime');p.add_argument('--report',type=Path);a=p.parse_args()
directory=a.output.resolve();directory.mkdir(parents=True,exist_ok=True)
with socket.socket() as sock:
 sock.bind(('127.0.0.1',0));port=sock.getsockname()[1]
executable=root/('.lake/build/bin/weave-realtime-fixture'+('.exe' if os.name=='nt' else ''))
with tempfile.TemporaryFile(mode='w+') as log:
 server=subprocess.Popen([str(executable),str(port)],stdout=log,stderr=log,creationflags=subprocess.CREATE_NEW_PROCESS_GROUP if os.name=='nt' else 0)
 cli=shlex.split(a.cli)+['--session','weave-realtime-'+uuid.uuid4().hex[:8]]
 def command(*args):
  result=subprocess.run(cli+list(args),cwd=directory,capture_output=True,text=True,timeout=50)
  if result.returncode: raise RuntimeError(result.stdout+result.stderr)
  return result.stdout
 opened=False
 try:
  for _ in range(200):
   if server.poll() is not None:log.seek(0);raise RuntimeError(log.read())
   try:
    with socket.create_connection(('127.0.0.1',port),.1):break
   except OSError:time.sleep(.025)
  else:raise RuntimeError('native browser fixture startup timeout')
  command('open',f'http://127.0.0.1:{port}/','--browser',a.browser);opened=True
  command('snapshot')
  source=(root/'test/browser_realtime.cjs').read_text().replace('/tmp/leanweave-realtime-browser/',str(directory)+'/')
  output=command('run-code',source)
  report=json.loads(output.split('### Result\n',1)[1].split('\n### ',1)[0])
  report['runner']='native loopback server + Playwright CLI real browser'
  report['source_sha256']={str(file.relative_to(root)):hashlib.sha256(file.read_bytes()).hexdigest() for file in [root/'test/browser_realtime.cjs',root/'LeanWeave/Test/RealtimeFixture.lean',root/'LeanWeave/Live.lean',root/'LeanWeave/Live/Client.lean',root/'LeanWeave/Security/Csp.lean']}
  if a.report:a.report.parent.mkdir(parents=True,exist_ok=True);a.report.write_text(json.dumps(report,indent=2)+'\n')
  print(json.dumps(report,indent=2))
 finally:
  if opened:command('close')
  if server.poll() is None:
   server.send_signal(signal.CTRL_BREAK_EVENT if os.name=='nt' else signal.SIGTERM)
   try:server.wait(timeout=5)
   except subprocess.TimeoutExpired:server.kill();server.wait(timeout=5)
  if server.returncode:log.seek(0);raise RuntimeError('fixture exit '+str(server.returncode)+': '+log.read())
