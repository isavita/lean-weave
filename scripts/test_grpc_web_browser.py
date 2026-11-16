#!/usr/bin/env python3
"""Official grpc-web in a real Playwright browser; native HTTP1 TCP server.

Requires test_protocols.py's Python environment, Node/npm and installed Playwright
browser. Runs CLI automation, not a DOM/network mock. Default WebKit; --browser
chrome/firefox/msedge works when installed. Use --cli for a local playwright-cli.
"""
import argparse,json,os,pathlib,shlex,subprocess,tempfile,uuid,hashlib
from protocol_evidence import source_receipt
from test_protocols import ROOT,fixture,default_executable

def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--executable',type=pathlib.Path,default=default_executable())
    parser.add_argument('--browser',default='webkit')
    parser.add_argument('--cli',default='npx --yes --package @playwright/cli@0.1.24 playwright-cli')
    parser.add_argument('--report',type=pathlib.Path)
    args=parser.parse_args()
    package=ROOT/'scripts/protocol-browser'
    subprocess.run(['npm','ci','--prefix',str(package),'--no-audit','--no-fund'],check=True,timeout=90)
    bundle=package/'browser.bundle.js'
    subprocess.run([str(package/'node_modules/.bin/esbuild'),str(package/'browser.js'),'--bundle','--outfile='+str(bundle)],check=True,timeout=30)
    previous=os.environ.get('WEAVE_PROTOCOL_BROWSER_BUNDLE')
    os.environ['WEAVE_PROTOCOL_BROWSER_BUNDLE']=str(bundle)
    try:
        with fixture(args.executable) as port, tempfile.TemporaryDirectory(prefix='weave-grpc-web-') as directory:
            cli=shlex.split(args.cli)+['--session','weave-grpc-'+uuid.uuid4().hex[:8]]
            def command(*parts):
                result=subprocess.run(cli+list(parts),cwd=directory,text=True,capture_output=True,timeout=45)
                if result.returncode:raise RuntimeError(result.stdout+result.stderr)
                return result.stdout
            try:
                command('open',f'http://127.0.0.1:{port}/health','--browser',args.browser)
                output=command('run-code',"async page => { await page.addScriptTag({url:'/grpc-web-client.js'}); return {...(await page.evaluate(()=>window.runGrpcWebAcceptance())),browser:page.context().browser().version()}; }")
                report=json.loads(output.split('### Result\n',1)[1].split('\n### ',1)[0])
                report.update(source_receipt(args.executable))
                report['browserEngine']=args.browser
                report['browser_bundle_sha256']=hashlib.sha256(bundle.read_bytes()).hexdigest()
                if args.report:
                    args.report.parent.mkdir(parents=True,exist_ok=True)
                    args.report.write_text(json.dumps(report,indent=2)+'\n')
                print(json.dumps(report,indent=2))
            finally:command('close')
    finally:
        if previous is None:os.environ.pop('WEAVE_PROTOCOL_BROWSER_BUNDLE',None)
        else:os.environ['WEAVE_PROTOCOL_BROWSER_BUNDLE']=previous

if __name__=='__main__':main()
