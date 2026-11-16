#!/usr/bin/env python3
"""Native HTTP/TLS integration using ephemeral servers, never a persistent service."""
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
import json, os, ssl, subprocess, tempfile, threading, time

class Server(ThreadingHTTPServer):
    daemon_threads = True
    def handle_error(self,*args): pass

class Handler(BaseHTTPRequestHandler):
    protocol_version = 'HTTP/1.1'
    traces = []
    def log_message(self,*args): pass
    def do_POST(self): self.handle_request()
    def do_GET(self): self.handle_request()
    def handle_request(self):
        if self.path == '/slow': time.sleep(.5)
        if self.path == '/echo':
            type(self).traces.append(self.headers.get('traceparent'))
            data = self.rfile.read(int(self.headers.get('content-length','0')))
        elif self.path == '/oversized': data = b'x'*8192
        elif self.path == '/redirect':
            self.send_response(302); self.send_header('Location','/echo'); self.send_header('Content-Length','0'); self.end_headers(); return
        elif self.path == '/chunked':
            self.send_response(200); self.send_header('Transfer-Encoding','chunked'); self.end_headers()
            self.wfile.write(b'3\r\none\r\n3\r\ntwo\r\n0\r\nX-End: yes\r\n\r\n'); return
        else: data=b'ok'
        try:
            self.send_response(200); self.send_header('Content-Length',str(len(data))); self.end_headers(); self.wfile.write(data)
        except (BrokenPipeError, ConnectionResetError, ssl.SSLError): pass

root=Path(__file__).resolve().parents[1]
results=[]
with tempfile.TemporaryDirectory(prefix='leanweave-http-') as temp:
    temp=Path(temp); cert=temp/'cert.pem'; key=temp/'key.pem'
    subprocess.run(['openssl','req','-x509','-newkey','rsa:2048','-nodes','-days','1','-keyout',str(key),'-out',str(cert),'-subj','/CN=localhost','-addext','subjectAltName=DNS:localhost'],check=True,capture_output=True)
    for tls in [False,True]:
        server=Server(('127.0.0.1',0),Handler)
        if tls:
            ctx=ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER); ctx.load_cert_chain(cert,key)
            server.socket=ctx.wrap_socket(server.socket,server_side=True)
        thread=threading.Thread(target=server.serve_forever,daemon=True); thread.start()
        try:
            env={**os.environ,'WEAVE_HTTP_PORT':str(server.server_port),'WEAVE_HTTP_TLS':str(int(tls)),'WEAVE_HTTP_CA':str(cert)}
            out=subprocess.run([str(root/'.lake/build/bin/weave-http-test')],env=env,capture_output=True,text=True,timeout=30)
            if out.returncode: raise RuntimeError(out.stdout+out.stderr)
            otlp=json.loads(out.stdout.splitlines()[0]); spans=otlp['resourceSpans'][0]['scopeSpans'][0]['spans']
            assert len(spans)==6
            assert Handler.traces[-1].startswith('00-4bf92f3577b34da6a3ce929d0e0e4736-')
            assert Handler.traces[-1].endswith('-01')
            results.append({'tls':tls,'passed':True,'automatic_spans':len(spans),'bounds_timeout_disconnect':True,'chunked':True})
        finally: server.shutdown(); server.server_close(); thread.join()
(root/'test/wire-result.json').write_text(json.dumps({'native':results},indent=2)+'\n')
print(json.dumps(results))
