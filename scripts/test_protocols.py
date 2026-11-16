#!/usr/bin/env python3
"""Real TCP interoperability: official protoc/protobuf/grpcio and gRPC-Web framing.

Run after `lake build weave-protocol-fixture`; install requirements-protocols.txt.
No network mocks; bounded readiness/deadlines, isolated child, graceful cleanup.
"""
from __future__ import annotations
import argparse, base64, contextlib, http.client, json, pathlib, platform, re, hashlib, datetime
import signal, socket, struct, subprocess, tempfile, threading, time
import grpc
from grpc_tools import protoc
from google.protobuf import descriptor_pb2, descriptor_pool, message_factory
import google.protobuf
import h2.config, h2.connection, h2.events
from protocol_evidence import source_receipt

ROOT = pathlib.Path(__file__).resolve().parents[1]
SERVICE = '/leanweave.test.EchoService/'

def frame(payload, flag=0):
    return bytes([flag]) + struct.pack('>I', len(payload)) + payload

def parse_frames(payload):
    result = []
    while payload:
        assert len(payload) >= 5, 'truncated frame header'
        flag, size = payload[0], struct.unpack('>I', payload[1:5])[0]
        assert len(payload) >= 5 + size, 'truncated frame body'
        result.append((flag, payload[5:5+size]))
        payload = payload[5+size:]
    return result

def decode_base64_segments(payload):
    assert len(payload) % 4 == 0
    return b''.join(base64.b64decode(payload[n:n+4], validate=True) for n in range(0, len(payload), 4))

def messages():
    text = (ROOT/'LeanWeave/Test/ProtocolFixture.lean').read_text()
    source = json.loads(re.search(r'protobuf_schema ProtocolTest := ("(?:[^"\\]|\\.)*")', text, re.S).group(1), strict=False)
    with tempfile.TemporaryDirectory(prefix='weave-proto-') as directory:
        path = pathlib.Path(directory)
        (path/'fixture.proto').write_text(source)
        assert protoc.main(['protoc', '-I'+directory, '--descriptor_set_out='+str(path/'schema.pb'), str(path/'fixture.proto')]) == 0
        file_set = descriptor_pb2.FileDescriptorSet.FromString((path/'schema.pb').read_bytes())
    pool = descriptor_pool.DescriptorPool()
    for item in file_set.file: pool.Add(item)
    return [message_factory.GetMessageClass(pool.FindMessageTypeByName('leanweave.test.'+name)) for name in ['Echo', 'Codec']]

@contextlib.contextmanager
def fixture(executable):
    with socket.socket() as listener:
        listener.bind(('127.0.0.1', 0)); port = listener.getsockname()[1]
    with tempfile.TemporaryFile() as log:
        process = subprocess.Popen([str(executable), str(port)], stdout=log, stderr=log)
        try:
            deadline = time.monotonic()+10
            while True:
                if process.poll() is not None: raise AssertionError('fixture exited before ready')
                try:
                    with socket.create_connection(('127.0.0.1', port), .1): break
                except OSError:
                    if time.monotonic()>deadline: raise AssertionError('fixture readiness timeout')
                    time.sleep(.02)
            yield port
        finally:
            if process.poll() is None:
                process.send_signal(signal.SIGTERM)
                try: process.wait(timeout=4)
                except subprocess.TimeoutExpired: process.kill(); process.wait(timeout=4)
            log.seek(0); output=log.read().decode(errors='replace')
            assert process.returncode == 0, f'fixture exit {process.returncode}: {output}'

def h1(port, path, payload=b'', media='application/grpc-web+proto', headers=None, method='POST'):
    connection = http.client.HTTPConnection('127.0.0.1', port, timeout=4)
    try:
        connection.request(method, path, payload, {'Content-Type':media, **(headers or {})})
        response = connection.getresponse(); body=response.read()
        return response.status, dict(response.getheaders()), body
    finally: connection.close()

def h2_request(port, method, payload, extra=None, media='application/grpc+proto'):
    with socket.create_connection(('127.0.0.1',port),4) as sock:
        sock.settimeout(4)
        connection=h2.connection.H2Connection(config=h2.config.H2Configuration(client_side=True, header_encoding='utf-8'))
        connection.initiate_connection();sock.sendall(connection.data_to_send())
        headers=[(':method','POST'),(':scheme','http'),(':authority',f'127.0.0.1:{port}'),(':path',SERVICE+method),('content-type',media),('te','trailers')]+(extra or [])
        connection.send_headers(1,headers,end_stream=not payload)
        if payload: connection.send_data(1,payload,end_stream=True)
        sock.sendall(connection.data_to_send())
        data=b''; initial=[]; trailers=[]
        while True:
            wire=sock.recv(65536);assert wire,'HTTP/2 disconnected'
            for event in connection.receive_data(wire):
                if isinstance(event,h2.events.ResponseReceived): initial=event.headers
                elif isinstance(event,h2.events.TrailersReceived): trailers=event.headers
                elif isinstance(event,h2.events.DataReceived):
                    data+=event.data;connection.acknowledge_received_data(event.flow_controlled_length,event.stream_id)
                elif isinstance(event,h2.events.StreamReset): raise AssertionError(f'HTTP/2 reset: {event}')
                elif isinstance(event,h2.events.StreamEnded): return dict(initial),dict(trailers),data
            pending=connection.data_to_send()
            if pending:sock.sendall(pending)

def run(executable):
    Echo,Codec=messages(); passed=[]
    def check(name, action):
        action();passed.append(name);print('PASS',name,flush=True)
    with fixture(executable) as port:
        channel=grpc.insecure_channel(f'127.0.0.1:{port}')
        grpc.channel_ready_future(channel).result(timeout=4)
        def rpc(method, request=Echo, response=Echo, kind='unary_unary'):
            return getattr(channel,kind)(SERVICE+method,request_serializer=request.SerializeToString,response_deserializer=response.FromString)
        value=Echo(text='héllo <>&',number=-42,values=[0,1,-1,2147483647,-2147483648],present=0,blob=b'\x00\xff',tags={'tenant':7})
        def unary(): assert rpc('Unary')(value,timeout=4)==value
        check('official grpcio unary; UTF8, packed integers, map, optional zero, bytes',unary)
        def server():
            stream=rpc('Server',kind='unary_stream')(value,timeout=4)
            values=list(stream);assert [item.number for item in values]==[0,1,2]
            assert all(item.text==value.text for item in values)
            assert stream.code()==grpc.StatusCode.OK
        check('official grpcio server streaming + status trailers',server)
        def client():
            result=rpc('Client',kind='stream_unary')(iter([Echo(text='a',number=11),Echo(text='b',number=-4)]),timeout=4)
            assert result.text=='ab' and result.number==7
        check('official grpcio client streaming',client)
        def bidi():
            advance=threading.Event()
            def produce():
                yield Echo(text='before-half-close')
                assert advance.wait(3),'bidi response stalled until request half-close'
                yield Echo(text='after-first-response')
            stream=rpc('Bidi',kind='stream_stream')(produce(),timeout=4)
            assert next(stream).text=='before-half-close';advance.set()
            assert next(stream).text=='after-first-response'
            assert list(stream)==[] and stream.code()==grpc.StatusCode.OK
        check('official grpcio bidirectional response before request half-close',bidi)
        def upload():
            values=[Echo(blob=bytes([i])*60000) for i in range(8)]
            result=list(rpc('Bidi',kind='stream_stream')(iter(values),timeout=6));assert result==values
        check('480KB duplex upload/output with HTTP2 flow control',upload)
        def codec():
            value=Codec(double_value=-1.25,float_value=3.5,int32_value=-2147483648,int64_value=-9223372036854775808,
                uint32_value=4294967295,uint64_value=18446744073709551615,sint32_value=-2147483648,sint64_value=-9223372036854775808,
                fixed32_value=4294967295,fixed64_value=18446744073709551615,sfixed32_value=-2147483648,sfixed64_value=-9223372036854775808,
                bool_value=True,string_value='λ🚀',bytes_value=b'\x00\x80\xff',choice=197,choices=[0,1,197,-1],fixeds=[0,18446744073709551615],
                email='me@example.test',unpacked=[-1,0,2147483647])
            value.child.string_value='nested';value.children['x'].uint64_value=42
            result=rpc('CodecRoundTrip',Codec,Codec)(value,timeout=4);assert result==value,(result,value)
            # Unknown fields, including deprecated groups, survive a typed round trip.
            raw=value.SerializeToString()+b'\xa0\x06\x7b'+b'\xab\x06\x08\x01\xac\x06'+b'\x08\x01'
            raw_call=channel.unary_unary(SERVICE+'CodecRoundTrip',request_serializer=lambda b:b,response_deserializer=lambda b:b)
            output=raw_call(raw,timeout=4)
            assert Codec.FromString(output)==Codec.FromString(raw)
            assert b'\xa0\x06\x7b' in output and b'\xab\x06\x08\x01\xac\x06' in output
            assert output.endswith(b'\x08\x01'), 'known-number unknown wire type must survive'
        check('official protobuf all scalar limits, enums, recursive messages, map, oneof, unknown groups',codec)
        def errors():
            for method,expected in [('Fail',grpc.StatusCode.PERMISSION_DENIED),('Missing',grpc.StatusCode.UNIMPLEMENTED)]:
                try:rpc(method)(Echo(),timeout=4);raise AssertionError('RPC unexpectedly succeeded')
                except grpc.RpcError as error:assert error.code()==expected,(method,error)
        check('standard application-error and unknown-method status trailers',errors)
        def raw_errors():
            cases=[('Unary',b'',[],3),('Unary',frame(b'')+frame(b''),[],3),('Unary',b'\x00\x00',[],3),
                ('Unary',frame(b'',1),[],12),('Unary',b'\x00\x00\x01\x00\x01',[],8),
                ('Unary',frame(b'\x80'),[],3),('Never',frame(b''),[('grpc-timeout','20m')],4),
                ('Unary',frame(b''),[('grpc-timeout','invalid')],3),('Unary',frame(b''),[('grpc-encoding','gzip')],12)]
            for method,body,headers,expected in cases:
                initial,trailers,_=h2_request(port,method,body,headers)
                assert initial[':status']=='200' and trailers.get('grpc-status')==str(expected),(method,trailers,expected)
        check('HTTP2 malformed/truncated/oversized frames, cardinality, compression, deadline',raw_errors)
        def web():
            for media,text in [('application/grpc-web+proto',False),('application/grpc-web-text+proto',True)]:
                for method,count in [('Unary',1),('Server',3)]:
                    body=frame(value.SerializeToString());body=base64.b64encode(body) if text else body
                    status,headers,response=h1(port,SERVICE+method,body,media)
                    assert status==200 and headers['content-type']==media
                    frames=parse_frames(decode_base64_segments(response) if text else response)
                    assert len(frames)==count+1 and frames[-1][0]==128 and b'grpc-status: 0\r\n' in frames[-1][1]
                    assert all(Echo.FromString(data).text==value.text for flag,data in frames[:-1])
            for mode in ['application/grpc-web+proto','application/grpc-web-text+proto']:
                request=frame(b''); request=base64.b64encode(request) if 'text' in mode else request
                initial,trailers,response=h2_request(port,'Server',request,media=mode)
                frames=parse_frames(decode_base64_segments(response) if 'text' in mode else response)
                assert len(frames)==4 and not trailers and b'grpc-status: 0\r\n' in frames[-1][1]
        check('gRPC-Web binary+text unary/server streaming over HTTP1 and HTTP2',web)
        def cancel():
            before=json.loads(h1(port,'/rpc-state',method='GET')[2])['cancelled']
            for _ in range(6):
                call=rpc('Never',kind='unary_stream')(Echo(),timeout=4)
                call.initial_metadata();assert call.cancel()
            assert rpc('Unary')(value,timeout=4)==value
            deadline=time.monotonic()+2
            while True:
                current=json.loads(h1(port,'/rpc-state',method='GET')[2])['cancelled']
                if current==before+6:break
                assert current<=before+6,'source closed more than once'
                assert time.monotonic()<deadline,'source cleanup was not invoked after cancellation'
                time.sleep(.01)
            time.sleep(.03)
            assert json.loads(h1(port,'/rpc-state',method='GET')[2])['cancelled']==before+6
        check('client cancellation cleanup and subsequent connection reuse',cancel)
        def openapi():
            status,_,body=h1(port,'/openapi.json',method='GET')
            spec=json.loads(body);assert status==200 and spec['openapi']=='3.1.1'
            assert spec['paths']['/hello/{name}']['get']['parameters'][0]['schema']['type']=='string'
            response=h1(port,'/hello',b'{"text":"ok","count":1}','application/json')
            assert response[0]==201 and json.loads(response[2])=={'text':'ok','count':1}
            assert h1(port,'/hello',b'{"text":"","count":-1}','application/json')[0]==422
        check('actual typed REST handler + generated OpenAPI HTTP endpoint',openapi)
        channel.close()
    return {'passed':len(passed),'checks':passed,'clients':{'grpcio':grpc.__version__,'protobuf':google.protobuf.__version__},'host':platform.platform(),'transport':'real TCP loopback; HTTP1 and h2c; no TLS',
        **source_receipt(executable)}

if __name__=='__main__':
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--executable',type=pathlib.Path,default=ROOT/'.lake/build/bin/weave-protocol-fixture')
    parser.add_argument('--report',type=pathlib.Path)
    args=parser.parse_args();report=run(args.executable)
    if args.report:args.report.parent.mkdir(parents=True,exist_ok=True);args.report.write_text(json.dumps(report,indent=2)+'\n')
    print(json.dumps(report,indent=2))
