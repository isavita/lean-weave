import * as grpc from 'grpc-web';
import jspb from 'google-protobuf';

class Echo {
  constructor(text='', number=0) { this.text=text; this.number=number; }
  static serialize(message) {
    const writer = new jspb.BinaryWriter();
    writer.writeString(1, message.text);
    writer.writeSint64(2, message.number);
    return writer.getResultBuffer();
  }
  static deserialize(bytes) {
    const reader = new jspb.BinaryReader(bytes);
    const message = new Echo();
    while (reader.nextField()) {
      if (reader.isEndGroup()) break;
      switch (reader.getFieldNumber()) {
        case 1: message.text=reader.readString(); break;
        case 2: message.number=reader.readSint64(); break;
        default: reader.skipField();
      }
    }
    return message;
  }
}
const path='/leanweave.test.EchoService/';
const descriptor=(method, streaming=false) => new grpc.MethodDescriptor(path+method,
  streaming?grpc.MethodType.SERVER_STREAMING:grpc.MethodType.UNARY, Echo, Echo, Echo.serialize, Echo.deserialize);
const unary=(client, method, request) => new Promise((resolve,reject) =>
  client.rpcCall(location.origin+path+method,request,{},descriptor(method),
    (error,response)=>error?reject(error):resolve(response)));
const stream=(client, method, request) => new Promise((resolve,reject) => {
  const values=[]; let status;
  client.serverStreaming(location.origin+path+method,request,{},descriptor(method,true))
    .on('data',message=>values.push(message)).on('error',reject)
    .on('status',value=>status=value).on('end',()=>resolve({values,status}));
});
window.runGrpcWebAcceptance=async()=> {
  const checks=[];
  for (const format of ['binary','text']) {
    const client = new grpc.GrpcWebClientBase({format});
    const input=new Echo('official grpc-web browser λ <>&',-42);
    const response=await unary(client,'Unary',input);
    if (response.text!==input.text || response.number!==-42) throw Error('unary mismatch '+format);
    checks.push(format+' official grpc-web unary with protobuf BinaryWriter/BinaryReader');
    const result=await stream(client,'Server',input);
    if (JSON.stringify(result.values.map(item=>item.number))!=='[0,1,2]' || result.values.some(item=>item.text!==input.text) || result.status?.code!==0)
      throw Error('server stream/status mismatch '+format+' '+JSON.stringify(result));
    checks.push(format+' official grpc-web server streaming + parsed status trailer');
    try { await unary(client,'Fail',input); throw Error('permission denied accepted'); }
    catch(error) { if(error.code!==7) throw error; }
    checks.push(format+' official grpc-web error status');
  }
  return {passed:checks.length, checks, client:'grpc-web 2.1.1; google-protobuf 4.0.3', transport:'real browser XMLHttpRequest to native HTTP1 TCP; no TLS'};
};
