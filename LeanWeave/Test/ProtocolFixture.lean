import LeanWeave.Core.Server
import LeanWeave.Grpc
import LeanWeave.OpenApi

open LeanWeave
open scoped LeanWeave.Protobuf LeanWeave.Grpc LeanWeave.Schema LeanWeave.OpenApi

protobuf_schema ProtocolTest := "syntax = \"proto3\"; package leanweave.test;
message Echo { string text = 1; sint64 number = 2; repeated int32 values = 3; optional uint32 present = 4;
  map<string,int32> tags = 5; bytes blob = 6; }
enum Choice { ZERO = 0; ONE = 1; }
message Codec { double double_value = 1; float float_value = 2; int32 int32_value = 3;
 int64 int64_value = 4; uint32 uint32_value = 5; uint64 uint64_value = 6;
 sint32 sint32_value = 7; sint64 sint64_value = 8; fixed32 fixed32_value = 9;
 fixed64 fixed64_value = 10; sfixed32 sfixed32_value = 11; sfixed64 sfixed64_value = 12;
 bool bool_value = 13; string string_value = 14; bytes bytes_value = 15;
 Choice choice = 16; repeated Choice choices = 17; repeated fixed64 fixeds = 18;
 Codec child = 19; oneof contact { string email = 20; Codec parent = 21; }
 map<string,Codec> children = 22; repeated int32 unpacked = 23 [packed = false]; }
service EchoService {
 rpc Unary(Echo) returns(Echo);
 rpc Server(Echo) returns(stream Echo);
 rpc Client(stream Echo) returns(Echo);
 rpc Bidi(stream Echo) returns(stream Echo);
 rpc Never(Echo) returns(stream Echo);
 rpc Fail(Echo) returns(Echo);
 rpc CodecRoundTrip(Codec) returns(Codec);
}"

private def unary (value : ProtocolTest.Echo) (_ : Context) : AppM ProtocolTest.Echo := pure value
private def codec (value : ProtocolTest.Codec) (_ : Context) : AppM ProtocolTest.Codec := pure value

private def server (value : ProtocolTest.Echo) (_ : Context) : AppM (Grpc.Source ProtocolTest.Echo) := do
  Grpc.Source.ofArray ((List.range 3).toArray.map fun n => { value with number := Int64.ofInt n })
    (value.text.utf8ByteSize * 3 + value.blob.size * 3 + 16384)

private def clientLoop (source : Grpc.Source ProtocolTest.Echo) (remaining : Nat)
    (number : Int64) (texts : String) : AppM ProtocolTest.Echo := do
  match remaining with
  | 0 => throw { «status» := 413, message := "too many messages" }
  | n + 1 =>
    match ← source.next 30000 with
    | .error failure => throw { «status» := 400, message := failure.message }
    | .ok none => return { (default : ProtocolTest.Echo) with text := texts, number }
    | .ok (some value) => clientLoop source n (number + value.number) (texts ++ value.text)

private def client (source : Grpc.Source ProtocolTest.Echo) (_ : Context) : AppM ProtocolTest.Echo :=
  clientLoop source 4096 0 ""

private def bidi (source : Grpc.Source ProtocolTest.Echo) (_ : Context) : AppM (Grpc.Source ProtocolTest.Echo) := pure source

private def never (_ : ProtocolTest.Echo) (_ : Context) : AppM (Grpc.Source ProtocolTest.Echo) :=
  pure { poll := pure none }

private def fail (_ : ProtocolTest.Echo) (_ : Context) : AppM ProtocolTest.Echo :=
  throw { «status» := 403, message := "permission denied" }

grpc_service echoService where {
  rpc ProtocolTest.EchoService.Unary => unary,
  rpc ProtocolTest.EchoService.Server => server,
  rpc ProtocolTest.EchoService.Client => client,
  rpc ProtocolTest.EchoService.Bidi => bidi,
  rpc ProtocolTest.EchoService.Never => never,
  rpc ProtocolTest.EchoService.Fail => fail,
  rpc ProtocolTest.EchoService.CodecRoundTrip => codec
}

weave_schema Greeting where {
  text : String checks { minLength 1, maxLength 100 },
  count : Nat checks { maximum 1000 }
}

private def greeting (name : String) (_ : Context) : AppM Greeting := pure ⟨name, 1⟩
private def create (value : Greeting) (_ : Context) : AppM Greeting := pure value
weave_api rest where {
  route "GET" "/hello/:name" as greeting returns Greeting => greeting,
  route "POST" "/hello" as create accepts Greeting returns Greeting status 201 => create
}

private def docs : Handler := match OpenApi.specHandler rest { title := "LeanWeave protocol acceptance", version := "1" } with
  | .ok handler => handler
  | .error error => fun _ => throw { message := error }

private def handler (service : Grpc.Service) (closed : IO.Ref (Nat × Nat)) (browserBundle : ByteArray) : Handler := fun ctx =>
  if ctx.path == #["openapi.json"] then docs ctx
  else if ctx.path == #["rpc-state"] then do
    let (normal, cancelled) ← closed.get
    return Response.json (Lean.Json.mkObj [("normal", Lean.toJson normal), ("cancelled", Lean.toJson cancelled)])
  else if ctx.path == #["grpc-web-client.js"] then
    pure (({ body := browserBundle } : Response).withHeader "content-type" "text/javascript; charset=utf-8")
  else if ctx.request.target == "/health".toUTF8 then pure (Response.text "ok")
  else if ((ctx.header "content-type").bind Grpc.mode?).isSome then service.handler ctx
  else rest.router.handler ctx

def main (args : List String) : IO Unit := do
  let some port := args[0]?.bind String.toNat? | throw (IO.userError "usage: weave-protocol-fixture PORT")
  match echoService.validate with
  | .error error => throw (IO.userError error)
  | .ok _ => pure ()
  let bundle ← match ← IO.getEnv "WEAVE_PROTOCOL_BROWSER_BUNDLE" with
    | some path => IO.FS.readBinFile path
    | none => pure ByteArray.empty
  Server.serve (fun _ => do
    let closed ← IO.mkRef (0, 0)
    let observed := Grpc.Method.serverStream (α := ProtocolTest.Echo) (β := ProtocolTest.Echo)
      "/leanweave.test.EchoService/Never" (fun _ _ => pure {
        poll := pure none
        close := fun reason => closed.modify fun (normal, cancelled) =>
          if reason.isSome then (normal, cancelled + 1) else (normal + 1, cancelled) })
    let service := { echoService with methods := echoService.methods.map fun method =>
      if method.path == observed.path then observed else method }
    pure (handler service closed bundle)) {
    port := port.toUInt16
    appRetainedBytes := echoService.limits.reservation + 65536
    appTimeoutMs := 31000
    streamRequests := echoService.streamRequests
    net := {
      workers := 1
      shutdownDrainMs := 200
      machine := { limits := { maxBodyBytes := 8 * 1024 * 1024 } }
      deferred := { pollIntervalMs := 1, maxTimeoutMs := 31000 }
      http2 := { maxRequestBodyBytes := 8 * 1024 * 1024 } } }
