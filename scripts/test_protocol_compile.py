#!/usr/bin/env python3
"""Build-time rejection checks for schema, protobuf, typed RPC and API DSLs."""
import argparse,json,pathlib,subprocess,tempfile
from protocol_evidence import source_receipt
ROOT=pathlib.Path(__file__).resolve().parents[1]
PRELUDE='''import LeanWeave.Schema
import LeanWeave.OpenApi
import LeanWeave.Grpc
open LeanWeave
open scoped LeanWeave.Schema LeanWeave.OpenApi LeanWeave.Protobuf LeanWeave.Grpc
'''

def run(source,success,fragment=''):
    with tempfile.TemporaryDirectory(prefix='weave-compile-') as directory:
        path=pathlib.Path(directory)/'Check.lean';path.write_text(PRELUDE+source,encoding='utf-8')
        result=subprocess.run(['lake','env','lean',str(path)],cwd=ROOT,text=True,encoding='utf-8',capture_output=True,timeout=40)
    output=result.stdout+result.stderr
    assert (result.returncode==0)==success,output
    assert fragment in output,(fragment,output)

cases=[
 ('schema contradictory text bounds','weave_schema Bad where { value : String checks { minLength 4, maxLength 3 } }','minLength exceeds maxLength'),
 ('schema duplicate bounds','weave_schema Bad where { value : String checks { maxLength 4, maxLength 3 } }','duplicate constraint'),
 ('schema constraints on wrong type','weave_schema Bad where { value : Nat checks { minLength 4 } }','length constraints require'),
 ('schema impossible natural range','weave_schema Bad where { value : Nat checks { maximum -1 } }','Nat maximum contradicts'),
 ('schema duplicate fields','weave_schema Bad where { value : Nat, value : String }','duplicate schema field'),
]
# JSON string escaping is also valid in these Lean string literals.
def proto(text):return 'protobuf_schema Bad := '+json.dumps('syntax="proto3"; '+text)
cases += [
 ('protobuf duplicate field number',proto('message A {string x=1;string y=1;}'),'duplicate'),
 ('protobuf reserved field',proto('message A {reserved 1;string x=1;}'),'reserved'),
 ('protobuf unknown type',proto('message A {Missing x=1;}'),'unresolved'),
 ('protobuf invalid map key',proto('message A {map<bytes,string> x=1;}'),'map key'),
 ('protobuf enum nonzero first',proto('enum E {A=1;}'),'zero'),
 ('protobuf service nonmessage',proto('service S {rpc Call(string) returns(string);}'),'unresolved'),
 ('protobuf codegen reserved field',proto('message A {string __unknownFields=1;}'),'generated codec metadata'),
 ('protobuf duplicate service method',proto('message A {} service S {rpc Call(A) returns(A);rpc Call(A) returns(A);}'),'duplicate'),
 ('typed RPC signature',proto('message A {} service S {rpc Call(A) returns(A);}')+'\ngrpc_service wrong where {rpc Bad.S.Call => fun (_ : String) (_ : Context) => pure "wrong"}','Application type mismatch'),
 ('typed RPC nonexistent method','grpc_service wrong where {rpc Missing.Service.Call => fun x => pure x}','unknown protobuf RPC'),
 ('typed RPC duplicate method',proto('message A {} service S {rpc Call(A) returns(A);}')+'\ndef h (x : Bad.A) (_ : Context) : AppM Bad.A := pure x\ngrpc_service wrong where {rpc Bad.S.Call => h, rpc Bad.S.Call => h}','duplicate RPC path'),
 ('typed API ambiguous routes','weave_schema A where {value : String}\ndef h (_ : String) (_ : Context) : AppM A := pure ⟨"ok"⟩\nweave_api bad where {route "GET" "/:x" as a returns A => h, route "GET" "/:y" as b returns A => h}','ambiguous'),
 ('typed API wrong input handler','weave_schema A where {value : String}\ndef h (_ : Nat) (_ : Context) : AppM A := pure ⟨"ok"⟩\nweave_api bad where {route "POST" "/" as a accepts A returns A => h}','Application type mismatch'),
]

if __name__=='__main__':
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report',type=pathlib.Path)
    args=parser.parse_args()
    run('weave_schema Good where {text : String checks {minLength 1}, age : Nat checks {maximum 99}}\n'+proto('message A {string text=1; optional int32 age=2;} service S {rpc Call(A) returns(A);}')+'\ndef h (x : Bad.A) (_ : Context) : AppM Bad.A := pure x\ngrpc_service good where {rpc Bad.S.Call => h}',True)
    for name,source,fragment in cases:
        run(source,False,fragment);print('PASS',name,flush=True)
    report={'passed':len(cases)+1,'positive':1,'negative':len(cases),'checks':[name for name,_,_ in cases],**source_receipt()}
    if args.report:
        args.report.parent.mkdir(parents=True,exist_ok=True)
        args.report.write_text(json.dumps(report,indent=2)+'\n',encoding='utf-8')
    print(json.dumps(report))
