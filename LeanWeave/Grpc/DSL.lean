import LeanWeave.Grpc.Service
import LeanWeave.Protobuf.Compiler

open Lean Elab Command

namespace LeanWeave.Grpc

declare_syntax_cat weave_rpc
scoped syntax "rpc " ident " => " term : weave_rpc
scoped syntax (name := grpcService) "grpc_service " ident " where " "{" weave_rpc,*,? "}" : command

@[command_elab grpcService] def elabGrpcService : CommandElab := fun stx => do
  let `(grpc_service $name:ident where { $[$entries:weave_rpc],* }) := stx | throwUnsupportedSyntax
  let mut methods : Array Term := #[]
  let mut paths : Array String := #[]
  for entry in entries do
    let `(weave_rpc| rpc $method:ident => $handler:term) := entry | throwUnsupportedSyntax
    let mut selected : Option Protobuf.MethodInfo := none
    let mut candidate ← getCurrNamespace
    while selected.isNone && candidate != .anonymous do
      selected := Protobuf.methodInfo? (← getEnv) (candidate ++ method.getId)
      candidate := candidate.getPrefix
    let info ← match selected with
      | some info => pure info
      | none => match Protobuf.methodInfo? (← getEnv) method.getId with
        | some info => pure info
        | none => throwErrorAt method "unknown protobuf RPC; generate it with protobuf_schema first"
    if paths.contains info.path then throwErrorAt method "duplicate RPC path {info.path}"
    paths := paths.push info.path
    let constructor := mkIdent (``Method ++ Name.mkSimple (if info.clientStreaming then
      if info.serverStreaming then "bidiStream" else "clientStream"
      else if info.serverStreaming then "serverStream" else "unary"))
    let input := mkIdent info.input
    let output := mkIdent info.output
    methods := methods.push (← `($constructor (α := $input) (β := $output) $(quote info.path) $handler))
  elabCommand (← `(def $name : Service := { methods := #[$methods,*] }))

end LeanWeave.Grpc
