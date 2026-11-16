import LeanWeave.Test.GraphQLSupport
import LeanWeave.Core.Server

open LeanWeave

private def application (port : Nat) : IO Server.Application := do
  let (schema, _, _) ← Test.GraphQLSupport.newSchema
  let socketConfig : GraphQL.WebSocketConfig := {
    origin := s!"http://127.0.0.1:{port}"
    initTimeoutMs := 200
    heartbeatMs := 30
    maxOperations := 4
    limits := { maxResponseBytes := 65536, operationTimeoutMs := 1000 }
    authorize := fun ctx payload => do
      if (payload.getObjValAs? String "token").toOption == some "deny" then return .error "denied"
      return .ok ctx }
  let api := GraphQL.handler schema { maxResponseBytes := 65536 }
  return {
    handler := api
    tickIntervalMs := 2
    onUpgrade := fun head _ => do
      if head.target == "/ws".toUTF8 && head.version != "HTTP/2" then return some (← GraphQL.acceptHTTP1 schema head socketConfig)
      return none
    onWebSocket := fun head => do
      if head.target == "/ws".toUTF8 then GraphQL.acceptHTTP2 schema head socketConfig else pure none }

def main (args : List String) : IO Unit := do
  let some port := args[0]?.bind String.toNat? | throw (IO.userError "usage: weave-graphql-fixture PORT")
  Server.serveApplications (fun _ use => do use (← application port)) {
    port := port.toUInt16
    appRetainedBytes := 1048576
    net := { workers := 1, shutdownDrainMs := 100, http2 := { enableConnectProtocol := true }, deferred := { pollIntervalMs := 2 } } }
