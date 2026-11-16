import LeanWeave.GraphQL
import LeanWeave.Realtime.PubSub

namespace LeanWeave.Test.GraphQLSupport
open GraphQL

structure Counter where
  value : Int
  label : String := "counter"
  deriving Lean.ToJson, Lean.FromJson
instance : Schema.HasSchema Counter := ⟨"Counter", .object #[("value", .scalar .integer, true), ("label", .scalar .string, true)]⟩

structure AddArgs where
  amount : Int
  deriving Lean.ToJson, Lean.FromJson
instance : Schema.HasSchema AddArgs := ⟨"AddArgs", .object #[("amount", .scalar .integer { minimum := some (-100), maximum := some 100 }, true)]⟩

structure EchoArgs where
  text : String
  deriving Lean.ToJson, Lean.FromJson
instance : Schema.HasSchema EchoArgs := ⟨"EchoArgs", .object #[("text", .scalar .string { maxLength := some 128 }, true)]⟩

structure ColorArgs where
  color : String
  deriving Lean.ToJson, Lean.FromJson
instance : Schema.HasSchema ColorArgs := ⟨"ColorArgs", .object #[("color", .enumeration #[.str "RED", .str "BLUE"], true)]⟩

def newSchema : IO (ExecutableSchema × IO.Ref Int × IO.Ref Nat) := do
  let count ← IO.mkRef (0 : Int)
  let cancellations ← IO.mkRef (0 : Nat)
  let broker ← Realtime.Broker.new { maxMessageBytes := 4096, maxBytes := 65536 }
  let query := #[
    Field.typed "counter" (fun (_ : Context) (_ : NoArgs) => do return Counter.mk (← count.get) "counter"),
    Field.typed "color" (fun (_ : Context) (args : ColorArgs) => pure args.color),
    Field.typed "echo" (fun (_ : Context) (args : EchoArgs) => pure args.text),
    Field.typed "cancelled" (fun (_ : Context) (_ : NoArgs) => do return ← cancellations.get),
    Field.typed "slow" (fun (_ : Context) (_ : NoArgs) => do
      let now ← IO.monoMsNow
      AppM.await (do
        if (← IO.monoMsNow) - now >= 50 then return some (.ok "done")
        return none) (fun _ => cancellations.modify (· + 1)) 1000 128)]
  let mutation := #[Field.typed "increment" (fun (_ : Context) (args : AddArgs) => do
    count.modify (· + args.amount)
    let counter := Counter.mk (← count.get) "counter"
    let .ok _ ← broker.publish ⟨"counter", "next", Lean.toJson counter⟩ | throw { status := 503, message := "broker overloaded" }
    return counter)]
  let subscription := #[Field.subscription "ticks" (fun (_ : Context) (_ : NoArgs) => do
    let .ok sub ← broker.subscribe "counter" | throw { status := 503, message := "subscriber capacity exhausted" }
    return ({
      retainedBytes := 65536
      poll := do
        match ← sub.poll with
        | none => return none
        | some (.closed _) => return some (.ok none)
        | some (.message message) =>
          match Schema.decode Counter message.payload with
          | .ok value => return some (.ok (some value))
          | .error error => return some (.error ⟨error, #[], "INVALID_PAYLOAD"⟩)
      close := fun _ => do
        let _ ← sub.close
        cancellations.modify (· + 1) } : Source Counter))]
  match ExecutableSchema.make query mutation subscription with
  | .ok schema => return (schema, count, cancellations)
  | .error error => throw (IO.userError error)

end LeanWeave.Test.GraphQLSupport
