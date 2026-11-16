import LeanWeave.Core
import LeanWeave.Tenant.Basic
import Lean

namespace LeanWeave.Security.Authorization

inductive Condition where
  | authenticated
  | role (name : String)
  | permission (name : String)
  | tenant (name : String)
  | all (conditions : Array Condition)
  | any (conditions : Array Condition)
  | negate (condition : Condition)
  deriving Repr, BEq, Lean.ToExpr

private def validName (name : String) : Bool :=
  !name.isEmpty && name.utf8ByteSize <= 128 && name.toUTF8.data.all (fun b =>
    (b >= 65 && b <= 90) || (b >= 97 && b <= 122) || (b >= 48 && b <= 57) || [45,46,58,95].contains b.toNat)

private def validate (condition : Condition) : Nat → Except String Nat
  | 0 => .error "authorization policy nesting exceeds 16"
  | depth + 1 => do
    match condition with
    | .authenticated => return 1
    | .role name | .permission name =>
      if validName name then return 1 else throw "invalid authorization role/permission name"
    | .tenant name =>
      let _ ← Tenant.Id.parse name
      return 1
    | .negate condition => return (← validate condition depth) + 1
    | .all conditions | .any conditions =>
      if conditions.isEmpty || conditions.size > 64 then throw "authorization groups require 1..64 conditions"
      let mut nodes := 1
      for condition in conditions do
        nodes := nodes + (← validate condition depth)
        if nodes > 128 then throw "authorization policy exceeds 128 conditions"
      return nodes

structure Policy where
  private mk ::
  condition : Condition
  deriving Repr, BEq, Lean.ToExpr

def checked (condition : Condition) : Except String Policy := do
  let nodes ← validate condition 16
  if nodes > 128 then throw "authorization policy exceeds 128 conditions"
  return ⟨condition⟩

private partial def evaluate : Condition → Context → Bool
  | .authenticated, ctx => ctx.authenticated
  | .role role, ctx => ctx.authenticated && ctx.attribute "role" == some role
  | .permission name, ctx => ctx.authenticated && ctx.attribute ("permission." ++ name) == some "true"
  | .tenant tenant, ctx => ctx.tenant == tenant
  | .all conditions, ctx => conditions.all (evaluate · ctx)
  | .any conditions, ctx => conditions.any (evaluate · ctx)
  | .negate condition, ctx => !evaluate condition ctx

/-- Evaluate only attributes minted by trusted authentication/authorization
middleware. A policy does not turn arbitrary HTTP headers into trusted claims. -/
def Policy.allows (policy : Policy) (context : Context) : Bool := evaluate policy.condition context

def Policy.middleware (policy : Policy) : Middleware := fun next ctx => do
  if !policy.allows ctx then throw { status := 403, message := "Forbidden" }
  next ctx

declare_syntax_cat weave_auth_policy
-- Read condition names as identifiers, not global keywords: importing the DSL
-- must not reserve Context field names such as tenant/authenticated.
syntax ident : weave_auth_policy
syntax ident str : weave_auth_policy
syntax ident "[" weave_auth_policy,* "]" : weave_auth_policy
syntax "!" weave_auth_policy : weave_auth_policy
syntax "auth_policy!" weave_auth_policy : term

private partial def compileCondition (stx : Lean.Syntax) : Lean.Elab.Term.TermElabM Condition := do
  match stx with
  | `(weave_auth_policy| $name:ident) =>
    if name.getId == `authenticated then return .authenticated
    Lean.throwErrorAt name "unknown authorization condition"
  | `(weave_auth_policy| $tag:ident $name:str) =>
    match tag.getId.toString with
    | "role" => return .role name.getString
    | "permission" => return .permission name.getString
    | "tenant" => return .tenant name.getString
    | _ => Lean.throwErrorAt tag "unknown authorization condition"
  | `(weave_auth_policy| $tag:ident [$conditions:weave_auth_policy,*]) =>
    let conditions ← conditions.getElems.mapM compileCondition
    match tag.getId.toString with
    | "all" => return .all conditions
    | "any" => return .any conditions
    | _ => Lean.throwErrorAt tag "unknown authorization group"
  | `(weave_auth_policy| ! $condition:weave_auth_policy) => return .negate (← compileCondition condition)
  | _ => Lean.Elab.throwUnsupportedSyntax

elab_rules : term
  | `(auth_policy! $condition:weave_auth_policy) => do
    let originalSyntax := condition
    let condition ← compileCondition condition
    match checked condition with
    | .error error => Lean.throwErrorAt originalSyntax error
    | .ok policy => return Lean.toExpr policy

end LeanWeave.Security.Authorization
