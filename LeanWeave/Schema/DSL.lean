import Lean
import LeanWeave.Schema.Basic

open Lean Elab Command
open Lean.Parser.Command

namespace LeanWeave.Schema

declare_syntax_cat weave_constraint
scoped syntax "minLength " num : weave_constraint
scoped syntax "maxLength " num : weave_constraint
scoped syntax "minItems " num : weave_constraint
scoped syntax "maxItems " num : weave_constraint
scoped syntax "minimum " num : weave_constraint
scoped syntax "minimum " "-" num : weave_constraint
scoped syntax "maximum " num : weave_constraint
scoped syntax "maximum " "-" num : weave_constraint
declare_syntax_cat weave_schema_field
scoped syntax ident " : " term : weave_schema_field
scoped syntax ident " : " term " checks " "{" weave_constraint,* "}" : weave_schema_field

/-- Generate a Lean structure, JSON codecs, and a shared validator/OpenAPI schema.
```
weave_schema Person where {
  name : String checks { minLength 1, maxLength 80 },
  age : Nat checks { maximum 150 },
  nickname : Option String
}
```
Contradictory/duplicate constraints and unknown codecs are build errors. -/
scoped syntax (name := weaveSchema) "weave_schema " ident " where " "{" weave_schema_field,* "}" : command

private def intSyntax : Option Int → CommandElabM Term
  | none => `((none : Option Int))
  | some n => if n < 0 then `(some (-($(quote n.natAbs) : Int))) else `(some ($(quote n.natAbs) : Int))

private def boundsSyntax (bounds : Bounds) : CommandElabM (TSyntax `term) := do
  let lo ← intSyntax bounds.minimum
  let hi ← intSyntax bounds.maximum
  `(({ «minLength» := $(quote bounds.minLength), «maxLength» := $(quote bounds.maxLength),
       «minimum» := $lo, «maximum» := $hi,
       «minItems» := $(quote bounds.minItems), «maxItems» := $(quote bounds.maxItems) } : Bounds))

private def parseBounds (entries : Array (TSyntax `weave_constraint)) : CommandElabM Bounds := do
  let mut bounds : Bounds := {}
  let mut names : Array String := #[]
  for entry in entries do
    let (name, value, negative) ← match entry with
      | `(weave_constraint| minLength $n:num) => pure ("minLength", n.getNat, false)
      | `(weave_constraint| maxLength $n:num) => pure ("maxLength", n.getNat, false)
      | `(weave_constraint| minItems $n:num) => pure ("minItems", n.getNat, false)
      | `(weave_constraint| maxItems $n:num) => pure ("maxItems", n.getNat, false)
      | `(weave_constraint| minimum $n:num) => pure ("minimum", n.getNat, false)
      | `(weave_constraint| minimum -$n:num) => pure ("minimum", n.getNat, true)
      | `(weave_constraint| maximum $n:num) => pure ("maximum", n.getNat, false)
      | `(weave_constraint| maximum -$n:num) => pure ("maximum", n.getNat, true)
      | _ => throwErrorAt entry "unsupported schema constraint"
    if names.contains name then throwErrorAt entry "duplicate constraint '{name}'"
    names := names.push name
    let int := if negative then -(Int.ofNat value) else Int.ofNat value
    bounds := match name with
      | "minLength" => { bounds with «minLength» := some value }
      | "maxLength" => { bounds with «maxLength» := some value }
      | "minItems" => { bounds with «minItems» := some value }
      | "maxItems" => { bounds with «maxItems» := some value }
      | "minimum" => { bounds with «minimum» := some int }
      | _ => { bounds with «maximum» := some int }
  if let some min := bounds.minLength then
    if bounds.maxLength.any (min > ·) then throwError "minLength exceeds maxLength"
  if let some min := bounds.minItems then
    if bounds.maxItems.any (min > ·) then throwError "minItems exceeds maxItems"
  if let some min := bounds.minimum then
    if bounds.maximum.any (min > ·) then throwError "minimum exceeds maximum"
  return bounds

private partial def typeKind (type : TSyntax `term) : String :=
  match type with
  | `(Option $inner) => typeKind inner
  | `(Array $_) | `(List $_) => "array"
  | `(String) => "string"
  | `(Nat) | `(Int) | `(Float) => "number"
  | _ => "custom"

private partial def naturalType (type : TSyntax `term) : Bool :=
  match type with
  | `(Option $inner) => naturalType inner
  | `(Nat) => true
  | _ => false

private def emitSchema (name : Ident) (entries : Array (TSyntax `weave_schema_field)) : CommandElabM Unit := do
  let mut fieldNames : Array Ident := #[]
  let mut fieldTypes : Array Term := #[]
  let mut definitions : Array (TSyntax `term) := #[]
  let mut names : Array Name := #[]
  for entry in entries do
    let (field, type, constraints) ← match entry with
      | `(weave_schema_field| $field:ident : $type:term) => pure (field, type, #[])
      | `(weave_schema_field| $field:ident : $type:term checks { $rules:weave_constraint,* }) =>
        pure (field, type, rules.getElems)
      | _ => throwErrorAt entry "invalid schema field"
    if names.contains field.getId then throwErrorAt field "duplicate schema field '{field.getId}'"
    names := names.push field.getId
    let bounds ← parseBounds constraints
    let kind := typeKind type
    if naturalType type && bounds.maximum.any (· < 0) then
      throwErrorAt entry "Nat maximum contradicts its nonnegative range"
    if (bounds.minLength.isSome || bounds.maxLength.isSome) && kind != "string" then
      throwErrorAt entry "length constraints require String or Option String"
    if (bounds.minItems.isSome || bounds.maxItems.isSome) && kind != "array" then
      throwErrorAt entry "item constraints require Array or List"
    if (bounds.minimum.isSome || bounds.maximum.isSome) && kind != "number" then
      throwErrorAt entry "numeric constraints require Nat, Int, or Float"
    fieldNames := fieldNames.push field
    fieldTypes := fieldTypes.push type
    let bounds ← boundsSyntax bounds
    let shape ← `((HasSchema.shape (α := $type)).constrain $bounds)
    definitions := definitions.push (← `(($(quote field.getId.toString), $shape, !($shape).optional)))
  elabCommand (← `(structure $name where
    $[$fieldNames:ident : $fieldTypes:term]*
    deriving Lean.FromJson, Lean.ToJson))
  let fullName ← resolveGlobalConstNoOverload name
  elabCommand (← `(instance : HasSchema $name where
    name := $(quote fullName.toString)
    shape := Shape.object #[$definitions,*]))

@[command_elab weaveSchema] def elabWeaveSchema : CommandElab := fun stx => do
  let `(weave_schema $name:ident where { $entries:weave_schema_field,* }) := stx | throwUnsupportedSyntax
  emitSchema name entries.getElems

end LeanWeave.Schema
