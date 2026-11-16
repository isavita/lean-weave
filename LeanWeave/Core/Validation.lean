import LeanWeave.Core.Types

namespace LeanWeave.Validation

structure FieldError where
  field : String
  message : String
  deriving Repr, BEq

abbrev Result (α : Type) := Except (Array FieldError) α
abbrev Validator (α : Type) := String → Result α

def required (field : String) : Validator String := fun value =>
  if value.trimAscii.toString.isEmpty then .error #[{ field, message := "is required" }] else .ok value

def bounded (field : String) (maximum : Nat) : Validator String := fun value =>
  if value.utf8ByteSize > maximum then .error #[{ field, message := s!"must be at most {maximum} bytes" }] else required field value

def natural (field : String) : Validator Nat := fun value =>
  match value.toNat? with
  | some n => .ok n
  | none => .error #[{ field, message := "must be a natural number" }]

def field (fields : Array (String × String)) (name : String) (validate : Validator α) : Result α := do
  let values := fields.filter (·.1 == name)
  if values.size > 1 then throw #[{ field := name, message := "must occur once" }]
  validate ((values[0]?.map Prod.snd).getD "")

/-- Accumulate independent errors while constructing a typed result. -/
def map2 (f : α → β → γ) (a : Result α) (b : Result β) : Result γ :=
  match a,b with
  | .ok a, .ok b => .ok (f a b)
  | .error a, .error b => .error (a ++ b)
  | .error e, _ | _, .error e => .error e

end LeanWeave.Validation
