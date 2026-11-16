import LeanWeave.Core.Body

namespace LeanWeave.Schema

structure Bounds where
  minLength : Option Nat := none
  maxLength : Option Nat := none
  minimum : Option Int := none
  maximum : Option Int := none
  minItems : Option Nat := none
  maxItems : Option Nat := none
  deriving Repr, BEq, Inhabited

private def lowerBound [Max α] (a b : Option α) : Option α :=
  match a, b with | some a, some b => some (max a b) | some a, none => some a | none, b => b
private def upperBound [Min α] (a b : Option α) : Option α :=
  match a, b with | some a, some b => some (min a b) | some a, none => some a | none, b => b

def Bounds.intersect (a b : Bounds) : Bounds := {
  minLength := lowerBound a.minLength b.minLength, maxLength := upperBound a.maxLength b.maxLength,
  minimum := lowerBound a.minimum b.minimum, maximum := upperBound a.maximum b.maximum,
  minItems := lowerBound a.minItems b.minItems, maxItems := upperBound a.maxItems b.maxItems }

inductive Scalar where
  | string | integer | number | boolean | null
  deriving Repr, BEq, Inhabited

/-- Runtime validation and OpenAPI share this finite schema tree. Proofs are not
    embedded in request data. Nested named definitions may be inlined safely. -/
inductive Shape where
  | scalar (kind : Scalar) (bounds : Bounds := {})
  | array (element : Shape) (bounds : Bounds := {})
  | object (fields : Array (String × Shape × Bool)) (additional : Bool := false)
  | nullable (value : Shape)
  | enumeration (values : Array Lean.Json)
  deriving BEq, Inhabited

def Shape.optional : Shape → Bool
  | .nullable _ => true
  | _ => false

def Shape.constrain (shape : Shape) (bounds : Bounds) : Shape :=
  match shape with
  | .scalar kind previous => .scalar kind (previous.intersect bounds)
  | .array element previous => .array element (previous.intersect bounds)
  | .nullable value => .nullable (value.constrain bounds)
  | _ => shape

class HasSchema (α : Type) where
  name : String
  shape : Shape

instance : HasSchema String := ⟨"String", .scalar .string⟩
instance : HasSchema Nat := ⟨"Nat", .scalar .integer { minimum := some 0 }⟩
instance : HasSchema Int := ⟨"Int", .scalar .integer⟩
instance : HasSchema Bool := ⟨"Bool", .scalar .boolean⟩
instance : HasSchema Float := ⟨"Float", .scalar .number⟩
instance : HasSchema Unit := ⟨"Unit", .scalar .null⟩
instance [HasSchema α] : HasSchema (Option α) :=
  ⟨"Optional_" ++ HasSchema.name (α := α), .nullable (HasSchema.shape (α := α))⟩
instance [HasSchema α] : HasSchema (Array α) :=
  ⟨"Array_" ++ HasSchema.name (α := α), .array (HasSchema.shape (α := α))⟩
instance [HasSchema α] : HasSchema (List α) :=
  ⟨"List_" ++ HasSchema.name (α := α), .array (HasSchema.shape (α := α))⟩

structure Definition where
  name : String
  shape : Shape
  deriving BEq

def definition (α : Type) [HasSchema α] : Definition :=
  ⟨HasSchema.name (α := α), HasSchema.shape (α := α)⟩

private def boundFields (bounds : Bounds) : List (String × Lean.Json) :=
  Lean.Json.opt "minLength" bounds.minLength ++ Lean.Json.opt "maxLength" bounds.maxLength ++
  Lean.Json.opt "minimum" bounds.minimum ++ Lean.Json.opt "maximum" bounds.maximum ++
  Lean.Json.opt "minItems" bounds.minItems ++ Lean.Json.opt "maxItems" bounds.maxItems

/-- JSON Schema 2020-12 forms, valid in OpenAPI 3.1. -/
partial def Shape.toJson : Shape → Lean.Json
  | .scalar kind bounds => Lean.Json.mkObj <|
      ("type", Lean.toJson (match kind with
        | .string => "string" | .integer => "integer" | .number => "number"
        | .boolean => "boolean" | .null => "null")) :: boundFields bounds
  | .array element bounds => Lean.Json.mkObj <|
      [("type", Lean.toJson "array"), ("items", element.toJson)] ++ boundFields bounds
  | .object fields additional => Lean.Json.mkObj [
      ("type", Lean.toJson "object"),
      ("properties", Lean.Json.mkObj (fields.toList.map fun (name, shape, _) => (name, shape.toJson))),
      ("required", Lean.toJson (fields.filterMap fun (name, _, required) => if required then some name else none)),
      ("additionalProperties", Lean.toJson additional)]
  | .nullable value => Lean.Json.mkObj [("anyOf", Lean.Json.arr #[value.toJson, Lean.Json.mkObj [("type", Lean.toJson "null")]])]
  | .enumeration values => Lean.Json.mkObj [("enum", Lean.Json.arr values)]

private def failAt (path message : String) : Except String Unit := .error s!"{path}: {message}"

/-- Validation is depth/collection bounded as well as bounded by the body decoder. -/
def Shape.validate (shape : Shape) (json : Lean.Json) (fuel : Nat := 32)
    (path : String := "$") : Except String Unit := do
  match fuel with
  | 0 => throw s!"{path}: schema nesting limit exceeded"
  | fuel + 1 =>
    match shape with
    | .nullable child => if json == .null then pure () else child.validate json fuel path
    | .enumeration values => unless values.contains json do failAt path "not an allowed value"
    | .scalar .null _ => unless json == .null do failAt path "expected null"
    | .scalar .boolean _ => unless json.getBool?.toOption.isSome do failAt path "expected boolean"
    | .scalar .number bounds =>
      let value ← json.getNum?.mapError (fun _ => s!"{path}: expected number")
      if bounds.minimum.any (fun n => value < (n : Lean.JsonNumber)) then failAt path "below minimum"
      else if bounds.maximum.any (fun n => value > (n : Lean.JsonNumber)) then failAt path "above maximum"
    | .scalar .integer bounds =>
      let value ← json.getInt?.mapError (fun _ => s!"{path}: expected integer")
      if bounds.minimum.any (value < ·) then failAt path "below minimum"
      else if bounds.maximum.any (value > ·) then failAt path "above maximum"
    | .scalar .string bounds =>
      let value ← json.getStr?.mapError (fun _ => s!"{path}: expected string")
      if bounds.minLength.any (value.length < ·) then failAt path "string too short"
      else if bounds.maxLength.any (value.length > ·) then failAt path "string too long"
    | .array child bounds =>
      let values ← json.getArr?.mapError (fun _ => s!"{path}: expected array")
      if values.size > 4096 then failAt path "array item budget exceeded"
      if bounds.minItems.any (values.size < ·) then failAt path "too few items"
      if bounds.maxItems.any (values.size > ·) then failAt path "too many items"
      for i in [:values.size] do child.validate values[i]! fuel s!"{path}[{i}]"
    | .object fields additional =>
      let object ← json.getObj?.mapError (fun _ => s!"{path}: expected object")
      if object.size > 1024 then failAt path "object field budget exceeded"
      if !additional then
        for (key, _) in object.toArray do
          unless fields.any (fun (name, _, _) => name == key) do failAt (path ++ "." ++ key) "unknown field"
      for (name, child, required) in fields do
        match json.getObjVal? name with
        | .error _ => if required then failAt (path ++ "." ++ name) "required field missing"
        | .ok value => child.validate value fuel (path ++ "." ++ name)

def decode (α : Type) [HasSchema α] [Lean.FromJson α] (json : Lean.Json) : Except String α := do
  (HasSchema.shape (α := α)).validate json
  Lean.fromJson? json

def decodeBody (α : Type) [HasSchema α] [Lean.FromJson α] (ctx : Context)
    (limits : Body.Limits := {}) : Except AppError α := do
  let media := (((ctx.header "content-type").getD "").splitOn ";").head!.trimAscii.toString.toLower
  unless media == "application/json" || (media.startsWith "application/" && media.endsWith "+json") do
    throw { status := 415, message := "JSON Content-Type required" }
  let json ← Body.json ctx.request.body limits
  (decode α json).mapError fun message => { status := 422, message }

private def typedResponse (β : Type) [HasSchema β] [Lean.ToJson β] (value : β) (status : Nat) : AppM Response := do
  if status == 204 || status == 205 then return { status }
  let json := Lean.toJson value
  match (HasSchema.shape (α := β)).validate json with
  | .error _ => throw { status := 500, message := "response violates its declared schema" }
  | .ok () => return Response.json json status

/-- Bind both request validation and response documentation to Lean types. -/
def jsonHandler (α β : Type) [HasSchema α] [Lean.FromJson α] [HasSchema β] [Lean.ToJson β]
    (action : α → Context → AppM β) (status : Nat := 200) (limits : Body.Limits := {}) : Handler := fun ctx => do
  let input ← match decodeBody α ctx limits with | .ok input => pure input | .error error => throw error
  typedResponse β (← action input ctx) status

def outputHandler (β : Type) [HasSchema β] [Lean.ToJson β]
    (action : Context → AppM β) (status : Nat := 200) : Handler := fun ctx =>
  do typedResponse β (← action ctx) status

end LeanWeave.Schema
