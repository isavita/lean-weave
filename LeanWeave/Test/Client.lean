import LeanWeave.Core

namespace LeanWeave.Test

/-- In-process test request. Use live socket fixtures to establish protocol behavior. -/
def context (method : String := "GET") (target : String := "/")
    (headers : Array LeanNet.HTTP1.Header := #[]) (body : ByteArray := ByteArray.empty) : Except AppError Context :=
  Context.fromRequest { method, target := target.toUTF8, headers := #[{ name := "host", value := "example.test".toUTF8 }] ++ headers, body }

def request (handler : Handler) (method : String := "GET") (target : String := "/")
    (headers : Array LeanNet.HTTP1.Header := #[]) (body : ByteArray := ByteArray.empty) : IO (Except AppError Response) :=
  match context method target headers body with
  | .error error => pure (.error error)
  | .ok ctx => (handler ctx).run

end LeanWeave.Test
