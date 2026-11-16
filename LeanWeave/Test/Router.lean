import LeanWeave.Router

namespace LeanWeave.Test.Router

open scoped LeanWeave.Router

private def text (value : String) : Handler := fun _ => pure (Response.text value)
private def number (value : Nat) : Handler := fun _ => pure (Response.text s!"nat:{value}")
private def integer (value : Int) : Handler := fun _ => pure (Response.text s!"int:{value}")
private def word (value : String) : Handler := fun _ => pure (Response.text s!"text:{value}")
private def tagged : Middleware := fun next ctx => do
  let response ← next { ctx with attributes := ctx.attributes.push ("group", "yes") }
  return response.withHeader "x-group" "yes"

private def controller : LeanWeave.Router.Resource := {
  index := text "index", new := text "new", create := text "create",
  «show» := number, edit := number, update := number, delete := number }

weave_routes routes where {
  route "GET" "/v/:value<text>" as fallback => word,
  route "GET" "/v/:value<int>" as integer => integer,
  route "GET" "/v/:value<nat>" as number => number,
  route "GET" "/v/new" as new => text "static",
  route "POST" "/v/new" as create => text "created",
  route "GET" "/method" as method => (fun ctx => pure (Response.text ctx.request.method)),
  route "GET" "/explicit" as explicitGet => text "GET",
  route "HEAD" "/explicit" as explicitHead => text "HEAD",
  route "GET" "/trailing/" as trailing => text "slash",
  route "GET" "/files/*path" as files => word,
  route "GET" "/files" as filesRoot => text "exact",
  route "GET" "/host" as anyHost => text "any",
  group "/" host "*.example.com" {
    route "GET" "/host" as wildcardHost => text "wildcard"
  },
  group "/" host "admin.example.com" {
    route "GET" "/host" as exactHost => text "exact"
  },
  group "/admin" using (tagged) {
    route "GET" "/" as admin => text "admin"
  },
  resources "/posts" as posts => controller
}

/-- error: ambiguous routes 'one' and 'two' -/
#guard_msgs in
weave_routes ambiguous where {
  route "GET" "/users/:id<nat>" as one => number,
  route "GET" "/users/:other<nat>" as two => number
}

/-- error: duplicate route name 'one' -/
#guard_msgs in
weave_routes duplicated where {
  route "GET" "/one" as one => text "one",
  route "POST" "/two" as one => text "two"
}

/-- error: unknown capture type 'uuid' (expected nat, int, text) -/
#guard_msgs in
weave_routes unknownType where {
  route "GET" "/users/:id<uuid>" as one => word
}

private def check (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw (IO.userError s!"router: {message}")

private def context (target : String) (method : String := "GET")
    (authority : String := "example.test") (version : String := "HTTP/1.1") : Except AppError Context :=
  Context.fromRequest {
    method, version, target := target.toUTF8,
    headers := #[{ name := "host", value := authority.toUTF8 }], body := ByteArray.empty }

private def request (target : String) (method : String := "GET")
    (authority : String := "example.test") (version : String := "HTTP/1.1") : IO Response := do
  let ctx ← match context target method authority version with
    | .ok ctx => pure ctx | .error err => throw (IO.userError err.message)
  match ← AppM.run (routes.handler ctx) with
  | .ok response => return response
  | .error error => throw (IO.userError error.message)

private def expectBody (target expected : String) (method : String := "GET")
    (authority : String := "example.test") : IO Unit := do
  let response ← request target method authority
  check (response.status == 200 && String.fromUTF8! response.body == expected) s!"{method} {target}"

/-- Routing acceptance tests run without network sockets; transport parity is tested separately. -/
def run : IO Unit := do
  expectBody "/v/new" "static"
  expectBody "/v/012" "nat:12"
  expectBody "/v/-12" "int:-12"
  expectBody "/v/hello" "text:hello"
  expectBody "/v/%31" "nat:1"
  expectBody "/v/%252F" "text:%2F"
  expectBody "/v/new" "created" "POST"
  expectBody "/host" "exact" "GET" "ADMIN.EXAMPLE.COM"
  expectBody "/host" "wildcard" "GET" "app.example.com"
  expectBody "/host" "any" "GET" "admin.example.com:8080"
  expectBody "/files" "exact"
  expectBody "/files/a/b" "text:a/b"
  expectBody "/trailing/" "slash"
  check ((← request "/trailing").status == 404) "trailing slash is significant"
  check ((← request "/missing").status == 404) "404"
  let wrong ← request "/v/new" "DELETE"
  check (wrong.status == 405 && wrong.getHeader "allow" == some "GET, HEAD, OPTIONS, POST") "405 Allow"
  let options ← request "/v/new" "OPTIONS"
  check (options.status == 204 && options.body.isEmpty && options.getHeader "allow" == wrong.getHeader "allow") "OPTIONS"
  let serverOptions ← request "*" "OPTIONS"
  check (serverOptions.status == 204 && serverOptions.getHeader "allow" ==
    some "DELETE, GET, HEAD, OPTIONS, PATCH, POST, PUT") "server OPTIONS *"
  check ((context "*" "GET").toOption.isNone) "asterisk target only accepts OPTIONS"
  expectBody "/method" "HEAD" "HEAD"
  expectBody "/explicit" "HEAD" "HEAD"
  let get ← request "/v/42"
  let head ← request "/v/42" "HEAD"
  check (head.body == get.body) "HEAD retains representation for lean-net Content-Length"
  let admin ← request "/admin"
  check (admin.getHeader "x-group" == some "yes") "group pipeline"
  expectBody "/posts" "index"
  expectBody "/posts/new" "new"
  expectBody "/posts" "create" "POST"
  for method in #["GET", "PUT", "PATCH", "DELETE"] do expectBody "/posts/42" "nat:42" method
  expectBody "/posts/42/edit" "nat:42"
  check ((routes.posts.showUrl 17).toOption == some "/posts/17") "typed resource reverse"
  check ((routes.numberUrl 3 #[("q", "a & b")]).toOption == some "/v/3?q=a%20%26%20b") "query encoding"
  check ((routes.fallbackUrl "a?# b").toOption == some "/v/a%3F%23%20b") "capture segment encoding"
  check ((routes.fallbackUrl "a/b").toOption.isNone) "forbidden separator in text capture"
  check ((routes.fallbackUrl "").toOption.isNone) "empty text reverse capture"
  check ((routes.url "missing").toOption.isNone) "unknown route name"
  check ((routes.url "number" #[("value", .text "3")]).toOption.isNone) "reverse codec mismatch"
  for target in #["/v/%2f", "/v/%5C", "/v/%00", "/v/%FF", "/v/%", "/v/%2e"] do
    check ((context target).toOption.isNone) s!"strict decode {target}"
  let h2 ← request "/v/42" "GET" "example.test" "HTTP/2"
  check (h2.status == get.status && h2.body == get.body) "normalized HTTP/1 and HTTP/2 request parity"

end LeanWeave.Test.Router
