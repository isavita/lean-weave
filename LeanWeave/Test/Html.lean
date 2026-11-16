import LeanWeave.Html

namespace LeanWeave.Test

/-- error: tag 'script' is not allowed; use rawUnsafe only for audited trusted HTML -/
#guard_msgs in
#check html! "<script>alert(1)</script>"

/-- error: attribute 'onclick' is not allowed -/
#guard_msgs in
#check html! "<a onclick='alert(1)'>bad</a>"

/-- error: URL scheme is not allowed -/
#guard_msgs in
#check html! "<a href='javascript:alert(1)'>bad</a>"

private def check (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw (IO.userError s!"HTML: {message}")

private def rendered (html : Html.Html) : String := String.fromUTF8! (Html.render html)

private def safeUrl (value : String) : IO Html.SafeUrl :=
  match Html.SafeUrl.parse value with
  | .ok url => pure url
  | .error message => throw (IO.userError message)

private def greeting : String → Html.Html := html! "<h1>Hello {{name}}</h1>"
private def link : Html.SafeUrl → String → Html.Html :=
  html! "<a href='{{location}}' title='{{label}}'>{{label}}</a>"
private def password : String → Html.Html :=
  html! "<input type='password' value='{{value}}' required readonly>"

/-- Runnable hostile-input, safe URL, template, forms and negotiation checks. -/
def htmlTests : IO Unit := do
  let attack := "<script>alert(1)</script>\"'&"
  check (rendered (Html.text attack) == "&lt;script&gt;alert(1)&lt;/script&gt;&quot;&#39;&amp;")
    "text escaped all HTML delimiters"
  let field := Html.void .input #[.text .value "\" autofocus onfocus=alert(1) x=\""]
  check (rendered field == "<input value=\"&quot; autofocus onfocus=alert(1) x=&quot;\">")
    "attribute breakout escaped"
  for url in ["javascript:alert(1)", "JaVaScRiPt:alert(1)", "data:text/html,evil", "vbscript:evil",
      "//evil.test", "/\\evil.test", "https://safe.test\r\nLocation:evil", " java\tscript:evil"] do
    check (!(Html.SafeUrl.parse url).isOk) s!"rejected unsafe URL {repr url}"
  for url in ["/", "/posts/1?x=2&y=3", "posts/1", "#fragment", "https://example.test/x", "mailto:a@example.test"] do
    check (Html.SafeUrl.parse url |>.isOk) s!"accepted safe URL {repr url}"
  check (rendered (greeting attack) == "<h1>Hello &lt;script&gt;alert(1)&lt;/script&gt;&quot;&#39;&amp;</h1>")
    "compiled template text slot escaped"
  let destination ← safeUrl "/posts?x=\"&y=2"
  check (rendered (link destination "\"<label>" ) ==
    "<a href=\"/posts?x=&quot;&amp;y=2\" title=\"&quot;&lt;label&gt;\">&quot;&lt;label&gt;</a>")
    "typed URL and repeated string template slots escaped"
  check (rendered (password "\"x") == "<input type=\"password\" value=\"&quot;x\" required readonly>")
    "boolean attributes and void elements compiled"
  for source in ["<script>evil</script>", "<img onerror='evil'>", "<a href='javascript:evil'>x</a>",
      "<div><span></div>", "<p id='a' id='b'>x</p>", "<div title={{x}}>x</div>",
      "<a href='/posts/{{id}}'>x</a>", "<a href='{{x}}'>{{x}}</a>", "<p>{{unclosed</p>"] do
    check (!(Html.Template.compile source).isOk) s!"template compiler rejected {repr source}"
  check (rendered (Html.rawUnsafe "<b>trusted</b>".toUTF8) == "<b>trusted</b>") "explicit raw hatch"
  let form := Html.form (← safeUrl "/login") .post #[Html.textField
    { name := "email", label := "Email", value := "\"<evil>", errors := #["<bad>"] }] "token\"<"
  let output := rendered form
  check ((output.splitOn "name=\"_csrf\"").length == 2) "mutation form has CSRF field"
  check ((output.splitOn "token&quot;&lt;").length == 2) "CSRF token escaped"
  check ((output.splitOn "&lt;bad&gt;").length == 2) "field error escaped"
  check ((output.splitOn "aria-invalid=\"true\"").length == 2) "field error accessibility"
  check (Html.negotiate none == some .html) "missing Accept defaults to HTML"
  check (Html.negotiate (some "application/json") == some .json) "JSON media type"
  check (Html.negotiate (some "text/html;q=0.1, application/json;q=0.9") == some .json) "Accept quality"
  check (Html.negotiate (some "text/html;q=0, */*;q=1") == some .json) "specific exclusion beats wildcard"
  check (Html.negotiate (some "text/html;q=0, application/json;q=0") == none) "unacceptable types rejected"
  let asset : Html.Asset := { path := "/app.css", fingerprint := "a1234" }
  check ((← safeUrl "/app.css?v=a1234").value == (asset.url.toOption.map (·.value)).getD "")
    "versioned asset URL"
  check (!({ path := "//evil", fingerprint := "1" } : Html.Asset).url.isOk) "asset host rejected"
  check ((Html.redirect (← safeUrl "/welcome")).getHeader "location" == some "/welcome") "redirect location"
  check ((Html.toResponse (Html.page "<title>" (Html.text "hello"))).getHeader "content-type" ==
    some "text/html; charset=utf-8") "HTML content type"

end LeanWeave.Test
