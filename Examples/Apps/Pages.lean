import Examples.Common

namespace LeanWeave.Examples.Pages

open scoped LeanWeave.Router

private def navigation : Html.Html := Html.element .nav #[] #[
  anchor "/" "Home", Html.text " · ", anchor "/about" "About", Html.text " · ", anchor "/hello/Lean" "Greeting"]

private def stylesheetUrl : Html.SafeUrl :=
  (({ path := "/assets/app.css", fingerprint := "weave-v1" } : Html.Asset).url).toOption.getD Html.SafeUrl.root

private def render (title : String) (body : Html.Html) : Response :=
  Html.toResponse <| Html.append (Html.rawUnsafe "<!doctype html>".toUTF8) <|
    Html.element .html #[.text .lang "en"] #[
      Html.element .head #[] #[
        Html.void .meta_ #[.text .charset "utf-8"],
        Html.element .title #[] #[Html.text title],
        Html.void .link #[.text .rel "stylesheet", .url .href stylesheetUrl]],
      Html.element .body #[] #[navigation, Html.element .main #[] #[body]]]

private def home : Handler := fun _ => pure <| render "LeanWeave pages" <|
  Html.element .section #[] #[
    (html! "<h1>LeanWeave pages</h1><p>Typed HTML, compiled templates and named routes.</p>"),
    anchor "/hello/World" "Try a typed greeting"]

private def about : Handler := fun _ => pure <| render "About LeanWeave" <|
  html! "<h1>About</h1><p>This page is rendered into UTF-8 byte builders.</p>"

private def hello (name : String) : Handler := fun ctx => pure <|
  Html.negotiated ctx (Html.page "Greeting" ((html! "<h1>Hello {{name}}</h1>") name))
    (Lean.Json.mkObj [("greeting", Lean.toJson s!"Hello {name}")])

private def stylesheet : Handler := fun ctx => do
  unless ctx.queryValue "v" == some "weave-v1" do return Response.text "Unknown asset version" 404
  pure <| (Response.text "body{max-width:48rem;margin:3rem auto;padding:1rem;font:18px system-ui}nav{margin-bottom:2rem}")
    |>.withHeader "content-type" "text/css; charset=utf-8"
    |>.withHeader "cache-control" "public, max-age=31536000, immutable"

weave_routes routes where {
  route "GET" "/" as home => home,
  route "GET" "/about" as about => about,
  route "GET" "/hello/:name<text>" as hello => hello,
  route "GET" "/assets/app.css" as stylesheet => stylesheet
}

def factory : Factory := fun _ => pure routes

end LeanWeave.Examples.Pages
