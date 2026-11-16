import LeanWeave.Tenant.Config
import LeanWeave.Html

namespace LeanWeave.Tenant

/-- Branding remains escaped HTML, even when configuration is changed remotely. -/
def brand (ctx : Context) : Html.Html :=
  Html.element .div #[.text .class_ "tenant-brand"] #[Html.text ((ctx.attribute "tenant.brand.name").getD "LeanWeave")]

/-- CSS contains only a validated hex color; scripts/style markup cannot be injected. -/
def brandingCss (ctx : Context) : Response :=
  let color := (ctx.attribute "tenant.brand.color").getD "#334155"
  let config : Config := { brandColor := color }
  let color := if config.validate.isOk then color else "#334155"
  (Response.text (":root{ --weave-brand: " ++ color ++ "; } .tenant-brand{color:var(--weave-brand);}"))
    |>.withHeader "content-type" "text/css; charset=utf-8"
    |>.withHeader "cache-control" "private, no-store"

end LeanWeave.Tenant
