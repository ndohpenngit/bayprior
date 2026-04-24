## bayprior — golem development workflow
## Run sections interactively. Do NOT source() this file whole.

# ── 0. Install dependencies ───────────────────────────────────────────────────
# install.packages(c(
#   "golem", "devtools", "usethis", "roxygen2", "testthat",
#   "shinydashboard", "shinyWidgets", "shinycssloaders",
#   "plotly", "ggplot2", "dplyr", "purrr", "rlang", "glue", "cli",
#   "rmarkdown", "knitr", "config", "DT"
# ))

# ── 1. Create package (run once) ─────────────────────────────────────────────
# golem::create_golem("bayprior")

# ── 2. Set metadata ───────────────────────────────────────────────────────────
usethis::use_description(fields = list(
  Title = "Bayesian Prior Elicitation for Clinical Trials",
  License = "GPL (>= 3)"
))
usethis::use_gpl_license(version = 3)

# ── 3. Add dependencies ───────────────────────────────────────────────────────
# Shiny / golem
usethis::use_package("golem")
usethis::use_package("shiny")
usethis::use_package("shinydashboard")
usethis::use_package("shinyWidgets")
usethis::use_package("shinycssloaders")
usethis::use_package("plotly")
usethis::use_package("DT")
usethis::use_package("config")
usethis::use_package("htmltools")
# Stats
usethis::use_package("ggplot2")
usethis::use_package("dplyr")
usethis::use_package("purrr")
usethis::use_package("rlang")
usethis::use_package("glue")
usethis::use_package("cli")
usethis::use_package("rmarkdown")
usethis::use_package("knitr")
# Suggests
usethis::use_package("testthat",   type = "Suggests")
usethis::use_package("shinytest2", type = "Suggests")
usethis::use_package("withr",      type = "Suggests")
usethis::use_package("covr",       type = "Suggests")
usethis::use_package("vdiffr",     type = "Suggests")

# ── 4. golem infrastructure ───────────────────────────────────────────────────
golem::add_css_file("custom")          # inst/app/www/custom.css
golem::add_js_file("custom")           # inst/app/www/custom.js
golem::add_favicon()                   # inst/app/www/favicon.ico
golem::add_shinyappsio_file()          # app.R at root for deployment

# ── 5. Register all modules (creates R/mod_NAME.R stubs if not yet existing) ─
# NOTE: stubs already created manually — these calls update the dev log.
golem::add_module(name = "welcome",     with_test = TRUE)
golem::add_module(name = "elicitation", with_test = TRUE)
golem::add_module(name = "roulette",    with_test = TRUE)
golem::add_module(name = "pooling",     with_test = TRUE)
golem::add_module(name = "conflict",    with_test = TRUE)
golem::add_module(name = "mahal",       with_test = TRUE)
golem::add_module(name = "sensitivity", with_test = TRUE)
golem::add_module(name = "sceptical",   with_test = TRUE)
golem::add_module(name = "robust",      with_test = TRUE)
golem::add_module(name = "power",       with_test = TRUE)
golem::add_module(name = "report",      with_test = TRUE)

# ── 6. Tests ──────────────────────────────────────────────────────────────────
usethis::use_testthat()
usethis::use_test("elicitation")
usethis::use_test("conflict_sensitivity")
usethis::use_test("aggregation")

# ── 7. Documentation ──────────────────────────────────────────────────────────
usethis::use_readme_rmd()
usethis::use_news_md()
usethis::use_vignette("bayprior-intro", title = "Introduction to bayprior")
devtools::document()

# ── 8. Check ──────────────────────────────────────────────────────────────────
devtools::load_all()
devtools::test()
devtools::check()

# ── 9. Run ────────────────────────────────────────────────────────────────────
bayprior::run_app()

# ── 10. Deploy ────────────────────────────────────────────────────────────────
# rsconnect::deployApp()        # shinyapps.io
# docker build -t bayprior .   # Docker
