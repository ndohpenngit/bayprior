# bayprior 0.1.2

## Bug fixes

* Fixed `prior_report()` failing on remote platforms (shinyapps.io, Posit
  Connect) due to Quarto spawning a subprocess that could not find the
  `bayprior` package. The report now uses a two-step approach: `knitr::knit()`
  executes all R code in the current session, then
  `quarto::quarto_render(execute = FALSE)` handles formatting via pandoc only.
  No R subprocess is spawned in the second step (#1).

* Fixed `mod_sensitivity`: sensitivity analysis no longer requires conflict
  diagnostics to have been run first. The module now has its own data entry
  UI (data type, events/n for binary; mean/SD/n for continuous) that
  auto-selects the correct type based on the prior family (Beta → binary,
  others → continuous). Previously the fallback always used
  `type = "continuous"`, causing conjugate updating to fail for Beta priors
  and returning NA for all grid evaluations.

* Fixed "Using data from Conflict Diagnostics" label appearing even when no
  conflict diagnostics had been run. The label is now generated inside
  `renderUI` conditionally on `shared$conflict`.

* Fixed non-ASCII characters (`─`, `—`) in `R/app_ui.R` causing an R CMD
  check WARNING. All box-drawing and em-dash characters replaced with ASCII
  equivalents.

* Fixed `prior_report()` missing `@param` documentation for `prior_plot`,
  `overlay_plot`, `tornado_plot`, and `heatmap_plot`, causing an R CMD
  check WARNING about undocumented arguments.

## New features

* Added `robust_prior`, `sceptical_prior`, `power_prior`, `robust_plot`,
  `sceptical_plot`, and `power_plot` arguments to `prior_report()`. When
  supplied, robust and sensitivity priors appear as a dedicated section in
  the report with parameter tables and density plots. The compliance
  checklist row "Robust / sceptical prior computed" is automatically marked
  Complete.

* Added bayprior hex logo to the Shiny app header and browser favicon.
  Logo is served from `inst/app/www/favicon.png`.

---

# bayprior 0.1.1

## Initial release

### Prior Elicitation
* Quantile matching, moment matching, and SHELF roulette for Beta, Normal,
  Gamma, and Log-Normal families
* `elicit_beta()`, `elicit_normal()`, `elicit_gamma()`, `elicit_lognormal()`,
  `elicit_roulette()`, `elicit_mixture()`

### Expert Pooling
* Linear and logarithmic pooling with Bhattacharyya agreement diagnostics
* `aggregate_experts()`

### Conflict Diagnostics
* Box p-value, surprise index, KL divergence, Bhattacharyya overlap
* Multivariate Mahalanobis distance for co-primary endpoints
* `prior_conflict()`, `conflict_mahalanobis()`

### Sensitivity Analysis
* Posterior quantity grid and credible interval sensitivity with tornado
  plots and influence heatmaps
* `sensitivity_grid()`, `sensitivity_cri()`

### Robust Priors
* Robust mixture prior (Schmidli et al., 2014): `robust_prior()`
* Sceptical prior (Spiegelhalter & Freedman, 1994): `sceptical_prior()`
* Calibrated power prior (Ibrahim & Chen, 2000): `calibrate_power_prior()`

### Reporting
* HTML, PDF, and Word prior justification reports via Quarto: `prior_report()`
* FDA/EMA regulatory compliance checklist

### Documentation

* Added Quarto GitHub Pages documentation site at
  `https://ndohpenngit.github.io/bayprior/` with six rendered vignettes,
  a Changelog page, and a Cheat Sheet. Site auto-rebuilds on every push
  to `main` via GitHub Actions.

* Updated all six vignettes to document new `prior_report()` arguments,
  the sensitivity analysis CrI toggle, and the robust priors report
  integration.

* Updated `README.md` with live app badge, and
  Documentation section linking to the GitHub Pages site.

### App
* Full interactive Shiny application: `run_app()`
* Dark/light mode toggle with localStorage persistence
* bayprior hex logo in header and browser tab