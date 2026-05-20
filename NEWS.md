# bayprior 0.2.4

## Documentation & UX

* **`robust_prior()` — added `@details` section** explaining that the vague
  component is always Normal, making cross-family mixtures (e.g. Beta +
  Normal) structurally inevitable for non-Normal informative priors. Clarifies
  that mixture mean and SD are always computed in closed form; only the density
  (used for plotting) is approximated numerically. Updated `@examples` to
  include a Beta informative prior case with `suppressWarnings()`.

* **`elicit_mixture()` — added `@details` section** documenting the same
  numerical density approximation behaviour at the lower level where mixtures
  are first constructed. Notes that `suppressWarnings()` should only be used
  after verifying the mixture is appropriate, not as a blanket silence.

* **Robust Mixture density plot — warning redirected from console to UI.**
  The "Components have different distribution families. Mixture densities
  computed numerically." warning is now intercepted and surfaced as an amber
  `showNotification()` in the Shiny app rather than printed to the
  R console. The warning is informative — it signals numerical approximation
  rather than closed-form density computation — and should be visible to the
  analyst. Previously it was suppressed entirely, which was not ideal.

---

# bayprior 0.2.3

## New features

* **Poisson data type in Power Prior Calibration** — the power prior module now
  supports Poisson/count endpoints alongside Binary and Continuous. Uses the
  Gamma-Poisson conjugate update (same closed-form formula as Gamma-continuous).

* **Prior-data compatibility warning in Power Prior** — an amber alert now
  appears when the active prior family is incompatible with the selected data
  type (e.g. Normal prior with binary data, Beta prior with Poisson data),
  guiding users to re-elicit with an appropriate family before calibrating.

* **Mahalanobis module limitations documented** — the Multivariate Conflict
  Setup panel now shows an inline note clarifying that the test assumes
  multivariate Normal summary statistics, that proportion/HR endpoints should
  be entered on the log-odds/log scale, and that k > 2 endpoints are a planned
  extension.

## Architecture

* **`base_prior` reactive** — introduced `shared$base_prior` (written only by
  elicitation and pooling) alongside `active_prior()` (written by all modules).
  Sensitivity analysis, Robust Mixture, and Power Prior reset observers now
  watch `base_prior()` instead of `active_prior()`, eliminating the
  self-invalidation loop that wiped results immediately after they were
  produced.

* **Robust mixture compounding SD bug fixed** — the robust prior fit handler
  now always uses `base_prior()` as the informative component. Previously,
  repeated clicks used the previous robust mixture as the input, causing the
  vague SD to compound exponentially across clicks.

## Tests

* Added `tests/testthat/test-robust.R` — 33 tests covering `robust_prior()`,
  `sceptical_prior()` (all three families and strength levels),
  `calibrate_power_prior()` (all methods and data types),
  `elicit_exponential()`, `elicit_weibull()`, `plot.bayprior_power_prior()`,
  and `print.bayprior_power_prior()`.

* Added `tests/testthat/test-validation.R` — 10 tests covering all four
  functions in `validation_utils.R`.

* Added `skip_on_ci()` to all `shinytest2` Chromote tests to prevent 30-second
  timeout failures in non-interactive and CI environments.

## Documentation

* Added `@details` to `conflict_mahalanobis()` documenting the multivariate
  Normal assumption, required scale transformations for proportion and HR
  endpoints, and the bivariate (k = 2) limitation.

---

# bayprior 0.2.2

## UX improvements

* **Three-state auto theme** — theme now cycles Auto → Dark → Light on toggle.
  In Auto mode the app follows the OS `prefers-color-scheme` setting, which
  most operating systems tie to time of day. The button label shows the active
  state and whether it is following the OS or set manually (e.g.
  `Auto (dark)`, `Dark (manual)`). Switching back to Auto removes the stored
  preference and resumes OS tracking.

* **Disabled downstream buttons** — analysis buttons (Run Diagnostics, Run
  Sensitivity Analysis, Build robust/sceptical/power prior, Compute consensus
  prior) are now greyed out with a `not-allowed` cursor when no prior has been
  fitted. Hovering shows the tooltip *"Fit a prior in Prior Elicitation first"*.
  Buttons re-enable immediately when a prior is fitted. The Mahalanobis
  Conflict Setup button is intentionally excluded as it does not require an
  elicited prior.

* **Prior summary card in sidebar** — the active prior badge now shows a
  mini stats table with Mean, SD, and 95% CrI (where available) below the
  distribution family and label. Updates reactively on every prior change.
  Mixture priors show Mean and SD only (no closed-form CrI).

* **Diagnostic tooltips** — each conflict value box (Box p-value, Surprise
  index, Overlap coefficient) now has an info icon that displays a plain-
  language explanation on hover, useful for non-statistician stakeholders.

## Bug fixes

* Accepted updated plot snapshots for `plot_tornado()` and
  `plot_sensitivity()` following the axis label formatting change in v0.2.1
  (snake_case -> title case).

## Documentation

* Sensitivity plot labels updated in `plot_tornado()` and `plot_sensitivity()`
  package functions directly (not just the Shiny layer), so label formatting
  applies when functions are called from R as well as in the app.

---

# bayprior 0.2.1

## UX improvements

* Welcome tab now shows an SVG workflow diagram of all six analytical steps
  with colour-coded boxes and arrows.
* Sidebar menu items now show a green glowing dot when each step is complete
  (elicitation, pooling, conflict, sensitivity, robust priors).
* Export Report compliance checklist is now fully reactive — items update live
  with colour-coded icons (green check, amber minus, red X) and a ready/not-
  ready summary alert.
* Package version (`bayprior vX.X.X`) now displayed in the sidebar footer.

## Deployment fixes

* Removed `pkgload::load_all()` from `app.R` — it was a development-only
  line that caused startup failure on shinyapps.io.
* Added `renv::snapshot(type = "explicit")` to deployment workflow to prevent
  dev tools (`pkgload`, `pkgbuild`, `callr`, `desc`) from entering `renv.lock`.

## Documentation

* Information boxes on Welcome tab updated: Distributions 4 → 6, Conflict
  diagnostics → Data types (Binary · Continuous · Poisson · Survival).

---

# bayprior 0.2.0

## New features

* Added `elicit_exponential()` — elicits an Exponential(rate) prior for
  constant hazard rates and Poisson rate priors. Supports `"moments"` (mean
  to rate), `"rate"` (direct), and `"quantile"` (1D optimisation) methods.
  The Exponential distribution is conjugate for Poisson and survival data via
  Gamma-Poisson/Exponential updating.

* Added `elicit_weibull()` — elicits a Weibull(shape, scale) prior for
  non-constant hazard survival times (OS, PFS). Supports `"moments"` (2D
  Nelder-Mead), `"params"` (direct shape and scale), and `"quantile"` (2D
  Nelder-Mead) methods. Posteriors are approximated via Normal matching.

* Added `"poisson"` data type to `prior_conflict()`, `sensitivity_grid()`,
  and `sensitivity_cri()`. Supports count/adverse-event-rate endpoints:
  `list(type = "poisson", x = 12, n = 100)` (events / person-time).
  Conjugate update: `Gamma(shape + x, rate + n)`.

* Added `"survival"` data type to `prior_conflict()`, `sensitivity_grid()`,
  and `sensitivity_cri()`. Supports OS/PFS hazard rate endpoints:
  `list(type = "survival", x = 20, n = 400)` (events / total follow-up time).
  Conjugate update: `Gamma(shape + x, rate + n)` (Gamma-Exponential).

* Added density and x-range support for Exponential and Weibull distributions
  in `plot.bayprior()`. Previously these distributions rendered blank density
  plots; they now display correctly.

* Added comprehensive validation layer across all modules:
  - `.check_prior_data_compat()` — warns when a prior family is atypical for
    the chosen data type (e.g. Beta prior + Poisson data).
  - `.check_pooling_compat()` — blocks pooling of distributions with
    incompatible supports (e.g. Beta + Normal); warns for same-support
    cross-family pooling (e.g. Gamma + Exponential).
  - `.check_sensitivity_compat()` — warns for single-parameter families
    (Exponential) and cross-family mixtures; blocks incompatible-support
    mixture sensitivity.
  - `.validation_alert()` — renders Bootstrap alert boxes for inline UI
    feedback in the Shiny app.

* Sensitivity analysis is now **fully independent of conflict diagnostics**.
  The Sensitivity Analysis module has its own data entry UI (data type
  selector, events/n for binary, mean/SD/n for continuous, events/exposure
  for Poisson, events/follow-up for survival) that auto-populates the correct
  type from the active prior family. Previously, running conflict diagnostics
  was required before sensitivity analysis could proceed.

* All Shiny modules now **reset outputs automatically on input change**,
  preventing stale results from being displayed alongside new inputs.
  Each module uses `observeEvent(list(...))` watching all its relevant inputs
  to reset result reactives immediately. The active prior sidebar indicator
  is preserved during elicitation input changes (only the density plot and
  parameter table reset; `shared$current_prior` is retained until a new prior
  is explicitly fitted).

## Bug fixes

* Fixed `prior_report()` producing blank figures in Word (.docx) reports.
  knitr was writing absolute figure paths to the markdown; pandoc could not
  resolve them. Fixed by using `fig.path = "figures/"` (relative) with
  `base.dir = tmp_dir` for docx/pdf, and `knitr::image_uri` for HTML.

* Fixed `prior_report()` failing for PDF with "tikzfill.image.sty not found".
  Added `tinytex::tlmgr_install()` recommendation in documentation. The
  `default-image-extension: pdf` Quarto default is now overridden to `png`
  via YAML injection in the pre-executed markdown.

* Fixed mixture prior hyperparameters section in `prior_report.qmd` rendering
  as literal markdown (`## Hyperparameters not available`) rather than as
  formatted text. Added `#| results: asis` to the `fitted-params` chunk and
  updated to show a component summary table for mixture priors.

* Fixed `mod_conflict_mahal.R` containing a stale duplicate definition of
  `mod_conflict_ui` that was overriding the correct 4-choice version in
  `mod_conflict.R` (files sourced alphabetically, mahal comes after conflict).
  Removed the duplicate; `mod_conflict_mahal.R` now only contains
  `mod_mahal_ui` and `mod_mahal_server`.

## Documentation

* Updated `bayprior-package.R` to document all six distribution families,
  all four data types for conflict diagnostics and sensitivity, and the new
  validation functions.
* Updated all six vignettes to cover Exponential and Weibull elicitation,
  Poisson and survival conflict diagnostics, independent sensitivity data
  entry, and the output reset behaviour.
* Updated README with new badges, six-family distribution table, four-data-type
  conflict table, and updated Quick Start examples.

---

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
