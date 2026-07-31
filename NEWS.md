# bayprior 0.3.0

## Bug fixes

* Fixed `print.bayprior()`, `print.bayprior_conflict()`,
  `print.bayprior_conflict_mv()`, and `print.bayprior_power_prior()`
  producing no output when called inside `rmarkdown::render()`, knitr
  vignettes, or any non-interactive R subprocess (e.g. `callr::r()`).
  Output now renders correctly in both interactive and non-interactive
  sessions (#40).

---

# bayprior 0.2.12

## CRAN reviewer fixes (v0.2.11 review)

* Wrapped software name in single quotes in DESCRIPTION: 'Shiny', per CRAN
  policy on package and software name formatting.

* Added six methodology references to DESCRIPTION in CRAN-required format
  (authors, year, doi/ISBN): O'Hagan et al. (2006), Box (1980), Oakley and
  O'Hagan (2010), Schmidli et al. (2014), Ibrahim and Chen (2000),
  Spiegelhalter et al. (1994).

* Added `\value` documentation tag to four exported print methods:
  `print.bayprior()`, `print.bayprior_conflict()`,
  `print.bayprior_conflict_mv()`, and `print.bayprior_power_prior()`.
  Each documents that the function returns the input object invisibly,
  called for its side effect of printing a formatted summary.

* Restored `par()` settings after use in the robust-priors vignette.
  The `par(oldpar)` call was incorrectly placed before the `for` loop
  and `legend()`. Moved to after all plotting operations, per CRAN policy.

* Removed `LICENSE` file and `| file LICENSE` from DESCRIPTION. The GPL-3
  licence does not require an additional file.

* Updated `inst/WORDLIST`: added `biom`, `doi`, and `ss` -- fragments
  extracted by the spell checker from DOI strings in the DESCRIPTION
  references field.

---

# bayprior 0.2.11

## CRAN resubmission fix

* Corrected malformed `.Rbuildignore` pattern for `cran-comments.md`.
  The pattern `^^cran-comments\\.md$$` (double caret, double dollar)
  never matched the file, so it was included in every previous tarball.
  Fixed to the correct regex `^cran-comments\.md$` and verified absent
  from the v0.2.11 tarball.

---

# bayprior 0.2.10

## CRAN resubmission fixes

* Switched `Language` from `en-US` to `en-GB` in DESCRIPTION to correctly
  reflect the British English used throughout the package (sceptical, colour,
  behaviour, normalised etc.). British spellings are now handled by the en-GB
  dictionary and no longer need to be in `inst/WORDLIST`.

* Replaced "Kullback-Leibler divergence" in DESCRIPTION with "information
  divergence". Kullback and Leibler are proper nouns not in any standard
  dictionary and cannot be resolved via `inst/WORDLIST` for CRAN's DESCRIPTION
  spell check.

* Updated `inst/WORDLIST`: added HR, Inf, Var, heatmaps, prior's, R's,
  VignetteBuilder, Poisson, etc. Removed British spellings now covered by
  en-GB dictionary.

---

# bayprior 0.2.9

## CRAN resubmission fix

* Added 'Kullback' and 'Leibler' to `inst/WORDLIST`. These are proper nouns
  (the Kullback-Leibler divergence is named after statisticians Solomon
  Kullback and Richard Leibler) and are standard terminology in Bayesian
  statistics. Flagged as possibly misspelled by CRAN's automated pre-check
  in v0.2.8.

---

# bayprior 0.2.8

## CRAN resubmission fixes

* Resolved the persistent "Package has a VignetteBuilder field but no
  prebuilt vignette index" NOTE that appeared across v0.2.4--v0.2.7.
  Root cause: `^build$` was incorrectly added to `.Rbuildignore`, which
  excluded `build/vignette.rds` from the tarball. This file is generated
  by `R CMD build` during vignette processing and is the exact file
  `R CMD check` looks for (tools/R/QC.R). Removing `^build$` from
  `.Rbuildignore` resolves the note permanently.

* Revised `Description` field in DESCRIPTION to remove all regulatory body
  references and abbreviations. The description now describes what the
  software does. Regulatory context is retained in vignettes and README.

---

# bayprior 0.2.7

## CRAN resubmission fixes

* Resolve `EMA` in DESCRIPTION. The `EMA` acronym (European Medicines Agency)
  was flagged as a possible misspelling in the DESCRIPTION file, causing a
  CRAN resubmission WARNING.

* Updated `inst/WORDLIST`.

---

# bayprior 0.2.6

## CRAN resubmission fixes

* Updated `inst/WORDLIST` to include all package-specific terms flagged by
  the spelling checker, and package infrastructure terms (golem,
  shinydashboard, knitr, pandoc). Resolved `spelling.Rout` vs
  `spelling.Rout.save` mismatch in tests.

* Changed `Language` field in DESCRIPTION from `en-US` to `en-GB` to reflect
  the package's consistent use of British English spelling throughout
  documentation and vignettes (sceptical, colour, behaviour, centred).

* Added prebuilt vignette index (`inst/doc/index.html`) to resolve the
  "no prebuilt vignette index" NOTE reported by CRAN's automated pre-check
  on Windows and Debian.

---

# bayprior 0.2.5

## CRAN resubmission fixes

* Removed all Unicode characters from R source files that caused LaTeX PDF
  generation errors on CRAN's Windows and Debian build servers. Affected
  characters: U+2014 em dash (replaced with --), U+2212 Unicode minus
  (replaced with -), U+221E infinity (replaced with Inf), U+2019/U+2018
  curly quotes (replaced with '), and others. All replacements made in
  comments and roxygen documentation only -- no functional code changed.

* Added `inst/WORDLIST` declaring package-specific acronyms (EMA, FDA, CrI,
  MAP, Mahalanobis, Bhattacharyya, SHELF, PFS, PK) that were flagged as
  possibly misspelled in the DESCRIPTION file.

* Rebuilt prebuilt vignette index via `devtools::build_vignettes()` to
  resolve the "no prebuilt vignette index" NOTE.

---

# bayprior 0.2.4

## Documentation & UX

* `robust_prior()` -- added `@details` section explaining that the vague
  component is always Normal, making cross-family mixtures (e.g. Beta +
  Normal) structurally inevitable for non-Normal informative priors.

* `elicit_mixture()` -- added `@details` section documenting the numerical
  density approximation behaviour at the lower level where mixtures are first
  constructed.

* Robust Mixture density plot warning redirected from console to UI. The
  "Components have different distribution families. Mixture densities computed
  numerically." warning is now surfaced as an amber `showNotification()` in
  the Shiny app.

---

# bayprior 0.2.3

## New features

* Poisson data type in Power Prior Calibration.
* Prior-data compatibility warning in Power Prior module.
* Mahalanobis module limitations documented inline.

## Architecture

* Introduced `shared$base_prior` reactive to eliminate the self-invalidation
  loop that wiped sensitivity/robust/power prior results immediately after
  they were produced.

* Fixed robust mixture compounding SD bug -- repeated clicks no longer
  compound the vague SD exponentially.

## Tests

* Added `tests/testthat/test-robust.R` (33 tests).
* Added `tests/testthat/test-validation.R` (10 tests).
* Added `skip_on_ci()` to all `shinytest2` Chromote tests.

---

# bayprior 0.2.2

## UX improvements

* Three-state auto theme (Auto / Dark / Light) following OS preference.
* Disabled downstream buttons when no prior has been fitted.
* Prior summary card in sidebar showing Mean, SD, and 95% CrI.
* Diagnostic tooltips on conflict value boxes.

---

# bayprior 0.2.1

## UX improvements

* Welcome tab SVG workflow diagram.
* Sidebar completion indicators (green glowing dot per completed step).
* Export Report compliance checklist is fully reactive.
* Package version displayed in sidebar footer.

## Deployment fixes

* Removed `pkgload::load_all()` from `app.R`.
* Added `renv::snapshot(type = "explicit")` to deployment workflow.

---

# bayprior 0.2.0

## New features

* `elicit_exponential()` -- Exponential(rate) prior for hazard rates and
  Poisson rate priors. Supports moments, rate, and quantile methods.

* `elicit_weibull()` -- Weibull(shape, scale) prior for survival times.
  Supports moments, params, and quantile methods.

* `"poisson"` and `"survival"` data types in `prior_conflict()`,
  `sensitivity_grid()`, and `sensitivity_cri()`.

* Density and x-range support for Exponential and Weibull in `plot.bayprior()`.

* Comprehensive validation layer: `.check_prior_data_compat()`,
  `.check_pooling_compat()`, `.check_sensitivity_compat()`,
  `.validation_alert()`.

* Sensitivity analysis fully independent of conflict diagnostics.

* All Shiny modules reset outputs automatically on input change.

## Bug fixes

* Fixed `prior_report()` blank figures in Word reports.
* Fixed `prior_report()` PDF "tikzfill.image.sty not found" error.
* Fixed mixture prior hyperparameters section rendering as literal markdown.
* Fixed `mod_conflict_mahal.R` stale duplicate definition of `mod_conflict_ui`.

---

# bayprior 0.1.2

## Bug fixes

* Fixed `prior_report()` failing on remote platforms due to Quarto spawning
  a subprocess that could not find the `bayprior` package.

* Fixed `mod_sensitivity` always using `type = "continuous"` regardless of
  prior family.

* Fixed non-ASCII characters in `R/app_ui.R` causing R CMD check WARNING.

* Fixed `prior_report()` missing `@param` documentation for plot arguments.

## New features

* Added robust, sceptical, and power prior arguments to `prior_report()`.

* Added bayprior hex logo to Shiny app header and browser favicon.

---

# bayprior 0.1.1

## Initial release

* Prior elicitation: `elicit_beta()`, `elicit_normal()`, `elicit_gamma()`,
  `elicit_lognormal()`, `elicit_roulette()`, `elicit_mixture()`.
* Expert pooling: `aggregate_experts()` with Bhattacharyya diagnostics.
* Conflict diagnostics: `prior_conflict()`, `conflict_mahalanobis()`.
* Sensitivity analysis: `sensitivity_grid()`, `sensitivity_cri()`.
* Robust priors: `robust_prior()`, `sceptical_prior()`,
  `calibrate_power_prior()`.
* Reporting: `prior_report()` (HTML, PDF, Word).
* Shiny application: `run_app()`.
* GitHub Pages documentation site.