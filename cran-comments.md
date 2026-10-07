## Resubmission

This is a resubmission. This submission (v0.4.0) contains the following changes:

* Added `map_prior()`: derives a meta-analytic-predictive (MAP) prior from
  historical trial summaries via a random-effects meta-analysis, with an
  explicit prior on the between-trial heterogeneity parameter tau
  (Schmidli et al., 2014). The meta-analysis engine is implemented
  entirely in base R -- no new hard dependency is introduced for it.

* Added `historical_effect_sizes()`, an optional convenience wrapper
  around `metafor::escalc()` for deriving `map_prior()`'s inputs from raw
  per-trial summary statistics. `metafor` is listed under `Suggests`
  only, guarded by `requireNamespace(..., quietly = TRUE)`, and is not
  required to use `map_prior()` itself.

* Added `resolve_tau_prior()` and `plot_tau_posterior()` in support of
  the above.

* Added a Shiny module exposing `map_prior()` ("MAP Prior (Historical)")
  as its own top-level sidebar item.

* `prior_conflict()` now also reports `s_value`, the surprisal
  `-log2(box_pvalue)` in bits -- an exploratory
  companion to the existing Box p-value, reported alongside it (not
  replacing it) in the console output, the Shiny app, and the
  `prior_report()` regulatory report.

* See NEWS.md for full details.

## R CMD check results

0 errors | 0 warnings | 1 note

* checking for future file timestamps -- unable to verify current time.
  Not package-related.

## Test environments

* macOS aarch64, R 4.4.2 (local)
* ubuntu-latest (release, devel, oldrel) via GitHub Actions
* Windows R-devel via devtools::check_win_devel()

## Downstream dependencies

There are no downstream dependencies.