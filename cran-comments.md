## Update to bayprior 0.3.2

This is an update of bayprior from 0.3.2 to 0.4.0. See NEWS.md for details.
The main changes are:

* `prior_conflict()` gains an `exact` argument (default `FALSE`, so existing
  behaviour is unchanged). `exact = TRUE` uses the exact prior predictive
  distribution (Beta-Binomial, Gamma-Poisson/Negative Binomial) for the Box
  p-value where one has a closed form, and falls back to the Normal
  approximation with a message otherwise.

* Added `map_prior()`: derives a meta-analytic-predictive (MAP) prior from
  historical trial summaries via a random-effects meta-analysis, with an
  explicit prior on the between-trial heterogeneity parameter tau
  (Schmidli et al., 2014). The meta-analysis engine is implemented in base
  R; no new hard dependency is introduced.

* Added `historical_effect_sizes()`, an optional wrapper around
  `metafor::escalc()`. `metafor` is listed under `Suggests` only and is
  guarded by `requireNamespace(..., quietly = TRUE)`.

* Added `resolve_tau_prior()` and `plot_tau_posterior()` in support of the
  above.

* `prior_conflict()` now also reports `s_value`, the surprisal
  `-log2(box_pvalue)` in bits, alongside the Box p-value.

* Shiny app: new "MAP Prior (Historical)" module, a restyled interface, a
  live workflow overview on the Welcome page, and a confirmation step
  before a fitted prior replaces a pooled consensus prior.

## R CMD check results

TODO: paste the final result lines from the last `devtools::check()` /
win-builder / R-hub runs on the exact tarball being submitted, e.g.
"0 errors | 0 warnings | 0 notes". Do not submit with this line unedited.

## Test environments

TODO: list the environments actually used for this release (local OS and R
version, GitHub Actions matrix, win-builder devel/release, R-hub).

## Downstream dependencies

There are no downstream dependencies.