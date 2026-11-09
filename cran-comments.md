## Update to bayprior 0.6.0

This updates bayprior from 0.5.0. It is a release of statistical corrections
and it changes default numerical results for some functions (see NEWS.md,
"Breaking changes" section). In summary:

* Marginal likelihoods and quantiles of mixture priors, and the logarithmic
  pool, are now computed exactly instead of by approximation.
* Power-prior calibration (delta) and the Box prior predictive p-value for
  robust priors were corrected; the Normal approximation now adjusts the
  standard error at the boundary only.
* The MAP predictive prior and the sensitivity influence scale were corrected
  (the influence scale is now unit-free).
* The sensitivity heatmap no longer draws as thin stripes when the
  prior-strength grid is unevenly spaced.
* The Shiny app, the Quarto report template and the vignettes were updated to
  match.

The corrections were checked against the simulation study in `validation/`
(not part of the package build). No new dependencies.

## R CMD check results

0 errors | 0 warnings | 1 note

* checking for future file timestamps ... NOTE: unable to verify current
  time (local environment).

## Test environments

* local macOS (aarch64-apple-darwin20), R 4.4.2
* win-builder: R 4.6.1 (2026-06-24 ucrt) and R-devel (2026-10-18 r90707
  ucrt): Status OK for both

## Downstream dependencies

There are no downstream dependencies.