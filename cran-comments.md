## Update to bayprior 0.5.0

This updates bayprior from 0.4.0 (accepted 2026-10-10). It follows the previous
release closely because simulations showed that the default Box p-value in
0.4.0 can flag prior-data conflict far more often than its nominal level for
informative priors and small samples. `prior_conflict()` now uses the exact
prior predictive distribution by default, which changes default results
(see NEWS.md). `plot_sensitivity()` and `plot_tornado()` gain an
`interactive` argument (default unchanged), and the Shiny app is updated.
No new dependencies.

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