## Resubmission

This is a resubmission. bayprior was first accepted on CRAN as v0.2.12.
This submission (v0.3.0) contains the following changes since v0.2.12:

* Fixed `print.bayprior*` methods producing no output in non-interactive
  R sessions (e.g. inside `rmarkdown::render()`, `callr::r()`, knitr
  vignettes).

* Expanded test suite coverage (excluding Shiny
  modules).

## R CMD check results

0 errors | 0 warnings | 1 notes

* checking for future file timestamps -- unable to verify current time.
   Not package-related.

## Test environments

* macOS aarch64, R 4.4.2 (local)
* Windows R-devel via devtools::check_win_devel()
* ubuntu-latest (release, devel, oldrel) via GitHub Actions

## Downstream dependencies

There are no downstream dependencies.