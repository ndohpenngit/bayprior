## R CMD check results

0 errors | 0 warnings | 2 notes

* NOTE: 'unable to verify current time'
  Transient macOS network/clock issue. Not package-related.

* NOTE: 'Non-standard file/directory: cran-comments.md'
  Intentionally included per CRAN submission guidelines.

## Resubmission (v0.2.5)

Fixes from v0.2.4 pre-test rejection:
  1. Removed all Unicode characters from R source files (U+2014 em dash,
     U+2019 curly quote, U+221E infinity, U+2212 minus, and others).
     Replaced with ASCII equivalents (-- - ' Inf).
  2. Added inst/WORDLIST for EMA, FDA, and other acronyms.
  3. Rebuilt vignette index via devtools::build_vignettes().

## Test environments

* macOS 26.3 aarch64, R 4.4.2 (local)
* devtools::check(cran = TRUE)
* devtools::check_win_devel() (Windows R-devel)
