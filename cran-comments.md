## R CMD check results

0 errors | 0 warnings | 2 notes

* NOTE: 'unable to verify current time'
  Transient macOS clock issue. Not package-related.

* NOTE: 'Non-standard file/directory: cran-comments.md'
  Intentionally included per CRAN submission guidelines.
  Excluded from the tarball via .Rbuildignore.

## Resubmission (v0.2.10)

Fixes from v0.2.9 pre-check:
  1. Replaced 'Kullback-Leibler divergence' with 'information divergence'
     in DESCRIPTION. Kullback and Leibler are proper nouns not resolvable
     via inst/WORDLIST for CRAN's DESCRIPTION spell check.
  2. Changed Language from en-US to en-GB. Package consistently uses
     British English throughout.
  3. Updated inst/WORDLIST: added HR, Inf, Var, heatmaps, prior's, R's,
     VignetteBuilder, Poisson. British spellings removed -- now covered
     by en-GB dictionary. spelling::spell_check_package() returns zero errors.

## Test environments

* macOS 26.3 aarch64, R 4.4.2 (local)
* devtools::check(cran = TRUE): 0 errors | 0 warnings | 2 notes
* Tarball verified: build/vignette.rds present (R CMD build from Terminal)

## Downstream dependencies

New submission. No downstream dependencies.
