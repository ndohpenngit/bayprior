## R CMD check results

0 errors | 0 warnings | 1 note

* NOTE: 'unable to verify current time'
  Transient macOS clock issue. Not package-related.

## Resubmission (v0.2.12)

Fixes from v0.2.11 human reviewer (CRAN team, 27 May 2026):

1. Wrapped software name in single quotes in DESCRIPTION: 'Shiny'.

2. Added methodology references to DESCRIPTION in CRAN-required
   format (authors, year, doi/ISBN)

3. Added \value documentation tag to four exported print methods:
   print.bayprior(), print.bayprior_conflict(),
   print.bayprior_conflict_mv(), and print.bayprior_power_prior().

4. Restored par() settings after use in the robust-priors vignette.

5. Removed LICENSE file and | file LICENSE from DESCRIPTION.

6. Updated inst/WORDLIST.

## Test environments

* macOS 26.5 aarch64, R 4.4.2 (local)
* devtools::check(cran = TRUE): 0 errors | 0 warnings | 1 note

## Downstream dependencies

New submission. No downstream dependencies.