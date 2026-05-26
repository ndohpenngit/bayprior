## R CMD check results

0 errors | 0 warnings | 1 note

* NOTE: 'unable to verify current time'
  Transient macOS clock issue. Not package-related.

## Resubmission (v0.2.11)

Fix from v0.2.10 review:

  cran-comments.md was incorrectly included in the tarball. The
  .Rbuildignore pattern was malformed (^^cran-comments\\.md$$) and
  never matched the file. Corrected in current v0.2.11.

## Test environments

* macOS 26.5 aarch64, R 4.4.2 (local)

## Downstream dependencies

New submission. No downstream dependencies.