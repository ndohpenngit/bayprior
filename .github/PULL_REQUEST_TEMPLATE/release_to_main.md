## Release checklist

**All CI checks must be green before merging to main.**

### Automated (must pass in CI before merge)

- [ ] R CMD check -- 0 errors, 0 warnings on Ubuntu, Windows, macOS
      (`R-CMD-check.yaml`)
- [ ] Non-ASCII check -- no Unicode characters in any R source file
      (`R-CMD-check.yaml`)
- [ ] Spelling -- no errors against `inst/WORDLIST`
      (`R-CMD-check.yaml`)
- [ ] Test coverage >= 70% (Shiny modules excluded)
      (`R-CMD-check.yaml`)
- [ ] CRAN tarball contains `build/vignette.rds` (prebuilt vignette index)
      (`R-CMD-check.yaml`)
- [ ] Tarball size < 5 MB
      (`R-CMD-check.yaml`)

### Manual (confirm before opening PR)

- [ ] `DESCRIPTION` version bumped (`usethis::use_version()`)
- [ ] `NEWS.md` updated -- new version heading with all changes
- [ ] No spell-check traps in DESCRIPTION:
      - No regulatory body abbreviations (write "European Medicines Agency"
        not "EMA")
      - All statistical proper nouns spelled out in full AND added to
        inst/WORDLIST (e.g. "Kullback-Leibler" requires both "Kullback" and
        "Leibler" in WORDLIST -- they are proper nouns, not misspellings)
      - Run `spelling::spell_check_package()` locally after any DESCRIPTION
        change to catch flags before CI does
- [ ] No Unicode characters (`--` not em dash, `Inf` not infinity symbol,
      straight quotes not curly quotes)
- [ ] `inst/WORDLIST` updated for any new terms via
      `spelling::update_wordlist()`. Check especially:
      - Author surnames in references (O'Hagan, Kullback, Leibler, etc.)
      - Statistical method names (Mahalanobis, Bhattacharyya, etc.)
      - Package/tool names (golem, shinydashboard, etc.)
      - Acronyms used inside R source or Rd files (not DESCRIPTION)
- [ ] `^build$` is NOT in `.Rbuildignore` -- this excludes
      `build/vignette.rds` which is the prebuilt vignette index
      CRAN checks for. Removing it causes the persistent NOTE:
      "Package has a VignetteBuilder field but no prebuilt vignette index"
- [ ] `cran-comments.md` updated to reflect current check results

### CRAN submission (after PR is merged to main)

### CRAN submission (after PR is merged to main)

- [ ] Build the submission tarball from Terminal (NOT devtools):
```bash
      R CMD build bayprior
```
- [ ] Verify tarball before uploading:
      `tar -tzf bayprior_x.x.x.tar.gz | grep "build/vignette"`
      Must show: `bayprior/build/vignette.rds`
      `tar -tzf bayprior_x.x.x.tar.gz | grep cran-comments`
      Must return NOTHING
- [ ] Upload at https://cran.r-project.org/submit.html
      **Do NOT use `devtools::submit_cran()` -- it rebuilds the tarball
      and loses the correct build environment**
- [ ] Click the confirmation link in the email within 1 hour
- [ ] Commit and push `CRAN-SUBMISSION` file:
      `git add CRAN-SUBMISSION && git commit -m "chore: submit vX.X.X to CRAN"`

### After CRAN acceptance

- [ ] `git tag -a vX.X.X -m "bayprior X.X.X -- CRAN accepted"`
- [ ] `git push origin vX.X.X`
- [ ] `gh release create vX.X.X --title "bayprior X.X.X" --latest`
- [ ] Add/update CRAN badge in README.md
- [ ] Deploy updated shinyapps.io:
      `rsconnect::deployApp(appDir = "~/Desktop/shinyapps/bayprior", envManagementR = FALSE)`

---

## What changed
<!-- Brief description of changes in this PR -->

## Related issues
<!-- Closes #XX -->

## Type of change
- [ ] Bug fix
- [ ] New feature
- [ ] Documentation update
- [ ] CRAN submission preparation