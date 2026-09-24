test_that("prior_conflict() computes s_value correctly as -log2(box_pvalue)", {
  prior <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments",
                       label = "Response rate")
  interim <- list(type = "binary", x = 18, n = 40)
  cd <- prior_conflict(prior, interim)

  expect_true(is.numeric(cd$s_value))
  expect_length(cd$s_value, 1L)
  expect_equal(cd$s_value, -log2(cd$box_pvalue), tolerance = 1e-10)
})

test_that("prior_conflict()'s s_value is finite even for extreme conflict (box_pvalue underflowing to 0)", {
  # A prior wildly inconsistent with the data should drive box_pvalue to
  # (numerically) zero; s_value must stay finite rather than becoming Inf.
  prior <- elicit_beta(mean = 0.02, sd = 0.005, method = "moments",
                       label = "Response rate (implausibly low)")
  interim <- list(type = "binary", x = 38, n = 40)  # observed rate = 95%
  cd <- prior_conflict(prior, interim)

  expect_true(cd$box_pvalue < 1e-10)
  expect_true(is.finite(cd$s_value))
  expect_gt(cd$s_value, 0)
})

test_that("prior_conflict()'s s_value decreases monotonically as box_pvalue increases", {
  # Sanity check on transform direction: more surprising data (lower
  # box_pvalue) should always correspond to a higher (not lower) s_value.
  prior <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments",
                       label = "Response rate")
  cd_close  <- prior_conflict(prior, list(type = "binary", x = 15, n = 40))
  cd_far    <- prior_conflict(prior, list(type = "binary", x = 35, n = 40))

  expect_true(cd_close$box_pvalue > cd_far$box_pvalue)
  expect_true(cd_close$s_value    < cd_far$s_value)
})

test_that("print.bayprior_conflict() renders the S-value line for a real conflict object", {
  prior <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments",
                       label = "Response rate")
  cd <- prior_conflict(prior, list(type = "binary", x = 18, n = 40))

  out <- withr::with_envvar(
    c(RSTUDIO = "", POSITRON = ""),  # force the non-cli/plain cat() path
    utils::capture.output(print(cd))
  )
  expect_true(any(grepl("S-value", out)))
  expect_true(any(grepl(sprintf("%.2f", round(cd$s_value, 2)), out)))
})

test_that("print.bayprior_conflict() skips the S-value line gracefully when absent (pre-existing objects)", {
  # Mirrors the real bug this was written to catch: hand-built or legacy
  # bayprior_conflict objects predating this field must not crash print().
  cd_legacy <- structure(
    list(
      prior               = list(label = "Legacy fixture"),
      box_pvalue          = 0.03,
      surprise_index      = 2.5,
      kl_prior_likelihood = 1.1,
      overlap             = 0.4,
      conflict_severity   = "mild",
      recommendation      = "Mild conflict (legacy fixture, no s_value)."
      # deliberately no s_value field
    ),
    class = "bayprior_conflict"
  )

  expect_error(print(cd_legacy), NA)

  out <- withr::with_envvar(
    c(RSTUDIO = "", POSITRON = ""),
    utils::capture.output(print(cd_legacy))
  )
  expect_false(any(grepl("S-value", out)))
})