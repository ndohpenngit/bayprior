test_that("prior_conflict returns correct structure (binary, no conflict)", {
  prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  cd    <- prior_conflict(prior, list(type = "binary", x = 12, n = 40))

  expect_s3_class(cd, "bayprior_conflict")
  expect_true(is.numeric(cd$box_pvalue))
  expect_true(is.numeric(cd$surprise_index))
  expect_true(is.numeric(cd$kl_prior_likelihood))
  expect_true(is.numeric(cd$overlap))
  expect_true(is.logical(cd$conflict_flag))
  expect_true(cd$conflict_severity %in% c("none", "mild", "severe"))
  expect_true(nzchar(cd$recommendation))

  # p-value in [0, 1]
  expect_gte(cd$box_pvalue, 0)
  expect_lte(cd$box_pvalue, 1)

  # Overlap in [0, 1]
  expect_gte(cd$overlap, 0)
  expect_lte(cd$overlap, 1)

  # No conflict: x/n = 0.30 matches prior mean
  expect_false(cd$conflict_flag)
  expect_equal(cd$conflict_severity, "none")
})

test_that("prior_conflict detects severe conflict", {
  prior <- elicit_beta(mean = 0.30, sd = 0.05, method = "moments")
  # Observe 38/40 = 95% — far from prior mean of 30%
  cd <- prior_conflict(prior, list(type = "binary", x = 38, n = 40))

  expect_true(cd$conflict_flag)
  expect_equal(cd$conflict_severity, "severe")
  expect_lt(cd$box_pvalue, 0.01)
})

test_that("prior_conflict handles continuous data", {
  prior <- elicit_normal(mean = 0.0, sd = 0.3, method = "moments")
  cd    <- prior_conflict(
    prior,
    list(type = "continuous", x = 0.0, sd = 0.2, n = 50)
  )
  expect_s3_class(cd, "bayprior_conflict")
  expect_false(cd$conflict_flag)
})

test_that("prior_conflict errors on non-bayprior input", {
  expect_error(
    prior_conflict(list(a = 1), list(type = "binary", x = 10, n = 40))
  )
})

test_that("prior_conflict custom alpha changes flag threshold", {
  prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  cd_05 <- prior_conflict(prior, list(type="binary", x=20, n=40), alpha=0.05)
  cd_20 <- prior_conflict(prior, list(type="binary", x=20, n=40), alpha=0.20)

  # With a more lenient alpha, flag may be TRUE more easily
  expect_gte(as.integer(cd_20$conflict_flag), as.integer(cd_05$conflict_flag))
})

test_that("print.bayprior_conflict runs without error", {
  prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  cd    <- prior_conflict(prior, list(type = "binary", x = 12, n = 40))
  # print should not error — cli output goes to stderr so we just check no error
  expect_error(print(cd), NA)
})

test_that("plot_prior_likelihood returns a ggplot", {
  prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  gp <- plot_prior_likelihood(
    prior,
    data_summary   = list(type = "binary", x = 12, n = 40),
    show_posterior = TRUE
  )
  expect_s3_class(gp, "gg")
})

test_that("conflict_mahalanobis returns correct structure", {
  mv <- conflict_mahalanobis(
    prior_means = c(0.35, 0.60),
    prior_cov   = matrix(c(0.010, 0.003, 0.003, 0.015), 2, 2),
    obs_means   = c(0.55, 0.58),
    obs_cov     = matrix(c(0.008, 0.002, 0.002, 0.010), 2, 2) / 50,
    labels      = c("ORR", "OS"),
    alpha       = 0.05
  )

  expect_true(is.list(mv))
  expect_true(is.numeric(mv$mahal_distance))
  expect_true(is.numeric(mv$pvalue))
  expect_true(is.logical(mv$conflict_flag))
  expect_length(mv$marginal_z, 2)
  expect_equal(mv$labels, c("ORR", "OS"))

  # Distance is non-negative
  expect_gte(mv$mahal_distance, 0)
  # p-value in [0, 1]
  expect_gte(mv$pvalue, 0)
  expect_lte(mv$pvalue, 1)
})

test_that("conflict_mahalanobis detects conflict when means are far apart", {
  mv <- conflict_mahalanobis(
    prior_means = c(0.20, 0.20),
    prior_cov   = matrix(c(0.001, 0, 0, 0.001), 2, 2),
    obs_means   = c(0.80, 0.80),
    obs_cov     = matrix(c(0.001, 0, 0, 0.001), 2, 2),
    labels      = c("ep1", "ep2")
  )
  expect_true(mv$conflict_flag)
})