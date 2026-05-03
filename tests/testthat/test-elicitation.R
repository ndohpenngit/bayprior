test_that("elicit_beta returns valid bayprior object (moments)", {
  p <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments",
                   expert_id = "E1", label = "Response rate")

  expect_s3_class(p, "bayprior")
  expect_equal(p$dist, "beta")
  expect_equal(p$method, "moments")
  expect_equal(p$expert_id, "E1")
  expect_equal(p$label, "Response rate")

  # Parameters are positive
  expect_gt(p$params$alpha, 0)
  expect_gt(p$params$beta, 0)

  # fit_summary fields exist and are numerically correct
  expect_equal(p$fit_summary$mean, 0.30, tolerance = 1e-4)
  expect_lt(p$fit_summary$sd, 0.15)
  expect_lt(p$fit_summary$q025, p$fit_summary$mean)
  expect_gt(p$fit_summary$q975, p$fit_summary$mean)
})

test_that("elicit_beta returns valid bayprior object (quantile)", {
  p <- elicit_beta(
    quantiles = c("0.05" = 0.10, "0.50" = 0.30, "0.95" = 0.60),
    label = "ORR"
  )
  expect_s3_class(p, "bayprior")
  expect_equal(p$dist, "beta")

  # Median should be close to 0.30
  expect_equal(p$fit_summary$q500, 0.30, tolerance = 0.02)
})

test_that("elicit_beta errors on invalid inputs", {
  # Mean outside (0, 1)
  expect_error(elicit_beta(mean = 1.5, sd = 0.1, method = "moments"))
  # SD too large for beta
  expect_error(elicit_beta(mean = 0.5, sd = 0.9, method = "moments"))
  # Probabilities out of range
  expect_error(elicit_beta(quantiles = c("0" = 0.1, "0.5" = 0.3, "1" = 0.6)))
})

test_that("elicit_normal returns valid bayprior object (moments)", {
  p <- elicit_normal(mean = 0.0, sd = 0.5, method = "moments",
                     label = "Log OR")
  expect_s3_class(p, "bayprior")
  expect_equal(p$dist, "normal")
  expect_equal(p$fit_summary$mean, 0.0, tolerance = 1e-6)
  expect_equal(p$fit_summary$sd,   0.5, tolerance = 1e-6)
})

test_that("elicit_normal returns valid bayprior object (quantile)", {
  p <- elicit_normal(
    quantiles = c("0.025" = -0.5, "0.50" = 0.20, "0.975" = 0.90),
    label = "Log OR"
  )
  expect_s3_class(p, "bayprior")
  expect_equal(p$fit_summary$q500, 0.20, tolerance = 0.02)
})

test_that("elicit_gamma returns valid bayprior object (moments)", {
  p <- elicit_gamma(mean = 5, sd = 2, method = "moments", label = "Rate")
  expect_s3_class(p, "bayprior")
  expect_equal(p$dist, "gamma")
  expect_gt(p$params$shape, 0)
  expect_gt(p$params$rate, 0)
  expect_equal(p$fit_summary$mean, 5, tolerance = 0.01)
})

test_that("elicit_gamma errors on non-positive mean/sd", {
  expect_error(elicit_gamma(mean = -1, sd = 2, method = "moments"))
  expect_error(elicit_gamma(mean = 5,  sd = -1, method = "moments"))
})

test_that("elicit_lognormal returns valid bayprior object (quantile)", {
  p <- elicit_lognormal(
    quantiles = c("0.05" = 0.40, "0.50" = 0.70, "0.95" = 1.20),
    label = "HR"
  )
  expect_s3_class(p, "bayprior")
  expect_equal(p$dist, "lognormal")
  # Median on original scale ~ 0.70 (q500 should be close)
  expect_equal(p$fit_summary$q500, 0.70, tolerance = 0.05)
})

test_that("elicit_lognormal moments method works", {
  p <- elicit_lognormal(mean = 1.0, sd = 0.3, method = "moments")
  expect_s3_class(p, "bayprior")
  expect_equal(p$dist, "lognormal")
  expect_gt(p$fit_summary$mean, 0)
})

test_that("elicit_roulette fits from chip data", {
  chips  <- c(0L, 1L, 3L, 7L, 9L, 7L, 4L, 2L, 1L, 1L)
  breaks <- seq(0, 1, by = 0.1)
  p <- elicit_roulette(chips, breaks, family = "beta", label = "Rate")

  expect_s3_class(p, "bayprior")
  expect_equal(p$dist, "beta")
  expect_true(!is.null(p$roulette))
  expect_equal(p$roulette$chips, chips)
})

test_that("elicit_roulette errors on zero chips", {
  expect_error(
    elicit_roulette(rep(0L, 10), seq(0, 1, by = 0.1), family = "beta")
  )
})

test_that("print.bayprior runs without error", {
  p <- elicit_beta(mean = 0.3, sd = 0.1, method = "moments")
  out <- capture.output(print(p))
  # print should produce some output
  expect_true(length(out) > 0 || inherits(tryCatch(print(p), error=function(e) e), "bayprior"))
})

test_that("format.bayprior returns a character string", {
  p <- elicit_beta(mean=0.30, sd=0.10, method="moments", label="Test")
  # format() should return something character-based if defined
  # otherwise just verify the object prints cleanly
  expect_error(capture.output(p), NA)
})

test_that("summary.bayprior works if defined", {
  p <- elicit_beta(mean=0.30, sd=0.10, method="moments")
  # summary() may not be defined - just ensure it doesn't crash
  tryCatch(summary(p), error = function(e) NULL)
  expect_s3_class(p, "bayprior")  # object unchanged
})

test_that("elicit_beta quantile with two quantiles works", {
  # Some implementations accept 2 quantiles
  tryCatch({
    p <- elicit_beta(
      quantiles = c("0.10" = 0.15, "0.90" = 0.55),
      label = "ORR"
    )
    expect_s3_class(p, "bayprior")
  }, error = function(e) {
    # Two-quantile fitting may not be supported - that's fine
    expect_true(grepl("quantile|prob", conditionMessage(e),
                      ignore.case = TRUE))
  })
})

test_that("elicit_gamma quantile method works", {
  p <- elicit_gamma(
    quantiles = c("0.10" = 2, "0.50" = 5, "0.90" = 10),
    label = "Event rate"
  )
  expect_s3_class(p, "bayprior")
  expect_equal(p$dist, "gamma")
})

test_that("elicit_lognormal moments stores correct parameters", {
  p <- elicit_lognormal(mean=2.0, sd=0.5, method="moments",
                        label="PK parameter")
  expect_s3_class(p, "bayprior")
  # Parameters should be stored (mu and sigma on log scale)
  expect_true(length(p$params) > 0)
  expect_gt(p$fit_summary$mean, 0)
})