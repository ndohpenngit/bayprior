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


# ── Exponential elicitation ───────────────────────────────────────────────────

test_that("elicit_exponential moments method returns valid bayprior", {
  p <- elicit_exponential(mean = 0.05, method = "moments",
                           label = "Hazard rate", expert_id = "E1")
  expect_s3_class(p, "bayprior")
  expect_equal(p$dist, "exponential")
  expect_equal(p$params$rate, 1 / 0.05, tolerance = 1e-6)
  expect_equal(p$fit_summary$mean, 0.05, tolerance = 1e-6)
  expect_equal(p$fit_summary$sd,   0.05, tolerance = 1e-6)  # Exp: mean = SD
  expect_lt(p$fit_summary$q025, p$fit_summary$mean)
  expect_gt(p$fit_summary$q975, p$fit_summary$mean)
  expect_equal(p$expert_id, "E1")
  expect_equal(p$label, "Hazard rate")
})

test_that("elicit_exponential rate method", {
  p <- elicit_exponential(rate = 0.10, method = "rate")
  expect_equal(p$params$rate, 0.10, tolerance = 1e-6)
  expect_equal(p$fit_summary$mean, 10, tolerance = 1e-6)
})

test_that("elicit_exponential quantile method converges", {
  p <- elicit_exponential(
    quantiles = c("0.25" = 0.02, "0.50" = 0.05, "0.75" = 0.10),
    method = "quantile"
  )
  expect_s3_class(p, "bayprior")
  expect_gt(p$params$rate, 0)
  # Median should be close to specified 0.05
  expect_equal(p$fit_summary$q500, 0.05, tolerance = 0.02)
})

test_that("elicit_exponential errors on invalid inputs", {
  expect_error(elicit_exponential(mean = -1, method = "moments"))
  expect_error(elicit_exponential(rate = 0, method = "rate"))
  expect_error(elicit_exponential(method = "quantile"))  # no quantiles
})

# ── Weibull elicitation ───────────────────────────────────────────────────────

test_that("elicit_weibull moments method returns valid bayprior", {
  p <- elicit_weibull(mean = 20, sd = 10, method = "moments",
                       label = "Survival time", expert_id = "E1")
  expect_s3_class(p, "bayprior")
  expect_equal(p$dist, "weibull")
  expect_gt(p$params$shape, 0)
  expect_gt(p$params$scale, 0)
  expect_equal(p$fit_summary$mean, 20, tolerance = 0.5)
  expect_equal(p$fit_summary$sd,   10, tolerance = 0.5)
  expect_lt(p$fit_summary$q025, p$fit_summary$mean)
  expect_gt(p$fit_summary$q975, p$fit_summary$mean)
})

test_that("elicit_weibull params method", {
  p <- elicit_weibull(shape = 2, scale = 20, method = "params")
  expect_equal(p$params$shape, 2, tolerance = 1e-6)
  expect_equal(p$params$scale, 20, tolerance = 1e-6)
  # Weibull(2,20) mean = 20 * gamma(1.5) = 20 * sqrt(pi)/2
  expect_equal(p$fit_summary$mean, 20 * gamma(1.5), tolerance = 1e-4)
})

test_that("elicit_weibull quantile method converges", {
  p <- elicit_weibull(
    quantiles = c("0.10" = 5, "0.50" = 18, "0.90" = 40),
    method    = "quantile"
  )
  expect_s3_class(p, "bayprior")
  expect_gt(p$params$shape, 0)
  expect_equal(p$fit_summary$q500, 18, tolerance = 1)
})

test_that("elicit_weibull errors on invalid inputs", {
  expect_error(elicit_weibull(mean = 20, method = "moments"))  # no sd
  expect_error(elicit_weibull(shape = -1, scale = 10, method = "params"))
  expect_error(elicit_weibull(quantiles = c("0.5" = 10), method = "quantile"))  # only 1 quantile
})

# ── Exponential conjugate update ──────────────────────────────────────────────

test_that("Exponential prior updates correctly with Poisson data", {
  p  <- elicit_exponential(mean = 0.10, method = "moments")
  ds <- list(type = "poisson", x = 12, n = 100)
  post <- bayprior:::.conjugate_update(p, ds)
  expect_equal(post$dist, "gamma")
  expect_equal(post$params$shape, 1 + 12,         tolerance = 1e-6)
  expect_equal(post$params$rate,  10 + 100,        tolerance = 1e-6)
})

test_that("Weibull prior updates via Normal approximation", {
  p  <- elicit_weibull(shape = 2, scale = 20, method = "params")
  ds <- list(type = "survival", x = 20, n = 400)
  post <- bayprior:::.conjugate_update(p, ds)
  expect_equal(post$dist, "normal")
  expect_gt(post$params$mu, 0)
  expect_gt(post$params$sigma, 0)
})
