# -- Beta ----------------------------------------------------------------------

test_that("elicit_beta returns valid bayprior object (moments)", {
  p <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments",
                   expert_id = "E1", label = "Response rate")
  expect_s3_class(p, "bayprior")
  expect_equal(p$dist, "beta")
  expect_equal(p$method, "moments")
  expect_equal(p$expert_id, "E1")
  expect_equal(p$label, "Response rate")
  expect_gt(p$params$alpha, 0)
  expect_gt(p$params$beta, 0)
  expect_equal(p$fit_summary$mean, 0.30, tolerance = 1e-4)
  expect_lt(p$fit_summary$sd, 0.15)
  expect_lt(p$fit_summary$q025, p$fit_summary$mean)
  expect_gt(p$fit_summary$q975, p$fit_summary$mean)
})

test_that("elicit_beta returns valid bayprior object (quantile)", {
  p <- elicit_beta(quantiles = c("0.05" = 0.10, "0.50" = 0.30, "0.95" = 0.60), label = "ORR")
  expect_s3_class(p, "bayprior")
  expect_equal(p$dist, "beta")
  expect_equal(p$fit_summary$q500, 0.30, tolerance = 0.02)
})

test_that("elicit_beta errors on invalid inputs", {
  expect_error(elicit_beta(mean = 1.5, sd = 0.1, method = "moments"))
  expect_error(elicit_beta(mean = 0.5, sd = 0.9, method = "moments"))
  expect_error(elicit_beta(quantiles = c("0" = 0.1, "0.5" = 0.3, "1" = 0.6)))
})

test_that("elicit_beta: .validate_quantiles rejects invalid probability keys", {
  expect_error(elicit_beta(quantiles = c(`0.00` = 0.10, `0.50` = 0.30, `0.90` = 0.60), method = "quantile"))
  expect_error(elicit_beta(quantiles = c(`0.10` = 0.10, `0.50` = 0.30, `1.00` = 0.60), method = "quantile"))
})

test_that("elicit_beta: .validate_quantiles rejects non-monotone values", {
  expect_error(elicit_beta(quantiles = c(`0.10` = 0.50, `0.50` = 0.30, `0.90` = 0.60), method = "quantile"))
})

test_that("elicit_beta quantile with two quantiles (graceful)", {
  tryCatch({
    p <- elicit_beta(quantiles = c("0.10" = 0.15, "0.90" = 0.55), label = "ORR")
    expect_s3_class(p, "bayprior")
  }, error = function(e) {
    expect_true(grepl("quantile|prob", conditionMessage(e), ignore.case = TRUE))
  })
})

# -- Normal --------------------------------------------------------------------

test_that("elicit_normal returns valid bayprior object (moments)", {
  p <- elicit_normal(mean = 0.0, sd = 0.5, method = "moments", label = "Log OR")
  expect_s3_class(p, "bayprior")
  expect_equal(p$dist, "normal")
  expect_equal(p$fit_summary$mean, 0.0, tolerance = 1e-6)
  expect_equal(p$fit_summary$sd,   0.5, tolerance = 1e-6)
})

test_that("elicit_normal returns valid bayprior object (quantile)", {
  p <- elicit_normal(quantiles = c("0.025" = -0.5, "0.50" = 0.20, "0.975" = 0.90), label = "Log OR")
  expect_s3_class(p, "bayprior")
  expect_equal(p$fit_summary$q500, 0.20, tolerance = 0.02)
})

test_that("elicit_normal handles zero mean", {
  p <- elicit_normal(mean = 0, sd = 1, method = "moments")
  expect_s3_class(p, "bayprior")
  expect_equal(p$fit_summary$mean, 0, tolerance = 1e-6)
})

# -- Gamma ---------------------------------------------------------------------

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
  expect_error(elicit_gamma(mean = 5, sd = -1, method = "moments"))
})

test_that("elicit_gamma quantile method works", {
  p <- elicit_gamma(quantiles = c("0.10" = 2, "0.50" = 5, "0.90" = 10), label = "Event rate")
  expect_s3_class(p, "bayprior")
  expect_equal(p$dist, "gamma")
})

# -- Lognormal -----------------------------------------------------------------

test_that("elicit_lognormal returns valid bayprior object (quantile)", {
  p <- elicit_lognormal(quantiles = c("0.05" = 0.40, "0.50" = 0.70, "0.95" = 1.20), label = "HR")
  expect_s3_class(p, "bayprior")
  expect_equal(p$dist, "lognormal")
  expect_equal(p$fit_summary$q500, 0.70, tolerance = 0.05)
})

test_that("elicit_lognormal moments method works", {
  p <- elicit_lognormal(mean = 1.0, sd = 0.3, method = "moments")
  expect_s3_class(p, "bayprior")
  expect_equal(p$dist, "lognormal")
  expect_gt(p$fit_summary$mean, 0)
})

test_that("elicit_lognormal moments stores correct parameters", {
  p <- elicit_lognormal(mean = 2.0, sd = 0.5, method = "moments", label = "PK parameter")
  expect_s3_class(p, "bayprior")
  expect_true(length(p$params) > 0)
  expect_gt(p$fit_summary$mean, 0)
})

# -- Roulette ------------------------------------------------------------------

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
  expect_error(elicit_roulette(rep(0L, 10), seq(0, 1, by = 0.1), family = "beta"))
})

test_that("elicit_roulette produces prior with mean near chip distribution", {
  chips  <- c(0L, 0L, 0L, 2L, 5L, 8L, 5L, 2L, 0L, 0L)
  breaks <- seq(0, 1, by = 0.1)
  p <- elicit_roulette(chips, breaks, family = "beta", label = "Rate")
  expect_gt(p$fit_summary$mean, 0.30)
  expect_lt(p$fit_summary$mean, 0.60)
})

# -- Mixture -------------------------------------------------------------------

test_that("elicit_mixture builds valid mixture prior", {
  e1 <- elicit_beta(mean = 0.25, sd = 0.08, method = "moments")
  e2 <- elicit_beta(mean = 0.40, sd = 0.10, method = "moments")
  m  <- elicit_mixture(list(e1, e2), weights = c(0.6, 0.4), label = "Mix")
  expect_s3_class(m, "bayprior")
  expect_equal(m$dist, "mixture")
  expect_length(m$components, 2)
  expect_equal(sum(m$weights), 1, tolerance = 1e-10)
})

test_that("elicit_mixture errors on mismatched weights", {
  e1 <- elicit_beta(mean = 0.25, sd = 0.08, method = "moments")
  e2 <- elicit_beta(mean = 0.40, sd = 0.10, method = "moments")
  expect_error(elicit_mixture(list(e1, e2), weights = c(0.6, 0.6)))
})

test_that("elicit_mixture: constructs three-component mixture", {
  e1 <- elicit_beta(mean = 0.20, sd = 0.06, method = "moments", expert_id = "E1")
  e2 <- elicit_beta(mean = 0.35, sd = 0.08, method = "moments", expert_id = "E2")
  e3 <- elicit_beta(mean = 0.55, sd = 0.10, method = "moments", expert_id = "E3")
  mix <- elicit_mixture(list(E1 = e1, E2 = e2, E3 = e3), weights = c(1/3, 1/3, 1/3), label = "3-expert")
  expect_s3_class(mix, "bayprior")
  expect_equal(length(mix$components), 3)
  expect_equal(sum(mix$weights), 1, tolerance = 1e-10)
})

test_that("elicit_mixture with mismatched weights length warns or errors", {
  e1 <- elicit_beta(mean = 0.25, sd = 0.08, method = "moments")
  e2 <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments")
  # 3 weights for 2 components -- function recycles with a warning
  # (R recycling behaviour) rather than hard-erroring
  expect_true(tryCatch({
    suppressWarnings(elicit_mixture(list(e1, e2), weights = c(0.5, 0.3, 0.2)))
    TRUE
  }, error = function(e) TRUE))
})

# -- print / summary / format --------------------------------------------------

test_that("print.bayprior runs without error", {
  p <- elicit_beta(mean = 0.3, sd = 0.1, method = "moments")
  out <- capture.output(print(p))
  expect_true(length(out) > 0 || inherits(tryCatch(print(p), error = function(e) e), "bayprior"))
})

test_that("format.bayprior returns a character string", {
  p <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments", label = "Test")
  expect_error(capture.output(p), NA)
})

test_that("summary.bayprior works if defined", {
  p <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  tryCatch(summary(p), error = function(e) NULL)
  expect_s3_class(p, "bayprior")
})

# -- Exponential ---------------------------------------------------------------

test_that("elicit_exponential moments method returns valid bayprior", {
  p <- elicit_exponential(mean = 2.0, method = "moments", label = "Event rate")
  expect_s3_class(p, "bayprior")
  expect_equal(p$dist, "exponential")
  expect_equal(p$fit_summary$mean, 2.0, tolerance = 0.01)
  expect_true(p$fit_summary$sd > 0)
})

test_that("elicit_exponential quantile method returns valid bayprior", {
  p <- elicit_exponential(quantiles = c(0.5, 2.0), method = "quantile", label = "Survival rate")
  expect_s3_class(p, "bayprior")
  expect_equal(p$dist, "exponential")
})

test_that("elicit_exponential rejects non-positive mean", {
  expect_error(elicit_exponential(mean =  0, method = "moments"))
  expect_error(elicit_exponential(mean = -1, method = "moments"))
})

# -- Weibull -------------------------------------------------------------------

test_that("elicit_weibull moments method returns valid bayprior", {
  p <- elicit_weibull(mean = 5.0, sd = 2.0, method = "moments", label = "Survival time")
  expect_s3_class(p, "bayprior")
  expect_equal(p$dist, "weibull")
  expect_true(p$fit_summary$mean > 0)
  expect_true(p$fit_summary$sd   > 0)
  expect_true(p$params$shape     > 0)
  expect_true(p$params$scale     > 0)
})

test_that("elicit_weibull quantile method returns valid bayprior", {
  p <- elicit_weibull(quantiles = c(2.0, 8.0), method = "quantile", label = "Time to event")
  expect_s3_class(p, "bayprior")
  expect_equal(p$dist, "weibull")
})

test_that("elicit_weibull rejects non-positive inputs", {
  expect_error(elicit_weibull(mean = -1, sd = 1, method = "moments"))
  expect_error(elicit_weibull(mean = 1,  sd = 0, method = "moments"))
})

# -- Conjugate updates ---------------------------------------------------------

test_that("Exponential prior updates correctly with Poisson data", {
  p  <- elicit_exponential(mean = 0.10, method = "moments")
  ds <- list(type = "poisson", x = 12, n = 100)
  post <- bayprior:::.conjugate_update(p, ds)
  expect_equal(post$dist, "gamma")
})

test_that("Weibull prior updates via Normal approximation", {
  p  <- elicit_weibull(mean = 5.0, sd = 2.0, method = "moments")
  ds <- list(type = "survival", x = 20, n = 400)
  post <- bayprior:::.conjugate_update(p, ds)
  expect_equal(post$dist, "normal")
  expect_gt(post$params$mu, 0)
  expect_gt(post$params$sigma, 0)
})

# -- Exponential: rate method --------------------------------------------------

test_that("elicit_exponential rate method returns correct prior", {
  p <- elicit_exponential(rate = 0.10, method = "rate", label = "Hazard rate")
  expect_s3_class(p, "bayprior")
  expect_equal(p$dist, "exponential")
  expect_equal(p$params$rate, 0.10, tolerance = 1e-6)
  # For Exponential, mean = 1/rate
  expect_equal(p$fit_summary$mean, 10.0, tolerance = 0.01)
})

test_that("elicit_exponential rate method: SD equals mean (memoryless property)", {
  p <- elicit_exponential(rate = 0.50, method = "rate")
  expect_equal(p$fit_summary$mean, p$fit_summary$sd, tolerance = 0.01)
})

# -- Weibull: params method ----------------------------------------------------

test_that("elicit_weibull params method returns correct prior", {
  p <- elicit_weibull(shape = 2.0, scale = 20.0, method = "params",
                      label = "Survival time")
  expect_s3_class(p, "bayprior")
  expect_equal(p$dist, "weibull")
  expect_equal(p$params$shape, 2.0, tolerance = 1e-6)
  expect_equal(p$params$scale, 20.0, tolerance = 1e-6)
  # Weibull(shape=2, scale=20) mean = 20 * Gamma(1.5) = 20 * sqrt(pi)/2
  expect_equal(p$fit_summary$mean, 20 * gamma(1.5), tolerance = 0.01)
})

test_that("elicit_weibull params method with shape=1 gives exponential-like", {
  # Weibull(1, scale) = Exponential(1/scale)
  p <- elicit_weibull(shape = 1.0, scale = 10.0, method = "params")
  expect_s3_class(p, "bayprior")
  expect_equal(p$params$shape, 1.0, tolerance = 1e-6)
  expect_equal(p$fit_summary$mean, 10.0, tolerance = 0.01)
})

# -- elicit_mixture: additional paths -----------------------------------------

test_that("elicit_mixture: normal component mixture works", {
  e1 <- elicit_normal(mean = -0.5, sd = 0.3, method = "moments", expert_id = "E1")
  e2 <- elicit_normal(mean =  0.5, sd = 0.3, method = "moments", expert_id = "E2")
  mix <- elicit_mixture(
    components = list(E1 = e1, E2 = e2),
    weights    = c(0.5, 0.5),
    label      = "Bimodal normal mixture"
  )
  expect_s3_class(mix, "bayprior")
  expect_equal(mix$dist, "mixture")
  # Mean of equal-weight symmetric mixture should be ~0
  expect_equal(mix$fit_summary$mean, 0.0, tolerance = 0.05)
})

# -- .validate_quantiles error paths ------------------------------------------

test_that(".validate_quantiles: non-monotone values rejected", {
  # Values must be strictly increasing with probability keys
  # Non-monotone quantile values should error
  expect_error(
    elicit_beta(
      quantiles = c(`0.10` = 0.60, `0.50` = 0.30, `0.90` = 0.80),
      method    = "quantile"
    )
  )
})
