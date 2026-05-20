# test-robust.R
# Tests for robust_prior(), sceptical_prior(), calibrate_power_prior()
# Also covers: plotting.R, validation_utils.R, zzz_patches.R,
#              elicit_exponential(), elicit_weibull()
# Target coverage uplift: validation_utils 0% -> ~80%, zzz_patches 65% -> ~80%
#                         robust_priors 70% -> ~80%, plotting 70% -> ~78%
# -----------------------------------------------------------------------------

library(testthat)
library(bayprior)

# -- robust_prior() ------------------------------------------------------------

test_that("robust_prior returns a valid mixture bayprior", {
  inf <- elicit_beta(mean = 0.30, sd = 0.08, method = "moments",
                     label = "Response rate")
  rob <- suppressWarnings(
    robust_prior(inf, vague_weight = 0.20, label = "Robust prior")
  )
  expect_s3_class(rob, "bayprior")
  expect_equal(rob$dist, "mixture")
  expect_equal(rob$vague_weight, 0.20)
  expect_length(rob$components, 2)
  expect_equal(sum(rob$weights), 1, tolerance = 1e-10)
  expect_gt(rob$fit_summary$mean, 0)
  expect_lt(rob$fit_summary$mean, 1)
})

test_that("robust_prior respects vague_weight parameter", {
  inf <- elicit_beta(mean = 0.30, sd = 0.08, method = "moments")
  r10 <- suppressWarnings(robust_prior(inf, vague_weight = 0.10))
  r40 <- suppressWarnings(robust_prior(inf, vague_weight = 0.40))
  expect_gt(r40$fit_summary$sd, r10$fit_summary$sd)
})

test_that("robust_prior works with custom vague_sd", {
  inf <- elicit_beta(mean = 0.30, sd = 0.08, method = "moments")
  rob <- suppressWarnings(
    robust_prior(inf, vague_weight = 0.20,
                 vague_sd = 5 * inf$fit_summary$sd)
  )
  expect_s3_class(rob, "bayprior")
  expect_equal(rob$vague_weight, 0.20)
})

test_that("robust_prior errors on invalid vague_weight", {
  inf <- elicit_beta(mean = 0.30, sd = 0.08, method = "moments")
  expect_error(suppressWarnings(robust_prior(inf, vague_weight = -0.1)))
  expect_error(suppressWarnings(robust_prior(inf, vague_weight =  1.5)))
})

test_that("robust_prior errors if informative is not a bayprior", {
  expect_error(robust_prior("not a prior", vague_weight = 0.2))
})

# -- sceptical_prior() ---------------------------------------------------------

test_that("sceptical_prior works for normal family -- all strengths", {
  sc_w <- sceptical_prior(0, "normal", "weak",     label = "Log OR")
  sc_m <- sceptical_prior(0, "normal", "moderate", label = "Log OR")
  sc_s <- sceptical_prior(0, "normal", "strong",   label = "Log OR")
  for (sc in list(sc_w, sc_m, sc_s)) {
    expect_s3_class(sc, "bayprior")
    expect_equal(sc$dist, "normal")
    expect_equal(sc$fit_summary$mean, 0, tolerance = 1e-6)
    expect_gt(sc$fit_summary$sd, 0)
  }
  expect_gt(sc_w$fit_summary$sd, sc_m$fit_summary$sd)
  expect_gt(sc_m$fit_summary$sd, sc_s$fit_summary$sd)
})

test_that("sceptical_prior works for beta family", {
  sc <- sceptical_prior(null_value = 0.20, family = "beta",
                        strength = "moderate")
  expect_s3_class(sc, "bayprior")
  expect_equal(sc$dist, "beta")
  expect_equal(as.numeric(sc$fit_summary$mean), 0.20, tolerance = 0.02)
})

test_that("sceptical_prior works for lognormal family -- all strengths", {
  for (s in c("weak", "moderate", "strong")) {
    sc <- sceptical_prior(null_value = 0, family = "lognormal", strength = s)
    expect_s3_class(sc, "bayprior")
    expect_equal(sc$dist, "lognormal")
    expect_equal(as.numeric(sc$fit_summary$mean), 1.0, tolerance = 0.15)
    expect_gt(as.numeric(sc$fit_summary$sd), 0)
  }
})

test_that("sceptical_prior errors on beta with null_value outside (0, 1)", {
  expect_error(sceptical_prior(0.0,  family = "beta", strength = "moderate"))
  expect_error(sceptical_prior(1.0,  family = "beta", strength = "moderate"))
  expect_error(sceptical_prior(-0.1, family = "beta", strength = "moderate"))
})

# -- calibrate_power_prior() ---------------------------------------------------

test_that("calibrate_power_prior returns correct structure (bayes_factor)", {
  base <- elicit_beta(mean = 0.50, sd = 0.20, method = "moments")
  cp   <- calibrate_power_prior(
    historical_data = list(type = "binary", x = 12, n = 40),
    current_data    = list(type = "binary", x = 18, n = 50),
    base_prior      = base,
    target_bf       = 3,
    delta_grid      = seq(0.10, 1.0, by = 0.10),
    method          = "bayes_factor"
  )
  expect_s3_class(cp, "bayprior_power_prior")
  expect_true(is.numeric(cp$delta_opt))
  expect_gte(cp$delta_opt, 0)
  expect_lte(cp$delta_opt, 1)
  expect_s3_class(cp$power_prior, "bayprior")
  expect_true(is.data.frame(cp$results))
  expect_true("delta"        %in% colnames(cp$results))
  expect_true("bayes_factor" %in% colnames(cp$results))
  expect_true("box_pvalue"   %in% colnames(cp$results))
})

test_that("calibrate_power_prior works with compatibility method", {
  base <- elicit_beta(mean = 0.50, sd = 0.20, method = "moments")
  cp   <- calibrate_power_prior(
    historical_data = list(type = "binary", x = 12, n = 40),
    current_data    = list(type = "binary", x = 18, n = 50),
    base_prior      = base,
    delta_grid      = seq(0.10, 1.0, by = 0.10),
    method          = "compatibility"
  )
  expect_s3_class(cp, "bayprior_power_prior")
  expect_gte(cp$delta_opt, 0)
  expect_lte(cp$delta_opt, 1)
})

test_that("calibrate_power_prior works for normal prior (continuous data)", {
  base <- elicit_normal(mean = 0.35, sd = 0.10, method = "moments")
  cp   <- calibrate_power_prior(
    historical_data = list(type = "continuous", x = 0.33, sd = 0.12, n = 30),
    current_data    = list(type = "continuous", x = 0.40, sd = 0.15, n = 40),
    base_prior      = base,
    delta_grid      = seq(0.20, 1.0, by = 0.20),
    method          = "bayes_factor"
  )
  expect_s3_class(cp, "bayprior_power_prior")
  expect_equal(cp$power_prior$dist, "normal")
})

test_that("calibrate_power_prior works for gamma prior (continuous data)", {
  base <- elicit_gamma(mean = 5.0, sd = 2.0, method = "moments", label = "Rate")
  cp   <- calibrate_power_prior(
    historical_data = list(type = "continuous", x = 4.5, sd = 1.5, n = 30),
    current_data    = list(type = "continuous", x = 5.5, sd = 2.0, n = 40),
    base_prior      = base,
    delta_grid      = seq(0.20, 1.0, by = 0.20),
    method          = "compatibility"
  )
  expect_s3_class(cp, "bayprior_power_prior")
})

test_that("calibrate_power_prior works with Poisson data", {
  base <- elicit_gamma(mean = 3.0, sd = 1.5, method = "moments",
                       label = "Event rate")
  cp   <- calibrate_power_prior(
    historical_data = list(type = "poisson", x = 12, n = 40),
    current_data    = list(type = "poisson", x = 18, n = 50),
    base_prior      = base,
    delta_grid      = seq(0.20, 1.0, by = 0.20),
    method          = "bayes_factor"
  )
  expect_s3_class(cp, "bayprior_power_prior")
  expect_true(cp$delta_opt >= 0.05 && cp$delta_opt <= 1)
})

test_that("calibrate_power_prior errors if base_prior is not bayprior", {
  expect_error(
    calibrate_power_prior(
      historical_data = list(type = "binary", x = 12, n = 40),
      current_data    = list(type = "binary", x = 18, n = 50),
      base_prior      = "not a prior"
    )
  )
})

# -- print / plot methods ------------------------------------------------------

test_that("print.bayprior_power_prior runs without error", {
  base <- elicit_beta(mean = 0.50, sd = 0.20, method = "moments")
  cp   <- calibrate_power_prior(
    historical_data = list(type = "binary", x = 12, n = 40),
    current_data    = list(type = "binary", x = 18, n = 50),
    base_prior      = base,
    delta_grid      = seq(0.20, 1.0, by = 0.20),
    method          = "bayes_factor"
  )
  expect_error(print(cp), NA)
})

test_that("plot.bayprior_power_prior returns a gg/patchwork object", {
  base <- elicit_beta(mean = 0.50, sd = 0.20, method = "moments")
  cp   <- calibrate_power_prior(
    historical_data = list(type = "binary", x = 12, n = 40),
    current_data    = list(type = "binary", x = 18, n = 50),
    base_prior      = base,
    delta_grid      = seq(0.20, 1.0, by = 0.20),
    method          = "bayes_factor"
  )
  gp <- plot(cp)
  expect_true(
    inherits(gp, "patchwork") || inherits(gp, "gg") || inherits(gp, "gtable")
  )
})

test_that("plot.bayprior works for all six distribution families", {
  dists <- list(
    beta        = elicit_beta(mean = 0.3, sd = 0.10, method = "moments"),
    normal      = elicit_normal(mean = 0.0, sd = 0.5, method = "moments"),
    gamma       = elicit_gamma(mean = 5.0, sd = 2.0,  method = "moments"),
    lognormal   = elicit_lognormal(mean = 1.0, sd = 0.3, method = "moments"),
    exponential = elicit_exponential(mean = 2.0, method = "moments"),
    weibull     = elicit_weibull(mean = 5.0, sd = 2.0, method = "moments")
  )
  for (nm in names(dists)) {
    gp <- plot(dists[[nm]])
    expect_true(inherits(gp, "gg"),
                label = paste("plot.bayprior:", nm, "should return gg"))
  }
})

# -- elicit_exponential() ------------------------------------------------------

test_that("elicit_exponential moments method returns valid bayprior", {
  pr <- elicit_exponential(mean = 2.0, method = "moments", label = "Event rate")
  expect_s3_class(pr, "bayprior")
  expect_equal(pr$dist, "exponential")
  expect_equal(pr$fit_summary$mean, 2.0, tolerance = 0.01)
  expect_true(pr$fit_summary$sd > 0)
})

test_that("elicit_exponential quantile method returns valid bayprior", {
  # quantiles = the actual quantile values (not probabilities)
  pr <- elicit_exponential(
    quantiles = c(0.5, 2.0),   # median = 0.5, 75th pctile = 2.0
    method    = "quantile",
    label     = "Survival rate"
  )
  expect_s3_class(pr, "bayprior")
  expect_equal(pr$dist, "exponential")
})

test_that("elicit_exponential rejects non-positive mean", {
  expect_error(elicit_exponential(mean =  0, method = "moments"))
  expect_error(elicit_exponential(mean = -1, method = "moments"))
})

# -- elicit_weibull() ----------------------------------------------------------

test_that("elicit_weibull moments method returns valid bayprior", {
  pr <- elicit_weibull(mean = 5.0, sd = 2.0, method = "moments",
                       label = "Survival time")
  expect_s3_class(pr, "bayprior")
  expect_equal(pr$dist, "weibull")
  expect_true(pr$fit_summary$mean > 0)
  expect_true(pr$fit_summary$sd   > 0)
  expect_true(pr$params$shape     > 0)
  expect_true(pr$params$scale     > 0)
})

test_that("elicit_weibull quantile method returns valid bayprior", {
  # quantiles = the actual quantile values on the time scale
  pr <- elicit_weibull(
    quantiles = c(2.0, 8.0),   # 10th pctile = 2, 90th pctile = 8
    method    = "quantile",
    label     = "Time to event"
  )
  expect_s3_class(pr, "bayprior")
  expect_equal(pr$dist, "weibull")
})

test_that("elicit_weibull rejects non-positive inputs", {
  expect_error(elicit_weibull(mean = -1, sd =  1, method = "moments"))
  expect_error(elicit_weibull(mean =  1, sd =  0, method = "moments"))
  expect_error(elicit_weibull(mean =  0, sd =  1, method = "moments"))
})

# -- validation_utils.R --------------------------------------------------------

test_that(".check_prior_data_compat passes valid combinations", {
  # Function takes a data_summary list, not a bare string
  r1 <- .check_prior_data_compat(
    elicit_beta(mean = 0.3, sd = 0.1, method = "moments"),
    list(type = "binary", x = 10, n = 30))
  expect_true(r1$ok)

  r2 <- .check_prior_data_compat(
    elicit_normal(mean = 0.3, sd = 0.1, method = "moments"),
    list(type = "continuous", x = 0.3, sd = 0.1, n = 30))
  expect_true(r2$ok)

  r3 <- .check_prior_data_compat(
    elicit_gamma(mean = 3.0, sd = 1.0, method = "moments"),
    list(type = "poisson", x = 9, n = 3))
  expect_true(r3$ok)
})

test_that(".check_prior_data_compat returns warning for mismatched family-type", {
  # Mismatch returns ok=TRUE with a warning message, it does not error
  r1 <- .check_prior_data_compat(
    elicit_normal(mean = 0.3, sd = 0.1, method = "moments"),
    list(type = "poisson", x = 10, n = 30))  # Normal does not support Poisson
  expect_equal(r1$severity, "warning")
  expect_true(nchar(r1$msg) > 0)

  r2 <- .check_prior_data_compat(
    elicit_lognormal(mean = 1.0, sd = 0.3, method = "moments"),
    list(type = "binary", x = 10, n = 30))   # LogNormal does not support binary
  expect_equal(r2$severity, "warning")
})

test_that(".check_pooling_compat passes matching families", {
  p1 <- elicit_beta(mean = 0.3, sd = 0.1, method = "moments")
  p2 <- elicit_beta(mean = 0.5, sd = 0.1, method = "moments")
  expect_no_error(.check_pooling_compat(list(p1, p2)))
})

test_that(".check_pooling_compat returns ok=FALSE for incompatible supports", {
  # Beta (unit) + Normal (real) have incompatible supports -- returns ok=FALSE
  p1 <- elicit_beta(mean  = 0.3, sd = 0.1, method = "moments")
  p2 <- elicit_normal(mean = 0.3, sd = 0.1, method = "moments")
  r  <- .check_pooling_compat(list(p1, p2))
  expect_false(r$ok)
  expect_equal(r$severity, "error")
  expect_true(nchar(r$msgs) > 0)
})

test_that(".check_pooling_compat returns warning for same-support mixed families", {
  # Gamma + Exponential: both positive support -- warning, not error
  p1 <- elicit_gamma(mean = 3.0, sd = 1.5, method = "moments")
  p2 <- elicit_exponential(mean = 2.0, method = "moments")
  r  <- .check_pooling_compat(list(p1, p2))
  expect_true(r$ok)
  expect_equal(r$severity, "warning")
})

test_that(".check_sensitivity_compat returns ok=TRUE for multi-param priors", {
  # Function takes only the prior -- param_grid is not an argument
  pr <- elicit_beta(mean = 0.3, sd = 0.1, method = "moments")
  r  <- .check_sensitivity_compat(pr)
  expect_true(r$ok)
})

test_that(".check_sensitivity_compat returns info for single-param priors", {
  # Exponential has only one parameter (rate) -- function flags this
  pr <- elicit_exponential(mean = 2.0, method = "moments")
  r  <- .check_sensitivity_compat(pr)
  expect_true(r$ok)
  expect_equal(r$severity, "info")
})

# -- zzz_patches.R -------------------------------------------------------------

test_that(".target_label maps all known targets correctly", {
  expect_equal(.target_label("posterior_mean"), "Posterior mean")
  expect_equal(.target_label("posterior_sd"),   "Posterior SD")
  expect_equal(.target_label("cri_lower"),      "95% CrI lower bound")
  expect_equal(.target_label("cri_upper"),      "95% CrI upper bound")
  expect_equal(.target_label("cri_width"),      "95% CrI width")
  expect_equal(.target_label("prob_efficacy"),  "Pr(efficacy)")
})

test_that(".target_label returns input unchanged for unknown targets", {
  expect_equal(.target_label("unknown_target"), "unknown_target")
  expect_equal(.target_label("custom_metric"),  "custom_metric")
})

test_that(".relabel_sensitivity handles NULL input", {
  expect_null(.relabel_sensitivity(NULL))
})

test_that(".relabel_sensitivity renames target names in sensitivity object", {
  pr <- elicit_beta(mean = 0.3, sd = 0.1, method = "moments")
  sa <- sensitivity_grid(
    pr,
    data_summary = list(type = "binary", x = 12, n = 40),
    param_grid   = list(alpha = seq(2, 6, by = 2),
                        beta  = seq(4, 12, by = 4))
  )
  sa2 <- .relabel_sensitivity(sa)
  # Should be a valid sensitivity object (not NULL, not broken)
  expect_false(is.null(sa2))
  expect_true(is.list(sa2))
})
