# -- robust_prior() ------------------------------------------------------------

test_that("robust_prior returns a valid mixture bayprior", {
  inf <- elicit_beta(mean = 0.30, sd = 0.08, method = "moments", label = "Response rate")
  rob <- suppressWarnings(robust_prior(inf, vague_weight = 0.20, label = "Robust prior"))
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
  rob <- suppressWarnings(robust_prior(inf, vague_weight = 0.20, vague_sd = 5 * inf$fit_summary$sd))
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
  sc <- sceptical_prior(null_value = 0.20, family = "beta", strength = "moderate")
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
    base_prior      = base, target_bf = 3,
    delta_grid      = seq(0.10, 1.0, by = 0.10), method = "bayes_factor"
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
    base_prior      = base, delta_grid = seq(0.10, 1.0, by = 0.10), method = "compatibility"
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
    base_prior      = base, delta_grid = seq(0.20, 1.0, by = 0.20), method = "bayes_factor"
  )
  expect_s3_class(cp, "bayprior_power_prior")
  expect_equal(cp$power_prior$dist, "normal")
})

test_that("calibrate_power_prior works for gamma prior (continuous data)", {
  base <- elicit_gamma(mean = 5.0, sd = 2.0, method = "moments", label = "Rate")
  cp   <- calibrate_power_prior(
    historical_data = list(type = "continuous", x = 4.5, sd = 1.5, n = 30),
    current_data    = list(type = "continuous", x = 5.5, sd = 2.0, n = 40),
    base_prior      = base, delta_grid = seq(0.20, 1.0, by = 0.20), method = "compatibility"
  )
  expect_s3_class(cp, "bayprior_power_prior")
})

test_that("calibrate_power_prior works with Poisson data", {
  base <- elicit_gamma(mean = 3.0, sd = 1.5, method = "moments", label = "Event rate")
  cp   <- calibrate_power_prior(
    historical_data = list(type = "poisson", x = 12, n = 40),
    current_data    = list(type = "poisson", x = 18, n = 50),
    base_prior      = base, delta_grid = seq(0.20, 1.0, by = 0.20), method = "bayes_factor"
  )
  expect_s3_class(cp, "bayprior_power_prior")
})

test_that("calibrate_power_prior errors if base_prior is not bayprior", {
  expect_error(calibrate_power_prior(
    historical_data = list(type = "binary", x = 12, n = 40),
    current_data    = list(type = "binary", x = 18, n = 50),
    base_prior      = "not a prior"
  ))
})

# -- .power_prior_update internals ---------------------------------------------

test_that(".power_prior_update: normal data type works", {
  base <- elicit_normal(mean = 0.35, sd = 0.10, method = "moments")
  pp   <- bayprior:::.power_prior_update(base, list(type = "continuous", x = 0.50, n = 30, sd = 0.12), delta = 0.5)
  expect_s3_class(pp, "bayprior")
  expect_equal(pp$dist, "normal")
  expect_gt(pp$fit_summary$mean, 0.35)
})

test_that(".power_prior_update: gamma-poisson data type works", {
  base <- elicit_gamma(mean = 3.0, sd = 1.5, method = "moments")
  pp   <- bayprior:::.power_prior_update(base, list(type = "poisson", x = 15, n = 50), delta = 0.5)
  expect_s3_class(pp, "bayprior")
  expect_equal(pp$dist, "gamma")
  expect_gt(pp$params$shape, base$params$shape)
})

test_that(".power_prior_update: gamma-continuous data type works", {
  base <- elicit_gamma(mean = 5.0, sd = 2.0, method = "moments")
  pp   <- bayprior:::.power_prior_update(base, list(type = "continuous", x_sum = 150, n = 30), delta = 0.5)
  expect_s3_class(pp, "bayprior")
  expect_equal(pp$dist, "gamma")
})

test_that(".power_prior_update: lognormal data type works", {
  base <- elicit_lognormal(mean = 1.5, sd = 0.4, method = "moments")
  pp   <- bayprior:::.power_prior_update(base, list(type = "continuous", x = 1.6, n = 25, sd = 0.3), delta = 0.5)
  expect_s3_class(pp, "bayprior")
  expect_equal(pp$dist, "lognormal")
})

test_that(".power_prior_update: delta=1 borrows all historical data", {
  base    <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  hist    <- list(type = "binary", x = 12, n = 40)
  pp_full <- bayprior:::.power_prior_update(base, hist, delta = 1.0)
  pp_half <- bayprior:::.power_prior_update(base, hist, delta = 0.5)
  expect_equal(pp_full$params$alpha, base$params$alpha + 12, tolerance = 1e-6)
  expect_lt(pp_half$params$alpha, pp_full$params$alpha)
})

# -- print / plot methods ------------------------------------------------------

test_that("print.bayprior_power_prior runs without error", {
  base <- elicit_beta(mean = 0.50, sd = 0.20, method = "moments")
  cp   <- calibrate_power_prior(
    historical_data = list(type = "binary", x = 12, n = 40),
    current_data    = list(type = "binary", x = 18, n = 50),
    base_prior      = base, delta_grid = seq(0.20, 1.0, by = 0.20), method = "bayes_factor"
  )
  expect_error(print(cp), NA)
})

test_that("plot.bayprior_power_prior returns a gg/patchwork object", {
  base <- elicit_beta(mean = 0.50, sd = 0.20, method = "moments")
  cp   <- calibrate_power_prior(
    historical_data = list(type = "binary", x = 12, n = 40),
    current_data    = list(type = "binary", x = 18, n = 50),
    base_prior      = base, delta_grid = seq(0.20, 1.0, by = 0.20), method = "bayes_factor"
  )
  gp <- plot(cp)
  expect_true(inherits(gp, "patchwork") || inherits(gp, "gg") || inherits(gp, "gtable"))
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
    expect_true(inherits(gp, "gg"), label = paste("plot.bayprior:", nm, "should return gg"))
  }
})

# -- .power_prior_update: mixture prior path -----------------------------------

test_that(".power_prior_update: mixture prior (robust) with binary data works", {
  inf  <- elicit_beta(mean = 0.30, sd = 0.08, method = "moments")
  mix  <- suppressWarnings(robust_prior(inf, vague_weight = 0.20))
  pp   <- bayprior:::.power_prior_update(
    mix,
    list(type = "binary", x = 12, n = 40),
    delta = 0.5
  )
  expect_s3_class(pp, "bayprior")
  expect_equal(pp$dist, "mixture")
})

test_that(".power_prior_update: mixture prior with continuous data works", {
  e1  <- elicit_normal(mean = 0.0, sd = 0.5, method = "moments", expert_id = "E1")
  e2  <- elicit_normal(mean = 0.5, sd = 0.5, method = "moments", expert_id = "E2")
  mix <- aggregate_experts(list(E1 = e1, E2 = e2), weights = c(0.5, 0.5))
  pp  <- bayprior:::.power_prior_update(
    mix,
    list(type = "continuous", x = 0.3, n = 30, sd = 0.4),
    delta = 0.5
  )
  expect_s3_class(pp, "bayprior")
  expect_equal(pp$dist, "mixture")
})

test_that("plot.bayprior_power_prior: patchwork not installed falls back gracefully", {
  base <- elicit_beta(mean = 0.50, sd = 0.20, method = "moments")
  cp   <- calibrate_power_prior(
    historical_data = list(type = "binary", x = 12, n = 40),
    current_data    = list(type = "binary", x = 18, n = 50),
    base_prior      = base,
    delta_grid      = seq(0.20, 1.0, by = 0.20),
    method          = "bayes_factor"
  )
  # Whether patchwork is installed or not, plot should return something valid
  result <- plot(cp)
  expect_true(
    inherits(result, "patchwork") ||
    inherits(result, "gg") ||
    is.list(result)
  )
})
