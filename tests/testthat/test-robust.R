test_that("robust_prior returns a valid mixture bayprior", {
  inf <- elicit_beta(mean = 0.30, sd = 0.08, method = "moments",
                     label = "Response rate")
  rob <- suppressWarnings(robust_prior(inf, vague_weight = 0.20, label = "Robust prior"))

  expect_s3_class(rob, "bayprior")
  expect_equal(rob$dist, "mixture")
  expect_equal(rob$vague_weight, 0.20)
  expect_length(rob$components, 2)

  # Weights sum to 1
  expect_equal(sum(rob$weights), 1, tolerance = 1e-10)

  # Mixture mean is between informative mean and vague mean
  expect_gt(rob$fit_summary$mean, 0)
  expect_lt(rob$fit_summary$mean, 1)
})

test_that("robust_prior respects vague_weight parameter", {
  inf  <- elicit_beta(mean = 0.30, sd = 0.08, method = "moments")
  r10  <- suppressWarnings(robust_prior(inf, vague_weight = 0.10))
  r40  <- suppressWarnings(robust_prior(inf, vague_weight = 0.40))

  # Higher vague weight should give wider (more uncertain) mixture
  expect_gt(r40$fit_summary$sd, r10$fit_summary$sd)
})

test_that("robust_prior errors on invalid vague_weight", {
  inf <- elicit_beta(mean = 0.30, sd = 0.08, method = "moments")
  expect_error(suppressWarnings(robust_prior(inf, vague_weight = -0.1)))
  expect_error(suppressWarnings(robust_prior(inf, vague_weight = 1.5)))
})

test_that("sceptical_prior works for normal family", {
  sc <- sceptical_prior(null_value = 0, family = "normal",
                        strength = "moderate", label = "Log OR (sceptical)")
  expect_s3_class(sc, "bayprior")
  expect_equal(sc$dist, "normal")
  # Centred at null
  expect_equal(sc$fit_summary$mean, 0, tolerance = 1e-6)
  # SD positive
  expect_gt(sc$fit_summary$sd, 0)
})

test_that("sceptical_prior works for beta family", {
  sc <- sceptical_prior(null_value = 0.20, family = "beta",
                        strength = "moderate")
  expect_s3_class(sc, "bayprior")
  expect_equal(sc$dist, "beta")
  expect_equal(as.numeric(sc$fit_summary$mean), 0.20, tolerance = 0.02)
})

test_that("sceptical_prior strength ordering is correct", {
  sc_weak     <- sceptical_prior(0, "normal", "weak")
  sc_moderate <- sceptical_prior(0, "normal", "moderate")
  sc_strong   <- sceptical_prior(0, "normal", "strong")

  # More sceptical = narrower prior (smaller SD)
  expect_gt(sc_weak$fit_summary$sd, sc_moderate$fit_summary$sd)
  expect_gt(sc_moderate$fit_summary$sd, sc_strong$fit_summary$sd)
})

test_that("sceptical_prior errors on beta with null_value outside (0,1)", {
  expect_error(sceptical_prior(0.0, family = "beta", strength = "moderate"))
  expect_error(sceptical_prior(1.0, family = "beta", strength = "moderate"))
  expect_error(sceptical_prior(-0.1, family = "beta", strength = "moderate"))
})

test_that("calibrate_power_prior returns correct structure (bayes_factor)", {
  base <- elicit_beta(mean = 0.50, sd = 0.20, method = "moments")
  cp <- calibrate_power_prior(
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
  expect_true("delta" %in% colnames(cp$results))
  expect_true("bayes_factor" %in% colnames(cp$results))
})

test_that("calibrate_power_prior works with compatibility method", {
  base <- elicit_beta(mean = 0.50, sd = 0.20, method = "moments")
  cp <- calibrate_power_prior(
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

test_that("plot.bayprior_power_prior returns a patchwork/gg object", {
  base <- elicit_beta(mean = 0.50, sd = 0.20, method = "moments")
  cp <- calibrate_power_prior(
    historical_data = list(type = "binary", x = 12, n = 40),
    current_data    = list(type = "binary", x = 18, n = 50),
    base_prior      = base,
    delta_grid      = seq(0.10, 1.0, by = 0.20),
    method          = "bayes_factor"
  )
  gp <- plot(cp)
  # Should be ggplot-based (patchwork or gg)
  expect_true(inherits(gp, "patchwork") || inherits(gp, "gg") || inherits(gp, "gtable"))
})

test_that("print.bayprior_power_prior runs without error", {
  base <- elicit_beta(mean = 0.50, sd = 0.20, method = "moments")
  cp <- calibrate_power_prior(
    historical_data = list(type = "binary", x = 12, n = 40),
    current_data    = list(type = "binary", x = 18, n = 50),
    base_prior      = base,
    delta_grid      = seq(0.10, 1.0, by = 0.20),
    method          = "bayes_factor"
  )
  # cli output goes to stderr; just check no error thrown
  expect_error(print(cp), NA)
})


# ── Additional coverage ──────────────────────────────────────────────────────

test_that("sceptical_prior works for lognormal family", {
  sc <- sceptical_prior(null_value=0, family="lognormal", strength="moderate")
  expect_s3_class(sc, "bayprior")
  expect_equal(sc$dist, "lognormal")
  # Mean should be close to exp(0) = 1 on original scale
  expect_equal(as.numeric(sc$fit_summary$mean), 1.0, tolerance=0.15)
  # Positive SD
  expect_gt(as.numeric(sc$fit_summary$sd), 0)
})

test_that("sceptical_prior works for all strength levels - lognormal", {
  for (s in c("weak", "moderate", "strong")) {
    sc <- sceptical_prior(null_value=0, family="lognormal", strength=s)
    expect_s3_class(sc, "bayprior")
  }
})

test_that("calibrate_power_prior works for gamma prior", {
  base <- elicit_gamma(mean=5, sd=2, method="moments", label="Rate")
  cp <- calibrate_power_prior(
    historical_data = list(type="continuous", x=4.5, sd=1.5, n=30),
    current_data    = list(type="continuous", x=5.5, sd=2.0, n=40),
    base_prior      = base,
    delta_grid      = seq(0.20, 1.0, by=0.20),
    method          = "compatibility"
  )
  expect_s3_class(cp, "bayprior_power_prior")
})

test_that("robust_prior works with custom vague_sd", {
  inf <- elicit_beta(mean=0.30, sd=0.08, method="moments")
  rob <- suppressWarnings(robust_prior(inf, vague_weight=0.20,
                      vague_sd = 5 * inf$fit_summary$sd))
  expect_s3_class(rob, "bayprior")
  expect_equal(rob$vague_weight, 0.20)
})

# ── Plotting coverage ─────────────────────────────────────────────────────────

test_that("plot.bayprior works for all distribution families", {
  dists <- list(
    beta      = elicit_beta(mean=0.3, sd=0.1, method="moments"),
    normal    = elicit_normal(mean=0, sd=0.5, method="moments"),
    gamma     = elicit_gamma(mean=5, sd=2, method="moments"),
    lognormal = elicit_lognormal(mean=1.0, sd=0.3, method="moments")
  )
  for (nm in names(dists)) {
    gp <- plot(dists[[nm]])
    expect_true(inherits(gp, "gg"),
                label = paste("plot.bayprior:", nm, "should return gg"))
  }
})