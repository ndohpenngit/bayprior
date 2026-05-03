test_that("plot.bayprior snapshot - beta moments", {
  skip_if_not_installed("vdiffr")
  p <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments",
                   label = "Response rate")
  vdiffr::expect_doppelganger("plot-bayprior-beta-moments", plot(p))
})

test_that("plot.bayprior snapshot - normal quantile", {
  skip_if_not_installed("vdiffr")
  p <- elicit_normal(
    quantiles = c("0.025" = -0.5, "0.50" = 0.20, "0.975" = 0.90),
    label = "Log OR"
  )
  vdiffr::expect_doppelganger("plot-bayprior-normal-quantile", plot(p))
})

test_that("plot.bayprior snapshot - gamma moments", {
  skip_if_not_installed("vdiffr")
  p <- elicit_gamma(mean = 5, sd = 2, method = "moments", label = "Rate")
  vdiffr::expect_doppelganger("plot-bayprior-gamma-moments", plot(p))
})

test_that("plot.bayprior snapshot - lognormal quantile", {
  skip_if_not_installed("vdiffr")
  p <- elicit_lognormal(
    quantiles = c("0.05" = 0.40, "0.50" = 0.70, "0.95" = 1.20),
    label = "Hazard ratio"
  )
  vdiffr::expect_doppelganger("plot-bayprior-lognormal-quantile", plot(p))
})

test_that("plot.bayprior snapshot - mixture (linear pooling)", {
  skip_if_not_installed("vdiffr")
  e1 <- elicit_beta(mean = 0.25, sd = 0.08, method = "moments",
                    expert_id = "E1", label = "ORR")
  e2 <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments",
                    expert_id = "E2", label = "ORR")
  con <- aggregate_experts(list(E1 = e1, E2 = e2),
                           weights = c(0.5, 0.5), method = "linear")
  vdiffr::expect_doppelganger("plot-bayprior-mixture-linear", plot(con))
})

test_that("plot_prior_likelihood snapshot - no conflict with posterior", {
  skip_if_not_installed("vdiffr")
  prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments",
                       label = "Response rate")
  gp <- plot_prior_likelihood(
    prior,
    data_summary   = list(type = "binary", x = 12, n = 40),
    show_posterior = TRUE
  )
  vdiffr::expect_doppelganger("plot-overlay-no-conflict", gp)
})

test_that("plot_prior_likelihood snapshot - severe conflict", {
  skip_if_not_installed("vdiffr")
  prior <- elicit_beta(mean = 0.30, sd = 0.05, method = "moments",
                       label = "Response rate")
  gp <- plot_prior_likelihood(
    prior,
    data_summary   = list(type = "binary", x = 38, n = 40),
    show_posterior = TRUE
  )
  vdiffr::expect_doppelganger("plot-overlay-severe-conflict", gp)
})

test_that("plot_tornado snapshot", {
  skip_if_not_installed("vdiffr")
  prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  sa <- sensitivity_grid(
    prior        = prior,
    data_summary = list(type = "binary", x = 14, n = 40),
    param_grid   = list(alpha = seq(1, 4, 1), beta = seq(2, 8, 2)),
    target       = c("posterior_mean", "prob_efficacy"),
    threshold    = 0.30
  )
  vdiffr::expect_doppelganger("plot-tornado", plot_tornado(sa))
})

test_that("plot_sensitivity snapshot - posterior_mean heatmap", {
  skip_if_not_installed("vdiffr")
  prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  sa <- sensitivity_grid(
    prior        = prior,
    data_summary = list(type = "binary", x = 14, n = 40),
    param_grid   = list(alpha = seq(1, 4, 1), beta = seq(2, 8, 2)),
    target       = "posterior_mean"
  )
  vdiffr::expect_doppelganger(
    "plot-sensitivity-heatmap",
    plot_sensitivity(sa, target = "posterior_mean")
  )
})

test_that("plot.bayprior_power_prior snapshot", {
  skip_if_not_installed("vdiffr")
  base <- elicit_beta(mean = 0.50, sd = 0.20, method = "moments")
  cp <- calibrate_power_prior(
    historical_data = list(type = "binary", x = 12, n = 40),
    current_data    = list(type = "binary", x = 18, n = 50),
    base_prior      = base,
    delta_grid      = seq(0.10, 1.0, by = 0.10),
    method          = "bayes_factor",
    target_bf       = 3
  )
  vdiffr::expect_doppelganger("plot-power-prior-calib", plot(cp))
})

test_that("plot.bayprior_robust snapshot", {
  skip_if_not_installed("vdiffr")
  inf <- elicit_beta(mean = 0.30, sd = 0.08, method = "moments",
                     label = "Response rate")
  rob <- suppressWarnings(robust_prior(inf, vague_weight = 0.20, label = "Robust"))
  vdiffr::expect_doppelganger("plot-robust-prior", plot(rob))
})