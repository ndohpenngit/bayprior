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

# -- Functional (non-snapshot) plot tests -------------------------------------

test_that("plot.bayprior_conflict: returns ggplot or list", {
  prior <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments",
                       label = "Response rate")
  cd    <- prior_conflict(prior, list(type = "binary", x = 18, n = 40))
  p     <- plot(cd)
  expect_true(inherits(p, "gg") || inherits(p, "patchwork") || is.list(p))
})

test_that("plot.bayprior_conflict: severe conflict case runs", {
  prior <- elicit_beta(mean = 0.10, sd = 0.03, method = "moments")
  cd    <- prior_conflict(prior, list(type = "binary", x = 38, n = 40))
  expect_no_error(plot(cd))
})

test_that("plot.bayprior_conflict works for a Normal prior (continuous data)
           -- previously errored: hardcoded stats::dbeta(grid, p$alpha, p$beta)
           assumed Beta, but p$alpha/p$beta are NULL for Normal priors", {
  prior <- elicit_normal(mean = 0.5, sd = 0.2, method = "moments",
                         label = "Mean difference")
  cd    <- prior_conflict(prior, list(type = "continuous", x = 0.6, n = 40, sd = 0.3))
  expect_no_error(plot(cd))
})

test_that("plot.bayprior_conflict works for a Gamma prior (poisson data)
           -- previously errored the same way as the Normal case above", {
  prior <- elicit_gamma(mean = 3.0, sd = 1.0, method = "moments",
                        label = "Event rate")
  cd    <- prior_conflict(prior, list(type = "poisson", x = 15, n = 40))
  expect_no_error(plot(cd))
})

test_that("plot.bayprior_conflict works for a Lognormal prior
           -- also previously errored the same way", {
  prior <- elicit_lognormal(mean = 1.5, sd = 0.5, method = "moments",
                            label = "Hazard ratio")
  cd    <- prior_conflict(prior, list(type = "continuous", x = 1.8, n = 40, sd = 0.6))
  expect_no_error(plot(cd))
})

test_that("plot.bayprior_conflict x-axis range is not clamped to [0, 1]
           for a prior whose actual support extends well past it", {
  prior <- elicit_gamma(mean = 20, sd = 5, method = "moments",
                        label = "Survival time")
  cd    <- prior_conflict(prior, list(type = "survival", x = 12, n = 40))
  p     <- plot(cd)
  # Previously hardcoded to min(1, ...): the x-axis would have been
  # incorrectly truncated to [0, 1], nowhere near this prior's actual mass.
  built <- ggplot2::ggplot_build(p)
  x_range <- built$layout$panel_params[[1]]$x.range
  expect_gt(x_range[2], 1)
})

test_that("plot_sensitivity: prob_efficacy target works", {
  prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  sa    <- sensitivity_grid(
    prior,
    data_summary = list(type = "binary", x = 14, n = 40),
    param_grid   = list(alpha = seq(1, 4, 1), beta = seq(2, 8, 2)),
    target       = "prob_efficacy",
    threshold    = 0.30
  )
  gp <- plot_sensitivity(sa, target = "prob_efficacy")
  expect_s3_class(gp, "gg")
})

test_that("plot_sensitivity: cri_width target works", {
  prior  <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  cri_sa <- sensitivity_cri(
    prior,
    data_summary = list(type = "binary", x = 14, n = 40),
    param_grid   = list(alpha = seq(1, 4, 1), beta = seq(2, 8, 2)),
    cri_level    = 0.95
  )
  gp <- plot_sensitivity(cri_sa, target = "cri_width")
  expect_s3_class(gp, "gg")
})