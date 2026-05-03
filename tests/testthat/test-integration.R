test_that("full binary endpoint workflow runs without error", {
  # 1. Elicit from three experts
  e1 <- elicit_beta(mean = 0.28, sd = 0.08, method = "moments",
                    expert_id = "E1", label = "ORR")
  e2 <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments",
                    expert_id = "E2", label = "ORR")
  e3 <- elicit_beta(
    quantiles = c("0.10" = 0.15, "0.50" = 0.30, "0.90" = 0.52),
    expert_id = "E3", label = "ORR"
  )

  # 2. Pool into consensus
  con <- aggregate_experts(
    priors  = list(E1 = e1, E2 = e2, E3 = e3),
    weights = c(0.40, 0.35, 0.25),
    method  = "linear"
  )
  expect_s3_class(con, "bayprior")

  # 3. Conflict diagnostics
  data_obs <- list(type = "binary", x = 20, n = 55)
  cd <- prior_conflict(con, data_obs)
  expect_s3_class(cd, "bayprior_conflict")

  # 4. Sensitivity analysis
  sa <- sensitivity_grid(
    prior        = con,
    data_summary = data_obs,
    param_grid   = list(alpha = seq(1, 4, 1), beta = seq(2, 8, 2)),
    target       = c("posterior_mean", "prob_efficacy"),
    threshold    = 0.25
  )
  expect_s3_class(sa, "bayprior_sensitivity")

  # 5. Robust prior
  rob <- suppressWarnings(robust_prior(con, vague_weight = 0.20))
  expect_s3_class(rob, "bayprior")

  # 6. All plots run
  expect_s3_class(plot(con), "gg")
  expect_s3_class(plot_prior_likelihood(con, data_obs, show_posterior=TRUE), "gg")
  expect_s3_class(plot_tornado(sa), "gg")
  expect_s3_class(plot_sensitivity(sa, target = "posterior_mean"), "gg")
})

test_that("full continuous endpoint workflow runs without error", {
  prior <- elicit_normal(mean = 0.0, sd = 0.4, method = "moments",
                         label = "Log OR")
  data_obs <- list(type = "continuous", x = 0.35, sd = 0.3, n = 80)

  cd <- prior_conflict(prior, data_obs)
  expect_s3_class(cd, "bayprior_conflict")

  sa <- sensitivity_grid(
    prior        = prior,
    data_summary = data_obs,
    param_grid   = list(mu = c(-0.5, 0, 0.5), sigma = c(0.2, 0.4, 0.6)),
    target       = c("posterior_mean", "posterior_sd")
  )
  expect_s3_class(sa, "bayprior_sensitivity")
})

test_that("power prior workflow with normal prior runs without error", {
  base <- elicit_normal(mean = 0.0, sd = 0.5, method = "moments")
  cp <- calibrate_power_prior(
    historical_data = list(type = "continuous", x = 0.35, sd = 0.3, n = 60),
    current_data    = list(type = "continuous", x = 0.42, sd = 0.3, n = 80),
    base_prior      = base,
    target_bf       = 3,
    delta_grid      = seq(0.10, 1.0, by = 0.20),
    method          = "bayes_factor"
  )
  expect_s3_class(cp, "bayprior_power_prior")
  expect_s3_class(cp$power_prior, "bayprior")
})

test_that("sceptical vs enthusiastic pair produces different posteriors", {
  data_obs <- list(type = "binary", x = 22, n = 50)

  enthusiastic <- elicit_beta(mean = 0.45, sd = 0.08, method = "moments")
  sceptical    <- sceptical_prior(0.20, "beta", "moderate")

  cd_enth <- prior_conflict(enthusiastic, data_obs)
  cd_scep <- prior_conflict(sceptical,    data_obs)

  # Sceptical prior should show more surprise
  expect_gt(cd_scep$surprise_index, cd_enth$surprise_index)
})

test_that("roulette elicitation integrates with conflict check", {
  chips  <- c(0L, 1L, 3L, 7L, 9L, 7L, 4L, 2L, 1L, 1L)
  breaks <- seq(0, 1, by = 0.1)
  p  <- elicit_roulette(chips, breaks, family = "beta", label = "Rate")
  cd <- prior_conflict(p, list(type = "binary", x = 14, n = 40))

  expect_s3_class(p, "bayprior")
  expect_s3_class(cd, "bayprior_conflict")
})

test_that("mixture prior works through conflict and sensitivity", {
  e1 <- elicit_beta(mean = 0.25, sd = 0.08, method = "moments",
                    expert_id = "E1", label = "ORR")
  e2 <- elicit_beta(mean = 0.40, sd = 0.10, method = "moments",
                    expert_id = "E2", label = "ORR")
  mix <- aggregate_experts(list(E1=e1, E2=e2), weights=c(0.5,0.5))

  data_obs <- list(type = "binary", x = 14, n = 40)
  cd <- suppressWarnings(prior_conflict(mix, data_obs))
  expect_s3_class(cd, "bayprior_conflict")

  sa <- suppressWarnings(sensitivity_grid(
    prior        = mix,
    data_summary = data_obs,
    param_grid   = list(alpha = seq(1, 4, 1), beta = seq(2, 8, 2)),
    target       = "posterior_mean"
  ))
  expect_s3_class(sa, "bayprior_sensitivity")
})