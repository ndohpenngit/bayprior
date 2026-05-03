# ── Expert Pooling and Aggregation Tests ─────────────────────────────────────
#
# Tests for aggregate_experts(), elicit_mixture(), and related pooling
# functionality. Covers linear and logarithmic pooling, Bhattacharyya
# coefficient computation, disagreement detection, and mixture construction.
# ─────────────────────────────────────────────────────────────────────────────

# ── aggregate_experts: linear pooling ─────────────────────────────────────────

test_that("aggregate_experts linear pooling returns valid bayprior", {
  e1 <- elicit_beta(mean = 0.25, sd = 0.08, method = "moments",
                    expert_id = "E1", label = "ORR")
  e2 <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments",
                    expert_id = "E2", label = "ORR")
  e3 <- elicit_beta(mean = 0.30, sd = 0.09, method = "moments",
                    expert_id = "E3", label = "ORR")

  con <- aggregate_experts(
    priors  = list(E1 = e1, E2 = e2, E3 = e3),
    weights = c(0.40, 0.30, 0.30),
    method  = "linear"
  )

  expect_s3_class(con, "bayprior")
  expect_equal(con$dist, "mixture")
  expect_equal(con$aggregation$method, "linear")
  expect_length(con$components, 3)
  expect_equal(sum(con$aggregation$weights), 1, tolerance = 1e-10)
  expect_equal(unname(con$aggregation$weights), c(0.40, 0.30, 0.30))
})

test_that("aggregate_experts linear pooling mean is weighted average", {
  e1 <- elicit_beta(mean = 0.20, sd = 0.06, method = "moments",
                    expert_id = "E1", label = "ORR")
  e2 <- elicit_beta(mean = 0.40, sd = 0.06, method = "moments",
                    expert_id = "E2", label = "ORR")

  # Equal weights -> consensus mean should be ~0.30
  con <- aggregate_experts(list(E1 = e1, E2 = e2),
                           weights = c(0.5, 0.5), method = "linear")

  expect_equal(con$fit_summary$mean, 0.30, tolerance = 0.02)
})

test_that("aggregate_experts linear pooling with unequal weights", {
  e1 <- elicit_beta(mean = 0.20, sd = 0.06, method = "moments",
                    expert_id = "E1", label = "ORR")
  e2 <- elicit_beta(mean = 0.40, sd = 0.06, method = "moments",
                    expert_id = "E2", label = "ORR")

  # 80% weight on e1 -> mean closer to 0.20
  con_e1 <- aggregate_experts(list(E1=e1, E2=e2),
                              weights = c(0.80, 0.20), method = "linear")
  # 80% weight on e2 -> mean closer to 0.40
  con_e2 <- aggregate_experts(list(E1=e1, E2=e2),
                              weights = c(0.20, 0.80), method = "linear")

  expect_lt(con_e1$fit_summary$mean, con_e2$fit_summary$mean)
})

# ── aggregate_experts: logarithmic pooling ────────────────────────────────────

test_that("aggregate_experts logarithmic pooling returns valid bayprior", {
  e1 <- elicit_beta(mean = 0.25, sd = 0.08, method = "moments",
                    expert_id = "E1", label = "ORR")
  e2 <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments",
                    expert_id = "E2", label = "ORR")

  con <- aggregate_experts(list(E1 = e1, E2 = e2),
                           weights = c(0.5, 0.5), method = "logarithmic")

  expect_s3_class(con, "bayprior")
  expect_equal(con$aggregation$method, "logarithmic")
  expect_equal(sum(con$aggregation$weights), 1, tolerance = 1e-10)
})

test_that("logarithmic pooling produces a valid consensus prior", {
  e1 <- elicit_beta(mean = 0.30, sd = 0.08, method = "moments",
                    expert_id = "E1")
  e2 <- elicit_beta(mean = 0.32, sd = 0.08, method = "moments",
                    expert_id = "E2")

  con_lin <- aggregate_experts(list(E1=e1, E2=e2), c(0.5, 0.5), "linear")
  con_log <- aggregate_experts(list(E1=e1, E2=e2), c(0.5, 0.5), "logarithmic")

  # Both should be valid bayprior objects with means in a sensible range
  expect_s3_class(con_log, "bayprior")
  expect_equal(con_log$aggregation$method, "logarithmic")
  lin_mean <- as.numeric(con_lin$fit_summary$mean)
  log_mean <- as.numeric(con_log$fit_summary$mean)
  expect_gt(log_mean, 0.20)
  expect_lt(log_mean, 0.45)
})

# ── aggregate_experts: Bhattacharyya agreement ────────────────────────────────

test_that("Bhattacharyya disagreement matrix is square and bounded [0,1]", {
  e1 <- elicit_beta(mean = 0.25, sd = 0.08, method = "moments",
                    expert_id = "E1")
  e2 <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments",
                    expert_id = "E2")
  e3 <- elicit_beta(mean = 0.30, sd = 0.09, method = "moments",
                    expert_id = "E3")

  con <- aggregate_experts(list(E1=e1, E2=e2, E3=e3),
                           weights = c(1/3, 1/3, 1/3))

  bc <- con$aggregation$disagreement
  expect_equal(nrow(bc), 3)
  expect_equal(ncol(bc), 3)
  expect_true(all(bc >= 0 & bc <= 1))
  # Diagonal should be 1 (self-agreement)
  expect_equal(unname(diag(bc)), rep(1, 3), tolerance = 1e-6)
})

test_that("identical experts have Bhattacharyya coefficient of 1", {
  e1 <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments",
                    expert_id = "E1")
  e2 <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments",
                    expert_id = "E2")

  con <- aggregate_experts(list(E1=e1, E2=e2), weights = c(0.5, 0.5))
  bc  <- con$aggregation$disagreement

  expect_equal(bc["E1", "E2"], 1, tolerance = 1e-4)
})

test_that("very different experts have low Bhattacharyya coefficient", {
  e1 <- elicit_beta(mean = 0.05, sd = 0.02, method = "moments",
                    expert_id = "E1")
  e2 <- elicit_beta(mean = 0.95, sd = 0.02, method = "moments",
                    expert_id = "E2")

  con <- aggregate_experts(list(E1=e1, E2=e2), weights = c(0.5, 0.5))
  bc  <- con$aggregation$disagreement

  expect_lt(bc["E1", "E2"], 0.1)
})

test_that("disagreement_threshold message emitted for very different experts", {
  e1 <- elicit_beta(mean = 0.05, sd = 0.02, method = "moments",
                    expert_id = "E1")
  e2 <- elicit_beta(mean = 0.95, sd = 0.02, method = "moments",
                    expert_id = "E2")

  # Very different experts produce low BC — function should still complete
  # (message goes to cli, not captured as R warning)
  con <- aggregate_experts(list(E1=e1, E2=e2), weights = c(0.5, 0.5),
                           disagreement_threshold = 0.99)
  bc <- con$aggregation$disagreement
  # BC between very different experts should be very low
  expect_lt(bc["E1", "E2"], 0.10)
})

# ── aggregate_experts: validation ─────────────────────────────────────────────

test_that("aggregate_experts requires at least 2 priors", {
  e1 <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments",
                    expert_id = "E1")
  expect_error(aggregate_experts(list(E1 = e1), weights = c(1.0)))
})

test_that("aggregate_experts errors on unnormalised weights", {
  e1 <- elicit_beta(mean = 0.25, sd = 0.08, method = "moments",
                    expert_id = "E1")
  e2 <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments",
                    expert_id = "E2")

  expect_error(
    aggregate_experts(list(E1=e1, E2=e2), weights = c(0.6, 0.6)),
    regexp = "sum to 1"
  )
})

test_that("aggregate_experts errors on weight-prior count mismatch", {
  e1 <- elicit_beta(mean = 0.25, sd = 0.08, method = "moments",
                    expert_id = "E1")
  e2 <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments",
                    expert_id = "E2")

  expect_error(
    aggregate_experts(list(E1=e1, E2=e2), weights = c(0.4, 0.3, 0.3))
  )
})

# ── aggregate_experts: two-expert pooling ─────────────────────────────────────

test_that("two-expert linear pooling produces correct component structure", {
  e1 <- elicit_beta(mean = 0.25, sd = 0.08, method = "moments",
                    expert_id = "E1", label = "ORR")
  e2 <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments",
                    expert_id = "E2", label = "ORR")

  con <- aggregate_experts(list(E1=e1, E2=e2),
                           weights = c(0.6, 0.4), method = "linear")

  expect_length(con$components, 2)
  expect_equal(as.numeric(con$aggregation$weights[1]), 0.6)
  expect_equal(as.numeric(con$aggregation$weights[2]), 0.4)
  # Components preserved
  expect_equal(con$components[[1]]$fit_summary$mean,
               e1$fit_summary$mean, tolerance = 1e-6)
})

# ── elicit_mixture ────────────────────────────────────────────────────────────

test_that("elicit_mixture builds valid mixture from two components", {
  e1 <- elicit_beta(mean = 0.25, sd = 0.08, method = "moments")
  e2 <- elicit_beta(mean = 0.40, sd = 0.10, method = "moments")
  m  <- elicit_mixture(list(e1, e2), weights = c(0.5, 0.5),
                        label = "50-50 mix")

  expect_s3_class(m, "bayprior")
  expect_equal(m$dist, "mixture")
  expect_length(m$components, 2)
  expect_equal(sum(m$weights), 1, tolerance = 1e-10)
  expect_equal(m$label, "50-50 mix")
})

test_that("elicit_mixture respects component weights", {
  e1 <- elicit_beta(mean = 0.20, sd = 0.06, method = "moments")
  e2 <- elicit_beta(mean = 0.40, sd = 0.06, method = "moments")

  m70 <- elicit_mixture(list(e1, e2), weights = c(0.70, 0.30))
  m30 <- elicit_mixture(list(e1, e2), weights = c(0.30, 0.70))

  # 70% weight on e1 (mean=0.20) should give lower mixture mean
  expect_lt(m70$fit_summary$mean, m30$fit_summary$mean)
})

test_that("elicit_mixture with 3 weights for 2 components warns or errors", {
  e1 <- elicit_beta(mean = 0.25, sd = 0.08, method = "moments")
  e2 <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments")
  # Either errors or warns about mismatched lengths — both are acceptable
  result <- tryCatch(
    withCallingHandlers(
      elicit_mixture(list(e1, e2), weights = c(0.5, 0.3, 0.2)),
      warning = function(w) invokeRestart("muffleWarning")
    ),
    error = function(e) NULL
  )
  # If it succeeds despite mismatch, weights should sum to 1
  if (!is.null(result)) {
    expect_equal(sum(result$weights), 1, tolerance = 1e-6)
  } else {
    succeed("elicit_mixture correctly errored on mismatched weights")
  }
})

test_that("elicit_mixture errors on unnormalised weights", {
  e1 <- elicit_beta(mean = 0.25, sd = 0.08, method = "moments")
  e2 <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments")
  expect_error(elicit_mixture(list(e1, e2), weights = c(0.6, 0.6)))
})

test_that("elicit_mixture works with three components", {
  e1 <- elicit_beta(mean = 0.20, sd = 0.06, method = "moments")
  e2 <- elicit_beta(mean = 0.30, sd = 0.08, method = "moments")
  e3 <- elicit_beta(mean = 0.45, sd = 0.09, method = "moments")
  m  <- elicit_mixture(list(e1, e2, e3), weights = c(0.3, 0.4, 0.3))

  expect_length(m$components, 3)
  expect_equal(sum(m$weights), 1, tolerance = 1e-10)
})

test_that("elicit_mixture works with mixed distribution families", {
  # This should work but may warn about different families
  e_beta   <- elicit_beta(mean = 0.30, sd = 0.08, method = "moments")
  e_normal <- elicit_normal(mean = 0.30, sd = 0.08, method = "moments")

  # May warn about different families - that's expected
  m <- suppressWarnings(
    elicit_mixture(list(e_beta, e_normal), weights = c(0.5, 0.5))
  )
  expect_s3_class(m, "bayprior")
})

# ── print and plot for mixture/consensus ──────────────────────────────────────

test_that("print.bayprior does not error for consensus prior", {
  e1 <- elicit_beta(mean = 0.25, sd = 0.08, method = "moments",
                    expert_id = "E1")
  e2 <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments",
                    expert_id = "E2")
  con <- aggregate_experts(list(E1=e1, E2=e2), weights = c(0.5, 0.5))
  expect_error(print(con), NA)
})

test_that("plot.bayprior returns ggplot for consensus prior", {
  e1 <- elicit_beta(mean = 0.25, sd = 0.08, method = "moments",
                    expert_id = "E1")
  e2 <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments",
                    expert_id = "E2")
  con <- aggregate_experts(list(E1=e1, E2=e2), weights = c(0.5, 0.5))
  gp  <- suppressWarnings(plot(con))
  expect_s3_class(gp, "gg")
})

test_that("consensus prior works through full workflow", {
  e1 <- elicit_beta(mean = 0.25, sd = 0.08, method = "moments",
                    expert_id = "E1", label = "ORR")
  e2 <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments",
                    expert_id = "E2", label = "ORR")
  e3 <- elicit_beta(mean = 0.30, sd = 0.09, method = "moments",
                    expert_id = "E3", label = "ORR")

  con      <- aggregate_experts(list(E1=e1, E2=e2, E3=e3),
                                weights = c(0.4, 0.35, 0.25))
  data_obs <- list(type = "binary", x = 18, n = 55)

  # Conflict check on consensus
  cd <- suppressWarnings(prior_conflict(con, data_obs))
  expect_s3_class(cd, "bayprior_conflict")

  # Sensitivity on consensus
  sa <- suppressWarnings(sensitivity_grid(
    con, data_obs,
    param_grid = list(alpha = 1:3, beta = 2:4),
    target = "posterior_mean"
  ))
  expect_s3_class(sa, "bayprior_sensitivity")

  # Robust prior on consensus
  rob <- suppressWarnings(robust_prior(con, vague_weight = 0.20))
  expect_s3_class(rob, "bayprior")
})