test_that("aggregate_experts produces consensus prior", {
  e1 <- elicit_beta(mean = 0.25, sd = 0.08, method = "moments", expert_id = "E1")
  e2 <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments", expert_id = "E2")
  e3 <- elicit_beta(mean = 0.30, sd = 0.09, method = "moments", expert_id = "E3")
  con <- aggregate_experts(
    priors  = list(E1 = e1, E2 = e2, E3 = e3),
    weights = c(0.4, 0.3, 0.3),
    method  = "linear"
  )
  expect_s3_class(con, "bayprior")
  expect_equal(con$dist, "mixture")
  expect_equal(sum(con$aggregation$weights), 1, tolerance = 1e-10)
  bc <- con$aggregation$disagreement
  expect_equal(nrow(bc), 3)
  expect_equal(ncol(bc), 3)
  expect_true(all(bc >= 0 & bc <= 1))
})

test_that("aggregate_experts: logarithmic pooling works", {
  e1 <- elicit_beta(mean = 0.25, sd = 0.08, method = "moments", expert_id = "E1", label = "ORR")
  e2 <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments", expert_id = "E2", label = "ORR")
  con <- aggregate_experts(
    priors  = list(E1 = e1, E2 = e2),
    weights = c(0.5, 0.5),
    method  = "logarithmic"
  )
  expect_s3_class(con, "bayprior")
  expect_equal(con$aggregation$method, "logarithmic")
})

test_that("aggregate_experts: logarithmic pooling with unequal weights", {
  e1 <- elicit_beta(mean = 0.20, sd = 0.08, method = "moments", expert_id = "E1")
  e2 <- elicit_beta(mean = 0.60, sd = 0.10, method = "moments", expert_id = "E2")
  pool_eq  <- aggregate_experts(list(E1 = e1, E2 = e2), weights = c(0.5, 0.5), method = "logarithmic")
  pool_hi2 <- aggregate_experts(list(E1 = e1, E2 = e2), weights = c(0.2, 0.8), method = "logarithmic")
  expect_gt(pool_hi2$fit_summary$mean, pool_eq$fit_summary$mean)
})

test_that("aggregate_experts: linear pooling with unequal weights", {
  e1 <- elicit_beta(mean = 0.20, sd = 0.08, method = "moments", expert_id = "E1")
  e2 <- elicit_beta(mean = 0.60, sd = 0.10, method = "moments", expert_id = "E2")
  pool <- aggregate_experts(list(E1 = e1, E2 = e2), weights = c(0.7, 0.3), method = "linear")
  expect_lt(pool$fit_summary$mean, 0.40)
  expect_gt(pool$fit_summary$mean, 0.20)
})

test_that("aggregate_experts: normal family logarithmic pooling works", {
  e1 <- elicit_normal(mean = 0.0, sd = 1.0, method = "moments", expert_id = "E1")
  e2 <- elicit_normal(mean = 0.5, sd = 1.0, method = "moments", expert_id = "E2")
  pool <- aggregate_experts(list(E1 = e1, E2 = e2), weights = c(0.5, 0.5), method = "logarithmic")
  expect_s3_class(pool, "bayprior")
  expect_true(pool$fit_summary$mean >= 0.0 && pool$fit_summary$mean <= 0.5)
})

test_that("aggregate_experts: Bhattacharyya agreement computed", {
  e1 <- elicit_beta(mean = 0.30, sd = 0.08, method = "moments", expert_id = "E1")
  e2 <- elicit_beta(mean = 0.31, sd = 0.08, method = "moments", expert_id = "E2")
  e3 <- elicit_beta(mean = 0.80, sd = 0.08, method = "moments", expert_id = "E3")
  pool_close   <- aggregate_experts(list(E1 = e1, E2 = e2), weights = c(0.5, 0.5))
  pool_distant <- aggregate_experts(list(E1 = e1, E2 = e3), weights = c(0.5, 0.5))
  expect_s3_class(pool_close,   "bayprior")
  expect_s3_class(pool_distant, "bayprior")
})

test_that("aggregate_experts: highly disagreeing experts completes without error", {
  e1 <- elicit_beta(mean = 0.10, sd = 0.05, method = "moments", expert_id = "E1", label = "ORR")
  e2 <- elicit_beta(mean = 0.90, sd = 0.05, method = "moments", expert_id = "E2", label = "ORR")
  # Very distant experts -- bayprior emits a cli alert (not an R warning condition)
  # and still returns a valid pooled prior
  pool <- aggregate_experts(list(E1 = e1, E2 = e2), weights = c(0.5, 0.5))
  expect_s3_class(pool, "bayprior")
  # Bhattacharyya coefficient between experts should be very low (high disagreement)
  expect_lt(pool$aggregation$disagreement[1, 2], 0.10)
})

test_that("aggregate_experts with equal weights (three experts)", {
  e1 <- elicit_beta(mean = 0.25, sd = 0.08, method = "moments", expert_id = "E1")
  e2 <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments", expert_id = "E2")
  e3 <- elicit_beta(mean = 0.30, sd = 0.09, method = "moments", expert_id = "E3")
  con <- aggregate_experts(list(E1 = e1, E2 = e2, E3 = e3), weights = c(1/3, 1/3, 1/3))
  expect_equal(sum(con$aggregation$weights), 1, tolerance = 1e-10)
})

# -- .pairwise_bhattacharyya and .eval_density internal paths ------------------

test_that("aggregate_experts: three-expert logarithmic pooling hits internal paths", {
  e1 <- elicit_beta(mean = 0.20, sd = 0.06, method = "moments", expert_id = "E1")
  e2 <- elicit_beta(mean = 0.35, sd = 0.08, method = "moments", expert_id = "E2")
  e3 <- elicit_beta(mean = 0.50, sd = 0.10, method = "moments", expert_id = "E3")
  pool <- aggregate_experts(
    list(E1 = e1, E2 = e2, E3 = e3),
    weights = c(1/3, 1/3, 1/3),
    method  = "logarithmic"
  )
  expect_s3_class(pool, "bayprior")
  expect_equal(pool$aggregation$method, "logarithmic")
})

test_that("aggregate_experts: gamma family logarithmic pooling", {
  e1 <- elicit_gamma(mean = 3.0, sd = 1.0, method = "moments", expert_id = "E1")
  e2 <- elicit_gamma(mean = 5.0, sd = 1.5, method = "moments", expert_id = "E2")
  pool <- aggregate_experts(
    list(E1 = e1, E2 = e2),
    weights = c(0.5, 0.5),
    method  = "logarithmic"
  )
  expect_s3_class(pool, "bayprior")
})

test_that("aggregate_experts: lognormal family logarithmic pooling", {
  e1 <- elicit_lognormal(mean = 1.0, sd = 0.3, method = "moments", expert_id = "E1")
  e2 <- elicit_lognormal(mean = 1.5, sd = 0.4, method = "moments", expert_id = "E2")
  pool <- aggregate_experts(
    list(E1 = e1, E2 = e2),
    weights = c(0.5, 0.5),
    method  = "logarithmic"
  )
  expect_s3_class(pool, "bayprior")
})
