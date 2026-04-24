test_that("aggregate_experts normalises weights", {
  p1  <- elicit_beta(mean=0.25, sd=0.08, method="moments", expert_id="E1")
  p2  <- elicit_beta(mean=0.40, sd=0.10, method="moments", expert_id="E2")
  agg <- suppressMessages(aggregate_experts(list(E1=p1,E2=p2), weights=c(3,1)))
  expect_equal(sum(agg$aggregation$weights), 1, tolerance=1e-8)
  expect_equal(agg$aggregation$weights[["E1"]], 0.75)
})

test_that("aggregate_experts requires >= 2 priors", {
  p1 <- elicit_beta(mean=0.30, sd=0.10, method="moments")
  expect_error(aggregate_experts(list(p1)))
})

test_that("Bhattacharyya matrix is symmetric with 1 on diagonal", {
  p1  <- elicit_beta(mean=0.25, sd=0.08, method="moments", expert_id="E1")
  p2  <- elicit_beta(mean=0.40, sd=0.10, method="moments", expert_id="E2")
  agg <- suppressMessages(aggregate_experts(list(E1=p1,E2=p2)))
  bc  <- agg$aggregation$disagreement
  expect_equal(bc[1,1], 1, tolerance=1e-6)
  expect_equal(bc[1,2], bc[2,1], tolerance=1e-8)
})

test_that("sceptical_prior builds valid prior", {
  sc <- sceptical_prior(null_value=0, family="normal", strength="moderate")
  expect_s3_class(sc, "bayprior")
  expect_equal(sc$prior_type, "sceptical")
  expect_equal(sc$fit_summary$mean, 0, tolerance=1e-8)
})

test_that("robust_prior creates mixture prior", {
  inf <- elicit_normal(mean=0.30, sd=0.10, method="moments")
  rob <- robust_prior(inf, vague_weight=0.20)
  expect_equal(rob$dist, "mixture")
  expect_equal(rob$prior_type, "robust")
  expect_equal(rob$vague_weight, 0.20)
})
