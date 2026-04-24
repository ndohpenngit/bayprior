test_that("elicit_beta moment matching recovers parameters", {
  fit <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments", label = "Test")
  expect_s3_class(fit, "bayprior")
  expect_equal(fit$dist, "beta")
  a <- fit$params$alpha; b <- fit$params$beta
  expect_equal(a / (a + b), 0.35, tolerance = 1e-6)
  expect_equal(sqrt(a * b / ((a+b)^2 * (a+b+1))), 0.10, tolerance = 1e-4)
})

test_that("elicit_beta quantile matching converges", {
  fit <- elicit_beta(
    quantiles = c("0.05"=0.10,"0.50"=0.35,"0.95"=0.65),
    method = "quantile", label = "Test")
  expect_s3_class(fit, "bayprior")
  expect_equal(stats::qbeta(0.5, fit$params$alpha, fit$params$beta),
               0.35, tolerance = 0.02)
})

test_that("elicit_normal works", {
  fit <- elicit_normal(mean = 0.5, sd = 0.2, method = "moments")
  expect_equal(fit$params$mu, 0.5); expect_equal(fit$params$sigma, 0.2)
})

test_that("elicit_lognormal moment matching correct", {
  fit <- elicit_lognormal(mean = 2, sd = 0.5, method = "moments")
  expect_s3_class(fit, "bayprior")
  expect_equal(fit$dist, "lognormal")
  recovered_mean <- exp(fit$params$meanlog + fit$params$sdlog^2 / 2)
  expect_equal(recovered_mean, 2, tolerance = 1e-4)
})

test_that("elicit_roulette fits distribution", {
  chips  <- c(0L, 1L, 3L, 6L, 5L, 3L, 1L, 1L)
  breaks <- seq(0, 0.8, length.out = 9)
  fit    <- elicit_roulette(chips, breaks, family = "beta", label = "RR")
  expect_s3_class(fit, "bayprior")
  expect_equal(fit$dist, "beta")
  expect_true(!is.null(fit$roulette))
})

test_that("elicit_mixture equal weights by default", {
  p1  <- elicit_beta(mean = 0.25, sd = 0.08, method = "moments", expert_id = "E1")
  p2  <- elicit_beta(mean = 0.40, sd = 0.10, method = "moments", expert_id = "E2")
  mix <- suppressMessages(elicit_mixture(list(p1, p2)))
  expect_equal(mix$weights, c(0.5, 0.5))
})

test_that("fit_summary has required fields for all families", {
  families <- list(
    elicit_beta(mean=0.3, sd=0.1, method="moments"),
    elicit_normal(mean=0.3, sd=0.1, method="moments"),
    elicit_gamma(mean=5, sd=2, method="moments"),
    elicit_lognormal(mean=2, sd=0.5, method="moments")
  )
  for (f in families) {
    s <- f$fit_summary
    expect_true(all(c("mean","sd","q025","q500","q975") %in% names(s)))
    expect_true(s$q025 < s$q500 && s$q500 < s$q975)
  }
})
