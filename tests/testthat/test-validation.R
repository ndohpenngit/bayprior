# test-validation.R
# Tests for validation_utils.R internal functions
# ─────────────────────────────────────────────────────────────────────────────
# Key API facts discovered from function source:
#   .check_prior_data_compat(prior, data_summary)
#     - data_summary is a LIST (type=, x=, n=, ...), NOT a bare string
#     - always returns list(ok, msg, severity); never throws an error
#     - mismatched family/type returns ok=TRUE with severity="warning"
#
#   .check_pooling_compat(priors)
#     - takes a LIST of bayprior objects
#     - incompatible supports: returns list(ok=FALSE, severity="error")
#     - same support, mixed families: returns list(ok=TRUE, severity="warning")
#     - does NOT throw errors -- caller decides what to do with $ok
#
#   .check_sensitivity_compat(prior)
#     - takes ONLY the prior -- no param_grid argument
#     - returns list(ok, msg, severity)
#     - Exponential (single-param): ok=TRUE, severity="info"
# ─────────────────────────────────────────────────────────────────────────────

library(testthat)
library(bayprior)

# ── .check_prior_data_compat() ────────────────────────────────────────────────

test_that(".check_prior_data_compat passes for compatible family-type pairs", {
  r1 <- .check_prior_data_compat(
    elicit_beta(mean = 0.3, sd = 0.1, method = "moments"),
    list(type = "binary", x = 10, n = 30))
  expect_true(r1$ok)
  expect_equal(r1$severity, "none")

  r2 <- .check_prior_data_compat(
    elicit_normal(mean = 0.3, sd = 0.1, method = "moments"),
    list(type = "continuous", x = 0.3, sd = 0.1, n = 30))
  expect_true(r2$ok)
  expect_equal(r2$severity, "none")

  r3 <- .check_prior_data_compat(
    elicit_gamma(mean = 3.0, sd = 1.0, method = "moments"),
    list(type = "poisson", x = 9, n = 3))
  expect_true(r3$ok)
  expect_equal(r3$severity, "none")

  r4 <- .check_prior_data_compat(
    elicit_exponential(mean = 2.0, method = "moments"),
    list(type = "poisson", x = 5, n = 10))
  expect_true(r4$ok)
})

test_that(".check_prior_data_compat returns warning (not error) for mismatches", {
  # Normal supports binary and continuous -- NOT poisson
  r1 <- .check_prior_data_compat(
    elicit_normal(mean = 0.3, sd = 0.1, method = "moments"),
    list(type = "poisson", x = 10, n = 30))
  expect_true(r1$ok)          # ok=TRUE -- proceeds with approximation
  expect_equal(r1$severity, "warning")
  expect_true(nchar(r1$msg) > 0)

  # LogNormal only supports continuous -- binary is a mismatch
  r2 <- .check_prior_data_compat(
    elicit_lognormal(mean = 1.0, sd = 0.3, method = "moments"),
    list(type = "binary", x = 10, n = 30))
  expect_true(r2$ok)
  expect_equal(r2$severity, "warning")
})

test_that(".check_prior_data_compat returns list with expected fields", {
  r <- .check_prior_data_compat(
    elicit_beta(mean = 0.3, sd = 0.1, method = "moments"),
    list(type = "binary", x = 10, n = 30))
  expect_true("ok"       %in% names(r))
  expect_true("severity" %in% names(r))
})

# ── .check_pooling_compat() ───────────────────────────────────────────────────

test_that(".check_pooling_compat returns ok=TRUE for same-family priors", {
  p1 <- elicit_beta(mean = 0.3, sd = 0.1, method = "moments")
  p2 <- elicit_beta(mean = 0.5, sd = 0.1, method = "moments")
  p3 <- elicit_beta(mean = 0.7, sd = 0.1, method = "moments")
  r  <- .check_pooling_compat(list(p1, p2, p3))
  expect_true(r$ok)
  expect_equal(r$severity, "none")
})

test_that(".check_pooling_compat returns ok=FALSE for incompatible supports", {
  # Beta (unit support) + Normal (real support) -- incompatible
  p1 <- elicit_beta(mean  = 0.3, sd = 0.1, method = "moments")
  p2 <- elicit_normal(mean = 0.0, sd = 0.5, method = "moments")
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

# ── .check_sensitivity_compat() ───────────────────────────────────────────────

test_that(".check_sensitivity_compat returns ok=TRUE for standard priors", {
  for (pr in list(
    elicit_beta(mean = 0.3, sd = 0.1, method = "moments"),
    elicit_normal(mean = 0.0, sd = 0.5, method = "moments"),
    elicit_gamma(mean = 3.0, sd = 1.5, method = "moments"),
    elicit_lognormal(mean = 1.0, sd = 0.3, method = "moments")
  )) {
    r <- .check_sensitivity_compat(pr)
    expect_true(r$ok, label = paste("ok for dist:", pr$dist))
  }
})

test_that(".check_sensitivity_compat returns info for Exponential (single-param)", {
  pr <- elicit_exponential(mean = 2.0, method = "moments")
  r  <- .check_sensitivity_compat(pr)
  expect_true(r$ok)
  expect_equal(r$severity, "info")
  expect_true(nchar(r$msg) > 0)
})

test_that(".check_sensitivity_compat returns list with expected fields", {
  r <- .check_sensitivity_compat(
    elicit_beta(mean = 0.3, sd = 0.1, method = "moments"))
  expect_true("ok"       %in% names(r))
  expect_true("severity" %in% names(r))
})
