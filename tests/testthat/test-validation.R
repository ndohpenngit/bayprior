# test-validation.R
# Tests for validation_utils.R internal functions
# -----------------------------------------------------------------------------
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
# -----------------------------------------------------------------------------

library(testthat)
library(bayprior)

# -- .check_prior_data_compat() ------------------------------------------------

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

# -- .check_pooling_compat() ---------------------------------------------------

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

# -- .check_sensitivity_compat() -----------------------------------------------

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

# -- .dist_support() -----------------------------------------------------------
# No prior direct coverage -- only exercised incidentally via other functions.

test_that(".dist_support classifies each known family correctly", {
  expect_equal(.dist_support("beta"),        "unit")
  expect_equal(.dist_support("normal"),      "real")
  expect_equal(.dist_support("gamma"),       "positive")
  expect_equal(.dist_support("lognormal"),   "positive")
  expect_equal(.dist_support("exponential"), "positive")
  expect_equal(.dist_support("weibull"),     "positive")
  expect_equal(.dist_support("mixture"),     "mixed")
})

test_that(".dist_support falls back to \"unknown\" for an unrecognised family", {
  # e.g. "log_pool" -- .log_pool() sets dist = "log_pool", not "mixture",
  # so it is not one of the switch()'s named cases and falls through.
  expect_equal(.dist_support("log_pool"), "unknown")
  expect_equal(.dist_support("nonsense"), "unknown")
})

# -- .prior_support() ----------------------------------------------------------
# No prior coverage at all.

test_that(".prior_support passes through for non-mixture priors", {
  expect_equal(.prior_support(elicit_beta(mean = 0.3, sd = 0.1, method = "moments")),
              "unit")
  expect_equal(.prior_support(elicit_gamma(mean = 3, sd = 1, method = "moments")),
              "positive")
})

test_that(".prior_support returns a single support for a same-support mixture", {
  e1  <- elicit_gamma(mean = 3, sd = 1, method = "moments", expert_id = "E1")
  e2  <- elicit_exponential(mean = 2, method = "moments", expert_id = "E2")
  # aggregate_experts() -> elicit_mixture() warns when components share a
  # support but differ in family, since the mixture density then has to be
  # evaluated numerically rather than analytically. Expected here, since
  # this test specifically constructs a mixed-family, same-support pool.
  expect_warning(
    mix <- aggregate_experts(list(E1 = e1, E2 = e2), weights = c(0.5, 0.5),
                             method = "linear"),
    "different distribution families"
  )
  expect_equal(.prior_support(mix), "positive")
})

test_that(".prior_support returns \"mixed\" for an incompatible-support mixture", {
  # Bypasses aggregate_experts()'s own compatibility check (which would
  # normally reject this combination) by constructing the mixture object
  # directly, to isolate .prior_support()'s own logic.
  e1  <- elicit_beta(mean = 0.3, sd = 0.1, method = "moments", expert_id = "E1")
  e2  <- elicit_normal(mean = 0.0, sd = 0.5, method = "moments", expert_id = "E2")
  mix <- structure(
    list(dist = "mixture", components = list(e1, e2), weights = c(0.5, 0.5)),
    class = "bayprior"
  )
  expect_equal(.prior_support(mix), "mixed")
})

# -- .check_prior_data_compat() -- mixture and log_pool handling -------------
# The existing tests above never pass a mixture or log-pooled prior.

test_that(".check_prior_data_compat resolves a mixture to its dominant component", {
  e1  <- elicit_beta(mean = 0.30, sd = 0.08, method = "moments", expert_id = "E1")
  e2  <- elicit_beta(mean = 0.42, sd = 0.10, method = "moments", expert_id = "E2")
  pool <- aggregate_experts(list(E1 = e1, E2 = e2), weights = c(0.7, 0.3),
                            method = "linear")
  # Dominant component (weight 0.7) is Beta -> binary data is compatible.
  r <- .check_prior_data_compat(pool, list(type = "binary", x = 18, n = 40))
  expect_true(r$ok)
  expect_equal(r$severity, "none")
})

test_that(".check_prior_data_compat silently skips validation for a
           logarithmically-pooled prior", {
  # .log_pool() sets dist = "log_pool", which is neither "mixture" (so the
  # dominant-component extraction at line 35 never fires) nor one of the six
  # keys in the compat list -- so `recommended` is NULL and the function
  # returns ok=TRUE/severity="none" unconditionally, regardless of whether
  # the data type actually suits the pooled experts' family. This documents
  # that behavior explicitly rather than leaving it silently untested.
  e1  <- elicit_beta(mean = 0.30, sd = 0.08, method = "moments", expert_id = "E1")
  e2  <- elicit_beta(mean = 0.42, sd = 0.10, method = "moments", expert_id = "E2")
  pool_log <- aggregate_experts(list(E1 = e1, E2 = e2), weights = c(0.5, 0.5),
                                method = "logarithmic")
  expect_equal(pool_log$dist, "log_pool")

  # Even a data type nothing here supports (e.g. survival, unrelated to
  # Beta experts) is reported as fully compatible, with no warning.
  r <- .check_prior_data_compat(pool_log, list(type = "survival", x = 5, n = 40))
  expect_true(r$ok)
  expect_equal(r$severity, "none")
  expect_null(r$msg)
})

# -- .check_pooling_compat() -- nested mixture ---------------------------------

test_that(".check_pooling_compat treats a nested mixture as an incompatible support", {
  # A mixture passed inside the list of priors to pool: .dist_support()
  # classifies dist = "mixture" as support "mixed", which never matches
  # another prior's "unit"/"positive"/"real" support, so this is always
  # flagged as an incompatible-support error rather than silently accepted.
  beta1 <- elicit_beta(mean = 0.3, sd = 0.1, method = "moments", expert_id = "E1")
  beta2 <- elicit_beta(mean = 0.5, sd = 0.1, method = "moments", expert_id = "E2")
  inner_pool <- aggregate_experts(list(E1 = beta1, E2 = beta2),
                                  weights = c(0.5, 0.5), method = "linear")
  beta3 <- elicit_beta(mean = 0.4, sd = 0.1, method = "moments", expert_id = "E3")

  r <- .check_pooling_compat(list(inner_pool, beta3))
  expect_false(r$ok)
  expect_equal(r$severity, "error")
})

# -- .check_sensitivity_compat() -- mixture branches (previously 0% covered) --

test_that(".check_sensitivity_compat warns for a same-support mixed-family mixture", {
  e1  <- elicit_gamma(mean = 3, sd = 1, method = "moments", expert_id = "E1")
  e2  <- elicit_exponential(mean = 2, method = "moments", expert_id = "E2")
  expect_warning(
    pool <- aggregate_experts(list(E1 = e1, E2 = e2), weights = c(0.5, 0.5),
                              method = "linear"),
    "different distribution families"
  )
  r <- .check_sensitivity_compat(pool)
  expect_true(r$ok)
  expect_equal(r$severity, "warning")
  expect_true(nchar(r$msg) > 0)
})

test_that(".check_sensitivity_compat errors for an incompatible-support mixture", {
  e1  <- elicit_beta(mean = 0.3, sd = 0.1, method = "moments", expert_id = "E1")
  e2  <- elicit_normal(mean = 0.0, sd = 0.5, method = "moments", expert_id = "E2")
  mix <- structure(
    list(dist = "mixture", components = list(e1, e2), weights = c(0.5, 0.5)),
    class = "bayprior"
  )
  r <- .check_sensitivity_compat(mix)
  expect_false(r$ok)
  expect_equal(r$severity, "error")
})

# -- .validation_alert() -- 0% covered, no tests existed ----------------------

test_that(".validation_alert returns NULL for NULL or empty message", {
  expect_null(.validation_alert(NULL))
  expect_null(.validation_alert(""))
})

test_that(".validation_alert renders each severity with the right CSS class", {
  info_alert <- .validation_alert("An info message", severity = "info")
  expect_true(grepl("alert-info", as.character(info_alert), fixed = TRUE))

  warn_alert <- .validation_alert("A warning message", severity = "warning")
  expect_true(grepl("alert-warning", as.character(warn_alert), fixed = TRUE))

  err_alert <- .validation_alert("An error message", severity = "error")
  expect_true(grepl("alert-danger", as.character(err_alert), fixed = TRUE))
})

test_that(".validation_alert falls back to the info style for an unrecognised severity", {
  alert <- .validation_alert("A message", severity = "not_a_real_severity")
  expect_true(grepl("alert-info", as.character(alert), fixed = TRUE))
})