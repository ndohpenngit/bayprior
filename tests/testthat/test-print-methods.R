# tests/testthat/test-print-methods.R
# Tests for print.bayprior* methods in both interactive and non-interactive contexts

test_that("print.bayprior works in non-interactive (callr-like) context", {
  prior <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments")
  withr::with_envvar(c(RSTUDIO = "", POSITRON = ""), {
    out <- capture.output(print(prior))
    expect_true(any(grepl("BETA", out, ignore.case = TRUE)))
    expect_true(any(grepl("7.6125", out)))
    expect_true(any(grepl("moments", out, ignore.case = TRUE)))
    expect_true(any(grepl("0.35", out)))
  })
})

test_that("print.bayprior works for mixture prior in non-interactive context", {
  e1 <- elicit_beta(mean = 0.30, sd = 0.08, method = "moments", expert_id = "E1")
  e2 <- elicit_beta(mean = 0.42, sd = 0.10, method = "moments", expert_id = "E2")
  pool <- aggregate_experts(list(E1 = e1, E2 = e2), weights = c(0.5, 0.5))
  withr::with_envvar(c(RSTUDIO = "", POSITRON = ""), {
    out <- capture.output(print(pool))
    expect_true(any(grepl("mixture|MIXTURE", out, ignore.case = TRUE)))
    expect_true(any(grepl("2", out)))
  })
})

test_that("print.bayprior_conflict works in non-interactive context", {
  prior <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments")
  cd <- prior_conflict(prior, list(type = "binary", x = 18, n = 40))
  withr::with_envvar(c(RSTUDIO = "", POSITRON = ""), {
    out <- capture.output(print(cd))
    expect_true(any(grepl("Box|p-value|pvalue", out, ignore.case = TRUE)))
    expect_true(any(grepl("severity|conflict", out, ignore.case = TRUE)))
  })
})

test_that("print.bayprior_conflict_mv works in non-interactive context", {
  mv <- conflict_mahalanobis(
    prior_means = c(0.35, 0.60),
    prior_cov   = matrix(c(0.010, 0.003, 0.003, 0.015), 2, 2),
    obs_means   = c(0.52, 0.58),
    obs_cov     = matrix(c(2e-4, 4e-5, 4e-5, 2e-4), 2, 2),
    labels      = c("Response rate", "OS rate")
  )
  withr::with_envvar(c(RSTUDIO = "", POSITRON = ""), {
    out <- capture.output(print(mv))
    expect_true(any(grepl("Mahalanobis", out, ignore.case = TRUE)))
    expect_true(any(grepl("p-value|pvalue|chi", out, ignore.case = TRUE)))
  })
})

test_that("print.bayprior_power_prior works in non-interactive context", {
  base <- elicit_beta(mean = 0.50, sd = 0.20, method = "moments")
  calib <- calibrate_power_prior(
    historical_data = list(type = "binary", x = 12, n = 40),
    current_data    = list(type = "binary", x = 18, n = 50),
    base_prior      = base,
    target_bf       = 3
  )
  withr::with_envvar(c(RSTUDIO = "", POSITRON = ""), {
    out <- capture.output(print(calib))
    expect_true(any(grepl("delta|power", out, ignore.case = TRUE)))
    expect_true(any(grepl("method|bayes", out, ignore.case = TRUE)))
  })
})

test_that("as_prior constructs bayprior object correctly", {
  p <- as_prior("beta", list(alpha = 2, beta = 8), label = "Historical")
  expect_s3_class(p, "bayprior")
  expect_equal(p$dist, "beta")
  expect_equal(p$params$alpha, 2)
  expect_equal(p$params$beta, 8)
  expect_equal(p$label, "Historical")
  expect_equal(p$method, "direct")
})

test_that(".bp_use_cli returns FALSE when RSTUDIO and POSITRON are unset", {
  withr::with_envvar(c(RSTUDIO = "", POSITRON = ""), {
    result <- bayprior:::.bp_use_cli()
    # In a test subprocess, isatty(stdout()) is also FALSE
    expect_false(result)
  })
})

test_that(".bp_h1 produces output in non-interactive context", {
  withr::with_envvar(c(RSTUDIO = "", POSITRON = ""), {
    out <- capture.output(bayprior:::.bp_h1("Test header"))
    expect_true(any(grepl("Test header", out)))
  })
})

test_that(".bp_li produces output in non-interactive context", {
  withr::with_envvar(c(RSTUDIO = "", POSITRON = ""), {
    out <- capture.output(bayprior:::.bp_li("Label: ", "value"))
    expect_true(any(grepl("Label", out)))
    expect_true(any(grepl("value", out)))
  })
})

test_that(".bp_alert produces output in non-interactive context", {
  withr::with_envvar(c(RSTUDIO = "", POSITRON = ""), {
    out <- capture.output(bayprior:::.bp_alert("test warning"))
    expect_true(any(grepl("test warning", out)))
  })
})

test_that(".bp_alert_info produces output in non-interactive context", {
  withr::with_envvar(c(RSTUDIO = "", POSITRON = ""), {
    out <- capture.output(bayprior:::.bp_alert_info("test info"))
    expect_true(any(grepl("test info", out)))
  })
})

test_that(".bp_alert_success produces output in non-interactive context", {
  withr::with_envvar(c(RSTUDIO = "", POSITRON = ""), {
    out <- capture.output(bayprior:::.bp_alert_success("test success"))
    expect_true(any(grepl("test success", out)))
  })
})

# -- .bp_alert_success non-interactive path ------------------------------------

test_that(".bp_alert_success produces output in non-interactive context", {
  withr::with_envvar(c(RSTUDIO = "", POSITRON = ""), {
    out <- capture.output(bayprior:::.bp_alert_success("calibration complete"))
    expect_true(any(grepl("calibration complete", out)))
  })
})

test_that("calibrate_power_prior completes in non-interactive context", {
  base <- elicit_beta(mean = 0.50, sd = 0.20, method = "moments")
  withr::with_envvar(c(RSTUDIO = "", POSITRON = ""), {
    cp <- calibrate_power_prior(
      historical_data = list(type = "binary", x = 12, n = 40),
      current_data    = list(type = "binary", x = 18, n = 50),
      base_prior      = base,
      delta_grid      = seq(0.20, 1.0, by = 0.20),
      method          = "bayes_factor"
    )
    # .bp_alert_success fires inside calibrate_power_prior -- just verify
    # the function completed and returned a valid object
    expect_s3_class(cp, "bayprior_power_prior")
    expect_true(is.numeric(cp$delta_opt))
  })
})
