# -- Binary --------------------------------------------------------------------

test_that("prior_conflict returns correct structure (binary, no conflict)", {
  prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  cd    <- prior_conflict(prior, list(type = "binary", x = 12, n = 40))
  expect_s3_class(cd, "bayprior_conflict")
  expect_true(is.numeric(cd$box_pvalue))
  expect_true(is.numeric(cd$surprise_index))
  expect_true(is.numeric(cd$kl_prior_likelihood))
  expect_true(is.numeric(cd$overlap))
  expect_true(is.logical(cd$conflict_flag))
  expect_true(cd$conflict_severity %in% c("none", "mild", "severe"))
  expect_true(nzchar(cd$recommendation))
  expect_gte(cd$box_pvalue, 0)
  expect_lte(cd$box_pvalue, 1)
  expect_gte(cd$overlap, 0)
  expect_lte(cd$overlap, 1)
  expect_false(cd$conflict_flag)
  expect_equal(cd$conflict_severity, "none")
})

test_that("prior_conflict detects severe conflict", {
  prior <- elicit_beta(mean = 0.30, sd = 0.05, method = "moments")
  cd <- prior_conflict(prior, list(type = "binary", x = 38, n = 40))
  expect_true(cd$conflict_flag)
  expect_equal(cd$conflict_severity, "severe")
  expect_lt(cd$box_pvalue, 0.01)
})

test_that("prior_conflict handles continuous data", {
  prior <- elicit_normal(mean = 0.0, sd = 0.3, method = "moments")
  cd    <- prior_conflict(prior, list(type = "continuous", x = 0.0, sd = 0.2, n = 50))
  expect_s3_class(cd, "bayprior_conflict")
  expect_false(cd$conflict_flag)
})

test_that("prior_conflict errors on non-bayprior input", {
  expect_error(prior_conflict(list(a = 1), list(type = "binary", x = 10, n = 40)))
})

test_that("prior_conflict custom alpha changes flag threshold", {
  prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  cd_05 <- prior_conflict(prior, list(type = "binary", x = 20, n = 40), alpha = 0.05)
  cd_20 <- prior_conflict(prior, list(type = "binary", x = 20, n = 40), alpha = 0.20)
  expect_gte(as.integer(cd_20$conflict_flag), as.integer(cd_05$conflict_flag))
})

test_that("print.bayprior_conflict runs without error", {
  prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  cd    <- prior_conflict(prior, list(type = "binary", x = 12, n = 40))
  expect_error(print(cd), NA)
})

test_that("print.bayprior_conflict does not error for all severities", {
  prior <- elicit_beta(mean = 0.30, sd = 0.05, method = "moments")
  cd_none <- prior_conflict(prior, list(type = "binary", x = 12, n = 40))
  expect_error(print(cd_none), NA)
  cd_sev <- prior_conflict(prior, list(type = "binary", x = 38, n = 40))
  expect_error(print(cd_sev), NA)
})

test_that("plot_prior_likelihood returns a ggplot", {
  prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  gp <- plot_prior_likelihood(prior, data_summary = list(type = "binary", x = 12, n = 40),
                               show_posterior = TRUE)
  expect_s3_class(gp, "gg")
})

test_that("plot_prior_likelihood without posterior returns ggplot", {
  prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  gp <- plot_prior_likelihood(prior, data_summary = list(type = "binary", x = 12, n = 40),
                               show_posterior = FALSE)
  expect_s3_class(gp, "gg")
})

test_that("plot_prior_likelihood continuous data returns ggplot", {
  prior <- elicit_normal(mean = 0.0, sd = 0.3, method = "moments")
  gp <- plot_prior_likelihood(prior, data_summary = list(type = "continuous", x = 0.2, sd = 0.25, n = 60),
                               show_posterior = TRUE)
  expect_s3_class(gp, "gg")
})

test_that("prior_conflict alpha field stored correctly", {
  prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  cd <- prior_conflict(prior, list(type = "binary", x = 12, n = 40), alpha = 0.10)
  expect_equal(cd$alpha, 0.10)
})

test_that("prior_conflict stores data_summary in output", {
  prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  cd <- prior_conflict(prior, list(type = "binary", x = 14, n = 40))
  expect_equal(cd$data_summary$type, "binary")
  expect_equal(cd$data_summary$x, 14)
  expect_equal(cd$data_summary$n, 40)
})

# -- Mahalanobis ---------------------------------------------------------------

test_that("conflict_mahalanobis returns correct structure", {
  mv <- conflict_mahalanobis(
    prior_means = c(0.35, 0.60),
    prior_cov   = matrix(c(0.010, 0.003, 0.003, 0.015), 2, 2),
    obs_means   = c(0.55, 0.58),
    obs_cov     = matrix(c(0.008, 0.002, 0.002, 0.010), 2, 2) / 50,
    labels      = c("ORR", "OS"), alpha = 0.05
  )
  expect_true(is.list(mv))
  expect_true(is.numeric(mv$mahal_distance))
  expect_true(is.numeric(mv$pvalue))
  expect_true(is.logical(mv$conflict_flag))
  expect_length(mv$marginal_z, 2)
  expect_equal(mv$labels, c("ORR", "OS"))
  expect_gte(mv$mahal_distance, 0)
  expect_gte(mv$pvalue, 0)
  expect_lte(mv$pvalue, 1)
})

test_that("conflict_mahalanobis detects conflict when means are far apart", {
  mv <- conflict_mahalanobis(
    prior_means = c(0.20, 0.20),
    prior_cov   = matrix(c(0.001, 0, 0, 0.001), 2, 2),
    obs_means   = c(0.80, 0.80),
    obs_cov     = matrix(c(0.001, 0, 0, 0.001), 2, 2),
    labels      = c("ep1", "ep2")
  )
  expect_true(mv$conflict_flag)
})

test_that("conflict_mahalanobis: no conflict when prior matches data closely", {
  pm   <- c(0.35, 0.60)
  pcov <- matrix(c(0.050, 0.003, 0.003, 0.050), 2, 2)
  om   <- c(0.36, 0.61)
  ocov <- matrix(c(1e-3, 1e-4, 1e-4, 1e-3), 2, 2)
  mv   <- conflict_mahalanobis(pm, pcov, om, ocov, labels = c("Response rate", "OS rate"))
  expect_s3_class(mv, "bayprior_conflict_mv")
  expect_false(mv$conflict_flag)
  expect_gt(mv$pvalue, 0.05)
  expect_named(mv$marginal_z, c("Response rate", "OS rate"))
})

test_that("conflict_mahalanobis: custom alpha threshold", {
  pm   <- c(0.35, 0.60)
  pcov <- matrix(c(0.010, 0.003, 0.003, 0.015), 2, 2)
  om   <- c(0.52, 0.58)
  ocov <- matrix(c(2e-4, 4e-5, 4e-5, 2e-4), 2, 2)
  mv_strict <- conflict_mahalanobis(pm, pcov, om, ocov, alpha = 0.01)
  mv_loose  <- conflict_mahalanobis(pm, pcov, om, ocov, alpha = 0.20)
  expect_equal(mv_strict$mahal_distance, mv_loose$mahal_distance)
  expect_true(is.logical(mv_strict$conflict_flag))
  expect_true(is.logical(mv_loose$conflict_flag))
})

test_that("conflict_mahalanobis: print returns invisibly", {
  mv <- conflict_mahalanobis(
    c(0.35, 0.60), matrix(c(0.010, 0.003, 0.003, 0.015), 2, 2),
    c(0.52, 0.58), matrix(c(2e-4, 4e-5, 4e-5, 2e-4), 2, 2),
    labels = c("Response rate", "OS rate")
  )
  expect_invisible(print(mv))
})

test_that("print.bayprior_conflict_mv works in non-interactive context", {
  mv <- conflict_mahalanobis(
    c(0.35, 0.60), matrix(c(0.010, 0.003, 0.003, 0.015), 2, 2),
    c(0.52, 0.58), matrix(c(2e-4, 4e-5, 4e-5, 2e-4), 2, 2),
    labels = c("Response rate", "OS rate")
  )
  withr::with_envvar(c(RSTUDIO = "", POSITRON = ""), {
    out <- capture.output(print(mv))
    expect_true(any(grepl("Mahalanobis", out, ignore.case = TRUE)))
  })
})

test_that("conflict_mahalanobis: default labels generated when NULL", {
  mv <- conflict_mahalanobis(
    c(0.35, 0.60), matrix(c(0.010, 0.003, 0.003, 0.015), 2, 2),
    c(0.52, 0.58), matrix(c(2e-4, 4e-5, 4e-5, 2e-4), 2, 2)
  )
  expect_true(all(grepl("^param_", names(mv$marginal_z))))
})

test_that("conflict_mahalanobis with custom alpha stored", {
  mv <- conflict_mahalanobis(
    c(0.35, 0.60), matrix(c(0.010, 0.003, 0.003, 0.015), 2, 2),
    c(0.40, 0.62), matrix(c(0.008, 0.002, 0.002, 0.010), 2, 2) / 50,
    labels = c("ORR", "OS"), alpha = 0.10
  )
  expect_equal(mv$alpha, 0.10)
})

# -- Poisson data type ---------------------------------------------------------

test_that("prior_conflict Poisson data returns valid diagnostics", {
  prior <- elicit_gamma(mean = 0.15, sd = 0.05, method = "moments", label = "Rate")
  cd <- prior_conflict(prior, list(type = "poisson", x = 15, n = 100))
  expect_s3_class(cd, "bayprior_conflict")
  expect_true(cd$conflict_severity %in% c("none", "mild", "severe"))
  expect_equal(cd$data_summary$type, "poisson")
})

test_that("prior_conflict Poisson conjugate update via .conjugate_update", {
  prior <- elicit_gamma(mean = 0.20, sd = 0.08, method = "moments", label = "Rate")
  ds    <- list(type = "poisson", x = 20, n = 80)
  post  <- bayprior:::.conjugate_update(prior, ds)
  expect_equal(post$dist, "gamma")
  expect_equal(post$params$shape, prior$params$shape + 20, tolerance = 1e-6)
  expect_equal(post$params$rate,  prior$params$rate  + 80, tolerance = 1e-6)
})

test_that("prior_conflict Poisson severe conflict detected", {
  prior <- elicit_gamma(mean = 0.05, sd = 0.02, method = "moments")
  cd <- prior_conflict(prior, data_summary = list(type = "poisson", x = 30, n = 100))
  expect_equal(cd$conflict_severity, "severe")
})

# -- Survival data type --------------------------------------------------------

test_that("prior_conflict survival data returns valid diagnostics", {
  prior <- elicit_gamma(mean = 0.05, sd = 0.02, method = "moments", label = "Hazard rate")
  cd <- prior_conflict(prior, data_summary = list(type = "survival", x = 20, n = 400))
  expect_s3_class(cd, "bayprior_conflict")
  expect_true(cd$box_pvalue >= 0 && cd$box_pvalue <= 1)
  expect_equal(cd$data_summary$type, "survival")
})

test_that("survival conjugate update gives Gamma posterior", {
  prior <- elicit_gamma(mean = 0.05, sd = 0.02, method = "moments")
  ds    <- list(type = "survival", x = 20, n = 400)
  post  <- bayprior:::.conjugate_update(prior, ds)
  expect_equal(post$dist, "gamma")
  expect_equal(post$params$shape, prior$params$shape + 20, tolerance = 1e-6)
  expect_equal(post$params$rate,  prior$params$rate  + 400, tolerance = 1e-6)
})
