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

# -- exact = TRUE ----------------------------------------------------------

test_that("prior_conflict(exact = FALSE) reproduces the Normal approximation of 0.4.0", {
  prior <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments")
  cd    <- prior_conflict(prior, list(type = "binary", x = 18, n = 40), exact = FALSE)
  expect_equal(cd$box_pvalue_method, "normal_approx")
  expect_equal(cd$box_pvalue, 2 * stats::pnorm(-abs(
    (18 / 40 - cd$prior_mean) / sqrt(cd$prior_sd^2 + cd$obs_se^2)
  )))
})

test_that("default exact = NULL uses the exact p-value where a closed form exists", {
  prior_b <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments")
  ds_b    <- list(type = "binary", x = 18, n = 40)
  expect_equal(prior_conflict(prior_b, ds_b)$box_pvalue_method, "exact")
  expect_equal(prior_conflict(prior_b, ds_b)$box_pvalue,
               prior_conflict(prior_b, ds_b, exact = TRUE)$box_pvalue)

  prior_g <- elicit_gamma(mean = 5, sd = 2, method = "moments")
  expect_equal(prior_conflict(prior_g, list(type = "poisson", x = 15, n = 3))$box_pvalue_method,
               "exact")
  expect_equal(prior_conflict(prior_g, list(type = "survival", x = 15, n = 3))$box_pvalue_method,
               "exact")
})

test_that("default exact = NULL falls back to the approximation silently", {
  prior <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments")
  expect_no_message(
    cd <- prior_conflict(prior, list(type = "continuous", x = 0.4, sd = 0.15, n = 40))
  )
  expect_equal(cd$box_pvalue_method, "normal_approx")
})

test_that("non-whole-number counts fall back to the approximation rather than mis-indexing", {
  prior <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments")
  expect_no_message(
    cd <- prior_conflict(prior, list(type = "binary", x = 17.5, n = 40))
  )
  expect_equal(cd$box_pvalue_method, "normal_approx")
  expect_message(
    prior_conflict(prior, list(type = "binary", x = 17.5, n = 40), exact = TRUE),
    "no closed-form exact prior predictive"
  )
  # x outside 0..n (or a fractional n) is also unsupported by the exact check
  expect_null(.exact_box_pvalue(prior, "binary", x = 41, n = 40))
  expect_null(.exact_box_pvalue(prior, "binary", x = 5, n = 40.5))
})

test_that("`exact` is validated", {
  prior <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments")
  expect_error(prior_conflict(prior, list(type = "binary", x = 18, n = 40), exact = "yes"),
               "NULL, TRUE, or FALSE")
  expect_error(prior_conflict(prior, list(type = "binary", x = 18, n = 40), exact = NA),
               "NULL, TRUE, or FALSE")
})

test_that("zero events under an informative prior is not flagged as conflict (regression)", {
  # Beta(5, 45) prior (mean 0.1), n = 10, x = 0. Zero events has prior
  # predictive probability ~0.38, so there is no evidence of conflict. The
  # Normal approximation gave p ~ 0.017 here because its standard error is
  # estimated from the observed proportion (zero at x = 0); the exact
  # p-value is 1. In simulation this single outcome made the approximation
  # reject in ~39% of conflict-free datasets at the 5% level.
  prior <- elicit_beta(mean = 0.1, sd = sqrt(0.1 * 0.9 / 51), method = "moments")
  ds    <- list(type = "binary", x = 0, n = 10)
  cd    <- prior_conflict(prior, ds)
  expect_equal(cd$box_pvalue_method, "exact")
  expect_equal(cd$box_pvalue, 1)
  expect_false(cd$conflict_flag)
  # the approximation (0.4.0 behaviour) flags it, which is the defect
  expect_lt(prior_conflict(prior, ds, exact = FALSE)$box_pvalue, 0.05)
})

test_that("exact p-value is size-controlled under the prior predictive (Beta-Binomial)", {
  # Under no conflict (theta ~ prior), P(p_exact <= alpha) must not exceed alpha.
  a <- 5; b <- 45; n <- 10
  prior <- elicit_beta(mean = a / (a + b), sd = sqrt(a * b / ((a + b)^2 * (a + b + 1))),
                       method = "moments")
  k   <- 0:n
  pmf <- exp(lchoose(n, k) + lbeta(k + a, n - k + b) - lbeta(a, b))
  pe  <- vapply(k, function(x) prior_conflict(prior, list(type = "binary", x = x, n = n))$box_pvalue,
                numeric(1))
  for (al in c(0.05, 0.10)) expect_lte(sum(pmf[pe < al]), al + 1e-12)
})

test_that("prior_conflict(exact = TRUE) computes the exact Beta-Binomial p-value", {
  prior <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments")
  cd    <- prior_conflict(prior, list(type = "binary", x = 18, n = 40), exact = TRUE)
  expect_equal(cd$box_pvalue_method, "exact")

  # Independent brute-force Beta-Binomial computation, not sharing any code
  # path with the package implementation.
  a <- prior$params$alpha; b <- prior$params$beta; n <- 40; x <- 18
  k <- 0:n
  pmf <- exp(lchoose(n, k) + lbeta(k + a, n - k + b) - lbeta(a, b))
  pmf <- pmf / sum(pmf)
  expected_p <- sum(pmf[pmf <= pmf[x + 1] * (1 + 1e-8)])

  expect_equal(cd$box_pvalue, expected_p, tolerance = 1e-8)
  # The exact and Normal-approximation p-values should differ measurably
  # for this (meaningfully skewed) Beta(7.6, 14.1) prior -- guards against
  # exact = TRUE silently no-op'ing back to the approximation.
  cd_approx <- prior_conflict(prior, list(type = "binary", x = 18, n = 40), exact = FALSE)
  expect_gt(abs(cd$box_pvalue - cd_approx$box_pvalue), 0.01)
})

test_that("prior_conflict(exact = TRUE) computes the exact Gamma-Poisson p-value", {
  prior <- elicit_gamma(mean = 5, sd = 2, method = "moments")
  cd    <- prior_conflict(prior, list(type = "poisson", x = 15, n = 3), exact = TRUE)
  expect_equal(cd$box_pvalue_method, "exact")

  a <- prior$params$shape; rate <- prior$params$rate
  expected_p <- {
    nb_prob <- rate / (rate + 3)
    k <- 0:200
    pmf <- stats::dnbinom(k, size = a, prob = nb_prob)
    pmf <- pmf / sum(pmf)
    sum(pmf[pmf <= pmf[15 + 1] * (1 + 1e-8)])
  }
  expect_equal(cd$box_pvalue, expected_p, tolerance = 1e-6)
})

test_that("prior_conflict(exact = TRUE) applies the same exact check to survival data as poisson data", {
  # Gamma-Poisson and Gamma-Exponential conjugacy are the same model under
  # this package's (events, exposure) parameterisation -- "poisson" and
  # "survival" should therefore produce identical exact results for the
  # same (x, n), not one exact and the other falling back to the approximation.
  prior <- elicit_gamma(mean = 0.05, sd = 0.02, method = "moments")
  cd_survival <- prior_conflict(prior, list(type = "survival", x = 20, n = 300), exact = TRUE)
  cd_poisson  <- prior_conflict(prior, list(type = "poisson",  x = 20, n = 300), exact = TRUE)

  expect_equal(cd_survival$box_pvalue_method, "exact")
  expect_equal(cd_survival$box_pvalue, cd_poisson$box_pvalue)

  a <- prior$params$shape; rate <- prior$params$rate
  expected_p <- {
    nb_prob <- rate / (rate + 300)
    k <- 0:500
    pmf <- stats::dnbinom(k, size = a, prob = nb_prob)
    pmf <- pmf / sum(pmf)
    sum(pmf[pmf <= pmf[20 + 1] * (1 + 1e-8)])
  }
  expect_equal(cd_survival$box_pvalue, expected_p, tolerance = 1e-6)
})

test_that("prior_conflict(exact = TRUE) falls back to the approximation, with a message, when unsupported", {
  prior <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments")
  expect_message(
    cd <- prior_conflict(prior, list(type = "continuous", x = 0.4, sd = 0.15, n = 40), exact = TRUE),
    "no closed-form exact prior predictive"
  )
  expect_equal(cd$box_pvalue_method, "normal_approx")
})

test_that("prior_conflict(exact = TRUE) leaves surprise_index, kl, and overlap unaffected", {
  prior <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments")
  interim <- list(type = "binary", x = 18, n = 40)
  cd_approx <- prior_conflict(prior, interim, exact = FALSE)
  cd_exact  <- prior_conflict(prior, interim, exact = TRUE)
  expect_equal(cd_exact$surprise_index, cd_approx$surprise_index)
  expect_equal(cd_exact$kl_prior_likelihood, cd_approx$kl_prior_likelihood)
  expect_equal(cd_exact$overlap, cd_approx$overlap)
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

# -- Mixture / numerical prior predictive (development) -----------------------

# Brute-force reference: probability-ordered p-value from an explicit pmf.
.ref_p <- function(pmf, x) sum(pmf[pmf <= pmf[x + 1] * (1 + 1e-8)])

test_that("mixture of Betas gets the exact weighted Beta-Binomial p-value", {
  p1 <- elicit_beta(mean = 0.3, sd = 0.10, method = "moments")
  p2 <- elicit_beta(mean = 0.5, sd = 0.15, method = "moments")
  mix <- suppressWarnings(elicit_mixture(list(p1, p2), weights = c(0.7, 0.3)))
  n <- 25
  bb <- function(p) {
    k <- 0:n
    exp(lchoose(n, k) + lbeta(k + p$params$alpha, n - k + p$params$beta) -
          lbeta(p$params$alpha, p$params$beta))
  }
  pmf <- 0.7 * bb(p1) + 0.3 * bb(p2)
  for (x in c(0, 4, 8, 15, 25)) {
    r <- prior_conflict(mix, list(type = "binary", x = x, n = n))
    expect_equal(r$box_pvalue_method, "exact")
    expect_equal(r$box_pvalue, .ref_p(pmf, x), tolerance = 1e-8)
  }
})

test_that("mixture of Gammas gets the exact weighted Negative Binomial p-value", {
  g1 <- elicit_gamma(mean = 2, sd = 0.5, method = "moments")
  g2 <- elicit_gamma(mean = 4, sd = 1.0, method = "moments")
  mix <- suppressWarnings(elicit_mixture(list(g1, g2), weights = c(0.6, 0.4)))
  n <- 10
  nb <- function(g) stats::dnbinom(0:400, size = g$params$shape,
                                   prob = g$params$rate / (g$params$rate + n))
  pmf <- 0.6 * nb(g1) + 0.4 * nb(g2)
  r <- prior_conflict(mix, list(type = "poisson", x = 45, n = n))
  expect_equal(r$box_pvalue_method, "exact")
  expect_equal(r$box_pvalue, .ref_p(pmf, 45), tolerance = 1e-6)
})

test_that("robust prior (Beta + Normal vague) uses the numerical predictive", {
  b <- elicit_beta(mean = 0.3, sd = 0.10, method = "moments")
  rp <- suppressWarnings(robust_prior(b, vague_weight = 0.2))
  r <- prior_conflict(rp, list(type = "binary", x = 0, n = 10))
  expect_equal(r$box_pvalue_method, "numerical")
  expect_null(r$box_pvalue_caution)
  # reference: 0.8 * BetaBinomial + 0.2 * (Normal truncated to [0,1]) predictive
  n <- 10; k <- 0:n
  bb <- exp(lchoose(n, k) + lbeta(k + b$params$alpha, n - k + b$params$beta) -
              lbeta(b$params$alpha, b$params$beta))
  v <- rp$components[[2]]$params
  th <- seq(5e-6, 1 - 5e-6, by = 1e-5)
  w <- stats::dnorm(th, v$mu, v$sigma); w <- w / sum(w)
  vg <- vapply(k, function(kk) sum(w * stats::dbinom(kk, n, th)), numeric(1))
  expect_equal(r$box_pvalue, .ref_p(0.8 * bb + 0.2 * vg, 0), tolerance = 1e-4)
})

test_that("numerical path reproduces closed forms for conjugate priors", {
  b <- elicit_beta(mean = 0.3, sd = 0.10, method = "moments")
  a <- .predictive_pmf(b, "binary", 20, 5)
  q <- .predictive_pmf_numeric(b, "binary", 20, 5)
  expect_lt(max(abs(a - q)), 1e-5)
  g <- elicit_gamma(mean = 2, sd = 0.5, method = "moments")
  a <- .predictive_pmf(g, "poisson", 10, 5)
  q <- .predictive_pmf_numeric(g, "poisson", 10, 5)
  m <- min(length(a), length(q))
  expect_lt(max(abs(a[1:m] - q[1:m])), 1e-5)
})

test_that("non-conjugate single priors (lognormal rate) are handled numerically", {
  ln <- elicit_lognormal(mean = 2, sd = 0.5, method = "moments")
  r <- prior_conflict(ln, list(type = "poisson", x = 8, n = 10))
  expect_equal(r$box_pvalue_method, "numerical")
  expect_true(r$box_pvalue > 0 && r$box_pvalue <= 1)
})

test_that("exact = FALSE still returns the Normal approximation for mixtures", {
  b <- elicit_beta(mean = 0.3, sd = 0.10, method = "moments")
  rp <- suppressWarnings(robust_prior(b, vague_weight = 0.2))
  r <- prior_conflict(rp, list(type = "binary", x = 0, n = 10), exact = FALSE)
  expect_equal(r$box_pvalue_method, "normal_approx")
  expect_match(r$box_pvalue_caution, "boundary")
})

test_that("caution is raised for approximate p-values with small counts only", {
  p <- elicit_normal(mean = 0.3, sd = 0.1, method = "moments")
  # continuous data: approximation is exact, no caution
  r <- prior_conflict(p, list(type = "continuous", x = 0.4, n = 30, sd = 0.2))
  expect_null(r$box_pvalue_caution)
  b <- elicit_beta(mean = 0.3, sd = 0.10, method = "moments")
  r <- prior_conflict(b, list(type = "binary", x = 6, n = 20), exact = FALSE)
  expect_match(r$box_pvalue_caution, "Small observed counts")
  r <- prior_conflict(b, list(type = "binary", x = 300, n = 1000), exact = FALSE)
  expect_null(r$box_pvalue_caution)
})

test_that("severity follows the reported p-value on one scale", {
  b <- elicit_beta(mean = 0.3, sd = 0.10, method = "moments")
  r <- prior_conflict(b, list(type = "binary", x = 38, n = 40))
  expect_equal(r$conflict_severity, "severe")
  expect_lt(r$box_pvalue, 2 * stats::pnorm(-3))
  # the earlier rule coincides with the new one under the approximation
  r0 <- prior_conflict(b, list(type = "binary", x = 12, n = 30), exact = FALSE)
  old <- if (r0$box_pvalue >= 0.05) "none" else if (r0$surprise_index < 3) "mild" else "severe"
  expect_equal(r0$conflict_severity, old)
})

test_that("Gamma prior with shape <= 1 (heavy geometric tail) still gets an exact p", {
  g <- elicit_gamma(mean = 1, sd = 1, method = "moments")  # shape 1
  n <- 500
  pmf <- stats::dnbinom(0:60000, size = g$params$shape,
                        prob = g$params$rate / (g$params$rate + n))
  pmf <- pmf / sum(pmf)
  for (x in c(0, 100, 1500, 4000)) {
    r <- prior_conflict(g, list(type = "poisson", x = x, n = n))
    expect_equal(r$box_pvalue_method, "exact")
    expect_equal(r$box_pvalue, sum(pmf[pmf <= pmf[x + 1] * (1 + 1e-8)]),
                 tolerance = 1e-6)
  }
})
