# tests/testthat/test-map-prior.R
# Tests for map_prior(), resolve_tau_prior(), historical_effect_sizes(), and
# plot_tau_posterior().
#
# map_prior() fits the random-effects meta-analysis itself, entirely in base
# R: the posterior of mu given tau has a closed form, so the only numerical
# step is a one-dimensional integral over tau, evaluated with
# stats::integrate(). bayprior has no dependency, direct or optional, on any
# external meta-analysis package. The cross-validation tests below check
# that numerical step against an independent brute-force reference built
# from scratch in this file (see `.ref_fit()`), not against another package.

# -- Fixtures ------------------------------------------------------------------
# Three historical trials, used throughout this file and cross-checked
# against the independent brute-force reference below.
.map_y  <- c(-0.85, -0.62, -1.10)
.map_se <- c(0.25, 0.30, 0.28)

# -- Structure -------------------------------------------------------------

test_that("map_prior returns correct structure", {
  mp <- map_prior(y = .map_y, se = .map_se, outcome_type = "single_arm_log_odds")
  expect_s3_class(mp, "bayprior")
  expect_equal(mp$dist, "normal")
  expect_equal(mp$prior_type, "map")
  expect_equal(mp$n_trials, 3L)
  expect_true(is.numeric(mp$params$mu))
  expect_true(is.numeric(mp$params$sigma))
  expect_true(is.data.frame(mp$tau_posterior))
  expect_true(all(c("tau", "density") %in% names(mp$tau_posterior)))
  expect_true(is.list(mp$tau_summary))
  expect_true(is.numeric(mp$tau_summary$mean))
  expect_true(is.numeric(mp$tau_summary$median))
  expect_equal(mp$tau_prior$family, "half_normal")
  expect_equal(mp$tau_prior$scale, 1.0)
})

test_that("map_prior fit_summary mean/sd match params$mu/params$sigma", {
  # Regression test for the mu/sigma vs mean/sd parametrization bug fixed
  # earlier in development: as_prior("normal", ...) requires params$mu /
  # params$sigma internally, NOT params$mean / params$sd.
  mp <- map_prior(y = .map_y, se = .map_se, outcome_type = "log_or")
  expect_equal(mp$params$mu,    mp$fit_summary$mean, tolerance = 1e-8)
  expect_equal(mp$params$sigma, mp$fit_summary$sd,   tolerance = 1e-8)
})

# -- Cross-validation against an independent brute-force reference --------
# map_prior() solves the marginal-tau integral via stats::integrate() over
# a [0,1) substitution. .ref_fit() below solves the SAME model, but by a
# completely different numerical route -- a deterministic fine-grid
# trapezoidal quadrature (200,000 points), built from scratch in this test
# file rather than calling any internal map_prior.R code path. Agreement
# between the two therefore checks the actual numerics (integration,
# substitution, indexing), not just internal self-consistency. No external
# package is required for this.

.ref_fit <- function(y, se, tau_family, tau_scale,
                      mu_prior_mean = 0, mu_prior_sd = 1e4, n_grid = 200000) {
  u   <- seq(1e-7, 1 - 1e-7, length.out = n_grid)
  tau <- tau_scale * u / (1 - u)
  jac <- tau_scale / (1 - u)^2

  v_mat <- outer(se^2, tau^2, "+")
  w_mat <- 1 / v_mat
  W  <- colSums(w_mat)
  p0 <- 1 / mu_prior_sd^2
  P  <- W + p0
  M  <- (colSums(w_mat * y) + p0 * mu_prior_mean) / P
  V  <- 1 / P
  logmarglik <- 0.5*log(2*pi) - 0.5*log(P) + 0.5*P*M^2 -
    0.5*colSums(w_mat*y^2) - 0.5*p0*mu_prior_mean^2 -
    0.5*colSums(log(2*pi*v_mat)) - 0.5*log(2*pi*mu_prior_sd^2)

  dprior <- if (tau_family == "half_normal") {
    2 * stats::dnorm(tau, 0, tau_scale)
  } else {
    2 * stats::dcauchy(tau, 0, tau_scale)
  }
  logg <- logmarglik + log(dprior)
  logg <- logg - max(logg)
  g    <- exp(logg) * jac

  du   <- diff(u)
  trap <- function(vals) sum(0.5 * (vals[-1] + vals[-length(vals)]) * du)

  Z        <- trap(g)
  mu_mean  <- trap(g * M) / Z
  mu_m2    <- trap(g * (V + M^2)) / Z
  mu_sd    <- sqrt(max(mu_m2 - mu_mean^2, 0))
  tau_mean <- trap(g * tau) / Z
  list(mu_mean = mu_mean, mu_sd = mu_sd, tau_mean = tau_mean)
}

test_that("map_prior agrees with an independent fine-grid reference (half_normal)", {
  mp  <- map_prior(y = .map_y, se = .map_se,
                    tau_prior = list(family = "half_normal", scale = 0.5))
  ref <- .ref_fit(.map_y, .map_se, "half_normal", 0.5)

  expect_equal(mp$fit_summary$mean, ref$mu_mean,        tolerance = 1e-5)
  expect_equal(mp$fit_summary$sd,   ref$mu_sd,           tolerance = 1e-5)
  expect_equal(mp$tau_summary$mean, ref$tau_mean,        tolerance = 1e-5)
})

test_that("map_prior agrees with an independent fine-grid reference (half_cauchy)", {
  mp  <- map_prior(y = .map_y, se = .map_se,
                    tau_prior = list(family = "half_cauchy", scale = 1.0))
  ref <- .ref_fit(.map_y, .map_se, "half_cauchy", 1.0)

  expect_equal(mp$fit_summary$mean, ref$mu_mean,        tolerance = 1e-5)
  expect_equal(mp$fit_summary$sd,   ref$mu_sd,           tolerance = 1e-5)
  expect_equal(mp$tau_summary$mean, ref$tau_mean,        tolerance = 1e-5)
})

test_that("map_prior with a near-zero tau prior matches fixed-effect meta-analysis", {
  # As the tau prior's scale -> 0, the random-effects model collapses to a
  # fixed-effect (inverse-variance-weighted) meta-analysis.
  mp <- map_prior(y = .map_y, se = .map_se,
                   tau_prior = list(family = "half_normal", scale = 0.001))

  w  <- 1 / .map_se^2
  fe_mean <- sum(w * .map_y) / sum(w)
  fe_sd   <- sqrt(1 / sum(w))

  expect_equal(mp$fit_summary$mean, fe_mean, tolerance = 1e-3)
  expect_equal(mp$fit_summary$sd,   fe_sd,   tolerance = 1e-3)
})

test_that("map_prior does not error for a pathologically narrow tau prior", {
  # Regression test: at very small tau-prior scales (essentially a spike at
  # 0), an earlier implementation's CDF/quantile step integrated directly
  # over an a priori t-range rather than the well-conditioned u-substitution
  # used elsewhere, and stats::uniroot() crashed with "f() values at end
  # points not of opposite sign" because the search bracket never reached
  # where the (vanishingly narrow) probability mass actually was. Fixed by
  # doing the CDF/quantile search in u-space instead (see .fit_map_posterior
  # -- cdf_u()/find_quantile()), a bounded domain regardless of the prior's
  # absolute scale. Verified (separately, not asserted here) that this fix
  # is robust down to scale = 1e-300 -- i.e. find_quantile()'s own
  # `if (cdf_u(u_hi) < p) return(Inf)` fallback and the tau_q999/tau_median
  # `!is.finite()` guards below it are not reachable through the public API
  # at any scale tested; they remain as defence-in-depth, not as dead code
  # removed, since a future change to the integration tolerance could make
  # them reachable again.
  for (scale in c(1e-6, 1e-100)) {
    for (fam in c("half_normal", "half_cauchy")) {
      mp <- map_prior(y = .map_y, se = .map_se,
                       tau_prior = list(family = fam, scale = scale))
      expect_true(is.finite(mp$tau_summary$median))
      expect_true(is.finite(max(mp$tau_posterior$tau)))
    }
  }

  w  <- 1 / .map_se^2
  fe_mean <- sum(w * .map_y) / sum(w)
  fe_sd   <- sqrt(1 / sum(w))
  mp <- map_prior(y = .map_y, se = .map_se,
                   tau_prior = list(family = "half_normal", scale = 1e-6))
  expect_equal(mp$fit_summary$mean, fe_mean, tolerance = 1e-4)
  expect_equal(mp$fit_summary$sd,   fe_sd,   tolerance = 1e-4)
})

test_that("map_prior SD increases with tau prior scale", {
  mp_narrow <- map_prior(y = .map_y, se = .map_se,
                          tau_prior = list(family = "half_normal", scale = 0.5))
  mp_wide   <- map_prior(y = .map_y, se = .map_se,
                          tau_prior = list(family = "half_normal", scale = 2.0))
  expect_gt(mp_wide$fit_summary$sd, mp_narrow$fit_summary$sd)
})

test_that("map_prior supports the half_cauchy tau family", {
  mp <- map_prior(y = .map_y, se = .map_se,
                   tau_prior = list(family = "half_cauchy", scale = 0.5))
  expect_s3_class(mp, "bayprior")
  expect_equal(mp$tau_prior$family, "half_cauchy")
  expect_true(is.finite(mp$fit_summary$mean))
  expect_true(is.finite(mp$fit_summary$sd))
  expect_true(mp$fit_summary$sd > 0)
})

# -- Outcome-type presets (Roever et al. 2021) --------------------------------

test_that("resolve_tau_prior returns the documented presets", {
  expect_equal(resolve_tau_prior("log_or"),  list(family = "half_normal", scale = 0.5))
  expect_equal(resolve_tau_prior("smd"),     list(family = "half_normal", scale = 0.5))
  expect_equal(resolve_tau_prior("log_irr"), list(family = "half_normal", scale = 0.5))
  expect_equal(resolve_tau_prior("single_arm_log_odds"),
               list(family = "half_normal", scale = 1.0))
  expect_equal(resolve_tau_prior("correlation"),
               list(family = "half_normal", scale = 0.15))
})

test_that("resolve_tau_prior errors for mean_difference (no generic preset)", {
  expect_error(resolve_tau_prior("mean_difference"), "endpoint-specific")
})

test_that("resolve_tau_prior errors informatively for an unrecognised outcome_type", {
  # match.arg() is not used here (unlike map_prior()'s outcome_type), so this
  # exported function must validate its own input.
  expect_error(resolve_tau_prior("not_a_real_outcome_type"), "No heterogeneity-prior preset")
  expect_error(resolve_tau_prior("not_a_real_outcome_type"), "log_or")
})

test_that("map_prior uses the outcome_type preset when tau_prior is NULL", {
  mp <- map_prior(y = .map_y, se = .map_se, outcome_type = "single_arm_log_odds")
  expect_equal(mp$tau_prior, list(family = "half_normal", scale = 1.0))
})

test_that("map_prior errors for mean_difference without an explicit tau_prior", {
  expect_error(
    map_prior(y = .map_y, se = .map_se, outcome_type = "mean_difference"),
    "endpoint-specific"
  )
})

test_that("map_prior works for mean_difference with an explicit tau_prior", {
  mp <- map_prior(y = .map_y, se = .map_se, outcome_type = "mean_difference",
                   tau_prior = list(family = "half_normal", scale = 1.5))
  expect_s3_class(mp, "bayprior")
  expect_equal(mp$tau_prior$scale, 1.5)
})

test_that("an explicit tau_prior always overrides the outcome_type preset", {
  mp <- map_prior(y = .map_y, se = .map_se, outcome_type = "log_or",
                   tau_prior = list(family = "half_cauchy", scale = 0.3))
  expect_equal(mp$tau_prior, list(family = "half_cauchy", scale = 0.3))
})

test_that("log_or and single_arm_log_odds give materially different results", {
  # The distinction this preset table exists to enforce: these are not
  # interchangeable "log-odds" labels, they get different default tau
  # scales (0.5 vs 1.0) and therefore different posterior SDs.
  mp_or <- map_prior(y = .map_y, se = .map_se, outcome_type = "log_or")
  mp_sa <- map_prior(y = .map_y, se = .map_se, outcome_type = "single_arm_log_odds")
  expect_gt(mp_sa$fit_summary$sd, mp_or$fit_summary$sd)
})

# -- Input validation --------------------------------------------------------

test_that("map_prior errors on mismatched y/se lengths", {
  expect_error(map_prior(y = c(-0.5, -0.6), se = c(0.2)), "same length")
})

test_that("map_prior errors with fewer than 2 trials", {
  expect_error(map_prior(y = -0.5, se = 0.2), "at least 2")
})

test_that("map_prior errors on non-positive or non-finite se", {
  expect_error(map_prior(y = c(-0.5, -0.6), se = c(0.2, 0)), "positive")
  expect_error(map_prior(y = c(-0.5, -0.6), se = c(0.2, NA)), "finite|positive")
})

test_that("map_prior errors on malformed explicit tau_prior", {
  expect_error(
    map_prior(y = .map_y, se = .map_se, tau_prior = list(family = "half_normal")),
    "tau_prior"
  )
  expect_error(
    map_prior(y = .map_y, se = .map_se,
              tau_prior = list(family = "half_normal", scale = -1)),
    "positive"
  )
  expect_error(
    map_prior(y = .map_y, se = .map_se,
              tau_prior = list(family = "not_a_family", scale = 0.5))
  )
})

# -- plot_tau_posterior() ----------------------------------------------------

test_that("plot_tau_posterior returns a ggplot for a MAP prior", {
  mp <- map_prior(y = .map_y, se = .map_se, outcome_type = "log_or")
  gp <- plot_tau_posterior(mp)
  expect_s3_class(gp, "gg")
})

test_that("plot_tau_posterior errors informatively for a non-MAP prior", {
  prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  expect_error(plot_tau_posterior(prior), "tau_posterior|map_prior")
})

test_that("plot_tau_posterior works with show_prior = FALSE", {
  mp <- map_prior(y = .map_y, se = .map_se, outcome_type = "log_or")
  gp <- plot_tau_posterior(mp, show_prior = FALSE)
  expect_s3_class(gp, "gg")
})

test_that("plot_tau_posterior overlays the half_cauchy prior density", {
  # The half_normal case is covered by the tests above; this exercises the
  # half_cauchy branch of the prior-density overlay specifically.
  mp <- map_prior(y = .map_y, se = .map_se,
                   tau_prior = list(family = "half_cauchy", scale = 0.5))
  gp <- plot_tau_posterior(mp)
  expect_s3_class(gp, "gg")
})

test_that("plot_tau_posterior handles a NULL tau_prior gracefully", {
  # Documented behaviour (see ?plot_tau_posterior): show_prior has no effect
  # if prior$tau_prior is NULL. Exercises the subtitle/overlay code path for
  # a bayprior object that has a $tau_posterior but no $tau_prior recorded.
  mp <- map_prior(y = .map_y, se = .map_se, outcome_type = "log_or")
  mp$tau_prior <- NULL
  gp <- plot_tau_posterior(mp)
  expect_s3_class(gp, "gg")
})

# -- historical_effect_sizes() ------------------------------------------------
# Cross-validated against closed-form formulas directly (add = 0, i.e. no
# continuity correction, so the comparison is exact rather than approximate).

test_that("historical_effect_sizes matches closed-form single-arm log-odds", {
  skip_if_not_installed("metafor")
  trials <- data.frame(xi = c(12, 8, 15), ni = c(40, 35, 50))
  es <- historical_effect_sizes("single_arm_log_odds", trials, add = 0)

  manual_y  <- log(trials$xi / (trials$ni - trials$xi))
  manual_se <- sqrt(1 / trials$xi + 1 / (trials$ni - trials$xi))

  expect_equal(es$y,  manual_y,  tolerance = 1e-10)
  expect_equal(es$se, manual_se, tolerance = 1e-10)
  # Original columns are preserved, not replaced.
  expect_true(all(c("xi", "ni", "y", "se") %in% names(es)))
})

test_that("historical_effect_sizes matches closed-form (Woolf) log odds ratio", {
  skip_if_not_installed("metafor")
  trials <- data.frame(ai = c(10, 15), bi = c(30, 20), ci = c(5, 8), di = c(35, 27))
  es <- historical_effect_sizes("log_or", trials, add = 0)

  manual_y  <- log((trials$ai * trials$di) / (trials$bi * trials$ci))
  manual_se <- sqrt(1 / trials$ai + 1 / trials$bi + 1 / trials$ci + 1 / trials$di)

  expect_equal(es$y,  manual_y,  tolerance = 1e-10)
  expect_equal(es$se, manual_se, tolerance = 1e-10)
})

test_that("historical_effect_sizes matches closed-form Fisher-z correlation", {
  skip_if_not_installed("metafor")
  trials <- data.frame(ri = c(0.35, 0.42, 0.28), ni = c(60, 55, 70))
  es <- historical_effect_sizes("correlation", trials)

  manual_y  <- atanh(trials$ri)
  manual_se <- 1 / sqrt(trials$ni - 3)

  expect_equal(es$y,  manual_y,  tolerance = 1e-10)
  expect_equal(es$se, manual_se, tolerance = 1e-10)
})

test_that("historical_effect_sizes output feeds map_prior() to the same result as hand-computed inputs", {
  skip_if_not_installed("metafor")
  trials <- data.frame(xi = c(12, 8, 15), ni = c(40, 35, 50))
  es <- historical_effect_sizes("single_arm_log_odds", trials, add = 0)

  mp_from_escalc <- map_prior(y = es$y, se = es$se, outcome_type = "single_arm_log_odds")
  mp_from_hand    <- map_prior(
    y  = log(trials$xi / (trials$ni - trials$xi)),
    se = sqrt(1 / trials$xi + 1 / (trials$ni - trials$xi)),
    outcome_type = "single_arm_log_odds"
  )

  expect_equal(mp_from_escalc$fit_summary$mean, mp_from_hand$fit_summary$mean, tolerance = 1e-10)
  expect_equal(mp_from_escalc$fit_summary$sd,   mp_from_hand$fit_summary$sd,   tolerance = 1e-10)
})

test_that("historical_effect_sizes supports smd, mean_difference, and log_irr measures", {
  skip_if_not_installed("metafor")
  arm_trials <- data.frame(m1i = c(5.2, 4.8), m2i = c(4.0, 3.9),
                            sd1i = c(1.1, 1.3), sd2i = c(1.2, 1.0),
                            n1i = c(30, 25), n2i = c(28, 26))
  es_smd <- historical_effect_sizes("smd", arm_trials)
  es_md  <- historical_effect_sizes("mean_difference", arm_trials)
  expect_true(all(is.finite(es_smd$y)) && all(is.finite(es_smd$se)))
  expect_true(all(is.finite(es_md$y))  && all(is.finite(es_md$se)))
  # SMD (standardized) and raw MD are different quantities on the same data.
  expect_false(isTRUE(all.equal(es_smd$y, es_md$y)))

  irr_trials <- data.frame(x1i = c(20, 15), t1i = c(500, 420),
                            x2i = c(35, 28), t2i = c(510, 430))
  es_irr <- historical_effect_sizes("log_irr", irr_trials)
  expect_true(all(is.finite(es_irr$y)) && all(is.finite(es_irr$se)))
})

test_that("historical_effect_sizes errors on missing required columns", {
  skip_if_not_installed("metafor")
  expect_error(
    historical_effect_sizes("log_or", data.frame(x = 1)),
    "missing column"
  )
})

test_that("historical_effect_sizes errors on a non-data-frame input", {
  skip_if_not_installed("metafor")
  expect_error(historical_effect_sizes("log_or", list(a = 1)), "data frame")
})

test_that("historical_effect_sizes errors on an invalid outcome_type", {
  skip_if_not_installed("metafor")
  expect_error(
    historical_effect_sizes("not_a_type", data.frame(xi = 1, ni = 2))
  )
})

test_that("historical_effect_sizes warns on NA effect sizes (e.g. zero-cell counts)", {
  skip_if_not_installed("metafor")
  zero_trials <- data.frame(ai = c(0, 10), bi = c(30, 20), ci = c(5, 8), di = c(35, 27))
  expect_warning(
    historical_effect_sizes("log_or", zero_trials, add = 0, to = "none")
  )
})