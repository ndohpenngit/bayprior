# zzz_patches.R
#
# Loaded LAST by R (zzz_ prefix guarantees this).
# Defines shared internal helpers used by plotting.R, conflict_sensitivity.R,
# aggregation.R, and all Shiny modules.
# Do NOT export any function from this file.

# ── %||% null-coalescing operator ─────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a)) a else b


# ── .prior_summary_lognormal ──────────────────────────────────────────────────
.prior_summary_lognormal <- function(params) {
  ml <- params$meanlog
  sl <- params$sdlog
  mn <- exp(ml + sl^2 / 2)
  sd <- sqrt((exp(sl^2) - 1) * exp(2 * ml + sl^2))
  list(
    mean = mn,
    sd   = sd,
    q025 = stats::qlnorm(0.025, ml, sl),
    q500 = stats::qlnorm(0.500, ml, sl),
    q975 = stats::qlnorm(0.975, ml, sl)
  )
}


# ── .make_bayprior ────────────────────────────────────────────────────────────
# Central constructor for all bayprior objects.
.make_bayprior <- function(dist, params, method, expert_id, label, input) {
  fit_summary <- if (dist == "lognormal") {
    .prior_summary_lognormal(params)
  } else {
    .prior_summary(dist, params)
  }
  structure(
    list(
      dist        = dist,
      params      = params,
      method      = method,
      expert_id   = expert_id,
      label       = label,
      input       = input,
      fit_summary = fit_summary
    ),
    class = "bayprior"
  )
}


# ── .eval_density_vec ─────────────────────────────────────────────────────────
# Evaluates the density of a bayprior object at a vector of points x.
.eval_density_vec <- function(prior, x) {
  switch(prior$dist,
    beta      = stats::dbeta(x, prior$params$alpha, prior$params$beta),
    normal    = stats::dnorm(x, prior$params$mu, prior$params$sigma),
    gamma     = stats::dgamma(x, prior$params$shape, prior$params$rate),
    lognormal = stats::dlnorm(x, prior$params$meanlog, prior$params$sdlog),
    mixture   = {
      d <- numeric(length(x))
      for (i in seq_along(prior$components)) {
        d <- d + prior$weights[i] * .eval_density_vec(prior$components[[i]], x)
      }
      d
    },
    rep(NA_real_, length(x))
  )
}


# ── .density_grid ─────────────────────────────────────────────────────────────
# Returns list(x, y) of grid points and density values for plotting.
.density_grid <- function(prior, n_grid = 500) {

  if (prior$dist == "lognormal") {
    lo <- stats::qlnorm(0.001, prior$params$meanlog, prior$params$sdlog)
    hi <- stats::qlnorm(0.999, prior$params$meanlog, prior$params$sdlog)

  } else if (prior$dist == "gamma") {
    lo <- stats::qgamma(0.001, prior$params$shape, prior$params$rate)
    hi <- stats::qgamma(0.999, prior$params$shape, prior$params$rate)

  } else if (prior$dist == "beta") {
    lo <- 0
    hi <- 1

  } else if (prior$dist == "mixture") {
    summaries <- lapply(prior$components, function(p) p$fit_summary)
    lo <- min(sapply(summaries, function(s) s$q025 %||% (s$mean - 4 * s$sd)), na.rm = TRUE)
    hi <- max(sapply(summaries, function(s) s$q975 %||% (s$mean + 4 * s$sd)), na.rm = TRUE)

  } else {
    s  <- prior$fit_summary
    lo <- s$q025 %||% (s$mean - 4 * s$sd)
    hi <- s$q975 %||% (s$mean + 4 * s$sd)
  }

  lo <- max(lo, 1e-6)
  x  <- seq(lo, hi, length.out = n_grid)
  list(x = x, y = .eval_density_vec(prior, x))
}
