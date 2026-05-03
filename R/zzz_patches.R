# zzz_patches.R
#
# Loaded LAST by R (zzz_ prefix guarantees this).
# Defines shared internal helpers used by plotting.R, conflict_sensitivity.R,
# aggregation.R, and all Shiny modules.
# Do NOT export any function from this file.

# ── %||% null-coalescing operator ─────────────────────────────────────────────
#' @importFrom rlang `%||%`
`%||%` <- rlang::`%||%`


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


# ── .apply_plotly_theme ───────────────────────────────────────────────────────
# Sets a WHITE background explicitly.
# In dark mode, CSS filter: invert(1) hue-rotate(180deg) on .js-plotly-plot
# turns white → black and preserves data colours via hue-rotate.
# In light mode, white background looks correct as-is (no filter applied).
.apply_plotly_theme <- function(p, layout_args = NULL) {
  p$x$layout$paper_bgcolor <- "#ffffff"
  p$x$layout$plot_bgcolor  <- "#ffffff"
  if (is.list(p$x$layout$shapes)) {
    p$x$layout$shapes <- lapply(p$x$layout$shapes, function(s) {
      if (identical(s$type, "rect")) s$fillcolor <- "#ffffff"
      s
    })
  }
  p
}
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

    lo_vals <- sapply(summaries, function(s) s$q025 %||% (s$mean - 4 * s$sd))
    hi_vals <- sapply(summaries, function(s) s$q975 %||% (s$mean + 4 * s$sd))

    # FIX: Filter to finite values before calling min/max. When all components
    # have NULL or NA summaries, sapply() returns an all-NA vector and
    # min/max emit "no non-missing arguments; returning Inf/-Inf".
    lo_vals <- lo_vals[is.finite(lo_vals)]
    hi_vals <- hi_vals[is.finite(hi_vals)]

    if (length(lo_vals) == 0 || length(hi_vals) == 0) {
      rlang::abort(paste0(
        "Cannot determine density range for mixture prior: all component ",
        "fit_summary values are NULL or NA. Ensure every mixture component ",
        "has a valid fit_summary with mean and sd."
      ))
    }

    lo <- min(lo_vals)
    hi <- max(hi_vals)

  } else {
    s  <- prior$fit_summary
    lo <- s$q025 %||% (s$mean - 4 * s$sd)
    hi <- s$q975 %||% (s$mean + 4 * s$sd)
  }

  # FIX: Only clamp lo to 1e-6 for distributions with non-negative support.
  # Clamping Normal priors silently drops the left tail and produces misleading
  # density plots for negative-valued parameters (e.g. log odds ratios).
  if (prior$dist %in% c("beta", "gamma", "lognormal")) {
    lo <- max(lo, 1e-6)
  }

  x <- seq(lo, hi, length.out = n_grid)
  list(x = x, y = .eval_density_vec(prior, x))
}
