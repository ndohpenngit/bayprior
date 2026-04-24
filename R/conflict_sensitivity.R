#' Compute prior-data conflict diagnostics
#'
#' Evaluates conflict between a specified prior and observed data using
#' multiple complementary diagnostics: Box's (1980) predictive p-value,
#' the surprise index (standardised distance), Kullback-Leibler divergence,
#' and the Bhattacharyya overlap coefficient between the prior and the
#' (normalised) likelihood.
#'
#' @param prior       A `bayprior` object.
#' @param data_summary Named list describing the observed data:
#'   \describe{
#'     \item{`type`}{`"binary"` or `"continuous"`.}
#'     \item{`x`}{Number of events (binary) or observed mean (continuous).}
#'     \item{`n`}{Sample size.}
#'     \item{`sd`}{Observed standard deviation (continuous only).}
#'   }
#' @param alpha Numeric. Significance level for the Box p-value flag.
#'   Default `0.05`.
#'
#' @return An object of class `bayprior_conflict` containing:
#'   \describe{
#'     \item{`box_pvalue`}{Box's prior predictive p-value.}
#'     \item{`surprise_index`}{Standardised distance between prior mean
#'       and observed data.}
#'     \item{`kl_prior_likelihood`}{KL divergence from prior to likelihood.}
#'     \item{`overlap`}{Bhattacharyya overlap coefficient in \[0, 1\].}
#'     \item{`conflict_severity`}{One of `"none"`, `"mild"`, `"severe"`.}
#'     \item{`conflict_flag`}{Logical; `TRUE` if `box_pvalue < alpha`.}
#'     \item{`recommendation`}{Plain-language guidance string.}
#'     \item{`data_summary`}{The data summary passed in.}
#'     \item{`prior`}{The input prior.}
#'   }
#'
#' @references
#' Box, G. E. P. (1980). Sampling and Bayes' inference in scientific modelling
#' and robustness. *Journal of the Royal Statistical Society A*, 143, 383–430.
#'
#' @examples
#' prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments",
#'                      label = "Response rate")
#' cd <- prior_conflict(prior, list(type = "binary", x = 18, n = 40))
#' print(cd)
#' plot(cd)
#'
#' @export
prior_conflict <- function(prior, data_summary, alpha = 0.05) {

  if (!inherits(prior, "bayprior")) {
    rlang::abort("`prior` must be a bayprior object.")
  }

  type <- data_summary$type %||% "binary"
  n    <- data_summary$n
  x    <- data_summary$x

  # ── Approximate prior as Normal for analytic diagnostics ───────────────
  prior_mean <- prior$fit_summary$mean
  prior_sd   <- prior$fit_summary$sd

  # ── Likelihood parameters ─────────────────────────────────────────────
  if (type == "binary") {
    obs_mean <- x / n
    obs_se   <- sqrt(obs_mean * (1 - obs_mean) / n)
  } else {
    obs_mean <- x
    obs_se   <- data_summary$sd / sqrt(n)
  }

  # ── Box's prior predictive p-value ────────────────────────────────────
  # Under H0: data ~ prior predictive  →  (obs - prior_mean) / sqrt(prior_sd^2 + obs_se^2) ~ N(0,1)
  pred_sd <- sqrt(prior_sd^2 + obs_se^2)
  z        <- (obs_mean - prior_mean) / pred_sd
  box_p    <- 2 * stats::pnorm(-abs(z))

  # ── Surprise index ────────────────────────────────────────────────────
  surprise <- abs(z)

  # ── KL divergence (normal approximation) ─────────────────────────────
  kl <- .kl_normal(prior_mean, prior_sd, obs_mean, obs_se)

  # ── Bhattacharyya overlap ─────────────────────────────────────────────
  overlap <- .bhattacharyya_normal(prior_mean, prior_sd, obs_mean, obs_se)

  # ── Severity classification ───────────────────────────────────────────
  severity <- dplyr::case_when(
    box_p >= alpha             ~ "none",
    box_p < alpha & surprise < 3 ~ "mild",
    TRUE                       ~ "severe"
  )

  recommendation <- switch(severity,
    none   = glue::glue(
      "No evidence of prior-data conflict (Box p = {round(box_p, 3)}). ",
      "The prior appears consistent with the observed data."
    ),
    mild   = glue::glue(
      "Mild prior-data conflict detected (Box p = {round(box_p, 3)}, surprise = {round(surprise, 2)}). ",
      "Consider reporting a sensitivity analysis with a more diffuse prior."
    ),
    severe = glue::glue(
      "Severe prior-data conflict detected (Box p = {round(box_p, 4)}, surprise = {round(surprise, 2)}). ",
      "Re-elicitation or use of a robust/sceptical prior is strongly recommended."
    )
  )

  structure(
    list(
      box_pvalue            = box_p,
      surprise_index        = surprise,
      kl_prior_likelihood   = kl,
      overlap               = overlap,
      conflict_severity     = severity,
      conflict_flag         = box_p < alpha,
      recommendation        = as.character(recommendation),
      data_summary          = data_summary,
      prior                 = prior,
      prior_mean            = prior_mean,
      prior_sd              = prior_sd,
      obs_mean              = obs_mean,
      obs_se                = obs_se,
      alpha                 = alpha
    ),
    class = "bayprior_conflict"
  )
}


#' Sensitivity grid over prior hyperparameters
#'
#' Evaluates how posterior inferences change as prior hyperparameters vary
#' over a specified grid. This is the core function for demonstrating
#' robustness of trial conclusions to prior choice.
#'
#' @param prior       A `bayprior` object (the reference prior).
#' @param data_summary Named list as for `prior_conflict()`.
#' @param param_grid  Named list of numeric vectors, one per hyperparameter
#'   to vary. Names must match hyperparameter names in `prior$params`.
#'   Example: `list(alpha = seq(1, 8, 0.5), beta = seq(2, 20, 1))`.
#' @param target Character vector. Which posterior quantities to compute.
#'   Any of `"posterior_mean"`, `"posterior_sd"`, `"prob_efficacy"`.
#' @param threshold Numeric. Efficacy threshold used in `Pr(θ > threshold)`.
#'   Default `0.30`.
#'
#' @return An object of class `bayprior_sensitivity` with components:
#'   \describe{
#'     \item{`grid`}{Data frame with columns for each parameter and each target.}
#'     \item{`param_grid`}{Input grid specification.}
#'     \item{`target`}{Target quantity names.}
#'     \item{`reference_row`}{Index of the row corresponding to the reference prior.}
#'     \item{`influence_scores`}{Named vector of influence scores (range of each target).}
#'   }
#'
#' @examples
#' prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments",
#'                      label = "Response rate")
#' sa <- sensitivity_grid(
#'   prior,
#'   data_summary = list(type = "binary", x = 14, n = 40),
#'   param_grid   = list(alpha = seq(1, 8, 0.5), beta = seq(2, 20, 1))
#' )
#' plot_tornado(sa)
#' plot_sensitivity(sa, target = "posterior_mean")
#'
#' @export
sensitivity_grid <- function(prior,
                              data_summary,
                              param_grid,
                              target     = c("posterior_mean", "posterior_sd", "prob_efficacy"),
                              threshold  = 0.30) {

  target <- match.arg(target, several.ok = TRUE)

  type <- data_summary$type %||% "binary"
  n    <- data_summary$n
  x    <- data_summary$x

  # Build full Cartesian grid
  grid_df <- do.call(expand.grid, param_grid)

  # Identify reference row (closest to the reference prior's parameters)
  ref_params <- prior$params[names(param_grid)]
  dists      <- rowSums(mapply(function(col, ref) (grid_df[[col]] - ref)^2,
                               names(param_grid), ref_params))
  ref_row    <- which.min(dists)

  # Evaluate posterior summaries at each grid point
  results <- purrr::map_dfr(seq_len(nrow(grid_df)), function(i) {
    row    <- grid_df[i, , drop = FALSE]
    params <- as.list(row)

    # Build a temporary prior at this grid point
    tmp_prior <- tryCatch(
      .make_bayprior(prior$dist, params, prior$method, prior$expert_id,
                     prior$label, prior$input),
      error = function(e) NULL
    )
    if (is.null(tmp_prior)) return(data.frame(row, lapply(target, function(t) NA)))

    # Compute posterior via conjugate update
    post <- tryCatch(
      .conjugate_update(tmp_prior, data_summary),
      error = function(e) NULL
    )
    if (is.null(post)) return(data.frame(row, lapply(target, function(t) NA)))

    post_s <- post$fit_summary
    out    <- as.list(row)

    if ("posterior_mean" %in% target) out$posterior_mean <- post_s$mean
    if ("posterior_sd"   %in% target) out$posterior_sd   <- post_s$sd
    if ("prob_efficacy"  %in% target) {
      out$prob_efficacy <- if (post$dist == "beta") {
        stats::pbeta(threshold, post$params$alpha, post$params$beta, lower.tail = FALSE)
      } else if (post$dist == "normal") {
        stats::pnorm(threshold, post$params$mu, post$params$sigma, lower.tail = FALSE)
      } else NA
    }
    as.data.frame(out)
  })

  # Influence scores: range of each target across the grid
  influence <- vapply(target, function(t) {
    v <- results[[t]]
    if (all(is.na(v))) return(NA_real_)
    diff(range(v, na.rm = TRUE))
  }, numeric(1))

  structure(
    list(
      grid             = results,
      param_grid       = param_grid,
      target           = target,
      reference_row    = ref_row,
      influence_scores = influence,
      threshold        = threshold,
      prior            = prior
    ),
    class = "bayprior_sensitivity"
  )
}


# ── Internal helpers ──────────────────────────────────────────────────────────

.kl_normal <- function(m1, s1, m2, s2) {
  # KL(N(m1,s1) || N(m2,s2))
  log(s2 / s1) + (s1^2 + (m1 - m2)^2) / (2 * s2^2) - 0.5
}

.bhattacharyya_normal <- function(m1, s1, m2, s2) {
  t1 <- 0.25 * log(0.25 * (s1^2 / s2^2 + s2^2 / s1^2 + 2))
  t2 <- 0.25 * (m1 - m2)^2 / (s1^2 + s2^2)
  exp(-(t1 + t2))
}

.conjugate_update <- function(prior, data_summary) {
  type <- data_summary$type %||% "binary"
  n    <- data_summary$n
  x    <- data_summary$x

  if (prior$dist == "beta" && type == "binary") {
    a_post <- prior$params$alpha + x
    b_post <- prior$params$beta  + (n - x)
    return(.make_bayprior("beta", list(alpha = a_post, beta = b_post),
                          "posterior", prior$expert_id, prior$label, list()))
  }

  if (prior$dist == "normal") {
    obs_mean <- x
    obs_se   <- (data_summary$sd %||% prior$fit_summary$sd) / sqrt(n)
    prior_var <- prior$params$sigma^2
    lik_var   <- obs_se^2
    post_var  <- 1 / (1 / prior_var + 1 / lik_var)
    post_mean <- post_var * (prior$params$mu / prior_var + obs_mean / lik_var)
    return(.make_bayprior("normal", list(mu = post_mean, sigma = sqrt(post_var)),
                          "posterior", prior$expert_id, prior$label, list()))
  }

  if (prior$dist == "gamma" && type == "continuous") {
    shape_post <- prior$params$shape + n * x
    rate_post  <- prior$params$rate  + n
    return(.make_bayprior("gamma", list(shape = shape_post, rate = rate_post),
                          "posterior", prior$expert_id, prior$label, list()))
  }

  rlang::abort(glue::glue(
    "Conjugate update not implemented for dist='{prior$dist}' with type='{type}'."
  ))
}

# Density evaluation helpers shared with pooling module
.density_grid <- function(prior, n_grid = 500) {
  lo <- prior$fit_summary$q025 %||% (prior$fit_summary$mean - 4 * prior$fit_summary$sd)
  hi <- prior$fit_summary$q975 %||% (prior$fit_summary$mean + 4 * prior$fit_summary$sd)
  x  <- seq(lo * 0.5, hi * 1.5, length.out = n_grid)
  list(x = x, y = .eval_density_vec(prior, x))
}

.eval_density_vec <- function(prior, x) {
  switch(prior$dist,
    beta    = stats::dbeta(x, prior$params$alpha, prior$params$beta),
    normal  = stats::dnorm(x, prior$params$mu,    prior$params$sigma),
    gamma   = stats::dgamma(x, prior$params$shape, prior$params$rate),
    mixture = {
      d <- numeric(length(x))
      for (i in seq_along(prior$components)) {
        d <- d + prior$weights[i] * .eval_density_vec(prior$components[[i]], x)
      }
      d
    },
    rep(NA_real_, length(x))
  )
}
