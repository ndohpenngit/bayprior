#' Compute prior-data conflict diagnostics
#'
#' Evaluates conflict between a specified prior and observed data using
#' multiple complementary diagnostics: Box's (1980) predictive p-value,
#' the surprise index (standardised distance), Kullback-Leibler divergence,
#' and the Bhattacharyya overlap coefficient between the prior and the
#' (normalised) likelihood.
#'
#' @param prior       A \code{bayprior} object.
#' @param data_summary Named list describing the observed data:
#'   \describe{
#'     \item{\code{type}}{\code{"binary"} or \code{"continuous"}.}
#'     \item{\code{x}}{Number of events (binary) or observed mean (continuous).}
#'     \item{\code{n}}{Sample size.}
#'     \item{\code{sd}}{Observed standard deviation (continuous only).}
#'   }
#' @param alpha Numeric. Significance level for the Box p-value flag.
#'   Default \code{0.05}.
#'
#' @return An object of class \code{bayprior_conflict} containing:
#'   \describe{
#'     \item{\code{box_pvalue}}{Box's prior predictive p-value.}
#'     \item{\code{surprise_index}}{Standardised distance between prior mean
#'       and observed data.}
#'     \item{\code{kl_prior_likelihood}}{KL divergence from prior to likelihood.}
#'     \item{\code{overlap}}{Bhattacharyya overlap coefficient in [0, 1].}
#'     \item{\code{conflict_severity}}{One of \code{"none"}, \code{"mild"},
#'       \code{"severe"}.}
#'     \item{\code{conflict_flag}}{Logical; \code{TRUE} if
#'       \code{box_pvalue < alpha}.}
#'     \item{\code{recommendation}}{Plain-language guidance string.}
#'     \item{\code{data_summary}}{The data summary passed in.}
#'     \item{\code{prior}}{The input prior.}
#'   }
#'
#' @references
#' Box, G. E. P. (1980). Sampling and Bayes' inference in scientific modelling
#' and robustness. \emph{Journal of the Royal Statistical Society A}, 143,
#' 383-430.
#'
#' @examples
#' prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments",
#'                      label = "Response rate")
#' cd <- prior_conflict(prior, list(type = "binary", x = 18, n = 40))
#' print(cd)
#'
#' @importFrom rlang %||% abort
#' @export
prior_conflict <- function(prior, data_summary, alpha = 0.05) {

  if (!inherits(prior, "bayprior")) {
    rlang::abort("`prior` must be a bayprior object.")
  }

  type <- data_summary$type %||% "binary"
  n    <- data_summary$n
  x    <- data_summary$x

  # Approximate prior as Normal for analytic diagnostics
  prior_mean <- prior$fit_summary$mean
  prior_sd   <- prior$fit_summary$sd

  # Likelihood parameters
  if (type == "binary") {
    obs_mean <- x / n
    obs_se   <- sqrt(obs_mean * (1 - obs_mean) / n)
  } else {
    obs_mean <- x
    obs_se   <- data_summary$sd / sqrt(n)
  }

  # FIX #7: Guard against obs_se = 0 (e.g. all successes / all failures),
  # which would cause division-by-zero in pred_sd, z, kl, and overlap.
  obs_se <- max(obs_se, 1e-8)

  # Box's prior predictive p-value
  pred_sd <- sqrt(prior_sd^2 + obs_se^2)
  z       <- (obs_mean - prior_mean) / pred_sd
  box_p   <- 2 * stats::pnorm(-abs(z))

  # Surprise index
  surprise <- abs(z)

  # KL divergence (normal approximation); see .kl_normal() for convention
  kl <- .kl_normal(prior_mean, prior_sd, obs_mean, obs_se)

  # Bhattacharyya overlap
  overlap <- .bhattacharyya_normal(prior_mean, prior_sd, obs_mean, obs_se)

  # Severity classification
  severity <- dplyr::case_when(
    box_p >= alpha               ~ "none",
    box_p < alpha & surprise < 3 ~ "mild",
    TRUE                         ~ "severe"
  )

  recommendation <- switch(severity,
    none = glue::glue(
      "No evidence of prior-data conflict (Box p = {round(box_p, 3)}). ",
      "The prior appears consistent with the observed data."
    ),
    mild = glue::glue(
      "Mild prior-data conflict detected (Box p = {round(box_p, 3)}, ",
      "surprise = {round(surprise, 2)}). ",
      "Consider reporting a sensitivity analysis with a more diffuse prior."
    ),
    severe = glue::glue(
      "Severe prior-data conflict detected (Box p = {round(box_p, 4)}, ",
      "surprise = {round(surprise, 2)}). ",
      "Re-elicitation or use of a robust/sceptical prior is strongly recommended."
    )
  )

  structure(
    list(
      box_pvalue          = box_p,
      surprise_index      = surprise,
      kl_prior_likelihood = kl,
      overlap             = overlap,
      conflict_severity   = severity,
      conflict_flag       = box_p < alpha,
      recommendation      = as.character(recommendation),
      data_summary        = data_summary,
      prior               = prior,
      prior_mean          = prior_mean,
      prior_sd            = prior_sd,
      obs_mean            = obs_mean,
      obs_se              = obs_se,
      alpha               = alpha
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
#' @param prior       A \code{bayprior} object (the reference prior).
#' @param data_summary Named list as for \code{\link{prior_conflict}}.
#' @param param_grid  Named list of numeric vectors, one per hyperparameter
#'   to vary. Names must match hyperparameter names in \code{prior$params}.
#'   Example: \code{list(alpha = seq(1, 8, 0.5), beta = seq(2, 20, 1))}.
#' @param target Character vector. Which posterior quantities to compute.
#'   Any of \code{"posterior_mean"}, \code{"posterior_sd"},
#'   \code{"prob_efficacy"}.
#' @param threshold Numeric. Efficacy threshold used in
#'   \code{Pr(theta > threshold)}. Default \code{0.30}.
#'
#' @return An object of class \code{bayprior_sensitivity}.
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
#' @importFrom rlang %||% abort warn
#' @export
sensitivity_grid <- function(prior,
                              data_summary,
                              param_grid,
                              target    = c("posterior_mean", "posterior_sd",
                                            "prob_efficacy"),
                              threshold = 0.30) {

  target <- match.arg(target, several.ok = TRUE)

  type <- data_summary$type %||% "binary"
  n    <- data_summary$n
  x    <- data_summary$x

  # ── For mixture priors, use the dominant component as working prior ─────────
  # Mixture params (weights) are not hyperparameters to grid over.
  working_prior <- if (prior$dist == "mixture") {
    dominant <- which.max(prior$weights)
    prior$components[[dominant]]
  } else {
    prior
  }

  # ── Auto-remap param_grid names to prior hyperparameter names ────────────────
  #
  # The Shiny UI generates generic names ("param1", "param2", …) that will
  # never literally match prior hyperparameter names ("alpha"/"beta", etc.).
  # We resolve this with a three-step strategy:
  #
  #   1. Exact match  — use as-is (names already correct).
  #   2. Positional remap — if no names match but the *count* equals the number
  #      of prior hyperparameters, rename param_grid entries to the prior's
  #      param names in order and warn the caller.
  #   3. Abort — counts also differ; the mapping is genuinely ambiguous.
  #
  prior_param_names <- names(working_prior$params)
  grid_param_names  <- names(param_grid)
  valid_names       <- intersect(grid_param_names, prior_param_names)

  if (length(valid_names) == 0) {
    # No exact matches — attempt positional remap
    if (length(grid_param_names) == length(prior_param_names)) {
      rlang::warn(paste0(
        "param_grid names (", paste(grid_param_names, collapse = ", "), ") ",
        "do not match prior hyperparameter names (",
        paste(prior_param_names, collapse = ", "), "). ",
        "Remapping positionally: ",
        paste(grid_param_names, "->", prior_param_names, collapse = ", "), "."
      ))
      names(param_grid) <- prior_param_names
      valid_names       <- prior_param_names
    } else {
      rlang::abort(paste0(
        "Cannot map param_grid to prior hyperparameters: names don't match ",
        "and counts differ.\n",
        "  param_grid names (", length(grid_param_names), "): ",
        paste(grid_param_names,  collapse = ", "), "\n",
        "  Prior param names (", length(prior_param_names), "): ",
        paste(prior_param_names, collapse = ", "), "\n",
        "Either rename param_grid entries to match the prior's hyperparameter ",
        "names, or supply exactly ", length(prior_param_names),
        " grid vector(s) in the same order."
      ))
    }
  }

  # Build full Cartesian grid (after any name remapping above)
  grid_df <- do.call(expand.grid, param_grid)

  # Identify reference row closest to working prior's parameters.
  # FIX #2: mapply() returns a plain vector when param_grid has only one entry,
  # making rowSums() produce wrong scalar results. matrix() with explicit nrow
  # handles both single- and multi-parameter cases correctly.
  dist_matrix <- matrix(
    mapply(
      function(col, ref) (grid_df[[col]] - ref)^2,
      valid_names,
      working_prior$params[valid_names]
    ),
    nrow = nrow(grid_df)
  )
  dists   <- rowSums(dist_matrix)
  ref_row <- which.min(dists)

  # Evaluate posterior summaries at each grid point
  results <- purrr::map_dfr(seq_len(nrow(grid_df)), function(i) {

    row    <- grid_df[i, , drop = FALSE]
    params <- as.list(row)

    # Build temporary prior at this grid point
    tmp_prior <- tryCatch(
      .make_bayprior(working_prior$dist, params, working_prior$method,
                     working_prior$expert_id, working_prior$label,
                     working_prior$input),
      error = function(e) NULL
    )
    if (is.null(tmp_prior)) {
      out <- as.list(row)
      for (t in target) out[[t]] <- NA_real_
      return(as.data.frame(out))
    }

    # Compute posterior via conjugate update
    post <- tryCatch(
      .conjugate_update(tmp_prior, data_summary),
      error = function(e) NULL
    )
    if (is.null(post)) {
      out <- as.list(row)
      for (t in target) out[[t]] <- NA_real_
      return(as.data.frame(out))
    }

    post_s <- post$fit_summary
    out    <- as.list(row)

    if ("posterior_mean" %in% target) out$posterior_mean <- post_s$mean
    if ("posterior_sd"   %in% target) out$posterior_sd   <- post_s$sd
    if ("prob_efficacy"  %in% target) {
      out$prob_efficacy <- tryCatch({
        if (post$dist == "beta") {
          stats::pbeta(threshold, post$params$alpha, post$params$beta,
                       lower.tail = FALSE)
        } else if (post$dist == "normal") {
          stats::pnorm(threshold, post$params$mu, post$params$sigma,
                       lower.tail = FALSE)
        } else if (post$dist == "gamma") {
          stats::pgamma(threshold, post$params$shape, post$params$rate,
                        lower.tail = FALSE)
        } else {
          # Mixture or unknown: normal approximation from fit_summary
          stats::pnorm(threshold, post_s$mean, post_s$sd,
                       lower.tail = FALSE)
        }
      }, error = function(e) NA_real_)
    }

    as.data.frame(out)
  })

  # FIX #B: Detect the case where every grid point produced NA (e.g. because
  # .make_bayprior or .conjugate_update failed for all rows) and abort with a
  # clear message before we ever call range() or build a colorscale.  This is
  # what causes the cascade of "no non-missing arguments to min/max" warnings
  # and the broken Plotly colorscale_json warnings downstream.
  all_na_targets <- Filter(function(t) {
    v <- results[[t]]
    is.null(v) || all(is.na(v))
  }, target)

  if (length(all_na_targets) > 0) {
    rlang::abort(paste0(
      "All grid evaluations returned NA for: ",
      paste(all_na_targets, collapse = ", "), ".\n",
      "This usually means .make_bayprior() or .conjugate_update() failed at ",
      "every grid point. Check that:\n",
      "  1. param_grid values produce valid hyperparameters (e.g. alpha > 0).\n",
      "  2. The prior distribution supports conjugate updating with this data type.\n",
      "  3. data_summary$type is set correctly ('binary' or 'continuous')."
    ))
  }

  # FIX #3 (refined): Warn about missing target columns (different from all-NA).
  missing_targets <- setdiff(target, names(results))
  if (length(missing_targets) > 0) {
    rlang::warn(paste0(
      "The following targets are missing from results entirely and will have ",
      "influence score 0: ", paste(missing_targets, collapse = ", ")
    ))
  }

  # Influence scores: range of each target across the grid.
  # Returns 0 when all values are NA or non-finite (after the all-NA guard above,
  # this only triggers for partial-NA columns, which is legitimate).
  influence <- vapply(target, function(t) {
    v <- results[[t]]
    if (is.null(v) || all(is.na(v))) return(0)
    fin <- v[is.finite(v)]
    if (length(fin) == 0) return(0)
    diff(range(fin))
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

# FIX #1: Define %||% explicitly so it is available regardless of whether rlang
# is attached (it may only be in Imports, not Depends).  Functions in this file
# that carry @importFrom rlang %||% will also satisfy R CMD CHECK.
`%||%` <- rlang::`%||%`


# KL(P||Q): KL divergence from P = Normal(m1, s1) to Q = Normal(m2, s2).
# Formula: log(s2/s1) + (s1^2 + (m1-m2)^2) / (2*s2^2) - 1/2
# FIX #5: Added explicit convention comment to prevent future sign/argument confusion.
.kl_normal <- function(m1, s1, m2, s2) {
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

  # ── Beta / binary ──────────────────────────────────────────────────────────
  if (prior$dist == "beta" && type == "binary") {
    a_post <- prior$params$alpha + x
    b_post <- prior$params$beta  + (n - x)
    return(.make_bayprior("beta", list(alpha = a_post, beta = b_post),
                          "posterior", prior$expert_id, prior$label, list()))
  }

  # ── Normal ─────────────────────────────────────────────────────────────────
  if (prior$dist == "normal") {
    obs_mean  <- x
    obs_se    <- (data_summary$sd %||% prior$fit_summary$sd) / sqrt(n)
    prior_var <- prior$params$sigma^2
    lik_var   <- obs_se^2
    post_var  <- 1 / (1 / prior_var + 1 / lik_var)
    post_mean <- post_var * (prior$params$mu / prior_var + obs_mean / lik_var)
    return(.make_bayprior("normal", list(mu = post_mean, sigma = sqrt(post_var)),
                          "posterior", prior$expert_id, prior$label, list()))
  }

  # ── Gamma ──────────────────────────────────────────────────────────────────
  # FIX #4: The original code used `n * x` where x is the *mean*, which is
  # numerically correct only when x happens to be the total count. For
  # continuous data, x is explicitly documented as the observed mean, so we
  # must recover the total. We prefer an explicit x_sum field when available
  # (e.g. Poisson count data) and fall back to n * x otherwise.
  if (prior$dist == "gamma" && type == "continuous") {
    x_sum      <- data_summary$x_sum %||% (x * n)
    shape_post <- prior$params$shape + x_sum
    rate_post  <- prior$params$rate  + n
    return(.make_bayprior("gamma", list(shape = shape_post, rate = rate_post),
                          "posterior", prior$expert_id, prior$label, list()))
  }

  # ── Mixture — update each component and re-weight by marginal likelihood ───
  if (prior$dist == "mixture") {
    components <- prior$components
    weights    <- prior$weights

    # Update each component individually
    post_components <- lapply(components, function(comp) {
      tryCatch(.conjugate_update(comp, data_summary), error = function(e) NULL)
    })

    # Drop components that failed to update
    keep <- !vapply(post_components, is.null, logical(1))
    if (!any(keep)) {
      rlang::abort("Could not update any mixture component with the supplied data.")
    }
    post_components <- post_components[keep]
    weights         <- weights[keep]

    # Re-weight components by marginal likelihood
    log_marg <- vapply(seq_along(post_components), function(i) {
      comp     <- components[keep][[i]]
      obs_mean <- if (type == "binary") x / n else x
      obs_se   <- if (type == "binary") {
        sqrt(obs_mean * (1 - obs_mean) / n)
      } else {
        (data_summary$sd %||% comp$fit_summary$sd) / sqrt(n)
      }
      stats::dnorm(obs_mean,
                   mean = comp$fit_summary$mean,
                   sd   = sqrt(comp$fit_summary$sd^2 + obs_se^2),
                   log  = TRUE)
    }, numeric(1))

    log_post_wts <- log(weights) + log_marg
    post_weights <- exp(log_post_wts - max(log_post_wts))
    post_weights <- post_weights / sum(post_weights)

    # Mixture posterior summary
    post_means <- vapply(post_components, function(p) p$fit_summary$mean, numeric(1))
    post_sds   <- vapply(post_components, function(p) p$fit_summary$sd,   numeric(1))
    mix_mean   <- sum(post_weights * post_means)
    mix_sd     <- sqrt(sum(post_weights * (post_sds^2 + (post_means - mix_mean)^2)))

    return(structure(
      list(
        dist        = "mixture",
        params      = list(weights = post_weights),
        components  = post_components,
        weights     = post_weights,
        method      = "posterior",
        expert_id   = prior$expert_id,
        label       = prior$label,
        input       = list(),
        fit_summary = list(
          mean = mix_mean,
          sd   = mix_sd,
          q025 = mix_mean - 1.96 * mix_sd,
          q500 = mix_mean,
          q975 = mix_mean + 1.96 * mix_sd
        )
      ),
      class = "bayprior"
    ))
  }

  rlang::abort(glue::glue(
    "Conjugate update not implemented for dist='{prior$dist}' with type='{type}'."
  ))
}


# ── Density helpers (shared with zzz_patches.R — defined here as fallback) ───

.density_grid <- function(prior, n_grid = 500) {

  if (prior$dist == "lognormal") {
    lo <- stats::qlnorm(0.001, prior$params$meanlog, prior$params$sdlog)
    hi <- stats::qlnorm(0.999, prior$params$meanlog, prior$params$sdlog)
  } else if (prior$dist == "gamma") {
    lo <- stats::qgamma(0.001, prior$params$shape, prior$params$rate)
    hi <- stats::qgamma(0.999, prior$params$shape, prior$params$rate)
  } else if (prior$dist == "beta") {
    lo <- 0; hi <- 1
  } else if (prior$dist == "mixture") {
    summaries <- lapply(prior$components, function(p) p$fit_summary)

    lo_vals <- sapply(summaries, function(s) s$q025 %||% (s$mean - 4 * s$sd))
    hi_vals <- sapply(summaries, function(s) s$q975 %||% (s$mean + 4 * s$sd))

    # FIX #C: Filter to finite values before calling min/max.  When all
    # components have NULL or NA summaries, sapply() returns a vector of NAs
    # and min/max emit "no non-missing arguments … returning Inf/-Inf", which
    # then propagates to seq() → .eval_density_vec → the Plotly colorscale.
    lo_vals <- lo_vals[is.finite(lo_vals)]
    hi_vals <- hi_vals[is.finite(hi_vals)]

    if (length(lo_vals) == 0 || length(hi_vals) == 0) {
      rlang::abort(
        "Cannot determine density range for mixture prior: all component ",
        "fit_summary values are NULL or NA. Ensure every mixture component ",
        "has a valid fit_summary with mean and sd."
      )
    }

    lo <- min(lo_vals)
    hi <- max(hi_vals)
  } else {
    s  <- prior$fit_summary
    lo <- s$q025 %||% (s$mean - 4 * s$sd)
    hi <- s$q975 %||% (s$mean + 4 * s$sd)
  }

  # FIX #6: Only clamp lo to 1e-6 for distributions with non-negative support
  # (beta, gamma, lognormal). Clamping Normal priors silently drops the left
  # tail and produces misleading density plots for negative-valued parameters.
  if (prior$dist %in% c("beta", "gamma", "lognormal")) {
    lo <- max(lo, 1e-6)
  }

  x  <- seq(lo, hi, length.out = n_grid)
  list(x = x, y = .eval_density_vec(prior, x))
}

.eval_density_vec <- function(prior, x) {
  switch(prior$dist,
    beta      = stats::dbeta(x, prior$params$alpha, prior$params$beta),
    normal    = stats::dnorm(x, prior$params$mu,    prior$params$sigma),
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
