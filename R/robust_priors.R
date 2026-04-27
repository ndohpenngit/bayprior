#' Construct a sceptical (penalised-enthusiasm) prior
#'
#' Generates a sceptical prior that places most mass at or near the null
#' value of the treatment effect, representing a conservative stance for
#' regulatory submissions. Implements the Spiegelhalter-Freedman sceptical
#' prior approach and the FDA-recommended "enthusiastic vs sceptical" prior
#' sensitivity pair.
#'
#' @param null_value Numeric. The null treatment effect (e.g. 0 for a mean
#'   difference, 1 for a hazard ratio, 0.5 for a response-rate difference).
#' @param family     Character. Distribution family. One of `"normal"`,
#'   `"beta"`, `"lognormal"`.
#' @param strength   Character. How concentrated the prior is around the null:
#'   `"weak"`, `"moderate"` (default), or `"strong"`.
#' @param label      Character. Description of the quantity.
#' @param expert_id  Character. Identifier for provenance.
#'
#' @return A `bayprior` object tagged with `prior_type = "sceptical"`.
#'
#' @details
#' For a Normal family, the sceptical prior is centred at `null_value` with
#' SD calibrated to the `strength` argument:
#' \itemize{
#'   \item `weak`:     SD = standard deviation of a "vague" half-normal
#'   \item `moderate`: SD chosen so that a 2-SD departure from null has ~5%
#'     prior probability - roughly equivalent to a p < 0.05 sceptic
#'   \item `strong`:   SD = half of `moderate` - very concentrated at null
#' }
#'
#' @references
#' Spiegelhalter, D. J., Freedman, L. S. & Parmar, M. K. B. (1994).
#' Bayesian approaches to randomized trials. *JRSS-A*, 157, 357-416.
#'
#' @examples
#' sc <- sceptical_prior(null_value = 0, family = "normal",
#'                       strength = "moderate", label = "Mean difference")
#' print(sc)
#' plot(sc)
#'
#' @export
sceptical_prior <- function(null_value = 0,
                             family     = c("normal", "beta", "lognormal"),
                             strength   = c("moderate", "weak", "strong"),
                             label      = "Treatment effect",
                             expert_id  = "Sceptic") {

  family   <- match.arg(family)
  strength <- match.arg(strength)

  if (family == "normal") {
    sd_map <- c(weak = 1.0, moderate = 0.5, strong = 0.25)
    sd_val <- sd_map[strength]
    prior  <- elicit_normal(mean = null_value, sd = sd_val,
                            method = "moments",
                            expert_id = expert_id,
                            label     = label)

  } else if (family == "lognormal") {
    # Sceptical about any benefit: centre log-HR at 0 (HR = 1)
    sd_map <- c(weak = 0.5, moderate = 0.25, strong = 0.125)
    prior  <- elicit_lognormal(mean = exp(null_value),
                               sd   = exp(null_value) * sd_map[strength],
                               method    = "moments",
                               expert_id = expert_id,
                               label     = label)

  } else {  # beta
    # Centre at null_value with small SD
    sd_map <- c(weak = 0.15, moderate = 0.08, strong = 0.04)
    prior  <- elicit_beta(mean = null_value, sd = sd_map[strength],
                          method    = "moments",
                          expert_id = expert_id,
                          label     = label)
  }

  prior$prior_type <- "sceptical"
  prior$null_value <- null_value
  prior$strength   <- strength
  prior
}


#' Construct a robust (heavy-tailed mixture) prior
#'
#' Builds a robust prior by mixing an informative component with a vague
#' (diffuse) component, following the RBesT/MAP robust mixture approach
#' (Schmidli et al., 2014). This protects against prior misspecification by
#' ensuring the posterior is not dominated by a conflicting informative prior.
#'
#' @param informative A `bayprior` object representing the informative
#'   component (e.g. an elicited or historical prior).
#' @param vague_weight Numeric in (0, 1). Weight assigned to the vague
#'   (diffuse) component. Default `0.20` (80% informative, 20% vague).
#' @param vague_sd    Numeric. SD of the vague Normal component (on the
#'   natural scale). If `NULL`, defaults to 10x the informative prior's SD.
#' @param label       Character. Label for the robust prior.
#'
#' @return A `bayprior` object with `dist = "mixture"` and
#'   `prior_type = "robust"`.
#'
#' @references
#' Schmidli, H. et al. (2014). Robust meta-analytic-predictive priors in
#' clinical trials with historical control information.
#' *Biometrics*, 70, 1023-1032.
#'
#' @examples
#' informative <- elicit_normal(mean = 0.30, sd = 0.10,
#'                              method = "moments", label = "Response rate")
#' robust      <- robust_prior(informative, vague_weight = 0.20)
#' plot(robust)
#'
#' @export
robust_prior <- function(informative,
                          vague_weight = 0.20,
                          vague_sd     = NULL,
                          label        = "Robust mixture prior") {

  if (!inherits(informative, "bayprior")) {
    rlang::abort("`informative` must be a bayprior object.")
  }
  if (vague_weight <= 0 || vague_weight >= 1) {
    rlang::abort("`vague_weight` must be strictly between 0 and 1.")
  }

  # Vague component: Normal centred at informative prior mean, very wide
  inf_mean <- informative$fit_summary$mean
  inf_sd   <- informative$fit_summary$sd
  vague_sd <- vague_sd %||% (10 * inf_sd)

  vague <- elicit_normal(
    mean      = inf_mean,
    sd        = vague_sd,
    method    = "moments",
    expert_id = "Vague component",
    label     = informative$label
  )

  result <- elicit_mixture(
    components = list(informative = informative, vague = vague),
    weights    = c(1 - vague_weight, vague_weight),
    label      = label
  )
  result$prior_type    <- "robust"
  result$vague_weight  <- vague_weight
  result$informative   <- informative
  result
}


#' Calibrate power prior weight via Bayes Factor
#'
#' Selects the power prior weight \eqn{\delta \in (0,1)} that down-weights
#' historical data before incorporating it into the current analysis. The
#' weight is calibrated by finding the \eqn{\delta} that achieves a target
#' Bayes Factor (or equivalently a target prior-data compatibility) between
#' the historical-data-based prior and the current likelihood.
#'
#' This addresses the **hdbayes gap** identified in the proposal: a
#' principled, calibrated approach to power prior weight selection.
#'
#' @param historical_data Named list describing historical data:
#'   \describe{
#'     \item{`type`}{`"binary"` or `"continuous"`.}
#'     \item{`x`}{Events (binary) or mean (continuous).}
#'     \item{`n`}{Historical sample size.}
#'     \item{`sd`}{SD (continuous only).}
#'   }
#' @param current_data Named list describing current data (same structure).
#' @param base_prior  A `bayprior` object representing the prior before
#'   incorporating historical data (usually a vague prior).
#' @param target_bf   Numeric. Target Bayes Factor (historical vs current
#'   model fit). A value of 1 gives equal weight; < 1 discounts historical.
#'   Default `3` (moderate evidence in favour of including historical data).
#' @param delta_grid  Numeric vector. Grid of \eqn{\delta} values to search.
#'   Default `seq(0.05, 1.0, by = 0.05)`.
#' @param method      Character. `"bayes_factor"` (default) or
#'   `"compatibility"` (uses Box p-value to calibrate compatibility).
#'
#' @return A list of class `bayprior_power_prior` with:
#'   \describe{
#'     \item{`delta_opt`}{Optimal weight.}
#'     \item{`delta_grid`}{Full grid of deltas.}
#'     \item{`bf_grid`}{Bayes Factors at each delta.}
#'     \item{`compatibility_grid`}{Box p-values at each delta.}
#'     \item{`power_prior`}{The resulting `bayprior` at `delta_opt`.}
#'     \item{`target_bf`}{The target BF supplied.}
#'   }
#'
#' @references
#' Ibrahim, J. G. & Chen, M.-H. (2000). Power prior distributions for
#' regression models. *Statistical Science*, 15, 46-60.
#'
#' Gravestock, I. & Held, L. (2017). Adaptive power priors with empirical
#' Bayes for clinical trials. *Pharmaceutical Statistics*, 16, 349-360.
#'
#' @examples
#' base  <- elicit_beta(mean = 0.5, sd = 0.2, method = "moments",
#'                      label = "Response rate")
#' calib <- calibrate_power_prior(
#'   historical_data = list(type = "binary", x = 12, n = 40),
#'   current_data    = list(type = "binary", x = 18, n = 50),
#'   base_prior      = base,
#'   target_bf       = 3
#' )
#' print(calib)
#' plot(calib)
#'
#' @export
calibrate_power_prior <- function(historical_data,
                                   current_data,
                                   base_prior,
                                   target_bf   = 3,
                                   delta_grid  = seq(0.05, 1.0, by = 0.05),
                                   method      = c("bayes_factor", "compatibility")) {

  method <- match.arg(method)

  if (!inherits(base_prior, "bayprior")) {
    rlang::abort("`base_prior` must be a bayprior object.")
  }

  results <- purrr::map_dfr(delta_grid, function(delta) {

    # Build power prior: update base prior with fractional historical data
    pp <- .power_prior_update(base_prior, historical_data, delta)

    # Evaluate compatibility with current data
    cd <- tryCatch(
      prior_conflict(pp, current_data, alpha = 0.10),
      error = function(e) NULL
    )

    # Bayes factor: ratio of marginal likelihoods under pp vs base_prior
    bf <- tryCatch(
      .marginal_bf(pp, base_prior, current_data),
      error = function(e) NA_real_
    )

    data.frame(
      delta         = delta,
      box_pvalue    = if (!is.null(cd)) cd$box_pvalue    else NA_real_,
      overlap       = if (!is.null(cd)) cd$overlap        else NA_real_,
      surprise      = if (!is.null(cd)) cd$surprise_index else NA_real_,
      bayes_factor  = bf
    )
  })

  # Find optimal delta
  if (method == "bayes_factor") {
    # Largest delta whose BF >= target_bf (most historical info while still
    # compatible with current data)
    eligible  <- results$delta[!is.na(results$bayes_factor) &
                                 results$bayes_factor >= target_bf]
    delta_opt <- if (length(eligible) > 0) max(eligible) else min(delta_grid)
  } else {
    # Largest delta whose Box p-value >= 0.05 (no conflict)
    eligible  <- results$delta[!is.na(results$box_pvalue) &
                                 results$box_pvalue >= 0.05]
    delta_opt <- if (length(eligible) > 0) max(eligible) else min(delta_grid)
  }

  # Build the calibrated power prior
  power_prior <- .power_prior_update(base_prior, historical_data, delta_opt)
  power_prior$prior_type  <- "power_prior"
  power_prior$delta       <- delta_opt

  cli::cli_alert_success(
    "Optimal power prior weight: delta = {delta_opt} (method = {method})."
  )

  structure(
    list(
      delta_opt          = delta_opt,
      delta_grid         = delta_grid,
      bf_grid            = results$bayes_factor,
      compatibility_grid = results$box_pvalue,
      overlap_grid       = results$overlap,
      results            = results,
      power_prior        = power_prior,
      target_bf          = target_bf,
      method             = method
    ),
    class = "bayprior_power_prior"
  )
}


#' Print method for bayprior_power_prior objects
#'
#' @param x A `bayprior_power_prior` object.
#' @param ... Ignored.
#' @export
print.bayprior_power_prior <- function(x, ...) {
  cli::cli_h1("Power Prior Calibration")
  cli::cli_ul()
  cli::cli_li("Method      : {x$method}")
  cli::cli_li("Target BF   : {x$target_bf}")
  cli::cli_li("Optimal delta: {x$delta_opt}")
  cli::cli_li("Power prior mean: {round(x$power_prior$fit_summary$mean, 4)}")
  cli::cli_li("Power prior SD  : {round(x$power_prior$fit_summary$sd,   4)}")
  invisible(x)
}


#' Plot calibration curve for power prior weight selection
#'
#' @param x A `bayprior_power_prior` object.
#' @param ... Ignored.
#' @return A `ggplot` object.
#' @export
plot.bayprior_power_prior <- function(x, ...) {
  res <- x$results
  delta_opt <- x$delta_opt

  p1 <- ggplot2::ggplot(res, ggplot2::aes(x = delta)) +
    ggplot2::geom_line(ggplot2::aes(y = bayes_factor),
                       colour = "#185FA5", linewidth = 1) +
    ggplot2::geom_hline(yintercept = x$target_bf,
                        linetype = "dashed", colour = "#D85A30") +
    ggplot2::geom_vline(xintercept = delta_opt,
                        linetype = "dotted", colour = "#1D9E75") +
    ggplot2::labs(title = "Bayes Factor vs power prior weight",
                  x = "delta", y = "Bayes Factor") +
    ggplot2::theme_minimal(base_size = 12)

  p2 <- ggplot2::ggplot(res, ggplot2::aes(x = delta)) +
    ggplot2::geom_line(ggplot2::aes(y = box_pvalue),
                       colour = "#D85A30", linewidth = 1) +
    ggplot2::geom_hline(yintercept = 0.05,
                        linetype = "dashed", colour = "#888780") +
    ggplot2::geom_vline(xintercept = delta_opt,
                        linetype = "dotted", colour = "#1D9E75") +
    ggplot2::labs(title = "Box p-value (conflict) vs power prior weight",
                  x = "delta", y = "Box p-value") +
    ggplot2::theme_minimal(base_size = 12)

  # Stack with patchwork if available, otherwise return list
  if (requireNamespace("patchwork", quietly = TRUE)) {
    p1 / p2
  } else {
    print(p1); print(p2)
    invisible(list(bf_plot = p1, pvalue_plot = p2))
  }
}


# ── Internal helpers ──────────────────────────────────────────────────────────

.power_prior_update <- function(base_prior, hist_data, delta) {
  # Fractional updating: incorporate historical data weighted by delta in (0,1].
  type <- hist_data$type %||% "binary"
  n    <- hist_data$n
  x    <- hist_data$x

  if (delta <= 0 || delta > 1) {
    rlang::abort(glue::glue("`delta` must be in (0, 1]; got {delta}."))
  }

  # ── Beta / binary ────────────────────────────────────────────────────────────
  if (base_prior$dist == "beta" && type == "binary") {
    a_new <- base_prior$params$alpha + delta * x
    b_new <- base_prior$params$beta  + delta * (n - x)
    return(.make_bayprior("beta", list(alpha = a_new, beta = b_new),
                          "power_prior", base_prior$expert_id,
                          base_prior$label, list(delta = delta)))
  }

  # ── Normal ───────────────────────────────────────────────────────────────────
  if (base_prior$dist == "normal") {
    obs_mean  <- x
    obs_se    <- (hist_data$sd %||% base_prior$fit_summary$sd) / sqrt(n * delta)
    prior_var <- base_prior$params$sigma^2
    lik_var   <- obs_se^2
    post_var  <- 1 / (1 / prior_var + 1 / lik_var)
    post_mean <- post_var * (base_prior$params$mu / prior_var + obs_mean / lik_var)
    return(.make_bayprior("normal",
                          list(mu = post_mean, sigma = sqrt(post_var)),
                          "power_prior", base_prior$expert_id,
                          base_prior$label, list(delta = delta)))
  }

  # ── Gamma / continuous ───────────────────────────────────────────────────────
  # Fractional sufficient statistics: scale both the event total and
  # the exposure by delta to achieve the desired down-weighting.
  if (base_prior$dist == "gamma" && type == "continuous") {
    x_sum     <- hist_data$x_sum %||% (x * n)   # prefer explicit total
    shape_new <- base_prior$params$shape + delta * x_sum
    rate_new  <- base_prior$params$rate  + delta * n
    return(.make_bayprior("gamma", list(shape = shape_new, rate = rate_new),
                          "power_prior", base_prior$expert_id,
                          base_prior$label, list(delta = delta)))
  }

  # ── Log-normal ───────────────────────────────────────────────────────────────
  # Normal conjugate update applied on the log scale.
  # Set hist_data$log_scale = TRUE if x and sd are already on the log scale.
  if (base_prior$dist == "lognormal") {
    log_obs_mean <- if (isTRUE(hist_data$log_scale)) x else log(x)
    raw_sd       <- hist_data$sd %||% base_prior$fit_summary$sd
    log_obs_se   <- (if (isTRUE(hist_data$log_scale)) raw_sd
                     else raw_sd / x) / sqrt(n * delta)
    prior_var  <- base_prior$params$sdlog^2
    lik_var    <- log_obs_se^2
    post_var   <- 1 / (1 / prior_var + 1 / lik_var)
    post_mean  <- post_var * (base_prior$params$meanlog / prior_var +
                                log_obs_mean / lik_var)
    return(.make_bayprior("lognormal",
                          list(meanlog = post_mean, sdlog = sqrt(post_var)),
                          "power_prior", base_prior$expert_id,
                          base_prior$label, list(delta = delta)))
  }

  # ── Mixture ──────────────────────────────────────────────────────────────────
  # Update each component independently with the same delta, then re-weight
  # by the component marginal likelihoods under the fractional historical data.
  # This is consistent with the mixture conjugate update in conflict_sensitivity.R.
  if (base_prior$dist == "mixture") {
    components <- base_prior$components
    weights    <- base_prior$weights

    # Update each component; NULL marks failures
    updated <- lapply(components, function(comp) {
      tryCatch(
        .power_prior_update(comp, hist_data, delta),
        error = function(e) NULL
      )
    })

    keep <- !vapply(updated, is.null, logical(1))
    if (!any(keep)) {
      rlang::abort(glue::glue(
        "Power prior update failed for all mixture components ",
        "(dist = 'mixture', type = '{type}', delta = {delta}). ",
        "Check that at least one component supports the data type '{type}'."
      ))
    }
    updated <- updated[keep]
    weights <- weights[keep]

    # Effective SE for fractional historical data (used for re-weighting)
    obs_mean_h <- if (type == "binary") x / n else x
    obs_se_h   <- if (type == "binary") {
      sqrt(obs_mean_h * (1 - obs_mean_h) / (n * delta))
    } else {
      (hist_data$sd %||% base_prior$fit_summary$sd) / sqrt(n * delta)
    }
    obs_se_h <- max(obs_se_h, 1e-8)  # guard against zero SE

    log_marg <- vapply(components[keep], function(comp) {
      stats::dnorm(obs_mean_h,
                   mean = comp$fit_summary$mean,
                   sd   = sqrt(comp$fit_summary$sd^2 + obs_se_h^2),
                   log  = TRUE)
    }, numeric(1))

    log_wts     <- log(weights) + log_marg
    new_weights <- exp(log_wts - max(log_wts))
    new_weights <- new_weights / sum(new_weights)

    # Updated mixture summary via law of total expectation/variance
    post_means <- vapply(updated, function(p) p$fit_summary$mean, numeric(1))
    post_sds   <- vapply(updated, function(p) p$fit_summary$sd,   numeric(1))
    mix_mean   <- sum(new_weights * post_means)
    mix_sd     <- sqrt(sum(new_weights * (post_sds^2 + (post_means - mix_mean)^2)))

    return(structure(
      list(
        dist        = "mixture",
        params      = list(weights = new_weights),
        components  = updated,
        weights     = new_weights,
        method      = "power_prior",
        expert_id   = base_prior$expert_id,
        label       = base_prior$label,
        input       = list(delta = delta),
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

  # ── Unsupported combination ───────────────────────────────────────────────────
  rlang::abort(glue::glue(
    "Power prior update not implemented for dist = '{base_prior$dist}' + ",
    "type = '{type}'."
  ))
}


.marginal_bf <- function(power_prior, base_prior, current_data) {
  # Log marginal likelihood ratio (normal approximation)
  type     <- current_data$type %||% "binary"
  n        <- current_data$n
  x        <- current_data$x
  obs_mean <- if (type == "binary") x / n else x
  obs_se   <- if (type == "binary") {
    sqrt(obs_mean * (1 - obs_mean) / n)
  } else {
    (current_data$sd %||% power_prior$fit_summary$sd) / sqrt(n)
  }

  # Guard zero SE
  obs_se <- max(obs_se, 1e-8)

  # Log marginal = log N(obs_mean; prior_mean, sqrt(prior_sd^2 + obs_se^2))
  .log_pred <- function(pr) {
    pm <- pr$fit_summary$mean
    ps <- pr$fit_summary$sd
    stats::dnorm(obs_mean, pm, sqrt(ps^2 + obs_se^2), log = TRUE)
  }

  exp(.log_pred(power_prior) - .log_pred(base_prior))
}
