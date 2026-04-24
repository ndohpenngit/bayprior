#' Aggregate multiple expert priors into a consensus prior
#'
#' Combines elicited priors from multiple experts into a single consensus
#' distribution using either linear pooling (mixture) or logarithmic pooling
#' (normalised product of densities). Includes diagnostics for inter-expert
#' disagreement.
#'
#' @param priors A named list of `bayprior` objects, one per expert.
#' @param weights Numeric vector of expert weights (summing to 1). If `NULL`,
#'   equal weights are applied.
#' @param method Character. `"linear"` (default) or `"logarithmic"` pooling.
#' @param disagreement_threshold Numeric in (0, 1). Triggers a warning when
#'   the pairwise Bhattacharyya coefficient drops below this value, flagging
#'   substantial expert disagreement. Default `0.5`.
#'
#' @return A `bayprior` object (`dist = "mixture"` for linear pooling,
#'   `dist = "log_pool"` for logarithmic), with an additional
#'   `$aggregation` component containing:
#'   \describe{
#'     \item{`method`}{Pooling method used}
#'     \item{`weights`}{Applied weights}
#'     \item{`disagreement`}{Pairwise Bhattacharyya coefficients}
#'     \item{`n_experts`}{Number of experts}
#'   }
#'
#' @details
#' **Linear pooling** (externally Bayesian): The consensus density is a
#' weighted mixture \eqn{\pi(\theta) = \sum_k w_k \pi_k(\theta)}. This is
#' the most commonly used approach in clinical trial settings (O'Hagan et al.,
#' 2006). The resulting prior always lies within the convex hull of individual
#' expert priors.
#'
#' **Logarithmic pooling** (internally Bayesian): The consensus density is
#' proportional to \eqn{\prod_k \pi_k(\theta)^{w_k}}, which produces a
#' sharper consensus when experts agree, but can be severely influenced by
#' outlying expert opinions.
#'
#' @references
#' O'Hagan, A., et al. (2006). *Uncertain Judgements: Eliciting Experts'
#' Probabilities*. Wiley.
#'
#' @examples
#' p1 <- elicit_beta(mean = 0.25, sd = 0.08, method = "moments", expert_id = "E1",
#'                   label = "Response rate")
#' p2 <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments", expert_id = "E2",
#'                   label = "Response rate")
#' p3 <- elicit_beta(mean = 0.30, sd = 0.09, method = "moments", expert_id = "E3",
#'                   label = "Response rate")
#'
#' consensus <- aggregate_experts(
#'   priors  = list(E1 = p1, E2 = p2, E3 = p3),
#'   weights = c(0.4, 0.3, 0.3),
#'   method  = "linear"
#' )
#' print(consensus)
#' plot(consensus)
#'
#' @export
aggregate_experts <- function(priors,
                              weights = NULL,
                              method = c("linear", "logarithmic"),
                              disagreement_threshold = 0.5) {

  method <- match.arg(method)
  if (!is.list(priors) || length(priors) < 2) {
    rlang::abort("`priors` must be a list of at least 2 bayprior objects.")
  }

  k <- length(priors)
  expert_ids <- names(priors)
  if (is.null(expert_ids)) expert_ids <- paste0("Expert_", seq_len(k))

  # Validate weights
  if (is.null(weights)) {
    weights <- rep(1 / k, k)
    cli::cli_alert_warning("No weights supplied; equal weights applied (1/{k} per expert).",
                           .envir = environment())
  } else {
    if (length(weights) != k) rlang::abort("`weights` length must match number of priors.")
    if (abs(sum(weights) - 1) > 1e-6) rlang::abort("`weights` must sum to 1.")
  }

  # Disagreement diagnostics
  disagreement <- .pairwise_bhattacharyya(priors)
  min_bc <- min(disagreement[lower.tri(disagreement)])

  if (min_bc < disagreement_threshold) {
    cli::cli_alert_warning(
      "Substantial expert disagreement detected (min Bhattacharyya coefficient = {round(min_bc, 3)}).
       Consider reviewing individual elicitations or using robust/sceptical priors.",
      .envir = environment()
    )
  } else {
    cli::cli_alert_success(
      "Expert agreement satisfactory (min Bhattacharyya coefficient = {round(min_bc, 3)}).",
      .envir = environment()
    )
  }

  # Aggregate
  if (method == "linear") {
    result <- elicit_mixture(priors, weights = weights,
                             label = "Linear pooled consensus prior")
  } else {
    result <- .log_pool(priors, weights)
  }

  result$aggregation <- list(
    method        = method,
    weights       = setNames(weights, expert_ids),
    disagreement  = disagreement,
    n_experts     = k
  )

  cli::cli_alert_success("Aggregated {k} expert priors using {method} pooling.")
  result
}


# ---- Internal helpers -------------------------------------------------------

#' Pairwise Bhattacharyya coefficients between Beta distributions
#' Extended numerically for other families.
.pairwise_bhattacharyya <- function(priors) {
  k   <- length(priors)
  nms <- names(priors)
  if (is.null(nms)) nms <- paste0("E", seq_len(k))
  mat <- matrix(1, k, k, dimnames = list(nms, nms))

  for (i in seq_len(k - 1)) {
    for (j in (i + 1):k) {
      bc <- .bc_coef(priors[[i]], priors[[j]])
      mat[i, j] <- bc
      mat[j, i] <- bc
    }
  }
  mat
}

.bc_coef <- function(p1, p2) {
  # Analytical BC for Beta-Beta; numerical otherwise
  if (p1$dist == "beta" && p2$dist == "beta") {
    a1 <- p1$params$alpha; b1 <- p1$params$beta
    a2 <- p2$params$alpha; b2 <- p2$params$beta
    log_bc <- lbeta((a1 + a2) / 2, (b1 + b2) / 2) -
              0.5 * (lbeta(a1, b1) + lbeta(a2, b2))
    return(exp(log_bc))
  }
  # Numerical fallback
  grid <- seq(.Machine$double.eps, 1 - .Machine$double.eps, length.out = 1000)
  d1   <- .eval_density(p1, grid)
  d2   <- .eval_density(p2, grid)
  dx   <- diff(range(grid)) / (length(grid) - 1)
  sum(sqrt(d1 * d2)) * dx
}

.eval_density <- function(prior, x) {
  switch(prior$dist,
    beta   = stats::dbeta(x, prior$params$alpha, prior$params$beta),
    normal = stats::dnorm(x, prior$params$mu, prior$params$sigma),
    gamma  = stats::dgamma(x, prior$params$shape, prior$params$rate),
    mixture = {
      d <- numeric(length(x))
      for (i in seq_along(prior$components)) {
        d <- d + prior$weights[i] * .eval_density(prior$components[[i]], x)
      }
      d
    }
  )
}

.log_pool <- function(priors, weights) {
  # Logarithmic pooling stored as a special bayprior; density evaluated numerically
  structure(
    list(
      dist       = "log_pool",
      components = priors,
      weights    = weights,
      label      = "Logarithmic pooled consensus prior",
      fit_summary = list(
        mean = sum(weights * vapply(priors, function(x) x$fit_summary$mean, numeric(1))),
        sd   = NULL
      )
    ),
    class = "bayprior"
  )
}
