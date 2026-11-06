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
#' @return A `bayprior` object (`dist = "mixture"` for linear pooling; for logarithmic
#'   pooling the same family as the experts, or `dist = "log_pool"` when the
#'   experts use different families), with an additional
#'   `$aggregation` component containing:
#'   \describe{
#'     \item{`method`}{Pooling method used}
#'     \item{`weights`}{Applied weights}
#'     \item{`disagreement`}{Pairwise Bhattacharyya coefficients}
#'     \item{`n_experts`}{Number of experts}
#'   }
#'
#' @details
#' **Linear pooling** satisfies the marginalization property (McConway,
#' 1981): pooling a joint distribution and then marginalising gives the
#' same result as marginalising each expert's distribution first and then
#' pooling. The consensus density is a weighted mixture
#' \eqn{\pi(\theta) = \sum_k w_k \pi_k(\theta)}. This is the most commonly
#' used approach in clinical trial settings (O'Hagan et al., 2006). The
#' resulting prior always lies within the convex hull of individual expert
#' priors. The mixture's summary mean and SD (in \code{$fit_summary}, used
#' e.g. by \code{\link{sensitivity_grid}} to derive a working prior) are
#' computed exactly from the component means/SDs and weights, without
#' numerical integration:
#' \deqn{\text{mean} = \sum_k w_k \bar{x}_k, \quad
#'       \text{var} = \sum_k w_k \left(s_k^2 + \bar{x}_k^2\right) - \text{mean}^2}
#'
#' **Logarithmic pooling** satisfies external Bayesianity (Genest, Weerahandi
#' & Zidek, 1984): pooling experts' priors and then updating on data gives
#' the same result as updating each expert's prior individually and then
#' pooling the posteriors -- pooling and Bayesian updating commute. This is
#' in fact the \emph{only} pooling operator with this property (Genest,
#' McConway & Schervish, 1986); no pooling method can satisfy both external
#' Bayesianity and marginalization simultaneously. The consensus density is
#' proportional to \eqn{\prod_k \pi_k(\theta)^{w_k}}, which produces a
#' sharper consensus when experts agree, but can be severely influenced by
#' outlying expert opinions. When all experts use the same Beta, Gamma,
#' exponential, Normal or lognormal family the pooled prior is again a member of
#' that family and is returned as such (Beta: \eqn{\mathrm{Beta}(\sum_k w_k
#' \alpha_k, \sum_k w_k \beta_k)}; Normal: precision-weighted mean and summed
#' weighted precision), so its parameters, mean, SD and credible interval are
#' exact. For mixed families the pooled density is normalised numerically and
#' its summary is taken from that grid.
#'
#' @references
#' O'Hagan, A., et al. (2006). *Uncertain Judgements: Eliciting Experts'
#' Probabilities*. Wiley.
#'
#' McConway, K. J. (1981). Marginalization and linear opinion pools.
#' *Journal of the American Statistical Association*, 76(374), 410-414.
#'
#' Genest, C., Weerahandi, S., & Zidek, J. V. (1984). Aggregating opinions
#' through logarithmic pooling. *Theory and Decision*, 17(1), 61-70.
#'
#' Genest, C., McConway, K. J., & Schervish, M. J. (1986). Characterization
#' of externally Bayesian pooling operators. *The Annals of Statistics*,
#' 14(2), 487-501.
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
    .check_weights(weights, k)
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

#' Pairwise Bhattacharyya Coefficients
#'
#' Computes pairwise Bhattacharyya coefficients between a list of prior
#' distributions. Analytical for Beta-Beta pairs; numerical otherwise.
#'
#' @param priors A named list of `bayprior` objects to compare pairwise.
#'
#' @return A symmetric matrix of Bhattacharyya coefficients (values in \[0, 1\]).
#' @keywords internal
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
  # Numerical fallback -- uses .eval_density_vec() (zzz_patches.R), which
  # covers all six elicitable families plus mixtures. A previous local
  # duplicate here (.eval_density()) covered only beta/normal/gamma/mixture
  # with no default case, so switch() silently returned NULL for lognormal,
  # exponential, or weibull priors -- causing sum(sqrt(NULL * NULL)) to
  # evaluate to 0 and falsely report complete disagreement for any such
  # pair, regardless of actual similarity.
  #
  # The integration grid must also span both priors' actual support --
  # previously hardcoded to (0, 1), correct only for Beta. For gamma,
  # lognormal, exponential, or weibull priors (positive real line, often
  # far from [0, 1]), a unit-interval grid would capture essentially none
  # of their probability mass and produce a near-zero BC regardless of the
  # priors' real similarity. .prior_range() (zzz_patches.R) is reused here,
  # unioning both priors' ranges, so the grid covers wherever either prior's
  # mass actually lies.
  r1 <- .prior_range(p1)
  r2 <- .prior_range(p2)
  grid <- seq(min(r1$lo, r2$lo), max(r1$hi, r2$hi), length.out = 1000)
  d1   <- .eval_density_vec(p1, grid)
  d2   <- .eval_density_vec(p2, grid)
  dx   <- diff(range(grid)) / (length(grid) - 1)
  sum(sqrt(d1 * d2)) * dx
}

.log_pool <- function(priors, weights) {
  # Logarithmic pool: density proportional to prod_k pi_k(theta)^w_k.
  fams <- unique(vapply(priors, function(x) x$dist, character(1)))
  label <- "Logarithmic pooled consensus prior"
  P <- function(nm) vapply(priors, function(x) x$params[[nm]], numeric(1))

  # Closed form when all experts use the same family: the pool is again a
  # member of that family (the exponents add up, e.g. Beta(sum w*alpha,
  # sum w*beta); Normal: precision-weighted mean and summed precision).
  if (length(fams) == 1 && fams %in% c("beta", "gamma", "normal",
                                       "exponential", "lognormal")) {
    pooled <- switch(fams,
      beta = list(alpha = sum(weights * P("alpha")), beta = sum(weights * P("beta"))),
      gamma = list(shape = sum(weights * P("shape")), rate = sum(weights * P("rate"))),
      exponential = list(rate = sum(weights * P("rate"))),
      normal = {
        prec <- sum(weights / P("sigma")^2)
        list(mu = sum(weights * P("mu") / P("sigma")^2) / prec, sigma = 1 / sqrt(prec))
      },
      lognormal = {
        prec <- sum(weights / P("sdlog")^2)
        list(meanlog = sum(weights * P("meanlog") / P("sdlog")^2) / prec,
             sdlog = 1 / sqrt(prec))
      })
    out <- .make_bayprior(fams, pooled, method = "log_pool",
                          expert_id = "Consensus", label = label,
                          input = list(weights = weights))
    out$pool_inputs <- list(components = priors, weights = weights)
    return(out)
  }

  # Otherwise (different families, Weibull): normalise the pooled density on a
  # grid and take the mean, SD and quantiles from it.
  rng <- lapply(priors, .prior_range)
  grid <- seq(min(vapply(rng, `[[`, 1, "lo")), max(vapply(rng, `[[`, 1, "hi")),
              length.out = 4001)
  logd <- Reduce(`+`, Map(function(p, w) w * log(pmax(.eval_density_vec(p, grid), 1e-300)),
                          priors, weights))
  d <- exp(logd - max(logd)); dx <- diff(grid)
  mass <- sum((d[-1] + d[-length(d)]) / 2 * dx); d <- d / mass
  cdfv <- c(0, cumsum((d[-1] + d[-length(d)]) / 2 * dx))
  m1 <- sum((grid[-1] * d[-1] + grid[-length(d)] * d[-length(d)]) / 2 * dx)
  m2 <- sum((grid[-1]^2 * d[-1] + grid[-length(d)]^2 * d[-length(d)]) / 2 * dx)
  qf <- function(p) stats::approx(cdfv, grid, xout = p, ties = "ordered")$y
  structure(
    list(
      dist       = "log_pool",
      components = priors,
      weights    = weights,
      label      = label,
      pool_grid  = list(x = grid, density = d),
      fit_summary = list(mean = m1, sd = sqrt(max(m2 - m1^2, 0)),
                         q025 = qf(0.025), q500 = qf(0.5), q975 = qf(0.975))
    ),
    class = "bayprior"
  )
}
