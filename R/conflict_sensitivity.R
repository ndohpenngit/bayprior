#' Prior predictive probability mass function (internal)
#'
#' Closed-form prior predictive pmf of the observed count for the families
#' that have one: Beta prior with binary data (Beta-Binomial) and Gamma
#' prior with Poisson/survival data (Negative Binomial). A (linear) mixture
#' of such priors has the weighted mixture of the component predictives as
#' its prior predictive, so mixtures -- robust priors from
#' \code{\link{robust_prior}} and pooled consensus priors from
#' \code{\link{aggregate_experts}} -- are handled exactly too, provided
#' every component is supported.
#'
#' @param prior A \code{bayprior} object.
#' @param type Data type.
#' @param n Sample size / exposure.
#' @param x Observed count; used only to make sure the (truncated) count
#'   support of a Negative Binomial predictive reaches the observation.
#'
#' @return A numeric vector of probabilities for counts \code{0, 1, ...},
#'   summing to 1, or \code{NULL} if the prior has no supported closed form
#'   (or its truncated tail would lose more than 1e-6 of the mass).
#'
#' @keywords internal
#' @noRd
.predictive_pmf <- function(prior, type, n, x) {
  if (identical(prior$dist, "beta") && identical(type, "binary")) {
    a <- prior$params$alpha; b <- prior$params$beta
    k <- 0:n
    pmf <- exp(lchoose(n, k) + lbeta(k + a, n - k + b) - lbeta(a, b))
    return(pmf / sum(pmf))
  }
  if (identical(prior$dist, "gamma") && type %in% c("poisson", "survival")) {
    # Gamma-Poisson and Gamma-Exponential conjugacy coincide under this
    # package's (x = events, n = exposure) parameterisation, matching the
    # Normal-approximation branch in prior_conflict().
    size <- prior$params$shape
    prob <- prior$params$rate / (prior$params$rate + n)
    # Truncate where the remaining tail mass is below 1e-12. (A fixed
    # multiple of the SD is not enough for shape <= 1, where the tail is
    # geometric.)
    q_hi  <- stats::qnbinom(1 - 1e-12, size = size, prob = prob)
    k_max <- max(x, q_hi) + 1
    if (!is.finite(k_max) || k_max > 5e6) return(NULL)
    pmf <- stats::dnbinom(0:k_max, size = size, prob = prob)
    if (!is.finite(sum(pmf)) || sum(pmf) < 1 - 1e-6) return(NULL)
    return(pmf / sum(pmf))
  }
  if (identical(prior$dist, "mixture") && !is.null(prior$components) &&
      length(prior$components) == length(prior$weights)) {
    pmfs <- lapply(prior$components, .predictive_pmf, type = type, n = n, x = x)
    if (any(vapply(pmfs, is.null, logical(1)))) return(NULL)
    len <- max(lengths(pmfs))
    pad <- function(p) c(p, numeric(len - length(p)))
    mat <- vapply(pmfs, pad, numeric(len))
    pmf <- as.numeric(mat %*% prior$weights)
    pmf <- pmf / sum(pmf)
    attr(pmf, "numerical") <- any(vapply(pmfs, function(p)
      isTRUE(attr(p, "numerical")), logical(1)))
    return(pmf)
  }
  .predictive_pmf_numeric(prior, type, n, x)
}

#' Prior predictive pmf by equal-probability quadrature (internal)
#'
#' For a single proper prior without a conjugate closed form (a Normal,
#' lognormal, Weibull or exponential prior, or a Beta prior with count data,
#' and so on), the prior predictive pmf \eqn{P(X = k) = \int p(k \mid
#' \theta)\pi(\theta)\,d\theta} is evaluated by the midpoint rule on the
#' prior's own probability scale: \eqn{\theta_j = Q((j - 1/2)/N)}, each with
#' weight \eqn{1/N}. This handles densities that are singular at a boundary
#' and costs a fixed number of evaluations. A prior whose support extends
#' beyond the parameter space (a Normal on a probability, or on a rate) is
#' truncated to that space and renormalised, i.e. the prior is read as
#' proper on the parameter space. The definition of the p-value is
#' unchanged; only the way the predictive is computed differs.
#'
#' @inheritParams .predictive_pmf
#' @return A pmf with attribute \code{numerical = TRUE}, or \code{NULL}
#'   when unsupported or when the grid cannot resolve the likelihood.
#' @keywords internal
#' @noRd
.predictive_pmf_numeric <- function(prior, type, n, x) {
  dist <- prior$dist; p <- prior$params
  if (is.null(p) || !dist %in% c("beta", "normal", "gamma", "lognormal",
                                 "exponential", "weibull")) return(NULL)
  cdf <- switch(dist,
    beta = function(q) stats::pbeta(q, p$alpha, p$beta),
    normal = function(q) stats::pnorm(q, p$mu, p$sigma),
    gamma = function(q) stats::pgamma(q, p$shape, p$rate),
    lognormal = function(q) stats::plnorm(q, p$meanlog, p$sdlog),
    exponential = function(q) stats::pexp(q, rate = p$rate),
    weibull = function(q) stats::pweibull(q, p$shape, p$scale))
  qf <- switch(dist,
    beta = function(u) stats::qbeta(u, p$alpha, p$beta),
    normal = function(u) stats::qnorm(u, p$mu, p$sigma),
    gamma = function(u) stats::qgamma(u, p$shape, p$rate),
    lognormal = function(u) stats::qlnorm(u, p$meanlog, p$sdlog),
    exponential = function(u) stats::qexp(u, rate = p$rate),
    weibull = function(u) stats::qweibull(u, p$shape, p$scale))
  binary <- identical(type, "binary")
  lo <- 0
  hi <- if (binary) 1 else Inf
  u_lo <- cdf(lo); u_hi <- if (is.finite(hi)) cdf(hi) else 1
  if (!is.finite(u_lo) || !is.finite(u_hi) || u_hi - u_lo < 1e-6) return(NULL)
  N <- if (binary) as.integer(min(40000, floor(2e7 / (n + 1)))) else 4000L
  u <- u_lo + (seq_len(N) - 0.5) / N * (u_hi - u_lo)
  theta <- qf(u)
  theta <- pmin(pmax(theta, if (binary) 1e-12 else 1e-12), if (binary) 1 - 1e-12 else Inf)
  if (any(!is.finite(theta))) return(NULL)
  if (binary) {
    if (n > 2000) return(NULL)
    k <- 0:n
    pmf <- numeric(length(k))
    for (blk in split(seq_len(N), ceiling(seq_len(N) / 500))) {
      pmf <- pmf + rowSums(vapply(theta[blk], function(t)
        stats::dbinom(k, n, t), numeric(length(k))))
    }
  } else {
    lam_max <- max(theta) * n
    k_max <- max(x, stats::qpois(1 - 1e-10, lam_max)) + 1
    if (!is.finite(k_max) || k_max > 20000 || N * (k_max + 1) > 1e8) return(NULL)
    k <- 0:k_max
    pmf <- numeric(length(k))
    for (blk in split(seq_len(N), ceiling(seq_len(N) / 200))) {
      pmf <- pmf + rowSums(vapply(theta[blk], function(t)
        stats::dpois(k, t * n), numeric(length(k))))
    }
  }
  pmf <- pmf / N
  if (!is.finite(sum(pmf)) || sum(pmf) < 1 - 1e-6) return(NULL)
  pmf <- pmf / sum(pmf)
  attr(pmf, "numerical") <- TRUE
  pmf
}

#' Dispatch to an exact prior predictive p-value where one exists (internal)
#'
#' Used by \code{\link{prior_conflict}}. Returns \code{NULL} (rather than
#' erroring) when the prior family and data type combination has no
#' closed-form prior predictive, so the caller can fall back to the
#' Normal-approximation \code{box_pvalue}. The p-value is the
#' probability-ordered one: the total prior predictive probability of all
#' outcomes no more probable than the observed one.
#'
#' @param prior A \code{bayprior} object.
#' @param type Data type (\code{data_summary$type}).
#' @param x,n Observed count/exposure, as used elsewhere in
#'   \code{\link{prior_conflict}}.
#'
#' @return A single numeric p-value, or \code{NULL} if unsupported.
#'
#' @keywords internal
#' @noRd
.exact_box_pvalue <- function(prior, type, x, n) {
  # The exact checks are defined for whole-number counts and (for binary
  # data) a whole-number sample size with 0 <= x <= n. Anything else, or a
  # support so large that enumerating it is impractical (where the Normal
  # approximation is accurate anyway), is reported as "unsupported" so the
  # caller can fall back rather than index a probability table wrongly.
  whole <- function(v) is.numeric(v) && length(v) == 1L && is.finite(v) &&
    abs(v - round(v)) < 1e-8
  if (!whole(x) || x < 0) return(NULL)
  if (identical(type, "binary")) {
    if (!whole(n) || n < 1 || x > n || n > 1e6) return(NULL)
  } else if (type %in% c("poisson", "survival")) {
    if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n <= 0) return(NULL)
  } else {
    # Normal-Normal (continuous data) is already exact under the existing
    # Normal machinery; no separate exact path is needed.
    return(NULL)
  }
  pmf <- .predictive_pmf(prior, type, n, x)
  if (is.null(pmf) || x + 1 > length(pmf)) return(NULL)
  p_obs <- pmf[x + 1]
  # Numerical tolerance on the "at least as extreme" comparison, since
  # p_obs and other pmf entries are floating-point sums of log-scale terms
  # and an exact `<=` can otherwise exclude p_obs itself due to rounding.
  # Quadrature carries a little more error, hence the looser tolerance.
  tol <- if (isTRUE(attr(pmf, "numerical"))) 1e-6 else 1e-8
  structure(min(1, sum(pmf[pmf <= p_obs * (1 + tol)])),
            numerical = isTRUE(attr(pmf, "numerical")))
}

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
#'     \item{\code{type}}{\code{"binary"}, \code{"continuous"},
#'       \code{"poisson"}, or \code{"survival"}.}
#'     \item{\code{x}}{Number of events (binary / poisson / survival)
#'       or observed mean (continuous).}
#'     \item{\code{n}}{Sample size (binary / continuous), total exposure
#'       (poisson: person-time), or total follow-up time (survival).}
#'     \item{\code{sd}}{Observed standard deviation (continuous only).}
#'   }
#' @param alpha Numeric. Significance level for the Box p-value flag.
#'   Default \code{0.05}.
#' @param exact \code{NULL} (the default), \code{TRUE}, or \code{FALSE}.
#'   Controls how \code{box_pvalue} (and the \code{s_value} derived from it)
#'   is computed.
#'   \describe{
#'     \item{\code{NULL} (default)}{Use the \strong{exact} prior predictive
#'       distribution whenever a closed form exists, and the Normal
#'       approximation otherwise, silently. Check \code{$box_pvalue_method}
#'       on the returned object to see which was used.}
#'     \item{\code{TRUE}}{As \code{NULL}, but signal a message when the
#'       prior/data combination has no closed-form check and the Normal
#'       approximation is used instead.}
#'     \item{\code{FALSE}}{Always use the Normal approximation to both the
#'       prior and the data likelihood (the behaviour of bayprior 0.4.0 and
#'       earlier).}
#'   }
#'   Closed forms exist for a Beta prior with \code{type = "binary"} data
#'   (Beta-Binomial) and for a Gamma prior with \code{type = "poisson"} or
#'   \code{type = "survival"} data (Negative Binomial; Gamma-Poisson and
#'   Gamma-Exponential conjugacy are the same model under this package's
#'   (events, exposure) parameterisation). The exact check also requires
#'   whole-number counts; otherwise the Normal approximation is used.
#'   \code{surprise_index}, \code{kl_prior_likelihood}, and \code{overlap}
#'   are unaffected by \code{exact} -- they are defined with respect to the
#'   Normal-approximated prior and likelihood (see Details).
#'
#' @return An object of class \code{bayprior_conflict} containing:
#'   \describe{
#'     \item{\code{box_pvalue}}{Box's prior predictive p-value.}
#'     \item{\code{box_pvalue_method}}{\code{"exact"} (closed-form prior
#'       predictive), \code{"numerical"} (the same prior predictive p-value,
#'       with the predictive computed by quadrature) or
#'       \code{"normal_approx"}, indicating how \code{box_pvalue} (and
#'       hence \code{s_value}) was computed. Always \code{"normal_approx"}
#'       when \code{exact = FALSE}.}
#'     \item{\code{box_pvalue_caution}}{\code{NULL}, or a short message
#'       when the Normal approximation produced \code{box_pvalue} for
#'       discrete data with a boundary (zero or all events) or small
#'       (fewer than 10) counts, where it can be anti-conservative.}
#'     \item{\code{s_value}}{Surprisal, \eqn{-\log_2(\text{box\_pvalue})},
#'       in bits. See Details.}
#'     \item{\code{surprise_index}}{Standardised distance between prior mean
#'       and observed data.}
#'     \item{\code{kl_prior_likelihood}}{KL divergence from prior to likelihood.}
#'     \item{\code{overlap}}{Bhattacharyya overlap coefficient in \[0, 1\].}
#'     \item{\code{conflict_severity}}{One of \code{"none"}, \code{"mild"},
#'       \code{"severe"}.}
#'     \item{\code{conflict_flag}}{Logical; \code{TRUE} if
#'       \code{box_pvalue < alpha}.}
#'     \item{\code{recommendation}}{Plain-language guidance string.}
#'     \item{\code{data_summary}}{The data summary passed in.}
#'     \item{\code{prior}}{The input prior.}
#'   }
#'
#' @details
#' \code{s_value} and \code{surprise_index} are easily confused by name but
#' measure different things. \code{surprise_index} is a \emph{distance}:
#' how many prior-SD units the observed data sits from the prior mean
#' (\code{abs(z)}). \code{s_value} is an \emph{evidence-strength}
#' reparametrisation of \code{box_pvalue} itself, expressed in bits --
#' intended as an exploratory companion to
#' \code{box_pvalue} rather than a thresholded decision rule, since
#' prior-data conflict checks are typically not powered for a binary
#' accept/reject decision. The two are correlated but nonlinearly related:
#' \code{s_value} grows faster than \code{surprise_index} as evidence
#' strengthens, since it derives from \code{box_pvalue}'s tail probability,
#' not from \code{z} directly.
#'
#' \strong{Mixture priors.} A linear mixture of Beta (binary data) or Gamma
#' (Poisson/survival data) components, such as a \code{\link{robust_prior}}
#' or a linearly pooled consensus from \code{\link{aggregate_experts}}, has
#' as prior predictive the weighted mixture of its components' predictives,
#' so its p-value is exact as well. \code{severity} is classified on the
#' reported \code{box_pvalue}: \code{"mild"} for \code{alpha > p >= 0.0027}
#' and \code{"severe"} below \code{0.0027} (the two-sided Normal tail
#' beyond 3 standard deviations).
#'
#' The numerical integration uses 4,000 (count data) to 40,000 (binary data,
#' small \code{n}) quadrature points, so the p-value is accurate to roughly
#' 1e-5 in the predictive probabilities. Run time grows with the number of
#' possible counts: well under a second typically, a few seconds when the count
#' support reaches several thousand (for example a heavy-tailed prior with
#' exposure in the hundreds), where the Normal approximation is also adequate.
#'
#' \strong{Why exact is the default.} The Normal approximation estimates the
#' sampling standard error from the observed data (for example
#' \eqn{\sqrt{\hat p(1-\hat p)/n}}), which is zero when no events are
#' observed. For an informative prior and a small sample, an observation of
#' zero events can be common under the prior predictive distribution yet
#' receive a very small approximate p-value, so the approximate check can
#' flag conflict far more often than its nominal level when there is none.
#' The exact prior predictive p-value does not have this problem: it is
#' computed from the prior predictive distribution itself, and its rejection
#' rate under no conflict is at most the nominal level (it can be below it
#' because the data are discrete). The two agree closely for moderate
#' samples and weakly informative priors. Use \code{exact = FALSE} to
#' reproduce the Normal-approximation results of earlier versions.
#'
#' The exact p-value is the prior predictive check of Box (1980) in the form
#' used by Evans and Moshonov (2006): the total prior predictive probability
#' of the outcomes that are no more probable than the observed one. The
#' Normal approximation with a plug-in standard error is a convenience
#' approximation to it, not part of either definition.
#'
#' \strong{What the check can and cannot tell you.} In the package's
#' simulation study (see the \code{validation} folder of the source
#' repository), the check had little power against true values within about
#' one prior standard deviation of the prior mean, whichever version was used,
#' so a non-significant result is weak evidence that a prior is adequate. For
#' skewed priors the probability ordering can also be less sensitive to true
#' values below the prior mean than to values above it, because low counts
#' (including zero) are probable under a right-skewed prior predictive
#' distribution. Use the result together with \code{\link{sensitivity_grid}}
#' and judgement about the source of the prior, not as a stand-alone test.
#'
#' @references
#' Evans, M. and Moshonov, H. (2006). Checking for prior-data conflict.
#' \emph{Bayesian Analysis}, 1(4).
#'
#' Box, G. E. P. (1980). Sampling and Bayes' inference in scientific modelling
#' and robustness. \emph{Journal of the Royal Statistical Society A}, 143,
#' 383-430.
#'
#' Greenland, S. (2023). Divergence versus decision P-values: A distinction
#' worth making in theory and keeping in practice. \emph{Scandinavian
#' Journal of Statistics}, 50(1), 54-88.
#'
#' @examples
#' prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments",
#'                      label = "Response rate")
#' cd <- prior_conflict(prior, list(type = "binary", x = 18, n = 40))
#' print(cd)
#'
#' @importFrom rlang %||% abort
#' @export
prior_conflict <- function(prior, data_summary, alpha = 0.05, exact = NULL) {

  if (!inherits(prior, "bayprior")) {
    rlang::abort("`prior` must be a bayprior object.")
  }

  if (!is.null(exact) &&
      !(is.logical(exact) && length(exact) == 1L && !is.na(exact))) {
    rlang::abort("`exact` must be NULL, TRUE, or FALSE.")
  }

  type <- data_summary$type %||% "binary"
  n    <- data_summary$n
  x    <- data_summary$x

  # Approximate prior as Normal for analytic diagnostics
  prior_mean <- prior$fit_summary$mean
  prior_sd   <- prior$fit_summary$sd

  # Likelihood parameters -- approximate as Normal for analytic diagnostics
  if (type == "binary") {
    obs_mean <- x / n
    obs_se   <- sqrt(obs_mean * (1 - obs_mean) / n)
  } else if (type %in% c("poisson", "survival")) {
    # Poisson: x events over exposure n; rate = x/n, SE via delta method
    # Survival: x events over total follow-up n; hazard = x/n
    obs_mean <- x / n
    obs_se   <- sqrt(x) / n        # delta method: SE(x/n) = sqrt(x)/n
  } else {
    obs_mean <- x
    obs_se   <- data_summary$sd / sqrt(n)
  }

  # Guard against obs_se = 0 (e.g. all successes / all failures),
  # which would cause division-by-zero in pred_sd, z, kl, and overlap.
  obs_se <- max(obs_se, 1e-8)

  # Box's prior predictive p-value (Normal approximation -- always computed,
  # since it is needed below for surprise_index/severity regardless of
  # `exact`, and as the fallback when no closed form exists for
  # this prior family/data-type combination).
  pred_sd <- sqrt(prior_sd^2 + obs_se^2)
  z       <- (obs_mean - prior_mean) / pred_sd
  box_p   <- 2 * stats::pnorm(-abs(z))
  box_pvalue_method <- "normal_approx"

  # exact = NULL (default): exact where a closed form exists, silently
  # falling back to the approximation otherwise; exact = TRUE: same, but
  # with a message on fallback; exact = FALSE: approximation only.
  if (is.null(exact) || isTRUE(exact)) {
    exact_p <- .exact_box_pvalue(prior, type, x, n)
    if (!is.null(exact_p)) {
      box_pvalue_method <- if (isTRUE(attr(exact_p, "numerical"))) "numerical" else "exact"
      box_p <- as.numeric(exact_p)
    } else if (isTRUE(exact)) {
      message(
        "prior_conflict(): exact = TRUE requested, but no closed-form ",
        "exact prior predictive is available for a '", prior$dist,
        "' prior with type = '", type, "' data (or the counts are not ",
        "whole numbers). Falling back to the ",
        "Normal approximation for box_pvalue (see $box_pvalue_method on ",
        "the returned object)."
      )
    }
  }

  # Surprise index: standardised distance between the prior mean and the
  # observed data, in prior-predictive SD units. Always the Normal-
  # approximation version (see `exact` argument docs above) -- it is not
  # redefined by an exact box_pvalue.
  surprise <- abs(z)

  # Surprisal / S-value (Greenland, 2023): -log2(box_pvalue), reported as an
  # exploratory, continuous companion to box_pvalue rather than a thresholded
  # decision rule. box_p is bounded in (0, 1]; guard the box_p == 0 edge case
  # (extreme conflict) to avoid returning Inf.
  s_value <- if (box_p > 0) -log2(box_p) else -log2(.Machine$double.xmin)

  # KL divergence (normal approximation); see .kl_normal() for convention
  kl <- .kl_normal(prior_mean, prior_sd, obs_mean, obs_se)

  # Bhattacharyya overlap
  overlap <- .bhattacharyya_normal(prior_mean, prior_sd, obs_mean, obs_se)

  # Severity classification, on the same p-value that is reported (exact or
  # approximate). "Severe" is p < 2 * pnorm(-3) (about 0.0027), which is
  # exactly the earlier "surprise >= 3" rule whenever the Normal
  # approximation is in use, and now also applies consistently to exact
  # p-values rather than mixing an exact p with an approximate distance.
  severe_p <- 2 * stats::pnorm(-3)
  severity <- dplyr::case_when(
    box_p >= alpha  ~ "none",
    box_p >= severe_p ~ "mild",
    TRUE            ~ "severe"
  )

  # Caution: the Normal approximation is anti-conservative for discrete data
  # with informative priors and small or boundary counts (see the validation
  # study shipped in the repository). Flag when it is what produced the
  # p-value in that regime, so the result is not read as calibrated.
  box_pvalue_caution <- NULL
  if (identical(box_pvalue_method, "normal_approx") &&
      type %in% c("binary", "poisson", "survival")) {
    n_other <- if (type == "binary") n - x else Inf
    if (x == 0 || n_other == 0) {
      box_pvalue_caution <- paste0(
        "Observed count is at the boundary (", if (x == 0) "0 events" else
        "all events", "); the Normal-approximation p-value is unreliable ",
        "here and no exact prior predictive is available for this prior.")
    } else if (min(x, n_other) < 10) {
      box_pvalue_caution <- paste0(
        "Small observed counts (fewer than 10 events or non-events); the ",
        "Normal-approximation p-value can be anti-conservative and no ",
        "exact prior predictive is available for this prior.")
    }
  }

  recommendation <- switch(severity,
    none = glue::glue(
      "No evidence of prior-data conflict (Box p = {round(box_p, 3)}, ",
      "S-value = {round(s_value, 2)} bits). ",
      "The prior appears consistent with the observed data."
    ),
    mild = glue::glue(
      "Mild prior-data conflict detected (Box p = {round(box_p, 3)}, ",
      "S-value = {round(s_value, 2)} bits, surprise = {round(surprise, 2)}). ",
      "Consider reporting a sensitivity analysis with a more diffuse prior."
    ),
    severe = glue::glue(
      "Severe prior-data conflict detected (Box p = {round(box_p, 4)}, ",
      "S-value = {round(s_value, 2)} bits, surprise = {round(surprise, 2)}). ",
      "Re-elicitation or use of a robust/sceptical prior is strongly recommended."
    )
  )

  structure(
    list(
      box_pvalue          = box_p,
      box_pvalue_method   = box_pvalue_method,
      box_pvalue_caution  = box_pvalue_caution,
      s_value             = s_value,
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


#' Derive a single-family working prior from a mixture (internal)
#'
#' Sensitivity grids are defined over a single distribution family's
#' hyperparameters, so mixture priors (from \code{\link{aggregate_experts}}
#' or \code{\link{robust_prior}}) need a single-family stand-in. This
#' moment-matches the mixture's actual pooled mean and SD (from
#' \code{prior$fit_summary}) to the dominant component's family, so the
#' working prior reflects the full pooled information rather than
#' discarding all but one component. Falls back to the dominant component
#' alone, with a warning, if the family cannot be moment-matched from
#' mean/SD (e.g. \code{"exponential"}, \code{"weibull"}) or if
#' \code{fit_summary$sd} is unavailable (e.g. logarithmic pooling).
#'
#' Shared by \code{\link{sensitivity_grid}} and the Shiny sensitivity
#' module (\code{mod_sensitivity.R}) so that the parameter ranges shown in
#' the UI are always centred on the same working prior actually analysed.
#'
#' See \code{\link{aggregate_experts}} for the pooled mean/variance
#' formula, and \code{\link{elicit_beta}}, \code{\link{elicit_normal}},
#' \code{\link{elicit_gamma}}, \code{\link{elicit_lognormal}} for each
#' family's moment-matching identities.
#'
#' @param prior A \code{bayprior} object, mixture or single-family.
#' @param quiet Logical. If \code{TRUE}, suppress the informational message
#'   emitted when moment-matching succeeds (warnings for failures/fallback
#'   are still shown). Useful for reactive UI contexts. Default \code{FALSE}.
#'
#' @return A single-family \code{bayprior} object.
#' @keywords internal
.mixture_working_prior <- function(prior, quiet = FALSE) {

  if (!prior$dist %in% c("mixture", "log_pool")) return(prior)

  dominant    <- which.max(prior$weights)
  fam         <- prior$components[[dominant]]$dist
  fams        <- unique(vapply(prior$components, function(x) x$dist, character(1)))
  pooled_mean <- prior$fit_summary$mean
  pooled_sd   <- prior$fit_summary$sd

  if (length(fams) > 1) {
    rlang::warn(paste0(
      "[bayprior] mixture components have different distribution ",
      "families (", paste(fams, collapse = ", "), "). Moment-matching ",
      "the pooled mean/SD against the dominant component's family ('",
      fam, "')."
    ))
  }

  moment_matcher <- switch(fam,
    beta      = elicit_beta,
    normal    = elicit_normal,
    gamma     = elicit_gamma,
    lognormal = elicit_lognormal,
    NULL
  )

  matched <- if (is.null(moment_matcher) || is.null(pooled_sd) || is.na(pooled_sd)) {
    NULL
  } else {
    tryCatch(
      moment_matcher(mean = pooled_mean, sd = pooled_sd, method = "moments",
                     label = prior$label,
                     expert_id = "Pooled (moment-matched)"),
      error = function(e) NULL
    )
  }

  if (is.null(matched)) {
    rlang::warn(paste0(
      "[bayprior] could not moment-match the pooled '", fam, "' mixture ",
      "(mean = ", round(pooled_mean, 4), ", SD = ",
      round(pooled_sd %||% NA_real_, 4), "). Falling back to the dominant ",
      "component only (weight = ", round(prior$weights[dominant], 3),
      ", expert_id = '", prior$components[[dominant]]$expert_id, "'). ",
      "Results reflect this single component, not the full pooled prior."
    ))
    return(prior$components[[dominant]])
  }

  if (!quiet) {
    message(paste0(
      "[bayprior] using a moment-matched working prior (", toupper(fam),
      ", mean = ", round(pooled_mean, 4), ", SD = ", round(pooled_sd, 4),
      ") that represents the full pooled mixture, not a single component."
    ))
  }
  matched
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
#' @details
#' The hyperparameter grid is defined over a single distribution family, so
#' when \code{prior} is a mixture (e.g. from \code{\link{aggregate_experts}}
#' or \code{\link{robust_prior}}), a single-family \emph{working prior} is
#' derived first, via the internal \code{.mixture_working_prior()} helper.
#' As of this version, that working prior is obtained in two steps: first,
#' the mixture's pooled mean and SD are computed exactly (see the mean/var
#' formula in \code{\link{aggregate_experts}}); second, those pooled moments
#' are matched to the dominant component's distribution family using that
#' family's own moment-matching identities (see \code{\link{elicit_beta}},
#' \code{\link{elicit_normal}}, \code{\link{elicit_gamma}}, or
#' \code{\link{elicit_lognormal}} for the specific formula used). The grid
#' therefore reflects the full pooled information, not just one component.
#' A message reports the working prior used. If the dominant component's
#' family cannot be moment-matched from mean/SD alone (\code{"exponential"}
#' or \code{"weibull"}), the function falls back to the dominant component
#' by weight and issues an explicit warning identifying which component
#' was used and why.
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

  # -- For mixture priors, derive a single-family working prior ----------------
  # See .mixture_working_prior() for the moment-matching logic; shared with
  # the Shiny sensitivity module so UI defaults stay consistent with what
  # is actually analysed here.
  working_prior <- .mixture_working_prior(prior)

  # -- Auto-remap param_grid names to prior hyperparameter names ----------------
  #
  # The Shiny UI generates generic names ("param1", "param2", ?) that will
  # never literally match prior hyperparameter names ("alpha"/"beta", etc.).
  # We resolve this with a three-step strategy:
  #
  #   1. Exact match  -- use as-is (names already correct).
  #   2. Positional remap -- if no names match but the *count* equals the number
  #      of prior hyperparameters, rename param_grid entries to the prior's
  #      param names in order and warn the caller.
  #   3. Abort -- counts also differ; the mapping is genuinely ambiguous.
  #
  prior_param_names <- names(working_prior$params)
  grid_param_names  <- names(param_grid)
  valid_names       <- intersect(grid_param_names, prior_param_names)

  if (length(valid_names) == 0) {
    # No exact matches -- attempt positional remap
    if (length(grid_param_names) == length(prior_param_names)) {
      # Downgraded to a message (not a warning) because this is expected
      # behaviour when the Shiny UI passes generic names like param1/param2.
      # rlang::warn() causes a Shiny warning banner; message() does not.
      message(paste0(
        "[bayprior] sensitivity_grid: remapping param_grid names positionally: ",
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
  # mapply() returns a plain vector when param_grid has only one entry,
  # which would make rowSums() produce wrong scalar results. matrix() with
  # explicit nrow handles both single- and multi-parameter cases correctly.
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

  # Detect the case where every grid point produced NA (e.g. because
  # .make_bayprior() or .conjugate_update() failed for all rows) and abort
  # with a clear message before calling range() or building a colorscale --
  # otherwise this cascades into "no non-missing arguments to min/max"
  # warnings and a broken Plotly colorscale downstream.
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
      "  3. data_summary$type is set correctly ('binary', 'continuous', 'poisson', or 'survival')."
    ))
  }

  # Warn about target columns missing from results entirely (distinct from
  # a target column that exists but is all-NA, handled above).
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



#' Credible interval sensitivity over prior hyperparameters
#'
#' Evaluates how the posterior credible interval width and bounds change as
#' prior hyperparameters vary over a specified grid. This is the preferred
#' function for demonstrating that key regulatory conclusions (e.g., whether
#' the CrI excludes a null value) are robust to prior choice.
#'
#' @param prior        A \code{bayprior} object (the reference prior).
#' @param data_summary Named list as for \code{\link{prior_conflict}}.
#' @param param_grid   Named list of numeric vectors, one per hyperparameter.
#' @param cri_level    Numeric in (0, 1). Credible interval level. Default
#'   \code{0.95}.
#' @param threshold    Optional numeric. Computes \code{Pr(theta > threshold)}
#'   at each grid point if supplied.
#'
#' @details
#' As with \code{\link{sensitivity_grid}}, when \code{prior} is a mixture,
#' a single-family working prior is derived via \code{.mixture_working_prior()}
#' -- moment-matching the mixture's pooled mean/SD to the dominant
#' component's family -- rather than analysing the dominant component alone.
#'
#' @return An object of class \code{bayprior_sensitivity} whose grid contains
#'   columns \code{cri_lower}, \code{cri_upper}, and \code{cri_width}, plus
#'   optionally \code{posterior_mean}, \code{posterior_sd}, and
#'   \code{prob_efficacy}.
#'
#' @examples
#' prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments",
#'                      label = "Response rate")
#' cri_sa <- sensitivity_cri(
#'   prior,
#'   data_summary = list(type = "binary", x = 14, n = 40),
#'   param_grid   = list(alpha = seq(1, 8, 0.5), beta = seq(2, 20, 1)),
#'   cri_level    = 0.95
#' )
#' plot_sensitivity(cri_sa, target = "cri_width")
#'
#' @importFrom rlang %||% abort
#' @export
sensitivity_cri <- function(prior,
                             data_summary,
                             param_grid,
                             cri_level = 0.95,
                             threshold = NULL) {

  if (!inherits(prior, "bayprior"))
    rlang::abort("`prior` must be a bayprior object.")
  if (!is.numeric(cri_level) || cri_level <= 0 || cri_level >= 1)
    rlang::abort("`cri_level` must be a number strictly between 0 and 1.")

  alpha_lo <- (1 - cri_level) / 2
  alpha_hi <- 1 - alpha_lo

  # Moment-match a mixture prior's pooled mean/SD to a working prior in the
  # dominant component's family (see .mixture_working_prior() for details
  # and the same fix applied to sensitivity_grid()), rather than discarding
  # all but the dominant component.
  working_prior <- .mixture_working_prior(prior)

  # Remap generic param names (e.g. "param1", "param2") to prior param names
  prior_param_names <- names(working_prior$params)
  grid_param_names  <- names(param_grid)
  valid_names       <- intersect(grid_param_names, prior_param_names)

  if (length(valid_names) == 0) {
    if (length(grid_param_names) == length(prior_param_names)) {
      message(paste0(
        "[bayprior] sensitivity_cri: remapping param_grid names positionally: ",
        paste(grid_param_names, "->", prior_param_names, collapse = ", "), "."
      ))
      names(param_grid) <- prior_param_names
      valid_names       <- prior_param_names
    } else {
      rlang::abort(paste0(
        "Cannot map param_grid to prior hyperparameters. ",
        "Names don't match and counts differ.\n",
        "  param_grid: ", paste(grid_param_names, collapse = ", "), "\n",
        "  Prior params: ", paste(prior_param_names, collapse = ", ")
      ))
    }
  }

  grid_df <- do.call(expand.grid, param_grid)

  # Reference row: closest grid point to working prior's current parameters
  ref_diffs <- vapply(valid_names, function(nm) {
    (grid_df[[nm]] - (working_prior$params[[nm]] %||% 0))^2
  }, numeric(nrow(grid_df)))

  # Always treat as matrix to avoid apply() dimension errors on single-param grids
  ref_diffs <- matrix(ref_diffs, nrow = nrow(grid_df))
  ref_row   <- which.min(rowSums(ref_diffs))

  # Evaluate CrI at each grid point using purrr::map_dfr (never apply())
  target_cols <- c("cri_lower", "cri_upper", "cri_width", "posterior_mean",
                   "posterior_sd")
  if (!is.null(threshold)) target_cols <- c(target_cols, "prob_efficacy")

  results <- purrr::map_dfr(seq_len(nrow(grid_df)), function(i) {
    row    <- grid_df[i, , drop = FALSE]
    params <- as.list(row)

    tmp_prior <- tryCatch(
      .make_bayprior(working_prior$dist, params, working_prior$method,
                     working_prior$expert_id, working_prior$label,
                     working_prior$input),
      error = function(e) NULL
    )

    na_row <- as.data.frame(c(as.list(row),
                              list(cri_lower     = NA_real_,
                                   cri_upper     = NA_real_,
                                   cri_width     = NA_real_,
                                   posterior_mean = NA_real_,
                                   posterior_sd   = NA_real_)))
    if (!is.null(threshold)) na_row$prob_efficacy <- NA_real_
    if (is.null(tmp_prior)) return(na_row)

    post <- tryCatch(
      .conjugate_update(tmp_prior, data_summary),
      error = function(e) NULL
    )
    if (is.null(post)) return(na_row)

    s <- post$fit_summary
    out <- as.data.frame(as.list(row))

    # Compute posterior quantiles for CrI bounds
    bounds <- tryCatch({
      if (post$dist == "beta") {
        c(stats::qbeta(alpha_lo, post$params$alpha, post$params$beta),
          stats::qbeta(alpha_hi, post$params$alpha, post$params$beta))
      } else if (post$dist == "normal") {
        c(stats::qnorm(alpha_lo, post$params$mu, post$params$sigma),
          stats::qnorm(alpha_hi, post$params$mu, post$params$sigma))
      } else if (post$dist == "gamma") {
        c(stats::qgamma(alpha_lo, post$params$shape, post$params$rate),
          stats::qgamma(alpha_hi, post$params$shape, post$params$rate))
      } else {
        # Normal approximation for mixture / other
        c(stats::qnorm(alpha_lo, s$mean, s$sd),
          stats::qnorm(alpha_hi, s$mean, s$sd))
      }
    }, error = function(e) c(NA_real_, NA_real_))

    out$cri_lower      <- bounds[1]
    out$cri_upper      <- bounds[2]
    out$cri_width      <- bounds[2] - bounds[1]
    out$posterior_mean <- s$mean
    out$posterior_sd   <- s$sd

    if (!is.null(threshold)) {
      out$prob_efficacy <- tryCatch({
        if (post$dist == "beta")
          stats::pbeta(threshold, post$params$alpha, post$params$beta,
                       lower.tail = FALSE)
        else if (post$dist == "normal")
          stats::pnorm(threshold, post$params$mu, post$params$sigma,
                       lower.tail = FALSE)
        else if (post$dist == "gamma")
          stats::pgamma(threshold, post$params$shape, post$params$rate,
                        lower.tail = FALSE)
        else
          stats::pnorm(threshold, s$mean, s$sd, lower.tail = FALSE)
      }, error = function(e) NA_real_)
    }

    out
  })

  report_targets <- intersect(target_cols, names(results))

  influence <- vapply(report_targets, function(t) {
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
      target           = report_targets,
      reference_row    = ref_row,
      influence_scores = influence,
      cri_level        = cri_level,
      threshold        = threshold,
      prior            = prior
    ),
    class = "bayprior_sensitivity"
  )
}



# Define %||% explicitly so it is available regardless of whether rlang is
# attached (it may only be in Imports, not Depends). Functions in this file
# that carry @importFrom rlang %||% will also satisfy R CMD CHECK.
`%||%` <- rlang::`%||%`


# KL(P||Q): KL divergence from P = Normal(m1, s1) to Q = Normal(m2, s2).
# Formula: log(s2/s1) + (s1^2 + (m1-m2)^2) / (2*s2^2) - 1/2
# Argument order is (from_mean, from_sd, to_mean, to_sd) -- kept explicit
# here to prevent future sign/argument confusion.
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

  # -- Beta / binary ----------------------------------------------------------
  if (prior$dist == "beta" && type == "binary") {
    a_post <- prior$params$alpha + x
    b_post <- prior$params$beta  + (n - x)
    return(.make_bayprior("beta", list(alpha = a_post, beta = b_post),
                          "posterior", prior$expert_id, prior$label, list()))
  }

  # -- Normal -----------------------------------------------------------------
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

  # -- Exponential / Poisson or survival ---------------------------------------
  # Exponential prior on rate is equivalent to Gamma(1, rate) parameterisation.
  # Conjugate update with Poisson/survival data: posterior is still Gamma.
  # Map to Gamma update: shape = 1 + x, rate = rate + n.
  if (prior$dist == "exponential" && type %in% c("poisson", "survival", "binary")) {
    # Represent as Gamma(1, lambda) for conjugate update
    lambda     <- prior$params$rate
    shape_post <- 1 + x
    rate_post  <- lambda + n
    return(.make_bayprior("gamma", list(shape = shape_post, rate = rate_post),
                          "posterior", prior$expert_id, prior$label, list()))
  }

  # -- Weibull -- Normal approximation ------------------------------------------
  # No closed-form conjugate update for Weibull. Approximate via a Normal
  # distribution matched to the posterior mean and SD using the prior's
  # fit_summary as the prior parameters.
  if (prior$dist == "weibull") {
    prior_mean <- prior$fit_summary$mean
    prior_sd   <- prior$fit_summary$sd
    if (type %in% c("poisson", "survival")) {
      obs_mean_w <- x / n
      obs_se_w   <- sqrt(x) / n
    } else if (type == "binary") {
      obs_mean_w <- x / n
      obs_se_w   <- sqrt(obs_mean_w * (1 - obs_mean_w) / n)
    } else {
      obs_mean_w <- x
      obs_se_w   <- (data_summary$sd %||% prior_sd) / sqrt(n)
    }
    obs_se_w   <- max(obs_se_w, 1e-8)
    prior_var  <- prior_sd^2
    lik_var    <- obs_se_w^2
    post_var   <- 1 / (1 / prior_var + 1 / lik_var)
    post_mean  <- post_var * (prior_mean / prior_var + obs_mean_w / lik_var)
    return(.make_bayprior("normal",
                          list(mu = post_mean, sigma = sqrt(post_var)),
                          "posterior", prior$expert_id, prior$label, list()))
  }

  # -- Gamma / Poisson or survival ---------------------------------------------
  # Gamma(shape, rate) prior on rate lambda; Poisson(lambda * n) likelihood.
  # Posterior: Gamma(shape + x, rate + n).
  # For survival data: Gamma prior on hazard; Exponential(lambda) lifetimes.
  # Posterior: Gamma(shape + events, rate + total_follow_up).
  if (prior$dist == "gamma" && type %in% c("poisson", "survival")) {
    shape_post <- prior$params$shape + x
    rate_post  <- prior$params$rate  + n
    return(.make_bayprior("gamma", list(shape = shape_post, rate = rate_post),
                          "posterior", prior$expert_id, prior$label, list()))
  }

  # -- Gamma ------------------------------------------------------------------
  # For continuous data, x is the observed mean (not a total count), so the
  # total must be recovered before updating the Gamma rate. Prefer an
  # explicit x_sum field when available (e.g. Poisson count data) and fall
  # back to n * x otherwise.
  if (prior$dist == "gamma" && type == "continuous") {
    x_sum      <- data_summary$x_sum %||% (x * n)
    shape_post <- prior$params$shape + x_sum
    rate_post  <- prior$params$rate  + n
    return(.make_bayprior("gamma", list(shape = shape_post, rate = rate_post),
                          "posterior", prior$expert_id, prior$label, list()))
  }

  # -- Mixture -- update each component and re-weight by marginal likelihood ---
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

  # -- Generic fallback: Normal approximation ----------------------------------
  # No exact conjugate formula exists for this prior/data-type pairing (e.g.
  # a Beta prior with continuous data -- Beta is conjugate to Binomial, not
  # to a continuous likelihood). Previously this aborted, which contradicted
  # the compatibility warning shown to users elsewhere in the package, which
  # explicitly states the analysis will "proceed using a Normal
  # approximation." Generalises the same approach already used for Weibull
  # above (and the same approximation prior_conflict() uses for its
  # diagnostics): approximate both prior and likelihood as Normal via the
  # prior's fit_summary, and return a Normal posterior. This keeps the
  # promised approximation behaviour consistent between the numeric
  # diagnostics and any posterior-dependent plot or downstream computation,
  # including each component of a mixture prior.
  prior_mean <- prior$fit_summary$mean
  prior_sd   <- prior$fit_summary$sd

  if (is.null(prior_mean) || is.null(prior_sd) ||
      is.na(prior_mean)  || is.na(prior_sd)) {
    rlang::abort(glue::glue(
      "Conjugate update not implemented for dist='{prior$dist}' with ",
      "type='{type}', and no fit_summary mean/SD is available for a ",
      "Normal-approximation fallback."
    ))
  }

  if (type %in% c("poisson", "survival")) {
    obs_mean <- x / n
    obs_se   <- sqrt(x) / n
  } else if (type == "binary") {
    obs_mean <- x / n
    obs_se   <- sqrt(obs_mean * (1 - obs_mean) / n)
  } else {
    obs_mean <- x
    obs_se   <- (data_summary$sd %||% prior_sd) / sqrt(n)
  }
  obs_se <- max(obs_se, 1e-8)

  prior_var <- prior_sd^2
  lik_var   <- obs_se^2
  post_var  <- 1 / (1 / prior_var + 1 / lik_var)
  post_mean <- post_var * (prior_mean / prior_var + obs_mean / lik_var)

  rlang::inform(glue::glue(
    "No exact conjugate update exists for dist='{prior$dist}' with ",
    "type='{type}'; approximated the posterior as Normal(mean = ",
    "{round(post_mean, 4)}, sd = {round(sqrt(post_var), 4)})."
  ))

  .make_bayprior("normal", list(mu = post_mean, sigma = sqrt(post_var)),
                 "posterior (normal approximation)", prior$expert_id,
                 prior$label, list())
}