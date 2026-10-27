# CDF and quantiles of a bayprior, including mixtures.
#
# A mixture has no closed-form quantile function, and mean +/- 1.96 SD is not
# its 95% interval (a bimodal or skewed mixture is not Normal). The quantiles
# are found by solving  sum_i w_i F_i(q) = p  with uniroot().

#' @noRd
.family_cdf <- function(dist, p, q) {
  switch(dist,
    beta        = stats::pbeta(q, p$alpha, p$beta),
    normal      = stats::pnorm(q, p$mu, p$sigma),
    gamma       = stats::pgamma(q, p$shape, p$rate),
    lognormal   = stats::plnorm(q, p$meanlog, p$sdlog),
    exponential = stats::pexp(q, rate = p$rate),
    weibull     = stats::pweibull(q, p$shape, p$scale),
    rep(NA_real_, length(q)))
}

#' @noRd
.family_quantile <- function(dist, p, u) {
  switch(dist,
    beta        = stats::qbeta(u, p$alpha, p$beta),
    normal      = stats::qnorm(u, p$mu, p$sigma),
    gamma       = stats::qgamma(u, p$shape, p$rate),
    lognormal   = stats::qlnorm(u, p$meanlog, p$sdlog),
    exponential = stats::qexp(u, rate = p$rate),
    weibull     = stats::qweibull(u, p$shape, p$scale),
    rep(NA_real_, length(u)))
}

#' CDF of a bayprior (mixtures included). Returns NA for a family without one.
#' @noRd
.prior_cdf <- function(prior, q) {
  if (identical(prior$dist, "mixture")) {
    out <- numeric(length(q))
    for (i in seq_along(prior$components))
      out <- out + prior$weights[i] * .prior_cdf(prior$components[[i]], q)
    return(out)
  }
  .family_cdf(prior$dist, prior$params, q)
}

#' Quantiles of a bayprior, exact for mixtures (numerical root finding).
#' @noRd
.prior_quantile <- function(prior, probs) {
  if (!identical(prior$dist, "mixture"))
    return(.family_quantile(prior$dist, prior$params, probs))
  comps <- prior$components
  lo <- min(vapply(comps, function(k) .prior_quantile(k, 1e-10), 1))
  hi <- max(vapply(comps, function(k) .prior_quantile(k, 1 - 1e-10), 1))
  if (!is.finite(lo) || !is.finite(hi)) return(rep(NA_real_, length(probs)))
  vapply(probs, function(p) {
    tryCatch(stats::uniroot(function(q) .prior_cdf(prior, q) - p, c(lo, hi),
                            tol = 1e-10)$root, error = function(e) NA_real_)
  }, numeric(1))
}

#' fit_summary for a mixture: moment mean/SD plus exact quantiles.
#' @noRd
.mixture_fit_summary <- function(components, weights) {
  s <- .mixture_summary(components, weights)
  tmp <- list(dist = "mixture", components = components, weights = weights)
  qs <- tryCatch(.prior_quantile(tmp, c(0.025, 0.5, 0.975)),
                 error = function(e) rep(NA_real_, 3))
  if (anyNA(qs)) return(s)
  c(s, list(q025 = qs[1], q500 = qs[2], q975 = qs[3]))
}
