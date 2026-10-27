# Likelihood helpers shared by the conjugate update, the power prior and the
# Bayes factor.
#
# A prior meets the data through a likelihood. Three ways of handling it are
# used, in this order of preference:
#
#   * closed form (conjugate): Beta with binomial data, Gamma (or exponential)
#     with Poisson / survival data, Normal with a Normal summary of continuous
#     data;
#   * numerical integration of the exact likelihood over the prior, for the
#     other families (Normal on a proportion or a log-odds, lognormal, Weibull,
#     ...);
#   * a Normal approximation of the likelihood (observed estimate and SE), only
#     where the posterior itself has to be approximated by a Normal.
#
# `delta` in (0, 1] raises the likelihood to the power `delta` (power prior).

# -- Normal summary of the likelihood ------------------------------------------
# Observed estimate and standard error on the scale the prior lives on.
#   binary:     proportion x / n, or log-odds (scale = "log_odds");
#   poisson /
#   survival:   rate x / n;
#   continuous: the observed mean x with SE sd / sqrt(n).
# At 0 or n events the usual SE would be 0, so the SE of a proportion uses the
# continuity-adjusted proportion (x + 0.5) / (n + 1) and that of a rate uses
# max(x, 0.5); the estimate itself is not adjusted (except the log-odds).
# `sd` is the data SD for continuous data. `delta` scales the information.
#' @noRd
.normal_obs <- function(data_summary, scale = NA_character_, sd = NULL, delta = 1) {
  type <- data_summary$type %||% "binary"
  x <- data_summary$x; n <- data_summary$n
  if (identical(type, "binary") && identical(scale, "log_odds")) {
    lo <- .binary_logit(x, n)
    m <- lo$mean; se <- lo$se / sqrt(delta)
  } else if (identical(type, "binary")) {
    pa <- if (x <= 0 || x >= n) (x + 0.5) / (n + 1) else x / n
    m  <- x / n
    se <- sqrt(pa * (1 - pa) / n) / sqrt(delta)
  } else if (type %in% c("poisson", "survival")) {
    m  <- x / n
    se <- sqrt(max(x, 0.5)) / n / sqrt(delta)
  } else {
    m  <- x
    se <- (data_summary$sd %||% sd) / sqrt(n * delta)
  }
  list(mean = m, se = max(se, 1e-8))
}

# -- Log marginal likelihood of the data under one prior -----------------------
# log of  integral L(theta)^delta * pi(theta) dtheta,  up to a constant that does
# not depend on the prior (binomial coefficient, x!, ...), so values for
# different priors fitted to the same data can be compared. A mixture prior
# gives the log of the weighted sum of its components' marginal likelihoods.
# The attribute "method" is "exact" (closed form), "quadrature" (numerical
# integration of the exact likelihood) or "normal_approx".
#' @noRd
.log_marglik <- function(prior, data_summary, delta = 1, sd = NULL,
                         scale = NA_character_, n_quad = 20000L) {
  type <- data_summary$type %||% "binary"
  x <- data_summary$x; n <- data_summary$n
  tag <- function(v, method) structure(as.numeric(v), method = method)
  lse <- function(v) { m <- max(v); m + log(sum(exp(v - m))) }

  if (identical(prior$dist, "mixture")) {
    parts <- lapply(prior$components, .log_marglik, data_summary = data_summary,
                    delta = delta, sd = sd %||% prior$fit_summary$sd,
                    scale = scale, n_quad = n_quad)
    meth <- vapply(parts, function(p) attr(p, "method"), character(1))
    return(tag(lse(log(prior$weights) + unlist(parts)),
               if (all(meth == "exact")) "exact" else
                 if (any(meth == "normal_approx")) "normal_approx" else "quadrature"))
  }

  d <- prior$dist; p <- prior$params
  if (identical(type, "binary") && identical(d, "normal") && is.na(scale))
    scale <- .binary_scale(prior, data_summary)

  # closed forms
  if (d == "beta" && type == "binary")
    return(tag(lbeta(p$alpha + delta * x, p$beta + delta * (n - x)) -
                 lbeta(p$alpha, p$beta), "exact"))
  gam <- if (type %in% c("poisson", "survival")) {
    if (d == "gamma") c(p$shape, p$rate) else
      if (d == "exponential") c(1, p$rate) else NULL
  } else NULL
  if (!is.null(gam)) {
    a <- gam[1]; b <- gam[2]
    return(tag(lgamma(a + delta * x) - lgamma(a) + a * log(b) -
                 (a + delta * x) * log(b + delta * n), "exact"))
  }
  if (d == "normal" && identical(type, "continuous")) {
    o <- .normal_obs(data_summary, sd = sd %||% prior$fit_summary$sd, delta = delta)
    return(tag(stats::dnorm(o$mean, p$mu, sqrt(p$sigma^2 + o$se^2), log = TRUE), "exact"))
  }

  # numerical integration of the exact likelihood over the prior, truncated to
  # the parameter space of the likelihood (the prior is read as proper there,
  # as in the prior predictive p-value)
  cdf <- switch(d,
    beta = function(q) stats::pbeta(q, p$alpha, p$beta),
    normal = function(q) stats::pnorm(q, p$mu, p$sigma),
    gamma = function(q) stats::pgamma(q, p$shape, p$rate),
    lognormal = function(q) stats::plnorm(q, p$meanlog, p$sdlog),
    exponential = function(q) stats::pexp(q, rate = p$rate),
    weibull = function(q) stats::pweibull(q, p$shape, p$scale), NULL)
  qf <- switch(d,
    beta = function(u) stats::qbeta(u, p$alpha, p$beta),
    normal = function(u) stats::qnorm(u, p$mu, p$sigma),
    gamma = function(u) stats::qgamma(u, p$shape, p$rate),
    lognormal = function(u) stats::qlnorm(u, p$meanlog, p$sdlog),
    exponential = function(u) stats::qexp(u, rate = p$rate),
    weibull = function(u) stats::qweibull(u, p$shape, p$scale), NULL)
  if (!is.null(cdf)) {
    on_logit <- identical(type, "binary") && identical(d, "normal") &&
      identical(scale, "log_odds")
    if (on_logit) { u_lo <- 0; u_hi <- 1
    } else if (identical(type, "binary")) { u_lo <- cdf(0); u_hi <- cdf(1)
    } else if (type %in% c("poisson", "survival")) { u_lo <- cdf(0); u_hi <- 1
    } else { u_lo <- 0; u_hi <- 1 }
    if (is.finite(u_lo) && is.finite(u_hi) && u_hi - u_lo > 1e-6) {
      u  <- u_lo + (seq_len(n_quad) - 0.5) / n_quad * (u_hi - u_lo)
      th <- qf(u)
      ll <- if (identical(type, "binary")) {
        pr <- if (on_logit) stats::plogis(th) else pmin(pmax(th, 1e-300), 1)
        lq <- if (on_logit) stats::plogis(-th, log.p = TRUE) else log1p(-pmin(pr, 1 - 1e-16))
        lp <- if (on_logit) stats::plogis(th, log.p = TRUE) else log(pr)
        delta * (x * lp + (n - x) * lq)
      } else if (type %in% c("poisson", "survival")) {
        delta * (x * log(pmax(th, 1e-300)) - n * th)
      } else {
        o <- .normal_obs(data_summary, sd = sd %||% prior$fit_summary$sd, delta = delta)
        stats::dnorm(o$mean, th, o$se, log = TRUE)
      }
      if (all(is.finite(ll)))
        return(tag(lse(ll) - log(n_quad), "quadrature"))
    }
  }

  # last resort: Normal approximation of the likelihood
  o <- .normal_obs(data_summary, scale = scale, sd = sd %||% prior$fit_summary$sd,
                   delta = delta)
  tag(stats::dnorm(o$mean, prior$fit_summary$mean,
                   sqrt(prior$fit_summary$sd^2 + o$se^2), log = TRUE),
      "normal_approx")
}
