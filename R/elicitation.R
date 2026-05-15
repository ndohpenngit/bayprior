# ── Prior Elicitation Functions ───────────────────────────────────────────────
# All elicit_*() functions share the same bayprior S3 constructor
# (.make_bayprior in R/zzz_patches.R) so fit_summary is always consistent.

#' Elicit a Beta prior via quantile matching or moment matching
#'
#' Fits a Beta(alpha, beta) distribution to expert-specified quantiles or
#' moments. Implements the structured elicitation framework recommended in
#' the SHELF methodology and FDA guidance on Bayesian clinical trials.
#'
#' @param quantiles Named numeric vector of quantile specifications, e.g.
#'   \code{c("0.05" = 0.1, "0.50" = 0.3, "0.95" = 0.6)}. At least two
#'   quantiles required.
#' @param mean Optional numeric. Expert-specified mean for moment matching.
#' @param sd Optional numeric. Expert-specified SD for moment matching.
#' @param method Character. One of \code{"quantile"} (default) or
#'   \code{"moments"}.
#' @param expert_id Character. Identifier for this expert's elicitation.
#' @param label Character. Description of the quantity being elicited.
#' @param tol Numeric. Optimisation tolerance. Default \code{1e-6}.
#'
#' @return An object of class \code{bayprior} with components:
#'   \describe{
#'     \item{\code{dist}}{\code{"beta"}}
#'     \item{\code{params}}{Named list with \code{alpha} and \code{beta}}
#'     \item{\code{method}}{Elicitation method used}
#'     \item{\code{expert_id}}{Expert identifier}
#'     \item{\code{label}}{Quantity label}
#'     \item{\code{fit_summary}}{Summary statistics of fitted prior}
#'   }
#'
#' @examples
#' # Expert believes response rate is ~30%, with 90% CI of [10%, 60%]
#' prior <- elicit_beta(
#'   quantiles = c("0.05" = 0.10, "0.50" = 0.30, "0.95" = 0.60),
#'   expert_id = "Expert_1",
#'   label     = "Response rate (treatment arm)"
#' )
#' print(prior)
#' plot(prior)
#'
#' # Moment-based elicitation
#' prior_mom <- elicit_beta(
#'   mean   = 0.30, sd = 0.12,
#'   method = "moments",
#'   label  = "Response rate"
#' )
#'
#' @export
elicit_beta <- function(quantiles = NULL,
                        mean      = NULL,
                        sd        = NULL,
                        method    = c("quantile", "moments"),
                        expert_id = "Expert_1",
                        label     = "Unknown quantity",
                        tol       = 1e-6) {

  method <- match.arg(method)

  if (method == "quantile") {
    if (is.null(quantiles) || length(quantiles) < 2)
      rlang::abort("At least 2 quantile specifications required for quantile matching.")
    probs <- as.numeric(names(quantiles))
    vals  <- as.numeric(quantiles)
    .validate_quantiles(probs, vals, support = c(0, 1))

    obj_fn <- function(par) {
      a <- exp(par[1]); b <- exp(par[2])
      sum((stats::qbeta(probs, a, b) - vals)^2)
    }
    fit   <- stats::nlminb(c(0, 0), obj_fn, control = list(rel.tol = tol))
    alpha <- exp(fit$par[1])
    beta  <- exp(fit$par[2])
    input <- list(quantiles = quantiles)

  } else {
    if (is.null(mean) || is.null(sd))
      rlang::abort("Both `mean` and `sd` required for moment matching.")
    if (mean <= 0 || mean >= 1)
      rlang::abort("`mean` must be in (0, 1) for Beta distribution.")
    if (sd <= 0) rlang::abort("`sd` must be positive.")
    v <- sd^2
    alpha <- mean * (mean * (1 - mean) / v - 1)
    beta  <- (1 - mean) * (mean * (1 - mean) / v - 1)
    if (alpha <= 0 || beta <= 0)
      rlang::abort("Implied alpha/beta <= 0. SD too large for the given mean.")
    input <- list(mean = mean, sd = sd)
  }

  .make_bayprior("beta", list(alpha = alpha, beta = beta),
                 method, expert_id, label, input)
}


#' Elicit a Normal prior via quantile matching or moment matching
#'
#' @param quantiles Named numeric vector. E.g.
#'   \code{c("0.025" = -0.5, "0.50" = 0.2, "0.975" = 0.9)}.
#' @param mean Optional numeric. Expert mean for moment matching.
#' @param sd Optional numeric. Expert SD for moment matching.
#' @param method Character. \code{"quantile"} or \code{"moments"}.
#' @param expert_id Character. Expert identifier.
#' @param label Character. Quantity description.
#' @param tol Numeric. Optimisation tolerance.
#'
#' @return An object of class \code{bayprior}.
#'
#' @examples
#' prior <- elicit_normal(
#'   quantiles = c("0.025" = -0.5, "0.50" = 0.2, "0.975" = 0.9),
#'   label     = "Log odds ratio"
#' )
#'
#' @export
elicit_normal <- function(quantiles = NULL,
                          mean      = NULL,
                          sd        = NULL,
                          method    = c("quantile", "moments"),
                          expert_id = "Expert_1",
                          label     = "Unknown quantity",
                          tol       = 1e-6) {

  method <- match.arg(method)

  if (method == "quantile") {
    if (is.null(quantiles) || length(quantiles) < 2)
      rlang::abort("At least 2 quantile specifications required.")
    probs <- as.numeric(names(quantiles))
    vals  <- as.numeric(quantiles)

    obj_fn <- function(par) {
      mu <- par[1]; sigma <- exp(par[2])
      sum((stats::qnorm(probs, mu, sigma) - vals)^2)
    }
    fit   <- stats::nlminb(c(mean(vals), 0), obj_fn,
                           control = list(rel.tol = tol))
    mu    <- fit$par[1]
    sigma <- exp(fit$par[2])
    input <- list(quantiles = quantiles)

  } else {
    if (is.null(mean) || is.null(sd))
      rlang::abort("Both `mean` and `sd` required.")
    if (sd <= 0) rlang::abort("`sd` must be positive.")
    mu    <- mean
    sigma <- sd
    input <- list(mean = mean, sd = sd)
  }

  .make_bayprior("normal", list(mu = mu, sigma = sigma),
                 method, expert_id, label, input)
}


#' Elicit a Gamma prior via quantile matching or moment matching
#'
#' @param quantiles Named numeric vector of quantiles. Values must be positive.
#' @param mean Optional numeric. Expert mean.
#' @param sd Optional numeric. Expert SD.
#' @param method Character. \code{"quantile"} or \code{"moments"}.
#' @param expert_id Character. Expert identifier.
#' @param label Character. Quantity description.
#' @param tol Numeric. Optimisation tolerance.
#'
#' @return An object of class \code{bayprior}.
#'
#' @examples
#' prior <- elicit_gamma(
#'   mean   = 5, sd = 2,
#'   method = "moments",
#'   label  = "Median OS (months)"
#' )
#'
#' @export
elicit_gamma <- function(quantiles = NULL,
                         mean      = NULL,
                         sd        = NULL,
                         method    = c("quantile", "moments"),
                         expert_id = "Expert_1",
                         label     = "Unknown quantity",
                         tol       = 1e-6) {

  method <- match.arg(method)

  if (method == "quantile") {
    if (is.null(quantiles) || length(quantiles) < 2)
      rlang::abort("At least 2 quantile specifications required.")
    probs <- as.numeric(names(quantiles))
    vals  <- as.numeric(quantiles)
    if (any(vals <= 0))
      rlang::abort("All quantile values must be positive for Gamma.")

    obj_fn <- function(par) {
      shape <- exp(par[1]); rate <- exp(par[2])
      sum((stats::qgamma(probs, shape, rate) - vals)^2)
    }
    fit   <- stats::nlminb(c(1, 0), obj_fn, control = list(rel.tol = tol))
    shape <- exp(fit$par[1])
    rate  <- exp(fit$par[2])
    input <- list(quantiles = quantiles)

  } else {
    if (is.null(mean) || is.null(sd))
      rlang::abort("Both `mean` and `sd` required.")
    if (mean <= 0 || sd <= 0)
      rlang::abort("`mean` and `sd` must be positive.")
    shape <- (mean / sd)^2
    rate  <- mean / sd^2
    input <- list(mean = mean, sd = sd)
  }

  .make_bayprior("gamma", list(shape = shape, rate = rate),
                 method, expert_id, label, input)
}


#' Elicit a Log-Normal prior via quantile matching or moment matching
#'
#' Fits a Log-Normal distribution to expert-specified quantiles or moments.
#' Appropriate for positive-valued quantities such as hazard ratios, fold
#' changes, median survival times, or PK parameters.
#'
#' @param quantiles Named numeric vector. Values must be strictly positive.
#'   E.g. \code{c("0.05" = 0.5, "0.50" = 2.0, "0.95" = 8.0)}.
#' @param mean     Optional numeric. Expert mean on the original scale.
#' @param sd       Optional numeric. Expert SD on the original scale.
#' @param method   Character. \code{"quantile"} (default) or \code{"moments"}.
#' @param expert_id Character. Expert identifier.
#' @param label    Character. Quantity description.
#' @param tol      Numeric. Optimisation tolerance.
#'
#' @return An object of class \code{bayprior} with \code{dist = "lognormal"}.
#'
#' @examples
#' prior <- elicit_lognormal(
#'   quantiles = c("0.05" = 0.40, "0.50" = 0.70, "0.95" = 1.20),
#'   label     = "Hazard ratio (treatment vs control)"
#' )
#' print(prior)
#'
#' @export
elicit_lognormal <- function(quantiles  = NULL,
                              mean       = NULL,
                              sd         = NULL,
                              method     = c("quantile", "moments"),
                              expert_id  = "Expert_1",
                              label      = "Unknown quantity",
                              tol        = 1e-6) {

  method <- match.arg(method)

  if (method == "quantile") {
    if (is.null(quantiles) || length(quantiles) < 2)
      rlang::abort("At least 2 quantile specifications required for quantile matching.")
    probs <- as.numeric(names(quantiles))
    vals  <- as.numeric(quantiles)
    if (any(probs <= 0 | probs >= 1))
      rlang::abort("Probabilities must be in (0, 1).")
    if (any(vals <= 0))
      rlang::abort("All values must be strictly positive for Log-Normal.")
    if (!all(diff(vals) > 0))
      rlang::abort("Quantile values must be strictly increasing.")

    obj_fn <- function(par) {
      ml <- par[1]; sl <- exp(par[2])
      sum((stats::qlnorm(probs, ml, sl) - vals)^2)
    }
    fit     <- stats::nlminb(c(log(median(vals)), 0), obj_fn,
                             control = list(rel.tol = tol))
    meanlog <- fit$par[1]
    sdlog   <- exp(fit$par[2])
    input   <- list(quantiles = quantiles)

  } else {
    if (is.null(mean) || is.null(sd))
      rlang::abort("Both `mean` and `sd` required for moment matching.")
    if (mean <= 0 || sd <= 0)
      rlang::abort("`mean` and `sd` must be positive.")
    sdlog   <- sqrt(log(1 + (sd / mean)^2))
    meanlog <- log(mean) - sdlog^2 / 2
    input   <- list(mean = mean, sd = sd)
  }

  .make_bayprior("lognormal", list(meanlog = meanlog, sdlog = sdlog),
                 method, expert_id, label, input)
}


#' Elicit an Exponential prior via moments, rate, or quantile matching
#'
#' Fits an Exponential(rate) prior from expert-specified moments or quantiles.
#' Suitable for constant-hazard survival models and Poisson rate priors.
#' The Exponential distribution is the conjugate prior likelihood for a
#' Gamma prior on the rate parameter.
#'
#' @param rate      Numeric > 0. Rate parameter (= 1 / mean). Used when
#'   \code{method = "rate"}.
#' @param mean      Numeric > 0. Prior mean (= 1 / rate). Used when
#'   \code{method = "moments"}.
#' @param quantiles Named numeric vector with at least one interior quantile.
#'   Used when \code{method = "quantile"}.
#' @param method    Character. One of \code{"moments"}, \code{"rate"}, or
#'   \code{"quantile"}. Default \code{"moments"}.
#' @param expert_id Character. Identifier for the eliciting expert.
#' @param label     Character. Human-readable label for the quantity.
#'
#' @return A \code{bayprior} object with \code{dist = "exponential"}.
#'
#' @details
#' The Exponential distribution has a single parameter \eqn{\lambda > 0}
#' (the rate). Its mean is \eqn{1/\lambda} and its SD equals its mean.
#'
#' \strong{Typical use cases:}
#' \describe{
#'   \item{Hazard rates}{OS/PFS hazard in oncology trials}
#'   \item{Event rates}{Adverse event rates (events per person-time)}
#'   \item{Poisson rate priors}{Conjugate prior for Poisson count data}
#' }
#'
#' @examples
#' # Mean survival 20 months => hazard rate 1/20 = 0.05
#' p <- elicit_exponential(mean = 0.05, method = "moments",
#'                          label     = "Hazard rate",
#'                          expert_id = "Expert_1")
#' print(p)
#' plot(p)
#'
#' # Direct rate specification
#' p2 <- elicit_exponential(rate = 0.10, method = "rate",
#'                           label = "AE rate per person-year")
#'
#' # Quantile matching
#' p3 <- elicit_exponential(
#'   quantiles = c("0.25" = 0.02, "0.50" = 0.05, "0.75" = 0.10),
#'   method    = "quantile",
#'   label     = "Hazard rate"
#' )
#'
#' @export
elicit_exponential <- function(rate      = NULL,
                                mean      = NULL,
                                quantiles = NULL,
                                method    = c("moments", "rate", "quantile"),
                                expert_id = "Expert",
                                label     = "Quantity") {

  method <- match.arg(method)

  lambda <- switch(method,

    rate = {
      if (is.null(rate) || !is.numeric(rate) || length(rate) != 1 || rate <= 0)
        rlang::abort("`rate` must be a single positive number when method = 'rate'.")
      rate
    },

    moments = {
      if (is.null(mean) || !is.numeric(mean) || length(mean) != 1 || mean <= 0)
        rlang::abort("`mean` must be a single positive number when method = 'moments'.")
      1 / mean
    },

    quantile = {
      if (is.null(quantiles) || !is.numeric(quantiles) || length(quantiles) < 1)
        rlang::abort("`quantiles` must be a named numeric vector.")
      ps <- suppressWarnings(as.numeric(names(quantiles)))
      qs <- unname(quantiles)
      if (any(is.na(ps)) || any(ps <= 0) || any(ps >= 1))
        rlang::abort("Names of `quantiles` must be probabilities strictly between 0 and 1.")
      if (any(qs <= 0))
        rlang::abort("Exponential quantile values must be strictly positive.")
      obj <- function(log_lambda) {
        lam   <- exp(log_lambda)
        q_hat <- stats::qexp(ps, rate = lam)
        sum((log(q_hat) - log(qs))^2)
      }
      opt <- stats::optimise(obj, interval = c(-10, 10))
      exp(opt$minimum)
    }
  )

  fit_summary <- list(
    mean = 1 / lambda,
    sd   = 1 / lambda,
    q025 = stats::qexp(0.025, rate = lambda),
    q500 = stats::qexp(0.500, rate = lambda),
    q975 = stats::qexp(0.975, rate = lambda)
  )

  structure(
    list(
      dist        = "exponential",
      params      = list(rate = lambda),
      fit_summary = fit_summary,
      method      = method,
      expert_id   = expert_id,
      label       = label,
      input       = list(rate = rate, mean = mean, quantiles = quantiles),
      components  = NULL,
      weights     = NULL
    ),
    class = "bayprior"
  )
}


#' Elicit a Weibull prior via moments, direct parameters, or quantile matching
#'
#' Fits a Weibull(shape, scale) prior from expert-specified moments or
#' quantiles. The Weibull distribution generalises the Exponential and is
#' widely used for survival analysis with non-constant hazard.
#'
#' @param shape     Numeric > 0. Shape parameter \eqn{k}. Used when
#'   \code{method = "params"}.
#' @param scale     Numeric > 0. Scale parameter \eqn{\lambda}. Used when
#'   \code{method = "params"}.
#' @param mean      Numeric > 0. Prior mean. Used with \code{sd} when
#'   \code{method = "moments"}.
#' @param sd        Numeric > 0. Prior SD. Used with \code{mean} when
#'   \code{method = "moments"}.
#' @param quantiles Named numeric vector with at least two interior quantiles.
#'   Used when \code{method = "quantile"}.
#' @param method    Character. One of \code{"moments"}, \code{"params"}, or
#'   \code{"quantile"}. Default \code{"moments"}.
#' @param expert_id Character. Identifier for the eliciting expert.
#' @param label     Character. Human-readable label for the quantity.
#'
#' @return A \code{bayprior} object with \code{dist = "weibull"}.
#'
#' @details
#' Parameterised as in R's \code{stats::dweibull}: shape \eqn{k} and
#' scale \eqn{\lambda}, with mean \eqn{\lambda \Gamma(1 + 1/k)} and
#' variance \eqn{\lambda^2 [\Gamma(1 + 2/k) - \Gamma(1 + 1/k)^2]}.
#'
#' \strong{Shape parameter interpretation:}
#' \itemize{
#'   \item \eqn{k < 1}: decreasing hazard (e.g. early mortality selecting out)
#'   \item \eqn{k = 1}: constant hazard (reduces to Exponential)
#'   \item \eqn{k > 1}: increasing hazard (e.g. ageing, post-surgical)
#' }
#'
#' @examples
#' # Moment matching: mean 20 months, SD 10 months
#' p <- elicit_weibull(mean = 20, sd = 10, method = "moments",
#'                      label     = "Survival time (months)",
#'                      expert_id = "Expert_1")
#' print(p)
#' plot(p)
#'
#' # Direct parameters (shape = 2 = increasing hazard)
#' p2 <- elicit_weibull(shape = 2, scale = 20, method = "params",
#'                       label = "PFS (months)")
#'
#' # Quantile matching (at least 2 required)
#' p3 <- elicit_weibull(
#'   quantiles = c("0.10" = 5, "0.50" = 18, "0.90" = 40),
#'   method    = "quantile",
#'   label     = "OS (months)"
#' )
#'
#' @export
elicit_weibull <- function(shape     = NULL,
                            scale     = NULL,
                            mean      = NULL,
                            sd        = NULL,
                            quantiles = NULL,
                            method    = c("moments", "params", "quantile"),
                            expert_id = "Expert",
                            label     = "Quantity") {

  method <- match.arg(method)

  .wb_mean <- function(k, lam) lam * gamma(1 + 1 / k)
  .wb_var  <- function(k, lam) lam^2 * (gamma(1 + 2 / k) - gamma(1 + 1 / k)^2)

  params_fitted <- switch(method,

    params = {
      if (is.null(shape) || is.null(scale) ||
          !is.numeric(shape) || !is.numeric(scale) ||
          shape <= 0 || scale <= 0)
        rlang::abort("`shape` and `scale` must be positive numbers when method = 'params'.")
      list(shape = shape, scale = scale)
    },

    moments = {
      if (is.null(mean) || is.null(sd) ||
          !is.numeric(mean) || !is.numeric(sd) ||
          mean <= 0 || sd <= 0)
        rlang::abort("`mean` and `sd` must be positive numbers when method = 'moments'.")
      obj <- function(log_params) {
        k   <- exp(log_params[1])
        lam <- exp(log_params[2])
        (.wb_mean(k, lam) - mean)^2 + (sqrt(.wb_var(k, lam)) - sd)^2
      }
      opt <- tryCatch(
        stats::optim(c(log(1), log(mean)), obj, method = "Nelder-Mead",
                     control = list(maxit = 5000, reltol = 1e-10)),
        error = function(e)
          rlang::abort(paste0("Weibull moment matching failed: ", conditionMessage(e)))
      )
      if (opt$convergence != 0)
        rlang::abort("Weibull moment matching did not converge. Try 'quantile' or 'params' method.")
      list(shape = exp(opt$par[1]), scale = exp(opt$par[2]))
    },

    quantile = {
      if (is.null(quantiles) || !is.numeric(quantiles) || length(quantiles) < 2)
        rlang::abort("`quantiles` must be a named numeric vector with at least 2 entries.")
      ps <- suppressWarnings(as.numeric(names(quantiles)))
      qs <- unname(quantiles)
      if (any(is.na(ps)) || any(ps <= 0) || any(ps >= 1))
        rlang::abort("Names of `quantiles` must be probabilities strictly between 0 and 1.")
      if (any(qs <= 0))
        rlang::abort("Weibull quantile values must be strictly positive.")
      obj <- function(log_params) {
        k   <- exp(log_params[1])
        lam <- exp(log_params[2])
        q_hat <- stats::qweibull(ps, shape = k, scale = lam)
        sum((log(q_hat) - log(qs))^2)
      }
      opt <- tryCatch(
        stats::optim(c(log(1.5), log(median(qs))), obj, method = "Nelder-Mead",
                     control = list(maxit = 5000, reltol = 1e-10)),
        error = function(e)
          rlang::abort(paste0("Weibull quantile matching failed: ", conditionMessage(e)))
      )
      if (opt$convergence != 0)
        rlang::abort("Weibull quantile matching did not converge. Try different quantile values.")
      list(shape = exp(opt$par[1]), scale = exp(opt$par[2]))
    }
  )

  k   <- params_fitted$shape
  lam <- params_fitted$scale

  fit_summary <- list(
    mean = .wb_mean(k, lam),
    sd   = sqrt(.wb_var(k, lam)),
    q025 = stats::qweibull(0.025, shape = k, scale = lam),
    q500 = stats::qweibull(0.500, shape = k, scale = lam),
    q975 = stats::qweibull(0.975, shape = k, scale = lam)
  )

  structure(
    list(
      dist        = "weibull",
      params      = list(shape = k, scale = lam),
      fit_summary = fit_summary,
      method      = method,
      expert_id   = expert_id,
      label       = label,
      input       = list(shape = shape, scale = scale,
                         mean = mean, sd = sd, quantiles = quantiles),
      components  = NULL,
      weights     = NULL
    ),
    class = "bayprior"
  )
}


#' Elicit a mixture prior
#'
#' Constructs a finite mixture prior from a list of component \code{bayprior}
#' objects (e.g., from \code{elicit_beta}, \code{elicit_normal}). Mixing
#' weights can be specified or estimated via linear pooling.
#'
#' @param components List of \code{bayprior} objects.
#' @param weights Numeric vector of mixing weights (must sum to 1). If
#'   \code{NULL}, equal weights are assigned.
#' @param label Character. Label for the mixture prior.
#'
#' @return A \code{bayprior} object with \code{dist = "mixture"}.
#'
#' @examples
#' p1  <- elicit_beta(mean = 0.2, sd = 0.08, method = "moments", expert_id = "E1")
#' p2  <- elicit_beta(mean = 0.4, sd = 0.10, method = "moments", expert_id = "E2")
#' mix <- elicit_mixture(list(p1, p2), weights = c(0.5, 0.5),
#'                        label = "Pooled prior")
#'
#' @export
elicit_mixture <- function(components, weights = NULL, label = "Mixture prior") {
  if (!is.list(components) || length(components) < 2)
    rlang::abort("`components` must be a list of at least 2 bayprior objects.")
  families <- vapply(components, function(x) x$dist, character(1))
  if (length(unique(families)) > 1)
    rlang::warn("Components have different distribution families. Mixture densities computed numerically.")

  k <- length(components)
  if (is.null(weights)) {
    weights <- rep(1 / k, k)
    cli::cli_alert_warning(
      "No weights supplied; using equal weights (1/{k} each).",
      .envir = environment()
    )
  }
  if (abs(sum(weights) - 1) > 1e-6)
    rlang::abort("`weights` must sum to 1.")

  structure(
    list(
      dist        = "mixture",
      components  = components,
      weights     = weights,
      label       = label,
      fit_summary = .mixture_summary(components, weights)
    ),
    class = "bayprior"
  )
}


# ── Internal helpers ──────────────────────────────────────────────────────────
# Note: .make_bayprior() is the authoritative constructor defined in
# R/zzz_patches.R. It handles lognormal via .prior_summary_lognormal().
# elicit_exponential() and elicit_weibull() build fit_summary directly
# since their formulas are simple closed-form expressions.

.prior_summary <- function(dist, params) {
  switch(dist,
    beta   = list(
      mean = params$alpha / (params$alpha + params$beta),
      sd   = sqrt(params$alpha * params$beta /
               ((params$alpha + params$beta)^2 *
                (params$alpha + params$beta + 1))),
      q025 = stats::qbeta(0.025, params$alpha, params$beta),
      q500 = stats::qbeta(0.500, params$alpha, params$beta),
      q975 = stats::qbeta(0.975, params$alpha, params$beta)
    ),
    normal = list(
      mean = params$mu, sd = params$sigma,
      q025 = stats::qnorm(0.025, params$mu, params$sigma),
      q500 = params$mu,
      q975 = stats::qnorm(0.975, params$mu, params$sigma)
    ),
    gamma  = list(
      mean = params$shape / params$rate,
      sd   = sqrt(params$shape) / params$rate,
      q025 = stats::qgamma(0.025, params$shape, params$rate),
      q500 = stats::qgamma(0.500, params$shape, params$rate),
      q975 = stats::qgamma(0.975, params$shape, params$rate)
    )
  )
}

.mixture_summary <- function(components, weights) {
  means    <- vapply(components, function(x) x$fit_summary$mean, numeric(1))
  sds      <- vapply(components, function(x) x$fit_summary$sd,   numeric(1))
  mix_mean <- sum(weights * means)
  mix_var  <- sum(weights * (sds^2 + means^2)) - mix_mean^2
  list(mean = mix_mean, sd = sqrt(mix_var))
}

.validate_quantiles <- function(probs, vals, support = c(-Inf, Inf)) {
  if (any(probs <= 0 | probs >= 1))
    rlang::abort("Probabilities must be in (0, 1).")
  if (!all(diff(vals) > 0))
    rlang::abort("Quantile values must be strictly increasing.")
  if (any(vals < support[1] | vals > support[2]))
    rlang::abort(glue::glue(
      "Quantile values must be in [{support[1]}, {support[2]}]."))
}