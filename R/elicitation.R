#' Elicit a Beta prior via quantile matching or moment matching
#'
#' Fits a Beta(alpha, beta) distribution to expert-specified quantiles or
#' moments. Implements the structured elicitation framework recommended in
#' the SHELF methodology and FDA guidance on Bayesian clinical trials.
#'
#' @param quantiles Named numeric vector of quantile specifications, e.g.
#'   `c("0.05" = 0.1, "0.50" = 0.3, "0.95" = 0.6)`. At least two quantiles
#'   required.
#' @param mean Optional numeric. Expert-specified mean for moment matching.
#' @param sd Optional numeric. Expert-specified SD for moment matching.
#' @param method Character. One of `"quantile"` (default) or `"moments"`.
#' @param expert_id Character. Identifier for this expert's elicitation.
#' @param label Character. Description of the quantity being elicited.
#' @param tol Numeric. Optimisation tolerance. Default `1e-6`.
#'
#' @return An object of class `bayprior` with components:
#'   \describe{
#'     \item{`dist`}{`"beta"`}
#'     \item{`params`}{Named list with `alpha` and `beta`}
#'     \item{`method`}{Elicitation method used}
#'     \item{`expert_id`}{Expert identifier}
#'     \item{`label`}{Quantity label}
#'     \item{`input`}{Raw elicitation inputs}
#'     \item{`fit_summary`}{Summary statistics of fitted prior}
#'   }
#'
#' @examples
#' # Expert believes response rate is ~30%, with 90% CI of [10%, 60%]
#' prior <- elicit_beta(
#'   quantiles = c("0.05" = 0.10, "0.50" = 0.30, "0.95" = 0.60),
#'   expert_id = "Expert_1",
#'   label = "Response rate (treatment arm)"
#' )
#' print(prior)
#' plot(prior)
#'
#' # Moment-based elicitation
#' prior_mom <- elicit_beta(
#'   mean = 0.30, sd = 0.12,
#'   method = "moments",
#'   label = "Response rate (treatment arm)"
#' )
#'
#' @export
elicit_beta <- function(quantiles = NULL,
                        mean = NULL,
                        sd = NULL,
                        method = c("quantile", "moments"),
                        expert_id = "Expert_1",
                        label = "Unknown quantity",
                        tol = 1e-6) {

  method <- match.arg(method)

  if (method == "quantile") {
    if (is.null(quantiles) || length(quantiles) < 2) {
      rlang::abort("At least 2 quantile specifications required for quantile matching.")
    }
    probs <- as.numeric(names(quantiles))
    vals  <- as.numeric(quantiles)
    .validate_quantiles(probs, vals, support = c(0, 1))

    obj_fn <- function(par) {
      a <- exp(par[1]); b <- exp(par[2])
      sum((stats::qbeta(probs, a, b) - vals)^2)
    }
    fit <- stats::nlminb(c(0, 0), obj_fn, control = list(rel.tol = tol))
    alpha <- exp(fit$par[1])
    beta  <- exp(fit$par[2])
    input <- list(quantiles = quantiles)

  } else {
    if (is.null(mean) || is.null(sd)) {
      rlang::abort("Both `mean` and `sd` required for moment matching.")
    }
    if (mean <= 0 || mean >= 1) rlang::abort("`mean` must be in (0, 1) for Beta distribution.")
    if (sd <= 0) rlang::abort("`sd` must be positive.")
    v <- sd^2
    alpha <- mean * (mean * (1 - mean) / v - 1)
    beta  <- (1 - mean) * (mean * (1 - mean) / v - 1)
    if (alpha <= 0 || beta <= 0) {
      rlang::abort("Implied alpha/beta <= 0. SD too large for the given mean.")
    }
    input <- list(mean = mean, sd = sd)
  }

  .make_bayprior("beta", list(alpha = alpha, beta = beta),
                 method, expert_id, label, input)
}


#' Elicit a Normal prior via quantile matching or moment matching
#'
#' @param quantiles Named numeric vector. E.g. `c("0.025" = -0.5, "0.50" = 0.2, "0.975" = 0.9)`.
#' @param mean Optional numeric. Expert mean for moment matching.
#' @param sd Optional numeric. Expert SD for moment matching.
#' @param method Character. `"quantile"` or `"moments"`.
#' @param expert_id Character. Expert identifier.
#' @param label Character. Quantity description.
#' @param tol Numeric. Optimisation tolerance.
#'
#' @return An object of class `bayprior`.
#'
#' @examples
#' prior <- elicit_normal(
#'   quantiles = c("0.025" = -0.5, "0.50" = 0.2, "0.975" = 0.9),
#'   label = "Log odds ratio"
#' )
#'
#' @export
elicit_normal <- function(quantiles = NULL,
                          mean = NULL,
                          sd = NULL,
                          method = c("quantile", "moments"),
                          expert_id = "Expert_1",
                          label = "Unknown quantity",
                          tol = 1e-6) {

  method <- match.arg(method)

  if (method == "quantile") {
    if (is.null(quantiles) || length(quantiles) < 2) {
      rlang::abort("At least 2 quantile specifications required.")
    }
    probs <- as.numeric(names(quantiles))
    vals  <- as.numeric(quantiles)

    obj_fn <- function(par) {
      mu <- par[1]; sigma <- exp(par[2])
      sum((stats::qnorm(probs, mu, sigma) - vals)^2)
    }
    fit   <- stats::nlminb(c(mean(vals), 0), obj_fn, control = list(rel.tol = tol))
    mu    <- fit$par[1]
    sigma <- exp(fit$par[2])
    input <- list(quantiles = quantiles)

  } else {
    if (is.null(mean) || is.null(sd)) rlang::abort("Both `mean` and `sd` required.")
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
#' @param method Character. `"quantile"` or `"moments"`.
#' @param expert_id Character. Expert identifier.
#' @param label Character. Quantity description.
#' @param tol Numeric. Optimisation tolerance.
#'
#' @return An object of class `bayprior`.
#'
#' @examples
#' prior <- elicit_gamma(
#'   mean = 5, sd = 2,
#'   method = "moments",
#'   label = "Median OS (months)"
#' )
#'
#' @export
elicit_gamma <- function(quantiles = NULL,
                         mean = NULL,
                         sd = NULL,
                         method = c("quantile", "moments"),
                         expert_id = "Expert_1",
                         label = "Unknown quantity",
                         tol = 1e-6) {

  method <- match.arg(method)

  if (method == "quantile") {
    if (is.null(quantiles) || length(quantiles) < 2) {
      rlang::abort("At least 2 quantile specifications required.")
    }
    probs <- as.numeric(names(quantiles))
    vals  <- as.numeric(quantiles)
    if (any(vals <= 0)) rlang::abort("All quantile values must be positive for Gamma.")

    obj_fn <- function(par) {
      shape <- exp(par[1]); rate <- exp(par[2])
      sum((stats::qgamma(probs, shape, rate) - vals)^2)
    }
    fit   <- stats::nlminb(c(1, 0), obj_fn, control = list(rel.tol = tol))
    shape <- exp(fit$par[1])
    rate  <- exp(fit$par[2])
    input <- list(quantiles = quantiles)

  } else {
    if (is.null(mean) || is.null(sd)) rlang::abort("Both `mean` and `sd` required.")
    if (mean <= 0 || sd <= 0) rlang::abort("`mean` and `sd` must be positive.")
    shape <- (mean / sd)^2
    rate  <- mean / sd^2
    input <- list(mean = mean, sd = sd)
  }

  .make_bayprior("gamma", list(shape = shape, rate = rate),
                 method, expert_id, label, input)
}


#' Elicit a mixture prior
#'
#' Constructs a finite mixture prior from a list of component `bayprior`
#' objects (e.g., from `elicit_beta`, `elicit_normal`). Mixing weights can
#' be specified or estimated via linear pooling.
#'
#' @param components List of `bayprior` objects (all same distribution family).
#' @param weights Numeric vector of mixing weights (must sum to 1). If `NULL`,
#'   equal weights are assigned.
#' @param label Character. Label for the mixture prior.
#'
#' @return A `bayprior` object with `dist = "mixture"`.
#'
#' @examples
#' p1 <- elicit_beta(mean = 0.2, sd = 0.08, method = "moments", expert_id = "E1")
#' p2 <- elicit_beta(mean = 0.4, sd = 0.10, method = "moments", expert_id = "E2")
#' mix <- elicit_mixture(list(p1, p2), weights = c(0.5, 0.5), label = "Pooled prior")
#'
#' @export
elicit_mixture <- function(components, weights = NULL, label = "Mixture prior") {
  if (!is.list(components) || length(components) < 2) {
    rlang::abort("`components` must be a list of at least 2 bayprior objects.")
  }
  families <- vapply(components, function(x) x$dist, character(1))
  if (length(unique(families)) > 1) {
    rlang::warn("Components have different distribution families. Mixture densities computed numerically.")
  }

  k <- length(components)
  if (is.null(weights)) {
    weights <- rep(1 / k, k)
    cli::cli_alert_warning("No weights supplied; using equal weights (1/{k} each).", .envir = environment())
  }
  if (abs(sum(weights) - 1) > 1e-6) {
    rlang::abort("`weights` must sum to 1.")
  }

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


# ---- Internal helpers -------------------------------------------------------

.make_bayprior <- function(dist, params, method, expert_id, label, input) {
  structure(
    list(
      dist        = dist,
      params      = params,
      method      = method,
      expert_id   = expert_id,
      label       = label,
      input       = input,
      fit_summary = .prior_summary(dist, params)
    ),
    class = "bayprior"
  )
}

.prior_summary <- function(dist, params) {
  switch(dist,
    beta   = list(
      mean   = params$alpha / (params$alpha + params$beta),
      sd     = sqrt(params$alpha * params$beta /
                    ((params$alpha + params$beta)^2 * (params$alpha + params$beta + 1))),
      q025   = stats::qbeta(0.025, params$alpha, params$beta),
      q500   = stats::qbeta(0.500, params$alpha, params$beta),
      q975   = stats::qbeta(0.975, params$alpha, params$beta)
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
  # Numerical approximation for mixture summaries
  means <- vapply(components, function(x) x$fit_summary$mean, numeric(1))
  sds   <- vapply(components, function(x) x$fit_summary$sd,   numeric(1))
  mix_mean <- sum(weights * means)
  mix_var  <- sum(weights * (sds^2 + means^2)) - mix_mean^2
  list(mean = mix_mean, sd = sqrt(mix_var))
}

.validate_quantiles <- function(probs, vals, support = c(-Inf, Inf)) {
  if (any(probs <= 0 | probs >= 1)) rlang::abort("Probabilities must be in (0, 1).")
  if (!all(diff(vals) > 0)) rlang::abort("Quantile values must be strictly increasing.")
  if (any(vals < support[1] | vals > support[2])) {
    rlang::abort(glue::glue("Quantile values must be in [{support[1]}, {support[2]}]."))
  }
}
