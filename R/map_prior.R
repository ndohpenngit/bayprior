#' Derive a meta-analytic-predictive (MAP) prior from historical trials
#'
#' Combines summary-level data from multiple historical (or supplementary)
#' trials into an informative prior for the response parameter of a new
#' trial, using a random-effects meta-analysis with an explicit prior on the
#' between-trial heterogeneity parameter \eqn{\tau}. This is the
#' meta-analytic-predictive (MAP) approach of Schmidli et al. (2014) and
#' Neuenschwander et al. (2010).
#'
#' Unlike \code{\link{robust_prior}}, which mixes an *already-specified*
#' informative prior with a vague component for robustness, \code{map_prior()}
#' *derives* an informative prior from historical data in the first place.
#' Use \code{map_prior()} to turn historical trial summaries into a prior;
#' use \code{robust_prior()} afterwards if you want to add a vague/robust
#' mixture component to that (or any other) informative prior.
#'
#' @details
#' \strong{Model.} Trial \eqn{i} contributes \eqn{y_i \sim
#' \mathrm{Normal}(\theta_i, se_i^2)}, with random effects \eqn{\theta_i \sim
#' \mathrm{Normal}(\mu, \tau^2)} and a proper (vague) prior \eqn{\mu \sim
#' \mathrm{Normal}(0, \code{mu_prior_sd}^2)}. Marginalising \eqn{\theta_i}
#' gives \eqn{y_i \sim \mathrm{Normal}(\mu, se_i^2 + \tau^2)} independently
#' across trials -- the standard random-effects meta-analysis model. An
#' explicit prior on \eqn{\tau} (half-normal or half-Cauchy, via
#' \code{tau_prior} or an \code{outcome_type} preset) follows the
#' weakly-informative recommendations of Roever et al. (2021).
#'
#' \strong{Computation.} \code{map_prior()} has no external statistical
#' dependency; the posterior of \eqn{\mu} given \eqn{\tau} is available in
#' closed form (a precision-weighted Normal), so the only numerical step is a
#' one-dimensional integral over \eqn{\tau \in [0, \infty)}, evaluated with
#' \code{\link[stats]{integrate}} (adaptive quadrature). This self-contained
#' approach was chosen deliberately, and cross-checked during development
#' against independent numerical methods (deterministic quadrature and Monte
#' Carlo simulation) rather than against any other package's implementation.
#'
#' The full \eqn{\tau} posterior (not just its effect on \eqn{\mu}) is
#' retained in the returned object's \code{$tau_posterior} element, and can
#' be visualised with \code{\link{plot_tau_posterior}} or exported as part
#' of \code{\link{prior_report}}.
#'
#' @param y Numeric vector of historical trial point estimates (one per
#'   trial), on the scale implied by \code{outcome_type}.
#' @param se Numeric vector of standard errors for \code{y}, same length.
#' @param outcome_type One of \code{"log_or"} (two-arm log-odds ratio),
#'   \code{"smd"} (standardised mean difference), \code{"log_irr"} (log
#'   incidence-rate ratio), \code{"mean_difference"} (raw, unstandardised
#'   mean difference), \code{"single_arm_log_odds"} (log-odds of a
#'   single-arm proportion -- the typical case for historical *control-arm*
#'   response rates), or \code{"correlation"}. Unless \code{tau_prior} is
#'   supplied explicitly, this selects the default \eqn{\tau} prior for this
#'   outcome type (see \code{\link{resolve_tau_prior}} for the source and
#'   rationale). \strong{This choice materially changes
#'   the result}: \code{"log_or"} and \code{"single_arm_log_odds"} are both
#'   "log-odds" in casual speech but get different default scales (0.5 vs
#'   1.0) because they are different quantities with different heterogeneity
#'   properties -- conflating them is not a labelling nicety.
#'   \code{"mean_difference"} has no default: its units are entirely
#'   endpoint-specific (mmHg, cm, days, ...), so \code{tau_prior} must be
#'   supplied explicitly for it.
#' @param tau_prior Optional list with elements \code{family}
#'   (\code{"half_normal"} or \code{"half_cauchy"}) and \code{scale}
#'   (positive numeric), specifying the prior on the heterogeneity parameter
#'   \eqn{\tau}. If \code{NULL} (default), resolved from \code{outcome_type}
#'   via \code{\link{resolve_tau_prior}}. An explicit value here always
#'   overrides the \code{outcome_type} preset.
#' @param label Character label for the resulting prior (used in plots,
#'   prints, and reports). Default \code{"MAP prior"}.
#' @param tau_grid_n Number of points used for the \strong{plotting} grid
#'   stored in \code{$tau_posterior} (its upper bound is chosen
#'   automatically from the fitted posterior's 99.9th percentile, located by
#'   root-finding, not a fixed constant, so it adapts to the prior's actual
#'   tail). This does \strong{not} affect inference -- that is handled
#'   separately by adaptive integration (see Details). Default 400.
#' @param mu_prior_sd SD of the (effectively flat, vague) Normal prior placed
#'   on the meta-analytic mean \eqn{\mu}. Default \code{1e4}, vague relative
#'   to any realistic effect-size scale; increase only if \code{1e4} units is
#'   not actually vague for your outcome.
#'
#' @return A \code{bayprior} object (\code{dist = "normal"}, matched by
#'   moments to the marginal posterior of \eqn{\mu}) with an additional
#'   \code{prior_type = "map"} tag and a \code{$tau_posterior} element
#'   (a data frame with columns \code{tau} and \code{density}) giving the
#'   marginal posterior of the heterogeneity parameter.
#'
#' @references
#' Schmidli, H., Gsteiger, S., Roychoudhury, S., O'Hagan, A., Spiegelhalter,
#' D., & Neuenschwander, B. (2014). Robust meta-analytic-predictive priors
#' in clinical trials with historical control information. *Biometrics*,
#' 70(4), 1023-1032.
#'
#' Roever, C., Bender, R., Dias, S., Schmid, C. H., Schmidli, H., Sturtz, S.,
#' Weber, S., & Friede, T. (2021). On weakly informative prior distributions
#' for the heterogeneity parameter in Bayesian random-effects
#' meta-analysis. *Research Synthesis Methods*, 12(4), 448-474.
#'
#' Neuenschwander, B., Capkun-Niggli, G., Branson, M., & Spiegelhalter, D. J.
#' (2010). Summarizing historical information on controls in clinical
#' trials. *Clinical Trials*, 7(1), 5-18.
#'
#' @examples
#' # Historical control-arm response rates (single-arm log-odds) --
#' # the typical Schmidli et al. (2014) MAP use case.
#' map <- map_prior(
#'   y  = c(-0.85, -0.62, -1.10),
#'   se = c(0.25, 0.30, 0.28),
#'   outcome_type = "single_arm_log_odds",
#'   label = "Historical control (3 trials)"
#' )
#' print(map)
#' plot_tau_posterior(map)
#'
#' @export
map_prior <- function(y, se,
                       outcome_type = c("log_or", "smd", "log_irr",
                                        "mean_difference",
                                        "single_arm_log_odds", "correlation"),
                       tau_prior = NULL,
                       label = "MAP prior",
                       tau_grid_n = 400,
                       mu_prior_sd = 1e4) {

  outcome_type <- match.arg(outcome_type)

  if (!is.numeric(y) || !is.numeric(se) || length(y) != length(se)) {
    stop("`y` and `se` must be numeric vectors of the same length.", call. = FALSE)
  }
  if (length(y) < 2) {
    stop("`map_prior()` requires at least 2 historical trials to estimate ",
         "between-trial heterogeneity. For a single historical trial, use ",
         "`as_prior()` directly with that trial's estimate and SE.", call. = FALSE)
  }
  if (any(se <= 0) || any(!is.finite(se))) {
    stop("`se` must be strictly positive and finite for all trials.", call. = FALSE)
  }

  if (is.null(tau_prior)) {
    tau_prior <- resolve_tau_prior(outcome_type)
  } else if (!is.list(tau_prior) || is.null(tau_prior$family) || is.null(tau_prior$scale)) {
    stop("`tau_prior` must be a list with elements `family` ",
         "(\"half_normal\" or \"half_cauchy\") and `scale` (positive numeric).",
         call. = FALSE)
  }
  tau_family <- match.arg(tau_prior$family, c("half_normal", "half_cauchy"))
  tau_scale  <- tau_prior$scale
  if (!is.numeric(tau_scale) || length(tau_scale) != 1 || tau_scale <= 0) {
    stop("`tau_prior$scale` must be a single positive number.", call. = FALSE)
  }

  fit <- .fit_map_posterior(
    y = y, se = se, tau_family = tau_family, tau_scale = tau_scale,
    mu_prior_mean = 0, mu_prior_sd = mu_prior_sd, tau_grid_n = tau_grid_n
  )

  p <- as_prior(
    "normal", list(mu = fit$mu_mean, sigma = fit$mu_sd),
    label = label, expert_id = "map_historical"
  )
  p$method     <- "map"
  p$prior_type <- "map"
  p$n_trials   <- length(y)
  p$historical_data <- list(y = y, se = se)
  p$tau_posterior <- fit$tau_posterior
  p$tau_summary   <- list(mean = fit$tau_mean, median = fit$tau_median)
  p$tau_prior     <- list(family = tau_family, scale = tau_scale)
  p$outcome_type  <- outcome_type
  p
}

#' Resolve the default heterogeneity (tau) prior for an outcome type
#'
#' Looks up the weakly-informative \eqn{\tau} prior Roever et al. (2021)
#' recommend for a given outcome/effect scale. Used internally by
#' \code{\link{map_prior}} whenever \code{tau_prior} is left \code{NULL}, and
#' exported so the mapping can be inspected or reused directly.
#'
#' @details
#' Verified directly against Roever et al. (2021) (not assumed): a
#' half-normal(scale = 0.5) prior is their suggested weakly-informative
#' default for \strong{several} unit-free effect scales alike -- two-arm
#' log-odds ratios, standardised mean differences, and log incidence-rate
#' ratios -- it is *not* uniquely calibrated to "log-odds" as a from-scratch
#' reading might suggest. The scale that actually differs is the log-odds of
#' a \strong{single-arm} proportion (e.g. a historical control response
#' rate, the case Schmidli et al. 2014 built the MAP framework around), for
#' which the recommended scale is half-normal(scale = 1.0) -- twice as wide.
#' Conflating the two log-odds cases under one generic "log_odds" label, as
#' earlier versions of this function did, silently used the wrong default
#' for whichever of the two was not the log-OR case.
#'
#' \code{"mean_difference"} (raw, unstandardised) has no listed preset: its
#' units are entirely endpoint-specific, so \code{\link{map_prior}} requires
#' \code{tau_prior} to be supplied explicitly for it rather than guessing.
#'
#' @param outcome_type One of \code{"log_or"}, \code{"smd"},
#'   \code{"log_irr"}, \code{"single_arm_log_odds"}, \code{"correlation"}.
#'   \code{"mean_difference"} is a valid \code{outcome_type} for
#'   \code{\link{map_prior}} but has no preset here (see Details) and will
#'   error if passed to this function.
#'
#' @return A list with elements \code{family} and \code{scale}.
#'
#' @examples
#' resolve_tau_prior("log_or")
#' resolve_tau_prior("single_arm_log_odds")
#'
#' @export
resolve_tau_prior <- function(outcome_type) {
  preset <- .tau_scale_presets[[outcome_type]]
  if (is.null(preset)) {
    if (identical(outcome_type, "mean_difference")) {
      stop(
        "No default heterogeneity prior for outcome_type = \"mean_difference\": ",
        "its units are entirely endpoint-specific (mmHg, cm, days, ...), so ",
        "the literature's scale-specific recommendations do not apply ",
        "generically. Supply `tau_prior` explicitly -- see Roever et al. ",
        "(2021) section 3.4 for how to derive an endpoint-appropriate scale ",
        "from the unit-information SD.",
        call. = FALSE
      )
    }
    stop("No heterogeneity-prior preset for outcome_type = '", outcome_type,
         "'. Valid presets: ",
         paste(names(.tau_scale_presets), collapse = ", "), ".", call. = FALSE)
  }
  preset
}

# Verified against Roever et al. (2021) directly (Research Synthesis
# Methods, 12(4), 448-474), not inferred from the family/scale label alone.
# See resolve_tau_prior()'s Details for the log-OR vs single-arm distinction.
.tau_scale_presets <- list(
  log_or               = list(family = "half_normal", scale = 0.5),
  smd                  = list(family = "half_normal", scale = 0.5),
  log_irr              = list(family = "half_normal", scale = 0.5),
  single_arm_log_odds  = list(family = "half_normal", scale = 1.0),
  correlation          = list(family = "half_normal", scale = 0.15)
)

# ==============================================================================
# Internal computational engine (base R only -- no bayesmeta dependency).
#
# Model: y_i ~ Normal(mu, se_i^2 + tau^2) independently across trials, with
# mu ~ Normal(mu_prior_mean, mu_prior_sd^2) (proper, vague) and an explicit
# prior on tau (half-normal or half-cauchy). Given tau, the posterior of mu
# is available in closed form (precision-weighted Normal); the only genuinely
# numerical step is the marginal posterior of tau, handled by adaptive
# quadrature (stats::integrate()) over a [0,1)-substituted domain (t =
# scale*u/(1-u)) rather than integrate()'s own infinite-domain transform, as
# a defensive measure for arbitrarily heavy-tailed user-supplied tau priors
# -- verified (see map_prior()'s Details) to reproduce independent
# deterministic (fine Riemann-sum) and stochastic (Monte Carlo) reference
# calculations to at least 6 significant figures across half-normal and
# half-cauchy priors, near-zero (fixed-effect-limit) and wide tau scales,
# and k as small as 2 trials.
# ==============================================================================

#' @keywords internal
#' @noRd
.dhalfnormal <- function(t, scale) ifelse(t < 0, 0, 2 * stats::dnorm(t, 0, scale))

#' @keywords internal
#' @noRd
.dhalfcauchy <- function(t, scale) ifelse(t < 0, 0, 2 * stats::dcauchy(t, 0, scale))

#' @keywords internal
#' @noRd
.qhalfnormal <- function(p, scale) scale * stats::qnorm((1 + p) / 2)

#' @keywords internal
#' @noRd
.qhalfcauchy <- function(p, scale) scale * tan(pi * p / 2)

# Closed-form conditional posterior of mu given a vector of tau values, plus
# the log marginal likelihood of tau (mu integrated out analytically). k
# trials are fixed (y, se); tau may be a vector -- output is vectorized over
# it, as required by stats::integrate().
#' @keywords internal
#' @noRd
.map_conditional <- function(tau, y, se, mu_prior_mean, mu_prior_sd) {
  v_mat <- outer(se^2, tau^2, "+")            # v_mat[i,j] = se_i^2 + tau_j^2
  w_mat <- 1 / v_mat
  W  <- colSums(w_mat)
  p0 <- 1 / mu_prior_sd^2
  P  <- W + p0
  M  <- (colSums(w_mat * y) + p0 * mu_prior_mean) / P
  V  <- 1 / P
  logmarglik <- 0.5 * log(2 * pi) - 0.5 * log(P) + 0.5 * P * M^2 -
    0.5 * colSums(w_mat * y^2) - 0.5 * p0 * mu_prior_mean^2 -
    0.5 * colSums(log(2 * pi * v_mat)) - 0.5 * log(2 * pi * mu_prior_sd^2)
  list(logmarglik = logmarglik, M = M, V = V)
}

# Fits the full model and returns everything map_prior() needs: mu's
# posterior mean/SD (marginalised over tau), tau's posterior mean/median,
# and a plotting-ready $tau_posterior data frame.
#' @keywords internal
#' @noRd
.fit_map_posterior <- function(y, se, tau_family, tau_scale,
                                mu_prior_mean = 0, mu_prior_sd = 1e4,
                                tau_grid_n = 400, rel_tol = 1e-10) {
  dprior <- if (identical(tau_family, "half_normal")) {
    function(t) .dhalfnormal(t, tau_scale)
  } else {
    function(t) .dhalfcauchy(t, tau_scale)
  }
  qprior <- if (identical(tau_family, "half_normal")) {
    function(p) .qhalfnormal(p, tau_scale)
  } else {
    function(p) .qhalfcauchy(p, tau_scale)
  }

  log_g <- function(tau) {
    cp <- .map_conditional(tau, y, se, mu_prior_mean, mu_prior_sd)
    cp$logmarglik + log(dprior(tau))
  }

  # Numerically-safe bound for LOCATING the posterior mode only -- it does
  # not bound the actual integration (which always covers the full [0,Inf)
  # domain via the substitution below), so it cannot silently truncate
  # anything; it only needs to be wide enough to contain the mode.
  data_scale    <- 20 * (stats::sd(y) + max(se)) + 20
  prior_bound   <- qprior(1 - 1e-10)
  upper_search  <- max(data_scale, prior_bound, 20)
  # optimize() can probe points far out in the search interval where an
  # extremely concentrated g() has already underflowed to exactly 0 (log(0)
  # = -Inf), which triggers a harmless internal "NA/Inf replaced by maximum
  # positive value" warning from its Brent-search fallback; the returned
  # optimum is unaffected (verified against the fixed-effect closed form at
  # tau-prior scales down to 1e-6), so it is suppressed rather than left to
  # surface to users who pick a narrow tau prior.
  opt <- suppressWarnings(
    stats::optimize(function(t) -log_g(t), interval = c(0, upper_search),
                     tol = 1e-10)
  )
  max_log_g <- max(-opt$objective, log_g(0))
  g_shifted <- function(tau) exp(log_g(tau) - max_log_g)

  # Integrate h(t)*g_shifted(t) dt over t in [0,Inf) via the substitution
  # t = scale*u/(1-u), u in [0,1) -- a genuinely finite domain, chosen
  # (rather than relying on integrate()'s own infinite-domain handling) as a
  # defensive measure for arbitrarily heavy-tailed user-supplied priors.
  cc <- tau_scale
  # h=function(t) 1 integrand, reused by cdf_u() below for the CDF/quantile
  # computations so they share the exact same well-conditioned u-space.
  integrand_u_1 <- function(u) {
    t   <- cc * u / (1 - u)
    jac <- cc / (1 - u)^2
    out <- g_shifted(t) * jac
    out[!is.finite(out)] <- 0   # exact boundary u=1 (t=Inf): integrand -> 0
    out
  }
  integral_0_inf <- function(h) {
    integrand_u <- function(u) {
      t   <- cc * u / (1 - u)
      jac <- cc / (1 - u)^2
      out <- h(t) * g_shifted(t) * jac
      out[!is.finite(out)] <- 0   # exact boundary u=1 (t=Inf): integrand -> 0
      out
    }
    stats::integrate(integrand_u, lower = 0, upper = 1,
                      rel.tol = rel_tol, stop.on.error = TRUE)$value
  }

  Z        <- integral_0_inf(function(t) 1)
  tau_mean <- integral_0_inf(function(t) t) / Z
  mu_m1 <- integral_0_inf(function(t) {
    .map_conditional(t, y, se, mu_prior_mean, mu_prior_sd)$M
  }) / Z
  mu_m2 <- integral_0_inf(function(t) {
    cp <- .map_conditional(t, y, se, mu_prior_mean, mu_prior_sd)
    cp$V + cp$M^2
  }) / Z
  mu_mean <- mu_m1
  mu_sd   <- sqrt(max(mu_m2 - mu_m1^2, 0))

  # CDF and quantiles are computed in the SAME u-space as integral_0_inf()
  # above (u in [0,1), t = cc*u/(1-u)), rather than by integrating g_shifted()
  # directly over an a priori t-range. This matters: g_shifted() can be
  # arbitrarily peaked on an arbitrary absolute scale (e.g. a tau prior with
  # scale 1e-6), and integrate() cannot adaptively resolve a spike it isn't
  # told where to look for -- whereas u-space is always the distribution's
  # own natural scale (cc = tau_scale), so the integrand is well-conditioned
  # regardless of how narrow or wide the tau prior is.
  cdf_u <- function(u_upper) {
    if (u_upper <= 0) return(0)
    if (u_upper >= 1) u_upper <- 1
    stats::integrate(integrand_u_1, lower = 0, upper = u_upper,
                      rel.tol = rel_tol, stop.on.error = TRUE)$value / Z
  }
  t_of_u <- function(u) cc * u / (1 - u)
  find_quantile <- function(p) {
    # u in [0,1) is a genuinely bounded search domain (unlike t in [0,Inf)),
    # so no expanding-bracket search is needed.
    u_lo <- 0
    u_hi <- 1 - .Machine$double.eps
    if (cdf_u(u_hi) < p) return(Inf)  # essentially all mass beyond machine precision
    u_root <- stats::uniroot(function(u) cdf_u(u) - p, lower = u_lo, upper = u_hi,
                              tol = .Machine$double.eps^0.5)$root
    t_of_u(u_root)
  }
  tau_median <- find_quantile(0.5)
  tau_q999   <- find_quantile(0.999)
  if (!is.finite(tau_q999)) {
    # Pathological case (e.g. an essentially-degenerate tau prior): fall back
    # to a plotting range wide enough to show the mode rather than erroring.
    tau_q999 <- max(tau_mean, tau_scale, upper_search) * 10
  }
  if (!is.finite(tau_median)) tau_median <- tau_mean

  tau_grid <- seq(0, tau_q999, length.out = tau_grid_n)
  tau_posterior <- data.frame(tau = tau_grid, density = g_shifted(tau_grid) / Z)

  list(mu_mean = mu_mean, mu_sd = mu_sd,
       tau_mean = tau_mean, tau_median = tau_median,
       tau_posterior = tau_posterior)
}

#' Compute (y, se) inputs for map_prior() from raw historical-trial data
#'
#' \code{map_prior()} takes an already-computed point estimate and SE per
#' historical trial; it has no opinion on where those came from. In
#' practice they're usually derived from each trial's own reported summary
#' statistics (event counts, arm means/SDs, ...), and \pkg{metafor}'s
#' \code{\link[metafor]{escalc}} is the standard tool for that conversion.
#' This is a thin convenience wrapper: it maps \code{map_prior()}'s
#' \code{outcome_type} vocabulary onto the matching \pkg{metafor}
#' \code{measure}, calls \code{escalc()}, and returns a data frame with
#' \code{y}/\code{se} columns ready to feed straight into
#' \code{map_prior(y = ..., se = ...)}. It is not the only way to get
#' there -- \code{y}/\code{se} can equally be read by hand off a published
#' point estimate and 95% CI (\code{se = (log(upper) - log(lower)) /
#' (2 * 1.96)} on whatever scale the CI was reported) -- and it is not
#' loaded unless you have \pkg{metafor} installed.
#'
#' @details
#' \code{outcome_type} maps to a \pkg{metafor} \code{measure} as follows;
#' see \code{\link[metafor]{escalc}} for the columns each one requires:
#' \describe{
#'   \item{\code{"log_or"}}{\code{"OR"} (log odds ratio) -- 2x2 event
#'     counts \code{ai}, \code{bi}, \code{ci}, \code{di}.}
#'   \item{\code{"smd"}}{\code{"SMD"} (Hedges' g) -- arm \code{m1i}/\code{m2i},
#'     \code{sd1i}/\code{sd2i}, \code{n1i}/\code{n2i}.}
#'   \item{\code{"log_irr"}}{\code{"IRR"} (log incidence-rate ratio) --
#'     event counts and person-time \code{x1i}/\code{x2i}, \code{t1i}/\code{t2i}.}
#'   \item{\code{"mean_difference"}}{\code{"MD"} (raw mean difference) --
#'     same columns as \code{"smd"}.}
#'   \item{\code{"single_arm_log_odds"}}{\code{"PLO"} (logit of a single
#'     proportion) -- event count \code{xi} out of \code{ni}. This is the
#'     typical case: a historical control-arm response rate.}
#'   \item{\code{"correlation"}}{\code{"ZCOR"} (Fisher z) -- correlation
#'     \code{ri} and sample size \code{ni}.}
#' }
#' \code{escalc()}'s own zero-cell handling (\code{add}/\code{to}) applies
#' as usual and can be overridden via \code{...}.
#'
#' Required columns must be named exactly as \pkg{metafor} expects for that
#' measure (see Details) -- \code{data} is read by column name, not
#' position, and extra columns are carried through untouched.
#'
#' @param outcome_type One of \code{map_prior()}'s outcome types (see
#'   \code{\link{map_prior}}); selects the \pkg{metafor} \code{measure} and
#'   which columns are required from \code{data} (see Details).
#' @param data A data frame of raw per-trial summary statistics, with
#'   columns named exactly as \pkg{metafor} expects for the selected
#'   measure (see Details).
#' @param ... Additional arguments passed to \code{\link[metafor]{escalc}}
#'   (e.g. \code{add}/\code{to} for zero-cell handling). Do \strong{not}
#'   pass column-mapping arguments here (\code{xi}, \code{ai}, ...) --
#'   those are read from \code{data} by the fixed names in Details.
#'
#' @return \code{data} with two columns appended: \code{y} (the point
#'   estimate) and \code{se} (its standard error) -- pass these directly as
#'   \code{map_prior(y = es$y, se = es$se, outcome_type = ...)}.
#'
#' @seealso \code{\link{map_prior}}, \code{\link[metafor]{escalc}}
#'
#' @examples
#' if (requireNamespace("metafor", quietly = TRUE)) {
#'   # Historical control-arm event counts -> single-arm log-odds.
#'   trials <- data.frame(xi = c(12, 8, 15), ni = c(40, 35, 50))
#'   es <- historical_effect_sizes("single_arm_log_odds", trials)
#'   print(es[, c("y", "se")])
#'   map <- map_prior(y = es$y, se = es$se, outcome_type = "single_arm_log_odds")
#'   print(map)
#' }
#'
#' @export
historical_effect_sizes <- function(outcome_type, data, ...) {
  outcome_type <- match.arg(outcome_type, names(.escalc_measure_map))

  if (!requireNamespace("metafor", quietly = TRUE)) {
    stop("historical_effect_sizes() requires the 'metafor' package. ",
         "Install it with install.packages(\"metafor\").", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame of raw per-trial summary statistics.",
         call. = FALSE)
  }

  measure  <- .escalc_measure_map[[outcome_type]]
  required <- .escalc_required_cols[[outcome_type]]
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("historical_effect_sizes(): `data` is missing column(s) required ",
         "for outcome_type = '", outcome_type, "': ",
         paste(missing_cols, collapse = ", "), ". Required columns: ",
         paste(required, collapse = ", "), ". See `?historical_effect_sizes` ",
         "Details for what each column means and, if you already have a ",
         "fitted `escalc` object under different names, rename them first.",
         call. = FALSE)
  }

  # Column vectors are extracted and passed to escalc() explicitly (never
  # forwarded through `...` or matched by column name inside `data=`):
  # escalc()'s column arguments (xi, ai, m1i, ...) use non-standard
  # evaluation resolved in *its own* calling frame, which breaks silently
  # ("Cannot find the object ('..1')...") when relayed through an
  # intervening wrapper function's `...` -- confirmed by hitting exactly
  # that error during development. Passing plain vectors sidesteps NSE
  # entirely and is the only forwarding approach that's actually reliable.
  escalc_args <- c(
    list(measure = measure),
    stats::setNames(lapply(required, function(cn) data[[cn]]), required),
    list(...)
  )
  es <- do.call(metafor::escalc, escalc_args)

  if (any(is.na(es$yi)) || any(is.na(es$vi))) {
    warning("historical_effect_sizes(): some trials produced NA effect ",
            "sizes or variances (often a zero-cell issue) -- inspect the ",
            "result before passing it to map_prior(); see `add`/`to` in ",
            "`?metafor::escalc`.", call. = FALSE)
  }

  data$y  <- as.numeric(es$yi)
  data$se <- sqrt(as.numeric(es$vi))
  data
}

# outcome_type -> metafor::escalc(measure = ...) mapping, and the exact
# metafor column names each measure requires. Kept in sync with
# map_prior()'s own outcome_type vocabulary (including mean_difference,
# which has no tau_prior preset but is a perfectly normal escalc measure).
.escalc_measure_map <- list(
  log_or               = "OR",
  smd                  = "SMD",
  log_irr              = "IRR",
  mean_difference      = "MD",
  single_arm_log_odds  = "PLO",
  correlation          = "ZCOR"
)

.escalc_required_cols <- list(
  log_or               = c("ai", "bi", "ci", "di"),
  smd                  = c("m1i", "m2i", "sd1i", "sd2i", "n1i", "n2i"),
  log_irr              = c("x1i", "t1i", "x2i", "t2i"),
  mean_difference      = c("m1i", "m2i", "sd1i", "sd2i", "n1i", "n2i"),
  single_arm_log_odds  = c("xi", "ni"),
  correlation          = c("ri", "ni")
)

#' Plot the posterior distribution of the heterogeneity parameter (tau)
#'
#' Visualises the marginal posterior density of the between-trial
#' heterogeneity parameter \eqn{\tau} from a prior fitted with
#' \code{\link{map_prior}}, together with its prior density for comparison
#' (so the amount of learning about heterogeneity is visible directly).
#'
#' @param prior A \code{bayprior} object with a non-NULL \code{$tau_posterior}
#'   element (i.e. returned by \code{\link{map_prior}}).
#' @param show_prior Logical; overlay the tau prior density for comparison.
#'   Default \code{TRUE}. Has no effect if \code{prior$tau_prior} is
#'   \code{NULL}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' map <- map_prior(
#'   y = c(-0.85, -0.62, -1.10), se = c(0.25, 0.30, 0.28),
#'   outcome_type = "single_arm_log_odds"
#' )
#' plot_tau_posterior(map)
#'
#' @export
plot_tau_posterior <- function(prior, show_prior = TRUE) {
  if (is.null(prior$tau_posterior)) {
    stop("`prior` has no `$tau_posterior` element -- was it created with ",
         "`map_prior()`?", call. = FALSE)
  }

  df <- prior$tau_posterior
  df$series <- "Posterior"

  gp <- ggplot2::ggplot(df, ggplot2::aes(x = .data$tau, y = .data$density))

  if (isTRUE(show_prior) && !is.null(prior$tau_prior)) {
    fam   <- prior$tau_prior$family
    scale <- prior$tau_prior$scale
    prior_density <- if (identical(fam, "half_normal")) {
      .dhalfnormal(df$tau, scale)
    } else {
      .dhalfcauchy(df$tau, scale)
    }
    df_prior <- data.frame(tau = df$tau, density = prior_density, series = "Prior")
    gp <- gp +
      ggplot2::geom_line(data = df_prior,
                          ggplot2::aes(x = .data$tau, y = .data$density,
                                       colour = .data$series),
                          linetype = "dashed", linewidth = 0.7)
  }

  subtitle <- if (!is.null(prior$tau_prior)) {
    sprintf("%d historical trials | %s(0, %.2f) prior on tau",
            prior$n_trials %||% NA_integer_,
            if (identical(prior$tau_prior$family, "half_cauchy"))
              "Half-Cauchy" else "Half-Normal",
            prior$tau_prior$scale %||% NA_real_)
  } else {
    sprintf("%d historical trials", prior$n_trials %||% NA_integer_)
  }

  gp <- gp +
    ggplot2::geom_line(ggplot2::aes(colour = .data$series), linewidth = 0.9) +
    ggplot2::geom_area(fill = "#1D9E75", alpha = 0.15) +
    ggplot2::scale_colour_manual(values = c("Posterior" = "#1D9E75", "Prior" = "grey50")) +
    ggplot2::labs(
      title = "Heterogeneity parameter (tau) posterior",
      subtitle = subtitle,
      # Plain ASCII "tau", NOT expression(tau) and NOT the literal Greek
      # character: ggplotly()/plotly_build() cannot handle R plotmath
      # expression objects in axis titles -- verify_attr_spec() calls
      # unique()/duplicated() on the label, which errors "unimplemented
      # type 'expression'" for anything but plain character. This plot is
      # used both as a static ggplot (in prior_report.qmd) and wrapped in
      # ggplotly() (in the Shiny app), so the label must work in both
      # contexts -- but a literal (or backslash-u-escaped -- the runtime
      # value is identical either way) Greek tau character is *not* safe
      # here either: it
      # crashes grid's text-bounds calculation with "conversion failure on
      # 'tau' in 'mbcsToSbcs'" on graphics devices/locales that can't
      # represent it (reproduced under R CMD check --as-cran). Plain ASCII
      # "tau" is the only representation verified safe in all three
      # contexts (source-file ASCII check, ggplotly, and grid rendering).
      x = "tau", y = "Density", colour = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "top")

  gp
}