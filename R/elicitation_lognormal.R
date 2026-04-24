#' Elicit a Log-Normal prior via quantile matching or moment matching
#'
#' Fits a Log-Normal distribution to expert-specified quantiles or moments.
#' Appropriate for positive-valued quantities such as hazard ratios, fold
#' changes, median survival times, or PK parameters.
#'
#' @param quantiles Named numeric vector. Values must be strictly positive.
#'   E.g. `c("0.05" = 0.5, "0.50" = 2.0, "0.95" = 8.0)`.
#' @param mean     Optional numeric. Expert-specified mean on the **original**
#'   (not log) scale for moment matching.
#' @param sd       Optional numeric. Expert-specified SD on the original scale.
#' @param method   Character. `"quantile"` (default) or `"moments"`.
#' @param expert_id Character. Expert identifier.
#' @param label    Character. Description of the quantity.
#' @param tol      Numeric. Optimisation tolerance. Default `1e-6`.
#'
#' @return An object of class `bayprior` with `dist = "lognormal"` and
#'   `params` list containing `meanlog` and `sdlog`.
#'
#' @examples
#' # Hazard ratio: expert believes median HR ~ 0.7, 90% CI [0.4, 1.2]
#' prior <- elicit_lognormal(
#'   quantiles = c("0.05" = 0.40, "0.50" = 0.70, "0.95" = 1.20),
#'   label     = "Hazard ratio (treatment vs control)"
#' )
#' print(prior)
#' plot(prior)
#'
#' # Moment matching: mean HR = 0.75, SD = 0.25
#' prior_m <- elicit_lognormal(
#'   mean = 0.75, sd = 0.25,
#'   method = "moments",
#'   label  = "Hazard ratio"
#' )
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
    if (is.null(quantiles) || length(quantiles) < 2) {
      rlang::abort("At least 2 quantile specifications required for quantile matching.")
    }
    probs <- as.numeric(names(quantiles))
    vals  <- as.numeric(quantiles)
    if (any(probs <= 0 | probs >= 1)) rlang::abort("Probabilities must be in (0, 1).")
    if (any(vals  <= 0))              rlang::abort("All values must be strictly positive for Log-Normal.")
    if (!all(diff(vals) > 0))        rlang::abort("Quantile values must be strictly increasing.")

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
    if (is.null(mean) || is.null(sd)) {
      rlang::abort("Both `mean` and `sd` required for moment matching.")
    }
    if (mean <= 0 || sd <= 0) rlang::abort("`mean` and `sd` must be positive.")
    sdlog   <- sqrt(log(1 + (sd / mean)^2))
    meanlog <- log(mean) - sdlog^2 / 2
    input   <- list(mean = mean, sd = sd)
  }

  .make_bayprior("lognormal",
                 list(meanlog = meanlog, sdlog = sdlog),
                 method, expert_id, label, input)
}


#' Roulette-method elicitation (chip-allocation)
#'
#' Implements the SHELF roulette method: the expert allocates a fixed number
#' of "chips" across a set of pre-defined bins representing the range of the
#' quantity. The resulting histogram is fitted to a parametric distribution.
#'
#' In the Shiny app (`mod_elicitation`) the roulette grid is rendered
#' interactively. This function provides the **fitting back-end** that can
#' also be called programmatically when chips are known.
#'
#' @param chips   Integer vector. Number of chips in each bin (left-to-right).
#' @param breaks  Numeric vector of length `length(chips) + 1` defining the
#'   bin edges.
#' @param family  Character. Distribution to fit. One of `"beta"`,
#'   `"normal"`, `"gamma"`, `"lognormal"`.
#' @param expert_id Character. Expert identifier.
#' @param label   Character. Quantity description.
#'
#' @return A `bayprior` object fitted to the chip histogram.
#'
#' @details
#' Chips are converted to relative frequencies, and bin midpoints are used as
#' representative values. The chosen `family` is then fitted by minimising the
#' weighted sum of squared CDF differences (a histogram-matching approach).
#'
#' @references
#' Oakley, J. E. & O'Hagan, A. (2010). SHELF: the Sheffield Elicitation
#' Framework. University of Sheffield.
#'
#' @examples
#' # Expert places 0, 2, 5, 8, 5, 2, 1 chips across bins [0,.1,.2,...,.7]
#' prior <- elicit_roulette(
#'   chips   = c(0L, 2L, 5L, 8L, 5L, 2L, 1L),
#'   breaks  = seq(0, 0.7, by = 0.1),
#'   family  = "beta",
#'   label   = "Response rate"
#' )
#' print(prior)
#'
#' @export
elicit_roulette <- function(chips,
                             breaks,
                             family    = c("beta", "normal", "gamma", "lognormal"),
                             expert_id = "Expert_1",
                             label     = "Unknown quantity") {

  family <- match.arg(family)

  if (length(breaks) != length(chips) + 1) {
    rlang::abort("`breaks` must have exactly one more element than `chips`.")
  }
  if (any(chips < 0)) rlang::abort("All chip counts must be non-negative.")
  if (sum(chips) == 0) rlang::abort("At least one chip must be placed.")

  # Convert chips → empirical CDF at bin midpoints
  total    <- sum(chips)
  freqs    <- chips / total
  mids     <- (breaks[-length(breaks)] + breaks[-1]) / 2
  cum_freq <- cumsum(freqs)

  # Use midpoints with cumulative probability as quantile pairs
  # (exclude any zero-probability prefix / suffix for stability)
  keep     <- cum_freq > 0 & cum_freq < 1
  if (sum(keep) < 2) {
    # Fall back to using all non-zero midpoints
    keep   <- freqs > 0
  }
  probs    <- cum_freq[keep]
  vals     <- mids[keep]

  quantiles <- setNames(vals, as.character(probs))

  # Delegate to the appropriate quantile-matching elicitor
  result <- switch(family,
    beta      = elicit_beta(quantiles     = quantiles,
                            method        = "quantile",
                            expert_id     = expert_id,
                            label         = label),
    normal    = elicit_normal(quantiles   = quantiles,
                              method      = "quantile",
                              expert_id   = expert_id,
                              label       = label),
    gamma     = elicit_gamma(quantiles    = quantiles,
                             method       = "quantile",
                             expert_id    = expert_id,
                             label        = label),
    lognormal = elicit_lognormal(quantiles = quantiles,
                                 method    = "quantile",
                                 expert_id = expert_id,
                                 label     = label)
  )

  # Attach the raw chip data for provenance
  result$roulette <- list(chips = chips, breaks = breaks,
                          freqs = freqs, mids = mids)
  result
}
