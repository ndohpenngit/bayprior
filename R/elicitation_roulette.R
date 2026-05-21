#' Roulette-method elicitation (chip-allocation)
#'
#' Implements the SHELF roulette method: the expert allocates a fixed number
#' of "chips" across a set of pre-defined bins representing the range of the
#' quantity. The resulting histogram is fitted to a parametric distribution.
#'
#' In the Shiny app (`Prior elicitation tab`) the roulette grid is rendered
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

  # Convert chips -> empirical CDF at bin midpoints
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
