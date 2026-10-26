# Prior-strength grids for sensitivity analysis
#
# A grid over the raw hyperparameters (alpha, beta, ...) mixes two different
# questions: how informative the prior is, and where it is centred. These
# helpers build a grid in those two terms instead:
#
#   * prior strength: the prior's effective sample size, multiplied by a factor
#     around the elicited value (default 0.5x to 2x);
#   * prior mean: optionally shifted by a number of prior standard deviations.
#
# The result is a data frame with one row per grid point (hyperparameter
# columns plus `prior_mean` and `prior_strength`), which sensitivity_grid()
# and sensitivity_cri() accept in place of a named list of vectors.
#
# Supported working-prior families: beta, gamma, normal. Other families keep
# using a grid over their hyperparameters.

# -- Family specification ------------------------------------------------------
# Returns NULL for unsupported families.
#' @noRd
.design_spec <- function(working) {
  if (is.null(working) || is.null(working$dist) || is.null(working$params))
    return(NULL)
  p <- working$params
  ok <- function(...) all(vapply(list(...), function(v)
    is.numeric(v) && length(v) == 1L && is.finite(v) && v > 0, logical(1)))

  switch(working$dist,
    beta = {
      a <- p$alpha; b <- p$beta
      if (is.null(a) || is.null(b) || !ok(a, b)) return(NULL)
      S <- a + b; m <- a / S
      list(family = "beta", loc0 = m, str0 = S,
           sd0 = sqrt(m * (1 - m) / (S + 1)),
           lo = 0.01, hi = 0.99,
           strength_label = "effective sample size (alpha + beta)",
           strength_short = "Prior effective sample size",
           to_params = function(loc, str)
             list(alpha = loc * str, beta = (1 - loc) * str),
           implied = function(g) list(mean = g$alpha / (g$alpha + g$beta),
                                      strength = g$alpha + g$beta))
    },
    gamma = {
      s <- p$shape; r <- p$rate
      if (is.null(s) || is.null(r) || !ok(s, r)) return(NULL)
      list(family = "gamma", loc0 = s / r, str0 = r,
           sd0 = sqrt(s) / r,
           lo = 0.05 * s / r, hi = Inf,
           strength_label = "prior exposure (rate)",
           strength_short = "Prior exposure (rate)",
           to_params = function(loc, str) list(shape = loc * str, rate = str),
           implied = function(g) list(mean = g$shape / g$rate, strength = g$rate))
    },
    normal = {
      mu <- p$mu; sg <- p$sigma
      if (is.null(mu) || !is.finite(mu) || is.null(sg) || !ok(sg)) return(NULL)
      list(family = "normal", loc0 = mu, str0 = 1 / sg^2,
           sd0 = sg, lo = -Inf, hi = Inf,
           strength_label = "prior precision (1 / sigma^2)",
           strength_short = "Prior precision",
           to_params = function(loc, str) list(mu = loc, sigma = 1 / sqrt(str)),
           implied = function(g) list(mean = g$mu, strength = 1 / g$sigma^2))
    },
    NULL
  )
}

#' @noRd
.design_supported <- function(working) !is.null(.design_spec(working))

# -- Strength grid -------------------------------------------------------------
#' Build a prior-strength grid for sensitivity analysis
#'
#' @param working A single-family \code{bayprior} object (use
#'   \code{.mixture_working_prior()} first for mixtures).
#' @param scale Length-2 numeric, the range of prior strength relative to the
#'   elicited value. Default \code{c(0.5, 2)}.
#' @param n_scale Number of strength values (log-spaced); the elicited value
#'   (factor 1) is always included.
#' @param shift_sd Non-negative number. The prior mean is moved by up to this
#'   many prior standard deviations either side; 0 holds the mean fixed.
#'   Limited so the mean stays inside the family's support.
#' @param n_shift Number of mean values (made odd so the elicited mean is
#'   included).
#' @return A data frame (one row per grid point) with the hyperparameter
#'   columns, \code{prior_mean} and \code{prior_strength}, and a
#'   \code{"design"} attribute describing the grid.
#' @noRd
.strength_grid <- function(working, scale = c(0.5, 2), n_scale = 9L,
                           shift_sd = 0, n_shift = 5L) {
  spec <- .design_spec(working)
  if (is.null(spec))
    rlang::abort("A prior-strength grid is available for beta, gamma and normal priors only.")

  scale <- sort(as.numeric(scale))
  if (length(scale) != 2L || any(!is.finite(scale)) || any(scale <= 0))
    rlang::abort("`scale` must be two positive numbers.")
  n_scale <- max(2L, as.integer(n_scale))

  # Strength factors: log-spaced (strength is multiplicative); 1 always present.
  k <- if (scale[1] == scale[2]) scale[1] else
    exp(seq(log(scale[1]), log(scale[2]), length.out = n_scale))
  if (scale[1] <= 1 && 1 <= scale[2]) k <- c(k[abs(k - 1) > 1e-9], 1)
  k <- sort(unique(k))

  # Mean values: symmetric in prior SDs, limited to the family's support.
  shift_req <- max(0, as.numeric(shift_sd))
  shift_use <- 0
  loc <- spec$loc0
  if (shift_req > 0) {
    room <- min((spec$loc0 - spec$lo) / spec$sd0, (spec$hi - spec$loc0) / spec$sd0)
    shift_use <- max(0, min(shift_req, room))
    if (shift_use > 0) {
      n_shift <- max(3L, as.integer(n_shift))
      if (n_shift %% 2L == 0L) n_shift <- n_shift + 1L
      z   <- seq(-shift_use, shift_use, length.out = n_shift)
      z[(n_shift + 1L) / 2L] <- 0
      loc <- spec$loc0 + z * spec$sd0
    }
  }

  g   <- expand.grid(prior_mean = loc, .k = k, KEEP.OUT.ATTRS = FALSE)
  str <- spec$str0 * g$.k
  hp  <- as.data.frame(spec$to_params(g$prior_mean, str))
  out <- cbind(hp, data.frame(prior_mean = g$prior_mean, prior_strength = str))

  attr(out, "design") <- list(
    type           = "strength",
    family         = spec$family,
    scale          = range(k),
    shift_sd       = shift_use,
    shift_sd_asked = shift_req,
    mean_fixed     = shift_use == 0,
    mean0          = spec$loc0,
    strength0      = spec$str0,
    strength_label = spec$strength_label,
    strength_short = spec$strength_short
  )
  out
}

# -- What a grid implies in prior terms ----------------------------------------
# `grid` is a data frame holding the working prior's hyperparameter columns.
# Returns the range of the implied prior mean and strength, or NULL.
#' @noRd
.grid_implied <- function(working, grid) {
  spec <- .design_spec(working)
  if (is.null(spec) || is.null(grid) || nrow(grid) == 0L) return(NULL)
  need <- names(spec$to_params(spec$loc0, spec$str0))
  if (!all(need %in% names(grid))) return(NULL)
  imp <- tryCatch(spec$implied(grid), error = function(e) NULL)
  if (is.null(imp)) return(NULL)
  fin <- function(v) { v <- v[is.finite(v)]; if (length(v)) range(v) else NULL }
  list(spec = spec, mean = fin(imp$mean), strength = fin(imp$strength))
}

#' @noRd
.fmt_num <- function(v) format(signif(v, 3), trim = TRUE)

# One sentence describing the grid in prior terms, or NULL.
#' @noRd
.grid_note_text <- function(working, grid, design = NULL) {
  imp <- .grid_implied(working, grid)
  if (is.null(imp) || is.null(imp$mean) || is.null(imp$strength)) return(NULL)
  sp <- imp$spec
  rng <- function(r) paste0(.fmt_num(r[1]), " to ", .fmt_num(r[2]))

  lead <- ""
  if (!is.null(design) && identical(design$type, "strength")) {
    lead <- paste0("Prior strength varied from ", .fmt_num(design$scale[1]), "x to ",
                   .fmt_num(design$scale[2]), "x the elicited value; ",
                   if (isTRUE(design$mean_fixed)) {
                     "the prior mean is held fixed. "
                   } else {
                     paste0("the prior mean shifted by up to +/-",
                            .fmt_num(design$shift_sd), " prior SD. ")
                   })
  }
  paste0(lead, "This grid covers prior means ", rng(imp$mean), " (elicited ",
         .fmt_num(sp$loc0), ") and ", sp$strength_label, " ", rng(imp$strength),
         " (elicited ", .fmt_num(sp$str0), ").")
}

# Note for a finished bayprior_sensitivity object (used by the report).
#' @noRd
.sensitivity_grid_note <- function(sens) {
  pg <- sens$param_grid
  if (is.null(pg) || is.null(sens$prior)) return(NULL)
  working <- tryCatch(suppressMessages(suppressWarnings(
    .mixture_working_prior(sens$prior, quiet = TRUE))), error = function(e) NULL)
  grid <- if (is.data.frame(pg)) pg else
    tryCatch(do.call(expand.grid, pg), error = function(e) NULL)
  .grid_note_text(working, grid, attr(pg, "design"))
}