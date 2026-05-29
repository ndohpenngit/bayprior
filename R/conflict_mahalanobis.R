#' Multivariate prior-data conflict via Mahalanobis distance
#'
#' Tests joint prior-data conflict across two correlated endpoints using the
#' Mahalanobis distance. Under the null (no conflict) the squared distance
#' follows a chi-squared distribution with degrees of freedom equal to the
#' number of endpoints.
#'
#' @param prior_means Numeric vector of length k. Prior means for each endpoint.
#' @param prior_cov  k x k numeric matrix. Prior covariance matrix.
#' @param obs_means   Numeric vector of length k. Observed data means.
#' @param obs_cov    k x k numeric matrix. Observed data covariance (Var/n for
#'   each diagonal; Cov/n for off-diagonal).
#' @param alpha      Numeric. Significance level for the chi-squared test.
#'   Default \code{0.05}.
#' @param labels     Character vector of length k. Endpoint labels for output.
#'
#' @return A named list with components:
#'   \describe{
#'     \item{mahal_distance}{Mahalanobis distance D.}
#'     \item{mahal_d2}{Squared distance D^2.}
#'     \item{p_value}{Chi-squared p-value (df = k).}
#'     \item{conflict_flag}{Logical. TRUE if p_value < alpha.}
#'     \item{labels}{Endpoint labels.}
#'   }
#'
#' @details
#' \strong{Assumptions:} This test assumes that the prior and observed summary
#' statistics are approximately multivariate Normal. For proportion endpoints
#' (e.g. response rates), transform to the \strong{log-odds scale} before
#' entering means and variances. For hazard ratios, use the \strong{log scale}.
#' Results may be unreliable if the Normal approximation is poor (e.g. for
#' small samples with extreme proportions).
#'
#' \strong{Current limitation:} The function is designed for bivariate
#' (k = 2) endpoints. While it will accept k > 2, the Shiny interface
#' currently only exposes two endpoints. Support for k >= 3 is a planned
#' extension.
#'
#' \strong{Distribution family:} The Mahalanobis approach is
#' distribution-agnostic at the summary statistic level -- it does not require
#' a specific prior family (Beta, Normal, etc.). Any continuous prior whose
#' mean and covariance can be extracted is supported.
#'
#' @references
#' Mahalanobis, P. C. (1936). On the generalised distance in statistics.
#' \emph{Proceedings of the National Institute of Sciences of India}, 2, 49--55.
#'
#' @examples
#' pm   <- c(0.35, 0.60)
#' pcov <- matrix(c(0.010, 0.003, 0.003, 0.015), 2, 2)
#' om   <- c(0.55, 0.58)
#' ocov <- matrix(c(2e-4, 4e-5, 4e-5, 2e-4), 2, 2)
#' conflict_mahalanobis(pm, pcov, om, ocov, labels = c("Response rate", "OS rate"))
#'
#' @export
conflict_mahalanobis <- function(prior_means,
                                  prior_cov,
                                  obs_means,
                                  obs_cov,
                                  alpha  = 0.05,
                                  labels = NULL) {

  p <- length(prior_means)
  if (length(obs_means) != p) {
    rlang::abort("`prior_means` and `obs_means` must have the same length.")
  }
  if (!all(dim(prior_cov) == p) || !all(dim(obs_cov) == p)) {
    rlang::abort("Covariance matrices must be p x p where p = length(prior_means).")
  }
  if (is.null(labels)) labels <- paste0("param_", seq_len(p))

  # Prior predictive covariance = prior_cov + obs_cov
  pred_cov <- prior_cov + obs_cov

  # Mahalanobis distance
  diff_vec <- obs_means - prior_means
  pred_inv <- tryCatch(
    solve(pred_cov),
    error = function(e) rlang::abort("Prior predictive covariance is not invertible.")
  )
  D2  <- as.numeric(t(diff_vec) %*% pred_inv %*% diff_vec)
  D   <- sqrt(D2)
  pval <- stats::pchisq(D2, df = p, lower.tail = FALSE)
  flag <- pval < alpha

  # Contribution of each parameter to total distance (marginal standardisation)
  marginal_z <- diff_vec / sqrt(diag(pred_cov))
  names(marginal_z) <- labels

  interp <- if (!flag) {
    glue::glue(
      "No multivariate prior-data conflict detected ",
      "(Mahalanobis D = {round(D, 3)}, p = {round(pval, 3)})."
    )
  } else {
    top_contrib <- labels[which.max(abs(marginal_z))]
    glue::glue(
      "Multivariate prior-data conflict detected ",
      "(D = {round(D, 3)}, chi-sq p = {round(pval, 4)}). ",
      "Largest individual contribution: {top_contrib} ",
      "(z = {round(marginal_z[top_contrib], 2)})."
    )
  }

  structure(
    list(
      mahal_distance = D,
      mahal_D2       = D2,
      pvalue         = pval,
      df             = p,
      conflict_flag  = flag,
      marginal_z     = marginal_z,
      interpretation = as.character(interp),
      labels         = labels,
      alpha          = alpha
    ),
    class = "bayprior_conflict_mv"
  )
}


#' Print method for multivariate conflict objects
#'
#' @param x A \code{bayprior_conflict_mv} object.
#' @param ... Ignored.
#' @return Invisibly returns the input \code{bayprior_conflict_mv} object.
#'   Called for its side effect of printing a formatted summary of the
#'   multivariate Mahalanobis conflict check, including the Mahalanobis
#'   distance, chi-squared p-value, conflict flag, per-parameter marginal
#'   z-scores, and an interpretation string.
#' @export
print.bayprior_conflict_mv <- function(x, ...) {
  cli::cli_h1("Multivariate Prior-Data Conflict (Mahalanobis)")
  cli::cli_ul()
  cli::cli_li("Mahalanobis D : {round(x$mahal_distance, 4)}")
  cli::cli_li("Chi-sq p-value: {round(x$pvalue, 4)} (df = {x$df})")
  cli::cli_li("Conflict flag : {x$conflict_flag}")
  cat("\nMarginal z-scores per parameter:\n")
  print(round(x$marginal_z, 3))
  cat("\n")
  cli::cli_alert_info("{x$interpretation}")
  invisible(x)
}


#' Sensitivity of posterior CrI to prior hyperparameters
#'
#' A focused sensitivity analysis that tracks the width and location of the
#' posterior credible interval (CrI) across a one- or two-dimensional
#' hyperparameter grid. This directly answers the regulatory question:
#' "Does the CrI change materially under plausible alternative priors?"
#'
#' @param prior       A \code{bayprior} object (reference prior).
#' @param data_summary Named list as for \code{prior_conflict()}.
#' @param param_grid  Named list of numeric vectors (1 or 2 parameters).
#' @param cri_level   Numeric in (0, 1). Credible level. Default \code{0.95}.
#' @param threshold   Numeric. Efficacy threshold for \code{Pr(theta > threshold)}.
#'   Default \code{NULL} (skips probability computation).
#'
#' @return A \code{bayprior_sensitivity} object (same class as
#'   \code{sensitivity_grid}) with additional columns \code{cri_lower},
#'   \code{cri_upper}, and \code{cri_width} in the result grid.
#'
#' @examples
#' prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
#' cri_sa <- sensitivity_cri(
#'   prior,
#'   data_summary = list(type = "binary", x = 14, n = 40),
#'   param_grid   = list(alpha = seq(1, 8, 0.5), beta = seq(2, 20, 1)),
#'   cri_level    = 0.95,
#'   threshold    = 0.30
#' )
#' plot_sensitivity(cri_sa, target = "cri_width")
#'
#' @export
sensitivity_cri <- function(prior,
                              data_summary,
                              param_grid,
                              cri_level = 0.95,
                              threshold = NULL) {

  lo_p <- (1 - cri_level) / 2
  hi_p <- 1 - lo_p

  type <- data_summary$type %||% "binary"
  n    <- data_summary$n
  x    <- data_summary$x

  grid_df <- do.call(expand.grid, param_grid)

  ref_params <- prior$params[names(param_grid)]
  dists      <- rowSums(mapply(function(col, ref) (grid_df[[col]] - ref)^2,
                               names(param_grid), ref_params))
  ref_row    <- which.min(dists)

  results <- purrr::map_dfr(seq_len(nrow(grid_df)), function(i) {
    row    <- grid_df[i, , drop = FALSE]
    params <- as.list(row)

    tmp <- tryCatch(
      .make_bayprior(prior$dist, params, prior$method,
                     prior$expert_id, prior$label, prior$input),
      error = function(e) NULL
    )
    if (is.null(tmp)) {
      out <- as.list(row)
      out$posterior_mean <- NA; out$posterior_sd <- NA
      out$cri_lower <- NA; out$cri_upper <- NA; out$cri_width <- NA
      return(as.data.frame(out))
    }

    post <- tryCatch(.conjugate_update(tmp, data_summary), error = function(e) NULL)
    if (is.null(post)) {
      out <- as.list(row)
      out$posterior_mean <- NA; out$posterior_sd <- NA
      out$cri_lower <- NA; out$cri_upper <- NA; out$cri_width <- NA
      return(as.data.frame(out))
    }

    ps <- post$fit_summary
    # CrI bounds
    cri_lo <- ps$mean - stats::qnorm(hi_p) * ps$sd
    cri_hi <- ps$mean + stats::qnorm(hi_p) * ps$sd
    # For Beta posterior use exact quantiles
    if (post$dist == "beta") {
      cri_lo <- stats::qbeta(lo_p, post$params$alpha, post$params$beta)
      cri_hi <- stats::qbeta(hi_p, post$params$alpha, post$params$beta)
    }

    out <- as.list(row)
    out$posterior_mean <- ps$mean
    out$posterior_sd   <- ps$sd
    out$cri_lower      <- cri_lo
    out$cri_upper      <- cri_hi
    out$cri_width      <- cri_hi - cri_lo

    if (!is.null(threshold)) {
      out$prob_efficacy <- if (post$dist == "beta") {
        stats::pbeta(threshold, post$params$alpha, post$params$beta, lower.tail = FALSE)
      } else {
        stats::pnorm(threshold, ps$mean, ps$sd, lower.tail = FALSE)
      }
    }
    as.data.frame(out)
  })

  tgt <- c("posterior_mean", "posterior_sd", "cri_lower", "cri_upper", "cri_width")
  if (!is.null(threshold)) tgt <- c(tgt, "prob_efficacy")

  infl <- vapply(tgt, function(t) {
    v <- results[[t]]
    if (all(is.na(v))) NA_real_ else diff(range(v, na.rm = TRUE))
  }, numeric(1))

  structure(
    list(
      grid             = results,
      param_grid       = param_grid,
      target           = tgt,
      reference_row    = ref_row,
      influence_scores = infl,
      threshold        = threshold,
      cri_level        = cri_level,
      prior            = prior
    ),
    class = "bayprior_sensitivity"
  )
}