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
#' @return A named list of class \code{bayprior_conflict_mv} with components:
#'   \describe{
#'     \item{mahal_distance}{Mahalanobis distance D.}
#'     \item{mahal_D2}{Squared distance D^2.}
#'     \item{pvalue}{Chi-squared p-value (df = p).}
#'     \item{df}{Degrees of freedom (number of endpoints, p).}
#'     \item{conflict_flag}{Logical. TRUE if pvalue < alpha.}
#'     \item{marginal_z}{Named numeric vector of per-parameter marginal
#'       z-scores (difference standardised by the marginal predictive SD).}
#'     \item{interpretation}{Character. Plain-language summary.}
#'     \item{labels}{Endpoint labels.}
#'     \item{alpha}{The significance level used.}
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
  .bp_h1("Multivariate Prior-Data Conflict (Mahalanobis)")
  .bp_li("Mahalanobis D : ", round(x$mahal_distance, 4))
  .bp_li("Chi-sq p-value: ", paste0(round(x$pvalue, 4), " (df = ", x$df, ")"))
  .bp_li("Conflict flag : ", x$conflict_flag)
  cat("\nMarginal z-scores per parameter:\n")
  print(round(x$marginal_z, 3))
  cat("\n")
  .bp_alert_info(x$interpretation)
  invisible(x)
}