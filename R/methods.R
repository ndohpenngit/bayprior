#' Print method for bayprior objects
#'
#' @param x A `bayprior` object.
#' @param ... Ignored.
#' @export
print.bayprior <- function(x, ...) {
  cli::cli_h1("bayprior: {x$label}")
  cli::cli_ul()
  cli::cli_li("Distribution : {toupper(x$dist)}")

  if (x$dist != "mixture" && x$dist != "log_pool") {
    param_str <- paste(
      names(x$params),
      round(unlist(x$params), 4),
      sep = " = ", collapse = ", "
    )
    cli::cli_li("Parameters   : {param_str}")
    cli::cli_li("Method       : {x$method} elicitation")
    cli::cli_li("Expert       : {x$expert_id}")
    s <- x$fit_summary
    cli::cli_li("Mean (SD)    : {round(s$mean, 4)} ({round(s$sd, 4)})")
    if (!is.null(s$q025)) {
      cli::cli_li("95% CrI      : [{round(s$q025, 4)}, {round(s$q975, 4)}]")
    }
  } else {
    cli::cli_li("Components   : {length(x$components)}")
    w_str <- paste(round(x$weights, 3), collapse = ", ")
    cli::cli_li("Weights      : {w_str}")
    cli::cli_li("Mean         : {round(x$fit_summary$mean, 4)}")
  }
  invisible(x)
}


#' Summary method for bayprior objects
#'
#' @param object A `bayprior` object.
#' @param ... Ignored.
#' @return A list with summary statistics (invisibly).
#' @export
summary.bayprior <- function(object, ...) {
  cat("\n== bayprior Summary ==\n\n")
  cat("Label       :", object$label, "\n")
  cat("Distribution:", toupper(object$dist), "\n")
  if (!is.null(object$params)) {
    cat("Parameters  :", paste(names(object$params),
                               round(unlist(object$params), 4),
                               sep = " = ", collapse = ", "), "\n")
  }
  s <- object$fit_summary
  cat("Mean        :", round(s$mean, 4), "\n")
  cat("SD          :", round(s$sd, 4), "\n")
  if (!is.null(s$q025)) {
    cat("2.5%  :     ", round(s$q025, 4), "\n")
    cat("50%   :     ", round(s$q500, 4), "\n")
    cat("97.5% :     ", round(s$q975, 4), "\n")
  }
  invisible(s)
}


#' Print method for bayprior_conflict objects
#'
#' @param x A `bayprior_conflict` object.
#' @param ... Ignored.
#' @export
print.bayprior_conflict <- function(x, ...) {
  cli::cli_h1("Prior-Data Conflict Diagnostics")
  cli::cli_h2("Prior: {x$prior$label}")

  sev_col <- switch(x$conflict_severity,
    none   = cli::col_green,
    mild   = cli::col_yellow,
    severe = cli::col_red
  )
  cat("\n")
  cli::cli_ul()
  cli::cli_li("Box's p-value       : {round(x$box_pvalue, 4)}")
  cli::cli_li("Surprise index      : {round(x$surprise_index, 4)}")
  cli::cli_li("KL divergence       : {round(x$kl_prior_likelihood, 4)}")
  cli::cli_li("Overlap coefficient : {round(x$overlap, 4)}")
  cli::cli_li("Conflict severity   : {sev_col(toupper(x$conflict_severity))}")
  cat("\n")
  cli::cli_alert_warning("{x$recommendation}")
  invisible(x)
}


#' Constructor for bayprior from raw parameters
#'
#' Construct a `bayprior` object directly from known hyperparameters
#' (e.g., literature-based priors), bypassing elicitation.
#'
#' @param dist Character. One of `"beta"`, `"normal"`, `"gamma"`.
#' @param params Named list of hyperparameters.
#' @param label Character. Description of the quantity.
#' @param expert_id Character. Source identifier.
#'
#' @return A `bayprior` object.
#'
#' @examples
#' # Historical Beta(2, 8) prior on response rate
#' prior <- as_prior("beta", list(alpha = 2, beta = 8),
#'                   label = "Historical response rate prior")
#'
#' @export
as_prior <- function(dist, params, label = "Prior", expert_id = "Literature") {
  dist <- match.arg(dist, c("beta", "normal", "gamma"))
  .make_bayprior(dist, params, method = "direct", expert_id = expert_id,
                 label = label, input = params)
}
