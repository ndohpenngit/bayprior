#' Print method for bayprior objects
#'
#' @param x A `bayprior` object.
#' @param ... Ignored.
#' @return Invisibly returns the input \code{bayprior} object. Called
#'   for its side effect of printing a formatted summary of the prior
#'   distribution including family, parameters, mean, SD, and 95\%
#'   credible interval.
#' @export
print.bayprior <- function(x, ...) {
  .bp_h1(paste0("bayprior: ", x$label))
  if (x$dist != "mixture" && x$dist != "log_pool") {
    param_str <- paste(
      names(x$params),
      round(unlist(x$params), 4),
      sep = " = ", collapse = ", "
    )
    .bp_li("Distribution : ", toupper(x$dist))
    .bp_li("Parameters   : ", param_str)
    .bp_li("Method       : ", paste(x$method, "elicitation"))
    .bp_li("Expert       : ", x$expert_id)
    s <- x$fit_summary
    .bp_li("Mean (SD)    : ", paste0(round(s$mean, 4), " (", round(s$sd, 4), ")"))
    if (!is.null(s$q025)) {
      .bp_li("95% CrI      : ", paste0("[", round(s$q025, 4), ", ", round(s$q975, 4), "]"))
    }
  } else {
    .bp_li("Distribution : ", toupper(x$dist))
    .bp_li("Components   : ", length(x$components))
    .bp_li("Weights      : ", paste(round(x$weights, 3), collapse = ", "))
    .bp_li("Mean         : ", round(x$fit_summary$mean, 4))
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
#' @return Invisibly returns the input \code{bayprior_conflict} object.
#'   Called for its side effect of printing conflict diagnostic statistics
#'   including Box p-value, surprise index, information divergence,
#'   Bhattacharyya overlap, and colour-coded conflict severity.
#' @export
print.bayprior_conflict <- function(x, ...) {
  .bp_h1("Prior-Data Conflict Diagnostics")
  .bp_h2(paste0("Prior: ", x$prior$label))
  cat("\n")
  .bp_li("Box's p-value       : ", round(x$box_pvalue, 4))
  .bp_li("Surprise index      : ", round(x$surprise_index, 4))
  .bp_li("KL divergence       : ", round(x$kl_prior_likelihood, 4))
  .bp_li("Overlap coefficient : ", round(x$overlap, 4))
  sev_label <- toupper(x$conflict_severity)
  .bp_li("Conflict severity   : ", sev_label)
  cat("\n")
  .bp_alert(x$recommendation)
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


# -- Internal print helpers ----------------------------------------------------
# Use cli when running interactively (IDE/terminal), fall back to plain cat()
# in non-interactive contexts (rmarkdown::render, callr subprocesses, etc.)

.bp_use_cli <- function() {
  # cli works reliably when RSTUDIO or POSITRON env var is set (IDE sessions),
  # or when stdout is a genuine terminal (isatty). Falls back to cat() otherwise.
  ide  <- nchar(Sys.getenv("RSTUDIO")) > 0 || nchar(Sys.getenv("POSITRON")) > 0
  tty  <- isatty(stdout())
  ide || tty
}

.bp_h1 <- function(txt) {
  if (.bp_use_cli()) {
    cli::cli_h1(txt)
  } else {
    cat("\n--", txt, "--\n")
  }
}

.bp_h2 <- function(txt) {
  if (.bp_use_cli()) {
    cli::cli_h2(txt)
  } else {
    cat("  ", txt, "\n")
  }
}

.bp_li <- function(label, value) {
  if (.bp_use_cli()) {
    cli::cli_li(paste0(label, value))
  } else {
    cat(paste0("\u2022 ", label, value, "\n"))
  }
}

.bp_alert <- function(txt) {
  if (.bp_use_cli()) {
    cli::cli_alert_warning(txt)
  } else {
    cat("! ", txt, "\n")
  }
}

.bp_alert_info <- function(txt) {
  if (.bp_use_cli()) {
    cli::cli_alert_info(txt)
  } else {
    cat("i ", txt, "\n")
  }
}

.bp_alert_success <- function(txt) {
  if (.bp_use_cli()) {
    cli::cli_alert_success(txt)
  } else {
    cat("v ", txt, "\n")
  }
}