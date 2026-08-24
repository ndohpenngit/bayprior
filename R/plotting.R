#' Plot prior, likelihood, and posterior density overlays
#'
#' @param prior A \code{bayprior} object from any \code{elicit_*()} function.
#' @param data_summary Named list with \code{n}, \code{x}, optionally
#'   \code{sd} and \code{type}.
#' @param show_posterior Logical. Default \code{TRUE}.
#' @param show_conflict Logical. Default \code{TRUE}.
#' @param n_grid Integer. Default \code{500}.
#' @param title Character. Plot title.
#' @return A \code{ggplot} object.
#' @examples
#' prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments",
#'                      label = "Response rate")
#' plot_prior_likelihood(prior, list(n = 40, x = 20, type = "binary"))
#' @export
plot_prior_likelihood <- function(prior,
                                  data_summary,
                                  show_posterior = TRUE,
                                  show_conflict  = TRUE,
                                  n_grid = 500,
                                  title = NULL) {

  type <- data_summary$type %||% "binary"
  n    <- data_summary$n
  x    <- data_summary$x

  # Determine the likelihood's location before building the grid, so the
  # grid can span both the prior's range and the likelihood's range (see
  # note below on why this matters). Binary is exempt: both the Beta
  # likelihood and the typical Beta prior are inherently confined to
  # [0, 1], so there is no clipping risk to guard against there.
  if (type %in% c("poisson", "survival")) {
    lik_obs_mean <- x / n
    lik_obs_se   <- max(sqrt(x) / n, 1e-8)
  } else if (type != "binary") {
    lik_obs_mean <- x
    lik_obs_se   <- max((data_summary$sd %||% NA_real_) / sqrt(n), 1e-8)
  }

  range_p <- .prior_range(prior)
  if (type == "binary") {
    grid <- seq(range_p$lo, range_p$hi, length.out = n_grid)
  } else {
    # Union with the likelihood's range -- not just the prior's. This plot
    # exists specifically to visualise prior-vs-data agreement/conflict;
    # using only the prior's range meant that in a genuinely severe
    # conflict (likelihood far from the prior), the likelihood curve could
    # fall partly or entirely outside the plotted range and be silently
    # clipped by ggplot2 -- hiding the exact scenario this plot exists to
    # surface.
    lik_lo <- lik_obs_mean - 4 * lik_obs_se
    lik_hi <- lik_obs_mean + 4 * lik_obs_se
    grid   <- seq(min(range_p$lo, lik_lo), max(range_p$hi, lik_hi),
                  length.out = n_grid)
  }

  prior_d <- .eval_density_vec(prior, grid)

  # Likelihood curve shape depends on data type. Binary has an exact
  # Beta-Binomial shape; poisson/survival and continuous need distinct
  # Normal-approximation parameters -- poisson/survival's "x" is an event
  # count, not a rate, and has no data_summary$sd field at all (only
  # continuous data collects one), so these cannot share a branch.
  if (type == "binary") {
    lik_d <- stats::dbeta(grid, x + 1, n - x + 1)
  } else {
    lik_d <- stats::dnorm(grid, lik_obs_mean, lik_obs_se)
  }
  lik_d <- lik_d / max(lik_d) * max(prior_d)

  df <- data.frame(
    theta   = rep(grid, 2),
    density = c(prior_d, lik_d),
    source  = rep(c("Prior", "Likelihood (scaled)"), each = n_grid)
  )

  if (show_posterior) {
    post   <- .conjugate_update(prior, data_summary)
    post_d <- .eval_density_vec(post, grid)
    df <- rbind(df, data.frame(
      theta   = grid,
      density = post_d,
      source  = "Posterior"
    ))
  }

  pal <- c(
    "Prior"               = "#185FA5",
    "Likelihood (scaled)" = "#D85A30",
    "Posterior"           = "#1D9E75"
  )

  ggplot2::ggplot(df, ggplot2::aes(x = theta, y = density,
                                   colour = source, fill = source)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_area(alpha = 0.15, position = "identity") +
    ggplot2::scale_colour_manual(values = pal, name = NULL) +
    ggplot2::scale_fill_manual(values = pal, name = NULL) +
    ggplot2::labs(
      title    = title %||% glue::glue("Prior-Likelihood-Posterior: {prior$label}"),
      subtitle = glue::glue("Data: n = {n}, events/mean = {x}"),
      x        = prior$label,
      y        = "Density"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "top")
}


#' Plot sensitivity analysis results
#'
#' @param sensitivity A \code{bayprior_sensitivity} object.
#' @param target Character. Which target quantity to plot.
#' @param highlight_reference Logical. Default \code{TRUE}.
#' @return A \code{ggplot} object.
#' @export
plot_sensitivity <- function(sensitivity,
                             target = NULL,
                             highlight_reference = TRUE) {

  if (!inherits(sensitivity, "bayprior_sensitivity"))
    rlang::abort("`sensitivity` must be a bayprior_sensitivity object.")
  if (is.null(target)) target <- sensitivity$target[1]

  grid    <- sensitivity$grid
  params  <- names(sensitivity$param_grid)
  ref_row <- sensitivity$reference_row

  # Professional axis label -- replace snake_case with readable title
  target_label <- .target_label(target)

  if (length(params) == 1) {
    p <- ggplot2::ggplot(
      grid, ggplot2::aes(x = .data[[params[1]]], y = .data[[target]])
    ) +
      ggplot2::geom_line(colour = "#185FA5", linewidth = 1) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = min(.data[[target]], na.rm = TRUE),
                     ymax = .data[[target]]),
        fill = "#185FA5", alpha = 0.12
      )
    if (highlight_reference)
      p <- p + ggplot2::geom_point(data = grid[ref_row, , drop = FALSE],
                                   colour = "#D85A30", size = 3, shape = 18)
    p + ggplot2::labs(
      title = glue::glue("Sensitivity: {target_label} vs {params[1]}"),
      x = params[1], y = target_label
    ) + ggplot2::theme_minimal(base_size = 13)

  } else if (length(params) == 2) {
    p <- ggplot2::ggplot(
      grid, ggplot2::aes(x = .data[[params[1]]], y = .data[[params[2]]],
                         fill = .data[[target]])
    ) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradientn(
        colours = c("#042C53", "#185FA5", "#9FE1CB", "#1D9E75"),
        name    = target_label)
    if (highlight_reference)
      p <- p + ggplot2::geom_point(
        data = grid[ref_row, , drop = FALSE],
        colour = "#D85A30", size = 4, shape = 23, fill = "#D85A30")
    p + ggplot2::labs(
      title = glue::glue("Sensitivity heatmap: {target_label}"),
      x = params[1], y = params[2]
    ) + ggplot2::theme_minimal(base_size = 13)

  } else {
    rlang::abort("plot_sensitivity supports 1 or 2 varied parameters.")
  }
}


#' Tornado plot of prior influence on posterior quantities
#'
#' @param sensitivity A \code{bayprior_sensitivity} object.
#' @param title Character. Plot title.
#' @return A \code{ggplot} object.
#' @export
plot_tornado <- function(sensitivity,
                         title = "Prior influence on posterior estimates") {

  scores <- sensitivity$influence_scores
  grid   <- sensitivity$grid

  df <- purrr::imap_dfr(scores, function(range_val, nm) {
    vals <- grid[[nm]]
    data.frame(
      target    = .target_label(nm),   # human-readable label, e.g. "Posterior mean"
      ref_value = grid[[nm]][sensitivity$reference_row],
      lower     = min(vals, na.rm = TRUE),
      upper     = max(vals, na.rm = TRUE),
      influence = range_val
    )
  })
  df        <- df[order(df$influence, decreasing = TRUE), ]
  df$target <- factor(df$target, levels = rev(df$target))

  ggplot2::ggplot(df, ggplot2::aes(y = target)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = lower, xend = upper, yend = target),
      linewidth = 5, colour = "#B5D4F4", lineend = "round"
    ) +
    ggplot2::geom_point(ggplot2::aes(x = ref_value),
                        colour = "#185FA5", size = 3) +
    ggplot2::labs(
      title    = title,
      subtitle = paste(strwrap(
        "Bar width = range across sensitivity grid. Blue dot = reference prior.",
        width = 50
      ), collapse = "\n"),
      x        = "Posterior estimate range",
      y        = NULL
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}


#' @method plot bayprior
#' @export
plot.bayprior <- function(x, ...) {

  grid    <- .density_grid(x, 500)$x
  dens    <- .eval_density_vec(x, grid)
  ci_low  <- x$fit_summary$q025 %||% (x$fit_summary$mean - 2 * x$fit_summary$sd)
  ci_high <- x$fit_summary$q975 %||% (x$fit_summary$mean + 2 * x$fit_summary$sd)

  df    <- data.frame(theta = grid, density = dens)
  df_ci <- df[df$theta >= ci_low & df$theta <= ci_high, ]

  ggplot2::ggplot(df, ggplot2::aes(x = theta, y = density)) +
    ggplot2::geom_area(data = df_ci, fill = "#B5D4F4", alpha = 0.5) +
    ggplot2::geom_line(colour = "#185FA5", linewidth = 1) +
    ggplot2::geom_vline(xintercept = x$fit_summary$mean,
                        linetype = "dashed", colour = "#185FA5") +
    ggplot2::labs(
      title    = glue::glue("Prior distribution: {x$label}"),
      subtitle = glue::glue(
        "{toupper(x$dist)} | ",
        "Mean = {round(x$fit_summary$mean, 3)}, ",
        "SD = {round(x$fit_summary$sd, 3)}"
      ),
      x       = x$label,
      y       = "Density",
      caption = "Shaded region: 95% credible interval"
    ) +
    ggplot2::theme_minimal(base_size = 13)
}


#' @export
plot.bayprior_conflict <- function(x, ...) {
  # Previously hardcoded to Beta: stats::dbeta(grid, p$alpha, p$beta) would
  # error for any other family (p$alpha/p$beta are NULL for Normal, Gamma,
  # Lognormal, Exponential, Weibull priors), and the grid range was clamped
  # to (0, 1) regardless of family, which is wrong even for a Normal prior
  # with negative support (e.g. a log-odds-ratio prior). Uses the same
  # family-aware helpers as the rest of the package's plotting instead.
  # The grid must span both the prior's range and the likelihood's range --
  # not just the prior's. This plot exists specifically to visualise
  # prior-data conflict; using only the prior's range meant that in a
  # genuinely severe conflict (likelihood far from the prior), the
  # likelihood curve could fall partly or entirely outside the plotted
  # range and be silently clipped by ggplot2 -- hiding the exact scenario
  # this plot is meant to surface.
  range_p  <- .prior_range(x$prior)
  lik_lo   <- x$obs_mean - 4 * x$obs_se
  lik_hi   <- x$obs_mean + 4 * x$obs_se
  grid_lo  <- min(range_p$lo, lik_lo)
  grid_hi  <- max(range_p$hi, lik_hi)
  grid     <- seq(grid_lo, grid_hi, length.out = 500)

  pri <- .eval_density_vec(x$prior, grid)

  lik <- stats::dnorm(grid, mean = x$obs_mean, sd = x$obs_se)
  lik <- lik / max(lik) * max(pri)

  df <- data.frame(
    theta   = rep(grid, 2),
    density = c(pri, lik),
    curve   = rep(c("Prior", "Likelihood"), each = length(grid))
  )

  severity <- toupper(x$conflict_severity)
  col_map  <- c("Prior" = "steelblue", "Likelihood" = "firebrick")

  p <- ggplot2::ggplot(df, ggplot2::aes(x = theta, y = density,
                                         colour = curve)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::scale_colour_manual(values = col_map) +
    ggplot2::labs(
      title    = paste0("Prior-Data Conflict: ", x$prior$label),
      subtitle = paste0("Box p = ", round(x$box_pvalue, 3),
                        " | Severity: ", severity),
      x = "theta", y = "Density", colour = NULL
    ) +
    ggplot2::theme_minimal()

  print(p)
  invisible(p)
}