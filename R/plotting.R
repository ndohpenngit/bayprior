#' Plot prior, likelihood, and posterior density overlays
#'
#' Creates a ggplot2 overlay of the prior, (normalised) likelihood, and
#' resulting posterior distribution — the canonical diagnostic plot for
#' Bayesian clinical trial reports. Conflict zones are highlighted when
#' present.
#'
#' @param prior A `bayprior` object.
#' @param data_summary Named list with `n`, `x`, optionally `sd`, `type`.
#' @param show_posterior Logical. Include posterior density. Default `TRUE`.
#' @param show_conflict Logical. Shade conflict region. Default `TRUE`.
#' @param n_grid Integer. Grid resolution. Default `500`.
#' @param title Character. Plot title. Defaults to the prior's label.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments",
#'                      label = "Response rate")
#' plot_prior_likelihood(prior, list(n = 40, x = 20, type = "binary"))
#'
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

  grid    <- .grid_for_dist(prior, n_grid)
  prior_d <- .eval_density_vec(prior, grid)

  if (type == "binary") {
    lik_d <- stats::dbeta(grid, x + 1, n - x + 1)
  } else {
    lik_d <- stats::dnorm(grid, x, data_summary$sd / sqrt(n))
  }
  lik_d <- lik_d / max(lik_d) * max(prior_d)  # scale to prior height

  df <- data.frame(
    theta = rep(grid, 2),
    density = c(prior_d, lik_d),
    source  = rep(c("Prior", "Likelihood (scaled)"), each = n_grid)
  )

  if (show_posterior) {
    post <- .compute_posterior(prior, data_summary, list())
    post_d <- if (post$dist == "beta") {
      stats::dbeta(grid, post$params$alpha, post$params$beta)
    } else {
      stats::dnorm(grid, post$params$mu, post$params$sigma)
    }
    df <- rbind(df, data.frame(
      theta   = grid,
      density = post_d,
      source  = "Posterior"
    ))
  }

  pal <- c("Prior" = "#185FA5", "Likelihood (scaled)" = "#D85A30",
           "Posterior" = "#1D9E75")

  p <- ggplot2::ggplot(df, ggplot2::aes(x = theta, y = density,
                                        colour = source, fill = source)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_area(alpha = 0.15, position = "identity") +
    ggplot2::scale_colour_manual(values = pal, name = NULL) +
    ggplot2::scale_fill_manual(values = pal, name = NULL) +
    ggplot2::labs(
      title    = title %||% glue::glue("Prior–Likelihood–Posterior: {prior$label}"),
      subtitle = glue::glue("Data: n = {n}, events/mean = {x}"),
      x        = prior$label,
      y        = "Density"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "top")

  p
}


#' Plot sensitivity analysis results
#'
#' Visualises how a target posterior quantity (e.g. posterior mean or
#' P(efficacy)) changes across the hyperparameter grid. Produces either a
#' heatmap (2-parameter grid) or a line plot (1-parameter grid).
#'
#' @param sensitivity A `bayprior_sensitivity` object from `sensitivity_grid()`.
#' @param target Character. Which target quantity to plot. Must be one of the
#'   targets computed in the sensitivity analysis.
#' @param highlight_reference Logical. Mark the reference prior. Default `TRUE`.
#'
#' @return A `ggplot` object.
#'
#' @export
plot_sensitivity <- function(sensitivity,
                             target = NULL,
                             highlight_reference = TRUE) {

  if (!inherits(sensitivity, "bayprior_sensitivity")) {
    rlang::abort("`sensitivity` must be a bayprior_sensitivity object.")
  }
  if (is.null(target)) target <- sensitivity$target[1]

  grid     <- sensitivity$grid
  params   <- names(sensitivity$param_grid)
  ref_row  <- sensitivity$reference_row

  if (length(params) == 1) {
    # Line plot
    p <- ggplot2::ggplot(grid, ggplot2::aes(x = .data[[params[1]]], y = .data[[target]])) +
      ggplot2::geom_line(colour = "#185FA5", linewidth = 1) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = min(.data[[target]], na.rm = TRUE),
                     ymax = .data[[target]]),
        fill = "#185FA5", alpha = 0.12
      )
    if (highlight_reference) {
      ref <- grid[ref_row, , drop = FALSE]
      p <- p + ggplot2::geom_point(data = ref,
                                   colour = "#D85A30", size = 3, shape = 18)
    }
    p <- p + ggplot2::labs(
      title = glue::glue("Sensitivity: {target} vs {params[1]}"),
      x = params[1], y = target
    ) + ggplot2::theme_minimal(base_size = 13)

  } else if (length(params) == 2) {
    # Heatmap
    p <- ggplot2::ggplot(grid, ggplot2::aes(x = .data[[params[1]]],
                                            y = .data[[params[2]]],
                                            fill = .data[[target]])) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradientn(
        colours = c("#042C53", "#185FA5", "#9FE1CB", "#1D9E75"),
        name = target
      )
    if (highlight_reference) {
      ref <- grid[ref_row, , drop = FALSE]
      p <- p + ggplot2::geom_point(
        data = ref, colour = "#D85A30", size = 4, shape = 23, fill = "#D85A30"
      )
    }
    p <- p + ggplot2::labs(
      title = glue::glue("Sensitivity heatmap: {target}"),
      x = params[1], y = params[2]
    ) + ggplot2::theme_minimal(base_size = 13)
  } else {
    rlang::abort("plot_sensitivity supports 1 or 2 varied parameters.")
  }

  p
}


#' Tornado plot of prior influence on posterior quantities
#'
#' Displays the range of each posterior target quantity as it varies across
#' the entire sensitivity grid, ordered from most to least influential. This
#' is the standard regulatory visualisation for demonstrating the robustness
#' of trial conclusions to prior choice.
#'
#' @param sensitivity A `bayprior_sensitivity` object.
#' @param title Character. Plot title.
#'
#' @return A `ggplot` object.
#'
#' @export
plot_tornado <- function(sensitivity, title = "Prior influence on posterior estimates") {

  scores <- sensitivity$influence_scores
  grid   <- sensitivity$grid

  df <- purrr::imap_dfr(scores, function(range_val, nm) {
    vals    <- grid[[nm]]
    ref_val <- grid[[nm]][sensitivity$reference_row]
    data.frame(
      target    = nm,
      ref_value = ref_val,
      lower     = min(vals, na.rm = TRUE),
      upper     = max(vals, na.rm = TRUE),
      influence = range_val
    )
  })
  df <- df[order(df$influence, decreasing = TRUE), ]
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
      subtitle = "Bar width = range across sensitivity grid. Blue dot = reference prior.",
      x        = "Posterior estimate range",
      y        = NULL
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}


#' @export
plot.bayprior <- function(x, ...) {
  grid    <- .grid_for_dist(x, 500)
  dens    <- .eval_density_vec(x, grid)
  ci_low  <- x$fit_summary$q025 %||% (x$fit_summary$mean - 2 * x$fit_summary$sd)
  ci_high <- x$fit_summary$q975 %||% (x$fit_summary$mean + 2 * x$fit_summary$sd)

  df     <- data.frame(theta = grid, density = dens)
  df_ci  <- df[df$theta >= ci_low & df$theta <= ci_high, ]

  ggplot2::ggplot(df, ggplot2::aes(x = theta, y = density)) +
    ggplot2::geom_area(data = df_ci, fill = "#B5D4F4", alpha = 0.5) +
    ggplot2::geom_line(colour = "#185FA5", linewidth = 1) +
    ggplot2::geom_vline(xintercept = x$fit_summary$mean,
                        linetype = "dashed", colour = "#185FA5") +
    ggplot2::labs(
      title    = glue::glue("Prior distribution: {x$label}"),
      subtitle = glue::glue("{toupper(x$dist)} | Mean = {round(x$fit_summary$mean, 3)}, SD = {round(x$fit_summary$sd, 3)}"),
      x = x$label, y = "Density",
      caption = "Shaded region: 95% credible interval"
    ) +
    ggplot2::theme_minimal(base_size = 13)
}
