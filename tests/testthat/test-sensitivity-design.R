beta_prior <- function() elicit_beta(mean = 0.36, sd = 0.10, method = "moments",
                                      label = "Response rate")

test_that("strength grid holds the mean fixed and scales effective sample size", {
  p <- beta_prior()
  S <- p$params$alpha + p$params$beta
  g <- .strength_grid(p, scale = c(0.5, 2), n_scale = 9, shift_sd = 0)

  expect_s3_class(g, "data.frame")
  expect_true(all(c("alpha", "beta", "prior_mean", "prior_strength") %in% names(g)))
  expect_equal(unique(round(g$prior_mean, 10)), p$fit_summary$mean, tolerance = 1e-8)
  expect_equal(range(g$prior_strength), S * c(0.5, 2), tolerance = 1e-8)
  expect_equal(g$alpha + g$beta, g$prior_strength, tolerance = 1e-10)
  expect_equal(g$alpha / (g$alpha + g$beta), g$prior_mean, tolerance = 1e-10)
  # the elicited prior is one of the grid points
  expect_true(any(abs(g$alpha - p$params$alpha) < 1e-9 &
                  abs(g$beta - p$params$beta) < 1e-9))
  expect_true(isTRUE(attr(g, "design")$mean_fixed))
})

test_that("strength grid shifts the mean symmetrically in prior SDs", {
  p  <- beta_prior()
  sd <- p$fit_summary$sd
  m  <- p$fit_summary$mean
  g  <- .strength_grid(p, scale = c(0.5, 2), n_scale = 5, shift_sd = 1, n_shift = 5)

  expect_equal(range(g$prior_mean), m + c(-1, 1) * sd, tolerance = 1e-6)
  expect_true(any(abs(g$prior_mean - m) < 1e-12))
  expect_equal(length(unique(round(g$prior_mean, 10))), 5L)
  expect_false(attr(g, "design")$mean_fixed)
})

test_that("mean shift is limited to the support of the family", {
  p <- elicit_beta(mean = 0.05, sd = 0.04, method = "moments", label = "Rare")
  g <- .strength_grid(p, shift_sd = 3)
  expect_gte(min(g$prior_mean), 0.01 - 1e-12)
  expect_lt(attr(g, "design")$shift_sd, 3)
  expect_equal(attr(g, "design")$shift_sd_asked, 3)
  expect_true(all(g$alpha > 0 & g$beta > 0))
})

test_that("gamma and normal priors are supported; others fall back", {
  pg <- elicit_gamma(mean = 2, sd = 0.8, method = "moments", label = "Rate")
  gg <- .strength_grid(pg, shift_sd = 1)
  expect_equal(gg$shape / gg$rate, gg$prior_mean, tolerance = 1e-10)
  expect_equal(gg$rate, gg$prior_strength)
  expect_gt(min(gg$prior_mean), 0)

  pn <- elicit_normal(mean = -0.2, sd = 0.5, method = "moments", label = "Effect")
  gn <- .strength_grid(pn, scale = c(0.5, 2), shift_sd = 0)
  expect_equal(unique(gn$mu), -0.2)
  expect_equal(1 / gn$sigma^2, gn$prior_strength, tolerance = 1e-10)
  expect_equal(range(gn$prior_strength) / (1 / 0.5^2), c(0.5, 2), tolerance = 1e-8)

  px <- structure(list(dist = "exponential", params = list(rate = 1)), class = "bayprior")
  expect_false(.design_supported(px))
  expect_error(.strength_grid(px), "beta, gamma and normal")
})

test_that("sensitivity_grid accepts a data-frame grid and matches the list form", {
  p  <- beta_prior()
  ds <- list(type = "binary", x = 18, n = 40)

  lst <- list(alpha = c(2, 4, 6), beta = c(3, 6, 9))
  a <- suppressMessages(sensitivity_grid(p, ds, lst))
  b <- suppressMessages(sensitivity_grid(p, ds, expand.grid(lst)))
  expect_equal(a$grid, b$grid)
  expect_equal(a$influence_scores, b$influence_scores)
  expect_equal(a$reference_row, b$reference_row)

  g <- .strength_grid(p, shift_sd = 1)
  s <- suppressMessages(sensitivity_grid(p, ds, g))
  expect_s3_class(s, "bayprior_sensitivity")
  expect_equal(nrow(s$grid), nrow(g))
  expect_true(all(c("prior_mean", "prior_strength") %in% names(s$grid)))
  expect_false(anyNA(s$grid$posterior_mean))

  # the reference row is the elicited prior and its posterior is the conjugate one
  r <- s$grid[s$reference_row, ]
  expect_equal(r$alpha, p$params$alpha, tolerance = 1e-9)
  expect_equal(r$posterior_mean,
               (p$params$alpha + 18) / (p$params$alpha + p$params$beta + 40),
               tolerance = 1e-9)
})

test_that("a narrower strength range gives a smaller influence score", {
  p  <- beta_prior()
  ds <- list(type = "binary", x = 18, n = 40)
  wide   <- suppressMessages(sensitivity_grid(p, ds, .strength_grid(p, c(0.5, 2)),
                                              target = "posterior_mean"))
  narrow <- suppressMessages(sensitivity_grid(p, ds, .strength_grid(p, c(0.8, 1.25)),
                                              target = "posterior_mean"))
  expect_lt(narrow$influence_scores[["posterior_mean"]],
            wide$influence_scores[["posterior_mean"]])
})

test_that("sensitivity_cri accepts a data-frame grid", {
  p  <- beta_prior()
  ds <- list(type = "binary", x = 18, n = 40)
  s  <- suppressMessages(sensitivity_cri(p, ds, .strength_grid(p, shift_sd = 0.5)))
  expect_true(all(c("cri_lower", "cri_upper", "cri_width", "prior_mean") %in% names(s$grid)))
  expect_false(anyNA(s$grid$cri_width))
})

test_that("grid note reports the implied prior mean and strength", {
  p  <- beta_prior()
  S  <- p$params$alpha + p$params$beta
  g  <- .strength_grid(p, scale = c(0.5, 2), shift_sd = 0)
  txt <- .grid_note_text(p, g, attr(g, "design"))
  expect_match(txt, "held fixed", fixed = TRUE)
  expect_match(txt, paste0(.fmt_num(S * 0.5), " to ", .fmt_num(S * 2)), fixed = TRUE)

  # a raw hyperparameter grid: the implied ranges come from its corners
  a <- p$params$alpha; b <- p$params$beta
  h <- expand.grid(alpha = c(0.5 * a, 2 * a), beta = c(0.5 * b, 2 * b))
  imp <- .grid_implied(p, h)
  expect_equal(imp$mean, c(0.5 * a / (0.5 * a + 2 * b), 2 * a / (2 * a + 0.5 * b)))
  expect_equal(imp$strength, c(0.5 * (a + b), 2 * (a + b)))
  expect_match(.grid_note_text(p, h), "prior means", fixed = TRUE)

  # an object returned by sensitivity_grid()
  s <- suppressMessages(sensitivity_grid(p, list(type = "binary", x = 18, n = 40), g))
  expect_match(.sensitivity_grid_note(s), "held fixed", fixed = TRUE)
})

test_that("plots use prior mean and strength as axes for a strength grid", {
  skip_if_not_installed("ggplot2")
  p  <- beta_prior()
  ds <- list(type = "binary", x = 18, n = 40)

  fixed <- suppressMessages(sensitivity_grid(p, ds, .strength_grid(p, shift_sd = 0)))
  g1 <- plot_sensitivity(fixed, target = "posterior_mean")
  expect_s3_class(g1, "ggplot")
  expect_match(g1$labels$x, "effective sample size")

  both <- suppressMessages(sensitivity_grid(p, ds, .strength_grid(p, shift_sd = 1)))
  g2 <- plot_sensitivity(both, target = "posterior_mean")
  expect_equal(unname(c(g2$labels$x, g2$labels$y)),
               c("Prior mean", "Prior effective sample size"))
  expect_s3_class(plot_tornado(both), "ggplot")
})

test_that("exported script keeps a data-frame grid as a data frame", {
  p  <- beta_prior()
  ds <- list(type = "binary", x = 18, n = 40)
  g  <- .strength_grid(p, scale = c(0.5, 2), n_scale = 3, shift_sd = 1, n_shift = 3)
  s  <- suppressMessages(sensitivity_grid(p, ds, g))
  attr(s, "bp_args") <- list(data_summary = ds)
  res <- .bp_export_script(p, NULL, s)
  expect_true(res$ok)
  expect_match(res$text, "param_grid <- data.frame(", fixed = TRUE)

  # running the script reproduces the recorded grid
  txt <- sub("library(bayprior)", "", res$text, fixed = TRUE)
  env <- new.env(parent = environment(.bp_export_script))
  grDevices::pdf(NULL); on.exit(grDevices::dev.off())
  suppressMessages(suppressWarnings(eval(parse(text = txt), env)))
  expect_equal(env$sensitivity$grid, s$grid, tolerance = 1e-10)
})

test_that("sensitivity module uses a strength grid by default and the old grid on request", {
  skip_if_not_installed("shiny")
  p <- beta_prior()
  shared <- shiny::reactiveValues(sensitivity = NULL, conflict = NULL)
  store <- new.env()
  run <- function(...) {
    shiny::testServer(mod_sensitivity_server,
      args = list(shared = shared, active_prior = shiny::reactive(p)), {
        session$setInputs(grid_size = 5, analysis_type = "grid", threshold = 0.3,
                          targets = c("posterior_mean", "prob_efficacy"),
                          sens_data_type = "binary", sens_x = 18, sens_n = 40, ...)
        note <- output$grid_note
        session$setInputs(run_btn = 1)
        res <- shiny::isolate(shared$sensitivity)
        store$out <- list(res = res, note = as.character(note$html))
      })
  }

  run(grid_design = "strength", strength_range = c(0.5, 2),
      vary_mean = TRUE, shift_sd = 1)
  out <- store$out
  expect_s3_class(out$res, "bayprior_sensitivity")
  expect_true(is.data.frame(out$res$param_grid))
  expect_equal(attr(out$res$param_grid, "design")$type, "strength")
  expect_false(attr(out$res$param_grid, "design")$mean_fixed)
  expect_match(out$note, "prior means")

  run(grid_design = "strength", strength_range = c(0.5, 2), vary_mean = FALSE)
  out <- store$out
  expect_true(attr(out$res$param_grid, "design")$mean_fixed)
  expect_equal(length(unique(round(out$res$grid$prior_mean, 10))), 1L)

  a <- p$params$alpha; b <- p$params$beta
  run(grid_design = "hyper", p1_range = c(0.5 * a, 2 * a), p2_range = c(0.5 * b, 2 * b))
  out <- store$out
  expect_true(is.list(out$res$param_grid) && !is.data.frame(out$res$param_grid))
  expect_equal(names(out$res$param_grid), c("alpha", "beta"))
  expect_match(out$note, "prior means")
})