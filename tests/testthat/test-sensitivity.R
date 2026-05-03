test_that("sensitivity_grid returns correct structure", {
  prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  sa <- sensitivity_grid(
    prior        = prior,
    data_summary = list(type = "binary", x = 14, n = 40),
    param_grid   = list(alpha = seq(1, 4, 1), beta = seq(2, 8, 2)),
    target       = c("posterior_mean", "prob_efficacy"),
    threshold    = 0.30
  )

  expect_s3_class(sa, "bayprior_sensitivity")
  expect_true(is.data.frame(sa$grid))
  expect_true(is.numeric(sa$influence_scores))
  expect_named(sa$influence_scores,
               c("posterior_mean", "prob_efficacy"),
               ignore.order = TRUE)

  # Influence scores are non-negative
  expect_true(all(sa$influence_scores >= 0))

  # Grid has rows for all combinations
  expect_equal(nrow(sa$grid), 4 * 4)

  # Posterior means are in (0, 1)
  expect_true(all(sa$grid$posterior_mean > 0 & sa$grid$posterior_mean < 1))
})

test_that("sensitivity_grid works for normal prior", {
  prior <- elicit_normal(mean = 0.0, sd = 0.3, method = "moments")
  sa <- sensitivity_grid(
    prior        = prior,
    data_summary = list(type = "continuous", x = 0.4, sd = 0.3, n = 60),
    param_grid   = list(mu = c(-0.5, 0, 0.5), sigma = c(0.1, 0.3, 0.5)),
    target       = c("posterior_mean", "posterior_sd")
  )
  expect_s3_class(sa, "bayprior_sensitivity")
  expect_equal(nrow(sa$grid), 9)
})

test_that("sensitivity_cri returns correct structure", {
  prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  cri <- sensitivity_cri(
    prior        = prior,
    data_summary = list(type = "binary", x = 14, n = 40),
    param_grid   = list(alpha = seq(1, 4, 1), beta = seq(2, 8, 2)),
    cri_level    = 0.95,
    threshold    = 0.30
  )

  expect_s3_class(cri, "bayprior_sensitivity")
  expect_true("cri_width" %in% names(cri$influence_scores))
  expect_true("cri_lower" %in% colnames(cri$grid))
  expect_true("cri_upper" %in% colnames(cri$grid))

  # CrI widths are positive
  expect_true(all(cri$grid$cri_width > 0))
  # Lower < upper for all rows
  expect_true(all(cri$grid$cri_lower < cri$grid$cri_upper))
})

test_that("plot_tornado returns a ggplot", {
  prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  sa <- sensitivity_grid(
    prior        = prior,
    data_summary = list(type = "binary", x = 14, n = 40),
    param_grid   = list(alpha = seq(1, 4, 1), beta = seq(2, 8, 2)),
    target       = c("posterior_mean", "prob_efficacy"),
    threshold    = 0.30
  )
  gp <- plot_tornado(sa)
  expect_s3_class(gp, "gg")
})

test_that("plot_sensitivity returns a ggplot", {
  prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  sa <- sensitivity_grid(
    prior        = prior,
    data_summary = list(type = "binary", x = 14, n = 40),
    param_grid   = list(alpha = seq(1, 4, 1), beta = seq(2, 8, 2)),
    target       = "posterior_mean",
    threshold    = 0.30
  )
  gp <- plot_sensitivity(sa, target = "posterior_mean")
  expect_s3_class(gp, "gg")
})

test_that("sensitivity_grid errors on bad prior", {
  expect_error(
    sensitivity_grid(
      prior        = list(not = "a bayprior"),
      data_summary = list(type = "binary", x = 14, n = 40),
      param_grid   = list(alpha = 1:3, beta = 2:4)
    )
  )
})

test_that("reference row is within the grid", {
  prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  sa <- sensitivity_grid(
    prior        = prior,
    data_summary = list(type = "binary", x = 14, n = 40),
    param_grid   = list(alpha = seq(1, 6, 1), beta = seq(2, 12, 2))
  )
  expect_gte(sa$reference_row, 1L)
  expect_lte(sa$reference_row, nrow(sa$grid))
})


# ── Additional coverage ──────────────────────────────────────────────────────

test_that("print.bayprior_sensitivity does not error", {
  prior <- elicit_beta(mean=0.30, sd=0.10, method="moments")
  sa <- sensitivity_grid(
    prior, list(type="binary", x=14, n=40),
    list(alpha=1:3, beta=2:4), target="posterior_mean"
  )
  expect_error(print(sa), NA)
})

test_that("plot_sensitivity works with prob_efficacy target", {
  prior <- elicit_beta(mean=0.30, sd=0.10, method="moments")
  sa <- sensitivity_grid(
    prior, list(type="binary", x=14, n=40),
    list(alpha=1:3, beta=2:4),
    target="prob_efficacy", threshold=0.30
  )
  gp <- plot_sensitivity(sa, target="prob_efficacy")
  expect_s3_class(gp, "gg")
})

test_that("sensitivity_cri plot works", {
  prior <- elicit_beta(mean=0.30, sd=0.10, method="moments")
  cri <- sensitivity_cri(
    prior, list(type="binary", x=14, n=40),
    list(alpha=1:3, beta=2:4), cri_level=0.95
  )
  gp <- plot_sensitivity(cri, target="cri_width")
  expect_s3_class(gp, "gg")
})

# ── Conflict sensitivity edge cases ──────────────────────────────────────────

test_that("sensitivity_grid with posterior_sd target works", {
  prior <- elicit_beta(mean=0.30, sd=0.10, method="moments")
  sa <- sensitivity_grid(
    prior, list(type="binary", x=14, n=40),
    list(alpha=1:3, beta=2:4),
    target = "posterior_sd"
  )
  expect_true("posterior_sd" %in% names(sa$influence_scores))
  expect_true(all(sa$grid$posterior_sd > 0))
})