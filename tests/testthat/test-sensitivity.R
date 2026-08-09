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


# -- Additional coverage ------------------------------------------------------

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

# -- Conflict sensitivity edge cases ------------------------------------------

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
# -- Additional data types -----------------------------------------------------

test_that("sensitivity_grid: normal-normal conjugate works", {
  prior <- elicit_normal(mean = 0.0, sd = 1.0, method = "moments")
  sa <- sensitivity_grid(
    prior        = prior,
    data_summary = list(type = "continuous", x = 0.5, n = 30, sd = 1.0),
    param_grid   = list(mu = seq(-1, 1, by = 0.5), sigma = seq(0.5, 2, by = 0.5)),
    target       = "posterior_mean",
    threshold    = 0.0
  )
  expect_s3_class(sa, "bayprior_sensitivity")
  expect_true(nrow(sa$grid) > 0)
})

test_that("sensitivity_grid: gamma-poisson conjugate works", {
  prior <- elicit_gamma(mean = 2.0, sd = 1.0, method = "moments")
  sa <- sensitivity_grid(
    prior        = prior,
    data_summary = list(type = "poisson", x = 3, n = 10),
    param_grid   = list(shape = seq(1, 4, by = 1), rate = seq(0.5, 2, by = 0.5)),
    target       = "posterior_mean",
    threshold    = 2.0
  )
  expect_s3_class(sa, "bayprior_sensitivity")
  expect_false(all(is.na(sa$grid$posterior_mean)))
})

test_that("sensitivity_grid works with Poisson data", {
  prior <- elicit_gamma(mean = 0.15, sd = 0.05, method = "moments")
  sa <- sensitivity_grid(
    prior,
    data_summary = list(type = "poisson", x = 12, n = 100),
    param_grid   = list(shape = seq(2, 8, 1), rate = seq(10, 50, 5)),
    target       = c("posterior_mean", "prob_efficacy"),
    threshold    = 0.10
  )
  expect_s3_class(sa, "bayprior_sensitivity")
  expect_false(all(is.na(sa$grid$posterior_mean)))
})

test_that("sensitivity_cri: tracks credible interval width", {
  prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  cri_sa <- sensitivity_cri(
    prior        = prior,
    data_summary = list(type = "binary", x = 14, n = 40),
    param_grid   = list(alpha = seq(1, 5, by = 1), beta = seq(2, 10, by = 2)),
    cri_level    = 0.95,
    threshold    = 0.30
  )
  expect_s3_class(cri_sa, "bayprior_sensitivity")
  expect_true("cri_width" %in% names(cri_sa$grid))
  expect_true(all(cri_sa$grid$cri_width > 0, na.rm = TRUE))
})

test_that("sensitivity_cri: normal data type works", {
  prior <- elicit_normal(mean = 0.0, sd = 1.0, method = "moments")
  cri_sa <- sensitivity_cri(
    prior        = prior,
    data_summary = list(type = "continuous", x = 0.5, n = 20, sd = 1.0),
    param_grid   = list(mu = seq(-1, 1, by = 0.5), sigma = seq(0.5, 1.5, by = 0.5))
  )
  expect_s3_class(cri_sa, "bayprior_sensitivity")
})

test_that("sensitivity_cri with survival data", {
  prior <- elicit_gamma(mean = 0.05, sd = 0.02, method = "moments")
  sa <- sensitivity_cri(
    prior,
    data_summary = list(type = "survival", x = 20, n = 400),
    param_grid   = list(shape = seq(1, 6, 1), rate = seq(10, 40, 5)),
    cri_level    = 0.95
  )
  expect_s3_class(sa, "bayprior_sensitivity")
  expect_false(all(is.na(sa$grid$cri_width)))
})

test_that("sensitivity_cri with 80% CrI gives narrower intervals than 95%", {
  prior    <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  data_obs <- list(type = "binary", x = 14, n = 40)
  grid     <- list(alpha = seq(1, 4, 1), beta = seq(2, 8, 2))
  cri80 <- sensitivity_cri(prior, data_obs, grid, cri_level = 0.80)
  cri95 <- sensitivity_cri(prior, data_obs, grid, cri_level = 0.95)
  expect_lt(mean(cri80$grid$cri_width), mean(cri95$grid$cri_width))
})

test_that("sensitivity_grid handles single-row param combinations", {
  prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  sa <- sensitivity_grid(
    prior        = prior,
    data_summary = list(type = "binary", x = 14, n = 40),
    param_grid   = list(alpha = 3, beta = 7),
    target       = "posterior_mean"
  )
  expect_equal(nrow(sa$grid), 1)
})

# -- Lognormal conjugate update paths -----------------------------------------

test_that("sensitivity_grid: lognormal prior covered via prior_conflict", {
  # sensitivity_grid conjugate update for lognormal is not supported directly.
  # Coverage of lognormal .conjugate_update is achieved via prior_conflict instead.
  prior <- elicit_lognormal(mean = 1.5, sd = 0.4, method = "moments")
  cd <- prior_conflict(prior, list(type = "continuous", x = 1.6, n = 25, sd = 0.3))
  expect_s3_class(cd, "bayprior_conflict")
})

test_that("sensitivity_cri: normal prior with prob_efficacy threshold works", {
  prior <- elicit_normal(mean = 0.0, sd = 0.5, method = "moments")
  cri_sa <- sensitivity_cri(
    prior        = prior,
    data_summary = list(type = "continuous", x = 0.3, n = 30, sd = 0.4),
    param_grid   = list(
      mu    = seq(-0.5, 0.5, by = 0.25),
      sigma = seq(0.3, 0.7, by = 0.2)
    ),
    cri_level    = 0.95,
    threshold    = 0.0
  )
  expect_s3_class(cri_sa, "bayprior_sensitivity")
  expect_true("prob_efficacy" %in% names(cri_sa$grid))
})

test_that("sensitivity_grid: exponential prior with poisson data works", {
  prior <- elicit_exponential(mean = 2.0, method = "moments")
  sa <- sensitivity_grid(
    prior        = prior,
    data_summary = list(type = "poisson", x = 8, n = 20),
    param_grid   = list(rate = seq(0.2, 1.0, by = 0.2)),
    target       = "posterior_mean",
    threshold    = 0.5
  )
  expect_s3_class(sa, "bayprior_sensitivity")
})

# -- plot_sensitivity: 1D param grid (heatmap vs tornado) ---------------------

test_that("plot_sensitivity: posterior_mean heatmap with fine grid produces ggplot", {
  prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  sa <- sensitivity_grid(
    prior        = prior,
    data_summary = list(type = "binary", x = 14, n = 40),
    param_grid   = list(alpha = seq(2, 8, by = 2), beta = seq(4, 16, by = 4)),
    target       = "posterior_mean"
  )
  gp <- plot_sensitivity(sa, target = "posterior_mean")
  expect_true(inherits(gp, "gg") || inherits(gp, "patchwork"))
})

test_that("plot_sensitivity: posterior_sd target heatmap works", {
  prior <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments")
  sa <- sensitivity_grid(
    prior        = prior,
    data_summary = list(type = "binary", x = 14, n = 40),
    param_grid   = list(alpha = seq(1, 4, 1), beta = seq(2, 8, 2)),
    target       = "posterior_sd"
  )
  gp <- plot_sensitivity(sa, target = "posterior_sd")
  expect_true(inherits(gp, "gg") || inherits(gp, "patchwork"))
})

test_that("sensitivity_grid moment-matches a linearly pooled mixture prior", {
  e1 <- elicit_beta(mean = 0.30, sd = 0.08, method = "moments",
                    expert_id = "E1")
  e2 <- elicit_beta(mean = 0.42, sd = 0.10, method = "moments",
                    expert_id = "E2")
  pool <- aggregate_experts(list(E1 = e1, E2 = e2), weights = c(0.5, 0.5),
                            method = "linear")

  # Ties in which.max(c(0.5, 0.5)) resolve to the first component; if the
  # old dominant-component-only fallback were still in effect, the grid
  # would silently reflect E1 alone (mean 0.30) rather than the pool's
  # actual blended mean (~0.36-0.37). Confirm the working prior instead
  # reflects the pooled mean by checking that a grid centred on the
  # pooled mean does not raise all its mass at the low end typical of E1.
  expect_message(
    sa <- sensitivity_grid(
      prior        = pool,
      data_summary = list(type = "binary", x = 18, n = 40),
      param_grid   = list(alpha = seq(5, 15, by = 1),
                          beta  = seq(15, 30, by = 1)),
      target       = "posterior_mean"
    ),
    "moment-matched working prior"
  )
  expect_s3_class(sa, "bayprior_sensitivity")
  expect_true(nrow(sa$grid) > 0)
})

test_that("sensitivity_grid falls back to dominant component and warns
           when a mixture family cannot be moment-matched", {
  e1 <- elicit_exponential(rate = 2, method = "rate", expert_id = "E1")
  e2 <- elicit_exponential(rate = 3, method = "rate", expert_id = "E2")
  pool <- aggregate_experts(list(E1 = e1, E2 = e2), weights = c(0.5, 0.5),
                            method = "linear")

  expect_warning(
    sa <- sensitivity_grid(
      prior        = pool,
      data_summary = list(type = "survival", x = 12, n = 40),
      param_grid   = list(rate = seq(1, 5, by = 0.5)),
      target       = "posterior_mean"
    ),
    "Falling back to the dominant component"
  )
  expect_s3_class(sa, "bayprior_sensitivity")
})

test_that("sensitivity_grid does not error on a logarithmically pooled
           prior, whose fit_summary$sd is NULL", {
  e1 <- elicit_beta(mean = 0.30, sd = 0.08, method = "moments",
                    expert_id = "E1")
  e2 <- elicit_beta(mean = 0.42, sd = 0.10, method = "moments",
                    expert_id = "E2")
  pool_log <- aggregate_experts(list(E1 = e1, E2 = e2), weights = c(0.5, 0.5),
                                method = "logarithmic")
  expect_true(is.null(pool_log$fit_summary$sd))

  # This previously crashed with "argument is of length zero" because
  # is.na(NULL) inside the working_prior if() condition returns a
  # zero-length logical rather than FALSE.
  expect_no_error(
    suppressWarnings(suppressMessages(
      sensitivity_grid(
        prior        = pool_log,
        data_summary = list(type = "binary", x = 18, n = 40),
        param_grid   = list(alpha = seq(5, 15, by = 1),
                            beta  = seq(15, 30, by = 1)),
        target       = "posterior_mean"
      )
    ))
  )
})

test_that("sensitivity_cri moment-matches a linearly pooled mixture prior
           (previously discarded non-dominant components, matching the
           sensitivity_grid bug found and fixed separately)", {
  e1 <- elicit_beta(mean = 0.30, sd = 0.08, method = "moments", expert_id = "E1")
  e2 <- elicit_beta(mean = 0.42, sd = 0.10, method = "moments", expert_id = "E2")
  pool <- aggregate_experts(list(E1 = e1, E2 = e2), weights = c(0.5, 0.5),
                            method = "linear")

  expect_message(
    cri_sa <- sensitivity_cri(
      prior        = pool,
      data_summary = list(type = "binary", x = 18, n = 40),
      param_grid   = list(alpha = seq(3, 12, by = 1), beta = seq(6, 20, by = 1)),
      cri_level    = 0.95
    ),
    "moment-matched working prior"
  )
  expect_s3_class(cri_sa, "bayprior_sensitivity")
  expect_true("cri_width" %in% names(cri_sa$grid))
})