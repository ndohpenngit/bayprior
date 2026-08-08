test_that(".target_label maps known targets", {
  expect_equal(.target_label("posterior_mean"), "Posterior mean")
  expect_equal(.target_label("posterior_sd"),   "Posterior SD")
  expect_equal(.target_label("cri_lower"),      "95% CrI lower bound")
  expect_equal(.target_label("cri_upper"),      "95% CrI upper bound")
  expect_equal(.target_label("cri_width"),      "95% CrI width")
})

test_that(".target_label returns input unchanged for unknown targets", {
  expect_equal(.target_label("unknown_target"), "unknown_target")
  expect_equal(.target_label("custom_metric"),  "custom_metric")
})

test_that(".relabel_sensitivity handles NULL input", {
  expect_null(.relabel_sensitivity(NULL))
})

# -- .target_label() -- the one missing TARGET_LABELS entry ------------------

test_that(".target_label maps prob_efficacy (the entry missing from prior coverage)", {
  expect_equal(.target_label("prob_efficacy"), "Pr(efficacy)")
})

# -- .relabel_sensitivity() -- non-NULL branches, previously untested --------

test_that(".relabel_sensitivity renames $target entries", {
  sa <- list(target = c("posterior_mean", "prob_efficacy"))
  out <- .relabel_sensitivity(sa)
  expect_equal(unname(out$target), c("Posterior mean", "Pr(efficacy)"))
})

test_that(".relabel_sensitivity renames $influence_scores rownames", {
  scores <- matrix(1:4, nrow = 2,
                   dimnames = list(c("posterior_mean", "cri_width"), c("a", "b")))
  sa <- list(influence_scores = scores)
  out <- .relabel_sensitivity(sa)
  expect_equal(unname(rownames(out$influence_scores)), c("Posterior mean", "95% CrI width"))
})

test_that(".relabel_sensitivity renames matching $grid columns and leaves others alone", {
  sa <- list(grid = data.frame(alpha = 1:2, posterior_mean = c(0.3, 0.4),
                               custom_col = c("x", "y")))
  out <- .relabel_sensitivity(sa)
  expect_true("Posterior mean" %in% names(out$grid))
  expect_true("alpha" %in% names(out$grid))       # untouched: not in TARGET_LABELS
  expect_true("custom_col" %in% names(out$grid))  # untouched: not in TARGET_LABELS
  expect_false("posterior_mean" %in% names(out$grid))  # renamed away
})

test_that(".relabel_sensitivity is a no-op when $target/$influence_scores/$grid are absent", {
  sa <- list(some_other_field = 42)
  out <- .relabel_sensitivity(sa)
  expect_equal(out, sa)
})

# -- .prior_summary_lognormal() -- zero prior coverage ------------------------

test_that(".prior_summary_lognormal computes correct mean/SD/quantiles", {
  # meanlog = 0, sdlog = 1 has well-known closed-form moments:
  # mean = exp(0 + 1/2) = exp(0.5); var = (exp(1)-1)*exp(1)
  r <- .prior_summary_lognormal(list(meanlog = 0, sdlog = 1))
  expect_equal(r$mean, exp(0.5), tolerance = 1e-8)
  expect_equal(r$sd,   sqrt((exp(1) - 1) * exp(1)), tolerance = 1e-8)
  expect_equal(r$q500, exp(0), tolerance = 1e-8)  # median of lognormal = exp(meanlog)
  expect_true(r$q025 < r$q500 && r$q500 < r$q975)
})

# -- .make_bayprior() -- zero direct/isolated tests ---------------------------

test_that(".make_bayprior routes lognormal through .prior_summary_lognormal", {
  obj <- .make_bayprior("lognormal", list(meanlog = 0, sdlog = 1), "moments",
                        "E1", "test quantity", list())
  expect_s3_class(obj, "bayprior")
  expect_equal(obj$dist, "lognormal")
  expect_equal(obj$fit_summary$mean, exp(0.5), tolerance = 1e-8)
})

test_that(".make_bayprior routes non-lognormal families through .prior_summary", {
  obj <- .make_bayprior("beta", list(alpha = 2, beta = 5), "moments",
                        "E1", "test quantity", list())
  expect_s3_class(obj, "bayprior")
  expect_equal(obj$dist, "beta")
  expect_true(is.numeric(obj$fit_summary$mean))
})

# -- .eval_density_vec() -- exponential/weibull/mixture/unknown, never plotted -
# vdiffr plot snapshots exercise beta, normal, gamma, lognormal, and linear
# mixture, but never exponential or weibull -- these branches are otherwise
# only exercised indirectly, if at all.

test_that(".eval_density_vec evaluates exponential and weibull correctly", {
  exp_prior <- elicit_exponential(rate = 2, method = "rate", expert_id = "E1")
  expect_equal(.eval_density_vec(exp_prior, 1), dexp(1, rate = 2))

  wei_prior <- elicit_weibull(mean = 20, sd = 10, method = "moments",
                              label = "Survival time (months)", expert_id = "E1")
  expect_equal(
    .eval_density_vec(wei_prior, 15),
    dweibull(15, shape = wei_prior$params$shape, scale = wei_prior$params$scale)
  )
})

test_that(".eval_density_vec recurses correctly through mixture components", {
  e1 <- elicit_beta(mean = 0.30, sd = 0.08, method = "moments", expert_id = "E1")
  e2 <- elicit_beta(mean = 0.42, sd = 0.10, method = "moments", expert_id = "E2")
  pool <- aggregate_experts(list(E1 = e1, E2 = e2), weights = c(0.5, 0.5),
                            method = "linear")
  expected <- 0.5 * .eval_density_vec(e1, 0.35) + 0.5 * .eval_density_vec(e2, 0.35)
  expect_equal(.eval_density_vec(pool, 0.35), expected)
})

test_that(".eval_density_vec returns NA for an unrecognised distribution", {
  fake <- structure(list(dist = "not_a_real_family", params = list()),
                    class = "bayprior")
  expect_true(all(is.na(.eval_density_vec(fake, c(1, 2, 3)))))
})

# -- .density_grid() -- exponential/weibull and the mixture error path --------

test_that(".density_grid produces a valid grid for exponential and weibull", {
  exp_prior <- elicit_exponential(rate = 2, method = "rate", expert_id = "E1")
  g <- .density_grid(exp_prior)
  expect_equal(g$x[1], 1e-6)          # lo clamped to 1e-6 for exponential
  expect_true(all(is.finite(g$y)))

  wei_prior <- elicit_weibull(mean = 20, sd = 10, method = "moments",
                              label = "Survival time (months)", expert_id = "E1")
  g2 <- .density_grid(wei_prior)
  expect_equal(g2$x[1], 1e-6)
  expect_true(all(is.finite(g2$y)))
})

test_that(".density_grid aborts when every mixture component has an unusable fit_summary", {
  # Directly construct components with NULL fit_summary fields so every
  # q025/q975/mean/sd lookup in the mixture branch resolves to NA/NULL,
  # triggering the "all lo/hi values non-finite" abort path.
  broken <- structure(
    list(dist = "beta", params = list(alpha = 1, beta = 1),
        fit_summary = list(mean = NA_real_, sd = NA_real_,
                           q025 = NA_real_, q975 = NA_real_)),
    class = "bayprior"
  )
  mix <- structure(
    list(dist = "mixture", components = list(broken, broken), weights = c(0.5, 0.5)),
    class = "bayprior"
  )
  expect_error(.density_grid(mix), "Cannot determine density range")
})

# -- .apply_plotly_theme() -- zero prior coverage -----------------------------

test_that(".apply_plotly_theme sets a white background and does not error on a minimal plotly-like object", {
  fake_plotly <- list(x = list(layout = list(shapes = list(
    list(type = "rect", fillcolor = "#000000")
  ))))
  out <- .apply_plotly_theme(fake_plotly)
  expect_equal(out$x$layout$paper_bgcolor, "#ffffff")
  expect_equal(out$x$layout$plot_bgcolor,  "#ffffff")
  expect_equal(out$x$layout$shapes[[1]]$fillcolor, "#ffffff")
})

test_that(".apply_plotly_theme handles a plotly object with no shapes", {
  fake_plotly <- list(x = list(layout = list()))
  out <- .apply_plotly_theme(fake_plotly)
  expect_equal(out$x$layout$paper_bgcolor, "#ffffff")
})