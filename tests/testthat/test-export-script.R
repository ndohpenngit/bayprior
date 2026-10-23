run_script <- function(txt) {
  txt <- sub("library(bayprior)", "", txt, fixed = TRUE)
  env <- new.env(parent = environment(.bp_export_script))
  pdf(NULL); on.exit(dev.off())
  suppressMessages(suppressWarnings(eval(parse(text = txt), env)))
  env
}

roundtrip <- function(p) {
  res <- .bp_export_script(p, date = as.Date("2026-10-23"), version = "0.5.0")
  expect_true(res$ok)
  env <- run_script(res$text)
  expect_equal(env$prior$dist, p$dist)
  expect_equal(env$prior$params, p$params, tolerance = 1e-12)
  expect_equal(env$prior$fit_summary, p$fit_summary, tolerance = 1e-12)
  expect_equal(env$prior$label, p$label)
  invisible(res)
}

test_that("elicited priors of every family are reproduced", {
  q <- c("0.05" = 0.10, "0.50" = 0.30, "0.95" = 0.60)
  roundtrip(elicit_beta(quantiles = q, expert_id = "E1", label = "ORR"))
  roundtrip(elicit_beta(mean = 0.3, sd = 0.1, method = "moments"))
  roundtrip(elicit_normal(quantiles = c("0.025" = -0.5, "0.5" = 0.2, "0.975" = 0.9)))
  roundtrip(elicit_normal(mean = 0, sd = 0.5, method = "moments"))
  roundtrip(elicit_gamma(mean = 18, sd = 6, method = "moments"))
  roundtrip(elicit_gamma(quantiles = c("0.1" = 2, "0.5" = 5, "0.9" = 10)))
  roundtrip(elicit_lognormal(quantiles = c("0.05" = 0.4, "0.5" = 0.7, "0.95" = 1.2)))
  roundtrip(elicit_lognormal(mean = 25, sd = 10, method = "moments"))
  roundtrip(elicit_exponential(mean = 0.05, method = "moments"))
  roundtrip(elicit_exponential(rate = 0.12, method = "rate"))
  roundtrip(elicit_exponential(quantiles = c("0.25" = 0.02, "0.5" = 0.05, "0.75" = 0.1),
                               method = "quantile"))
  roundtrip(elicit_weibull(mean = 20, sd = 10, method = "moments"))
  roundtrip(elicit_weibull(shape = 2, scale = 20, method = "params"))
  roundtrip(elicit_weibull(quantiles = c("0.1" = 5, "0.5" = 18, "0.9" = 40),
                           method = "quantile"))
})

test_that("roulette and direct priors are reproduced", {
  roundtrip(elicit_roulette(chips = c(0, 1, 3, 7, 9, 7, 4, 2, 1, 1),
                            breaks = seq(0, 1, by = 0.1), family = "beta"))
  roundtrip(as_prior("beta", list(alpha = 2, beta = 8), label = "Historical"))
  roundtrip(as_prior("gamma", list(shape = 3.3, rate = 1 / 7), label = "g"))
})

test_that("labels with quotes and backslashes survive", {
  roundtrip(elicit_beta(mean = 0.3, sd = 0.1, method = "moments",
                        label = "He said \"ORR\" \\ rate", expert_id = "Dr. O'Hagan"))
})

test_that("MAP priors are reproduced", {
  p <- map_prior(y = c(-0.85, -0.62, -1.10), se = c(0.25, 0.30, 0.28),
                 outcome_type = "single_arm_log_odds", label = "Hist")
  res <- .bp_export_script(p)
  env <- run_script(res$text)
  expect_equal(env$prior$params, p$params, tolerance = 1e-12)
  expect_equal(env$prior$tau_summary, p$tau_summary, tolerance = 1e-12)
  expect_equal(env$prior$tau_prior, p$tau_prior)
})

test_that("pooled priors are reproduced for both methods", {
  e1 <- elicit_beta(mean = 0.30, sd = 0.08, method = "moments", expert_id = "Expert_1")
  e2 <- elicit_beta(mean = 0.42, sd = 0.10, method = "moments", expert_id = "Expert 2")
  for (m in c("linear", "logarithmic")) {
    pool <- suppressMessages(aggregate_experts(list(Expert_1 = e1, `Expert 2` = e2),
                                               weights = c(0.35, 0.65), method = m))
    env <- run_script(.bp_export_script(pool)$text)
    expect_equal(env$prior$dist, pool$dist)
    expect_equal(env$prior$weights, pool$weights)
    expect_equal(env$prior$aggregation$weights, pool$aggregation$weights)
    expect_equal(env$prior$aggregation$method, m)
    expect_equal(env$prior$fit_summary$mean, pool$fit_summary$mean)
  }
})

test_that("conflict and sensitivity blocks reproduce the recorded results", {
  p <- elicit_beta(mean = 0.30, sd = 0.08, method = "moments")
  ds <- list(type = "binary", x = 18, n = 40)
  cf <- prior_conflict(p, ds, alpha = 0.1)
  attr(cf, "bp_args") <- list(alpha = 0.1, exact = NULL)
  pg <- list(alpha = seq(2, 20, length.out = 4), beta = seq(5, 40, length.out = 4))
  sg <- suppressMessages(sensitivity_grid(p, ds, pg, target = c("posterior_mean", "prob_efficacy"),
                                          threshold = 0.25))
  attr(sg, "bp_args") <- list(data_summary = ds)
  res <- .bp_export_script(p, cf, sg)
  expect_match(res$text, "alpha = 0.1", fixed = TRUE)
  env <- run_script(res$text)
  expect_equal(env$conflict$box_pvalue, cf$box_pvalue)
  expect_equal(env$sensitivity$grid, sg$grid)
  expect_equal(env$sensitivity$target, sg$target)

  cr <- suppressMessages(sensitivity_cri(p, ds, pg, cri_level = 0.9))
  attr(cr, "bp_args") <- list(data_summary = ds)
  env2 <- run_script(.bp_export_script(p, NULL, cr)$text)
  expect_equal(env2$sensitivity$grid, cr$grid)
})

test_that("analyses run on another prior are noted, not exported", {
  p <- elicit_beta(mean = 0.30, sd = 0.08, method = "moments")
  other <- elicit_beta(mean = 0.50, sd = 0.08, method = "moments")
  ds <- list(type = "binary", x = 18, n = 40)
  cf <- prior_conflict(other, ds)
  txt <- .bp_export_script(p, cf)$text
  expect_match(txt, "could not be reproduced", fixed = TRUE)
  expect_false(grepl("prior_conflict", txt, fixed = TRUE))
  expect_silent(parse(text = txt))
})

test_that("unsupported or missing priors give a reason and no script", {
  r0 <- .bp_export_script(NULL)
  expect_false(r0$ok)
  expect_null(r0$text)
  rb <- suppressWarnings(robust_prior(elicit_beta(mean = 0.3, sd = 0.08, method = "moments")))
  r1 <- .bp_export_script(rb)
  expect_false(r1$ok)
  expect_match(r1$reason, "robust", fixed = TRUE)
})

test_that("number formatting round-trips", {
  x <- c(0.1 + 0.2, 1e-8, 123456789.123, 1 / 3, 18)
  expect_equal(eval(parse(text = .bp_c(x))), x, tolerance = 1e-14)
})

test_that("a derived active prior is flagged in the script", {
  p <- elicit_beta(mean = 0.30, sd = 0.08, method = "moments")
  rb <- suppressWarnings(robust_prior(p))
  txt <- .bp_export_script(p, active = rb)$text
  expect_match(txt, "active prior in the app is not included", fixed = TRUE)
  expect_false(grepl("active prior", .bp_export_script(p, active = p)$text, fixed = TRUE))
  expect_false(grepl("active prior", .bp_export_script(p, robust = rb, active = rb)$text,
                     fixed = TRUE))
})

test_that("robust, sceptical and power priors are reproduced", {
  base <- elicit_beta(mean = 0.40, sd = 0.10, method = "moments", label = "Response rate")
  rb <- suppressWarnings(robust_prior(base, vague_weight = 0.25,
                                      vague_sd = 3.7 * base$fit_summary$sd,
                                      label = "Robust X"))
  sc <- sceptical_prior(null_value = 0.2, family = "beta", strength = "strong",
                        label = "Sceptic \"B\"")
  hist_d <- list(type = "binary", x = 12, n = 40)
  curr_d <- list(type = "binary", x = 18, n = 50)
  pw <- suppressMessages(calibrate_power_prior(hist_d, curr_d, base, target_bf = 2,
                                               method = "compatibility"))
  attr(pw, "bp_args") <- list(historical_data = hist_d, current_data = curr_d)

  res <- .bp_export_script(base, robust = rb, sceptical = sc, power = pw, active = pw$power_prior)
  expect_true(res$ok)
  expect_false(grepl("not included", res$text, fixed = TRUE))
  env <- run_script(res$text)
  expect_equal(env$robust$weights, rb$weights)
  expect_equal(env$robust$fit_summary, rb$fit_summary, tolerance = 1e-12)
  expect_equal(env$sceptical$params, sc$params)
  expect_equal(env$sceptical$label, sc$label)
  expect_equal(env$power_calib$delta_opt, pw$delta_opt)
  expect_equal(env$power_prior$params, pw$power_prior$params)

  for (fam in list(list("normal", 0), list("lognormal", 0))) {
    s2 <- sceptical_prior(null_value = fam[[2]], family = fam[[1]], strength = "weak")
    e2 <- run_script(.bp_export_script(base, sceptical = s2)$text)
    expect_equal(e2$sceptical$params, s2$params)
  }
})

test_that("conflict and sensitivity on a downstream prior use that prior", {
  base <- elicit_beta(mean = 0.40, sd = 0.10, method = "moments")
  rb <- suppressWarnings(robust_prior(base))
  ds <- list(type = "binary", x = 18, n = 40)
  cf <- suppressWarnings(prior_conflict(rb, ds))
  res <- .bp_export_script(base, conflict = cf, robust = rb, active = rb)
  expect_match(res$text, "prior_conflict(robust, data_summary", fixed = TRUE)
  env <- run_script(res$text)
  expect_equal(env$conflict$box_pvalue, cf$box_pvalue)
})

test_that("a power prior without recorded data, or built on another prior, is skipped", {
  base <- elicit_beta(mean = 0.40, sd = 0.10, method = "moments")
  other <- elicit_beta(mean = 0.20, sd = 0.10, method = "moments")
  hist_d <- list(type = "binary", x = 12, n = 40)
  curr_d <- list(type = "binary", x = 18, n = 50)
  pw <- suppressMessages(calibrate_power_prior(hist_d, curr_d, base))
  expect_match(.bp_export_script(base, power = pw)$text, "power prior could not", fixed = TRUE)
  attr(pw, "bp_args") <- list(historical_data = hist_d, current_data = curr_d)
  expect_match(.bp_export_script(other, power = pw)$text, "power prior could not", fixed = TRUE)
})