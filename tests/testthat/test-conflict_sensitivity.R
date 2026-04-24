test_that("prior_conflict returns correct structure", {
  pr <- elicit_beta(mean=0.30, sd=0.10, method="moments", label="RR")
  cd <- prior_conflict(pr, list(type="binary", x=18, n=40))
  expect_s3_class(cd, "bayprior_conflict")
  expect_true(cd$box_pvalue >= 0 && cd$box_pvalue <= 1)
  expect_true(cd$overlap    >= 0 && cd$overlap    <= 1)
  expect_type(cd$conflict_flag, "logical")
  expect_true(cd$conflict_severity %in% c("none","mild","severe"))
})

test_that("no conflict when prior agrees with data", {
  pr <- elicit_beta(mean=0.45, sd=0.10, method="moments")
  cd <- prior_conflict(pr, list(type="binary", x=18, n=40))
  expect_equal(cd$conflict_severity, "none")
})

test_that("severe conflict detected", {
  pr <- elicit_normal(mean=0.05, sd=0.02, method="moments")
  cd <- prior_conflict(pr, list(type="continuous", x=0.80, sd=0.10, n=100))
  expect_equal(cd$conflict_severity, "severe")
})

test_that("sensitivity_grid returns correct structure", {
  pr <- elicit_beta(mean=0.30, sd=0.10, method="moments")
  sa <- sensitivity_grid(pr, list(type="binary", x=14, n=40),
    param_grid = list(alpha=seq(1,5,1), beta=seq(2,10,2)),
    target = c("posterior_mean","prob_efficacy"))
  expect_s3_class(sa, "bayprior_sensitivity")
  expect_equal(nrow(sa$grid), 25L)
  expect_true(all(c("posterior_mean","prob_efficacy") %in% names(sa$grid)))
})

test_that("conflict_mahalanobis structure correct", {
  pm   <- c(0.35, 0.60)
  pcov <- matrix(c(0.01, 0.003, 0.003, 0.015), 2, 2)
  om   <- c(0.55, 0.58)
  ocov <- matrix(c(0.008,0.002,0.002,0.010), 2, 2) / 50
  mv   <- conflict_mahalanobis(pm, pcov, om, ocov)
  expect_s3_class(mv, "bayprior_conflict_mv")
  expect_true(mv$mahal_distance >= 0)
  expect_true(mv$pvalue >= 0 && mv$pvalue <= 1)
})

test_that("plot_tornado returns ggplot", {
  pr <- elicit_beta(mean=0.30, sd=0.10, method="moments")
  sa <- sensitivity_grid(pr, list(type="binary", x=14, n=40),
    param_grid = list(alpha=seq(1,5,1), beta=seq(2,10,2)))
  expect_s3_class(plot_tornado(sa), "gg")
})
