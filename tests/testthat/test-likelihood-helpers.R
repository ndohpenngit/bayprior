# Exact marginal likelihoods and the mixture posterior weights built on them.

mk <- function(dist, ...) as_prior(dist, list(...))

num_marg <- function(dens, lik, lo, hi) {
  log(stats::integrate(function(t) dens(t) * lik(t), lo, hi,
                       rel.tol = 1e-10, subdivisions = 2000L)$value)
}

test_that("Beta-binomial marginal is exact", {
  p <- mk("beta", alpha = 3, beta = 7); ds <- list(type = "binary", x = 4, n = 15)
  ref <- num_marg(function(t) dbeta(t, 3, 7), function(t) dbinom(4, 15, t), 0, 1) -
    log(choose(15, 4)) + log(choose(15, 4))
  got <- .log_marglik(p, ds) + log(choose(15, 4))
  expect_equal(as.numeric(got), ref, tolerance = 1e-8)
  expect_identical(attr(got, "method"), "exact")
})

test_that("Gamma-Poisson marginal is exact (and power version)", {
  p <- mk("gamma", shape = 2, rate = 3)
  ds <- list(type = "poisson", x = 9, n = 5)
  for (d in c(1, 0.4)) {
    ref <- num_marg(function(t) dgamma(t, 2, 3),
                    function(t) (t^9 * exp(-5 * t))^d, 0, Inf) + lgamma(10) * 0
    got <- .log_marglik(p, ds, delta = d)
    # same up to the constant -sum(log x!) which does not depend on the prior
    expect_equal(as.numeric(got), ref, tolerance = 1e-7)
  }
})

test_that("Normal-Normal marginal matches integration", {
  p <- mk("normal", mu = 1, sigma = 0.8)
  ds <- list(type = "continuous", x = 1.7, n = 12, sd = 2)
  ref <- num_marg(function(t) dnorm(t, 1, 0.8), function(t) dnorm(1.7, t, 2 / sqrt(12)), -20, 20)
  expect_equal(as.numeric(.log_marglik(p, ds)), ref, tolerance = 1e-8)
})

test_that("Normal on a proportion is integrated against the binomial", {
  p <- mk("normal", mu = 0.4, sigma = 0.1); ds <- list(type = "binary", x = 6, n = 20)
  tr <- diff(pnorm(c(0, 1), 0.4, 0.1))
  ref <- num_marg(function(t) dnorm(t, 0.4, 0.1) / tr,
                  function(t) dbinom(6, 20, t), 0, 1)
  got <- .log_marglik(p, ds, scale = "proportion") + lchoose(20, 6)
  expect_equal(as.numeric(got), ref, tolerance = 1e-4)
  expect_identical(attr(.log_marglik(p, ds, scale = "proportion"), "method"), "quadrature")
})

test_that("mixture weights use exact marginals for count data", {
  mix <- elicit_mixture(list(mk("gamma", shape = 4, rate = 4),
                             mk("gamma", shape = 20, rate = 4)),
                        weights = c(0.5, 0.5))
  ds <- list(type = "poisson", x = 20, n = 4)       # rate 5, far from comp 1
  post <- .conjugate_update(mix, ds)
  lm <- vapply(mix$components, function(k) as.numeric(.log_marglik(k, ds)), 1)
  w <- exp(lm - max(lm)); w <- w / sum(w)
  expect_equal(post$weights, w, tolerance = 1e-10)
  expect_gt(post$weights[2], 0.9)
})

test_that("mixture weights for binary data equal the Beta-binomial posterior weights", {
  mix <- elicit_mixture(list(mk("beta", alpha = 2, beta = 8),
                             mk("beta", alpha = 8, beta = 2)), weights = c(0.7, 0.3))
  ds <- list(type = "binary", x = 12, n = 20)
  post <- .conjugate_update(mix, ds)
  lm <- c(lbeta(2 + 12, 8 + 8) - lbeta(2, 8), lbeta(8 + 12, 2 + 8) - lbeta(8, 2)) + log(c(.7, .3))
  expect_equal(post$weights, exp(lm - max(lm)) / sum(exp(lm - max(lm))), tolerance = 1e-12)
})

test_that("power prior: Poisson counts and Normal-on-rate use delta correctly", {
  g <- mk("gamma", shape = 2, rate = 3)
  pp <- .power_prior_update(g, list(type = "poisson", x = 20, n = 10), delta = 0.5)
  expect_equal(pp$params, list(shape = 2 + 10, rate = 3 + 5))
  expect_identical(pp$method, "power_prior")
  expect_equal(pp$input$delta, 0.5)
  # Normal prior on a rate: likelihood summary is rate = x/n, SE = sqrt(x)/n / sqrt(delta)
  nm <- mk("normal", mu = 2, sigma = 1)
  pn <- .power_prior_update(nm, list(type = "poisson", x = 20, n = 10), delta = 0.25)
  se <- sqrt(20) / 10 / sqrt(0.25)
  w <- (1 / se^2) / (1 + 1 / se^2)
  expect_equal(pn$params$mu, (1 - w) * 2 + w * 2, tolerance = 1e-10)
  expect_equal(pn$params$sigma, sqrt(1 / (1 + 1 / se^2)), tolerance = 1e-10)
})

test_that("power prior mixture weights are the delta-power marginals", {
  mix <- elicit_mixture(list(mk("beta", alpha = 2, beta = 8),
                             mk("beta", alpha = 8, beta = 2)), weights = c(.5, .5))
  d <- 0.3; h <- list(type = "binary", x = 12, n = 20)
  pp <- .power_prior_update(mix, h, d)
  lm <- c(lbeta(2 + d * 12, 8 + d * 8) - lbeta(2, 8), lbeta(8 + d * 12, 2 + d * 8) - lbeta(8, 2))
  expect_equal(pp$weights, exp(lm) / sum(exp(lm)), tolerance = 1e-12)
})

test_that(".marginal_bf equals the ratio of exact marginals", {
  b <- mk("beta", alpha = 2, beta = 2); pw <- mk("beta", alpha = 10, beta = 6)
  cur <- list(type = "binary", x = 7, n = 12)
  expect_equal(.marginal_bf(pw, b, cur),
               exp((lbeta(17, 11) - lbeta(10, 6)) - (lbeta(9, 7) - lbeta(2, 2))), tolerance = 1e-12)
})
