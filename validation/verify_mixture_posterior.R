# Verification of the mixture / power-prior posterior against brute-force
# numerical integration (prior density x likelihood on a fine grid).
# Run from the package root: Rscript validation/verify_mixture_posterior.R
suppressMessages(devtools::load_all(".", quiet = TRUE))
one <- function(label, prior, ds, lik, lo, hi, delta = 1) {
  post <- if (delta == 1) .conjugate_update(prior, ds) else .power_prior_update(prior, ds, delta)
  th <- seq(lo, hi, length.out = 400001)
  w <- .eval_density_vec(prior, th) * lik(th)^delta; w <- w / sum(w)
  cdf <- cumsum(w); q <- sapply(c(.025, .975), function(p) th[which(cdf >= p)[1]])
  data.frame(case = label, delta = delta,
             mean_pkg = post$fit_summary$mean, mean_grid = sum(w * th),
             q025_pkg = post$fit_summary$q025, q025_grid = q[1],
             q975_pkg = post$fit_summary$q975, q975_grid = q[2])
}
b  <- elicit_beta(mean = .3, sd = .08, method = "moments")
g  <- elicit_gamma(mean = .2, sd = .05, method = "moments")
nm <- elicit_normal(mean = .3, sd = .1, method = "moments")
rb <- suppressWarnings(robust_prior(b, vague_weight = .25))
rg <- suppressWarnings(robust_prior(g, vague_weight = .25))
rn <- suppressWarnings(robust_prior(nm, vague_weight = .2))
res <- rbind(
  one("Beta robust, binary 14/30", rb, list(type = "binary", x = 14, n = 30),
      function(t) dbinom(14, 30, t), 1e-6, 1 - 1e-6),
  one("Gamma robust, Poisson 14/30", rg, list(type = "poisson", x = 14, n = 30),
      function(t) dpois(14, 30 * t), 1e-6, 3),
  one("Gamma robust, Poisson 14/30, power 0.5", rg, list(type = "poisson", x = 14, n = 30),
      function(t) dpois(14, 30 * t), 1e-6, 3, delta = 0.5),
  one("Normal robust, continuous", rn, list(type = "continuous", x = .55, sd = .6, n = 20),
      function(t) dnorm(.55, t, .6 / sqrt(20)), -5, 5))
res[-1] <- lapply(res[-1], round, 5)
print(res)
write.csv(res, "validation/verification_mixture_posterior.csv", row.names = FALSE)
