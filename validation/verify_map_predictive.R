# Verification of map_prior(): the prior is the PREDICTIVE distribution of the
# parameter of a new trial, theta* ~ Normal(mu, tau^2), given the historical data.
# Compared with a from-scratch Monte Carlo of the hierarchical model.
# Run from this directory:  Rscript verify_map_predictive.R
# Reads the package from BAYPRIOR_SRC (default: ../bayprior; use BAYPRIOR_SRC=.. from the repo root).
src <- Sys.getenv("BAYPRIOR_SRC", "../bayprior")
suppressMessages(devtools::load_all(src, quiet = TRUE))
RNGkind("L'Ecuyer-CMRG"); set.seed(20261027)

p  <- c(.25, .31, .28, .40, .33); n <- c(30, 40, 25, 35, 50)
y  <- qlogis(p); se <- sqrt(1 / (n * p * (1 - p)))
tau_scale <- 1; mu_sd0 <- 1e4
m <- map_prior(y, se, outcome_type = "single_arm_log_odds", mu_prior_sd = mu_sd0)

## Monte Carlo from the model: tau on a fine grid with its exact marginal
## posterior (mu integrated out on a grid), then mu | tau, then theta* | mu, tau.
tg <- seq(1e-4, 8, length.out = 4000)
log_marg <- function(t) {
  v <- se^2 + t^2
  mug <- seq(-4, 3, length.out = 3001); dmu <- mug[2] - mug[1]
  ll <- vapply(mug, function(mm) sum(dnorm(y, mm, sqrt(v), log = TRUE)) +
                 dnorm(mm, 0, mu_sd0, log = TRUE), numeric(1))
  mx <- max(ll); mx + log(sum(exp(ll - mx)) * dmu) + log(2 * dnorm(t, 0, tau_scale))
}
tt <- tg[seq(1, 4000, by = 8)]
L  <- vapply(tt, log_marg, numeric(1)); w <- exp(L - max(L)); w <- w / sum(w)
N  <- 1e6
tau_s <- abs(sample(tt, N, TRUE, prob = w) + runif(N, -0.002, 0.002))
M <- V <- numeric(N)
for (u in unique(round(tau_s, 3))) {
  i <- which(round(tau_s, 3) == u); v <- se^2 + u^2; P <- sum(1 / v) + 1 / mu_sd0^2
  M[i] <- sum(y / v) / P; V[i] <- 1 / P
}
mu_s <- rnorm(N, M, sqrt(V)); th <- rnorm(N, mu_s, tau_s)

out <- data.frame(
  quantity = c("mean", "SD (predictive, new trial)", "SD of mu only",
               "2.5% quantile", "50% quantile", "97.5% quantile"),
  package  = c(m$fit_summary$mean, m$fit_summary$sd, m$mu_summary$sd,
               m$predictive$q025, m$predictive$q500, m$predictive$q975),
  monte_carlo = c(mean(th), sd(th), sd(mu_s), unname(quantile(th, c(.025, .5, .975))))
)
out$difference <- out$package - out$monte_carlo
print(out, digits = 4, row.names = FALSE)
write.csv(out, "verification_map_predictive.csv", row.names = FALSE)
