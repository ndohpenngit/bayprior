# Validation Part F: size-adjusted power of the Box prior predictive check.
# Compares, for each cell, shift and alpha:
#   exact  : probability-ordered prior predictive p (package default)
#   mid-p  : exact p minus half the point probability (same ordering as exact)
#   approx : Normal approximation (0.4.0 default)
#   approxB: boundary-safe Normal approximation (candidate correction)
#   tail   : equal-tail (two-sided tail-area) p = min(1, 2 min(P(X<=x), P(X>=x))), an alternative definition
#   oracle : likelihood-ratio ordering for the specific alternative (upper bound)
# "Unadjusted" = reject when p < alpha as the package does. "Size-adjusted" =
# the ordering each method induces on outcomes, with a randomised cutoff so that
# the NULL size (theta ~ prior, i.e. the prior predictive) equals alpha exactly;
# this compares orderings at equal size. Alternatives: fixed theta = prior mean +
# shift * prior SD. Everything is a deterministic sum over outcomes.
# Run: Rscript sim_power.R
suppressMessages(devtools::load_all(Sys.getenv("BAYPRIOR_SRC", "../bayprior-fix"), quiet = TRUE))
set.seed(1)
shifts <- c(-3, -2, -1, -0.5, 0.5, 1, 2, 3); N_GRID <- c(5, 10, 20, 50, 100, 500); ALPHAS <- c(0.05, 0.10)

pmf_bb <- function(a, b, n) { k <- 0:n; p <- exp(lchoose(n, k) + lbeta(k + a, n - k + b) - lbeta(a, b)); p / sum(p) }
pmf_nb <- function(sh, rt, n, K) dnbinom(0:K, size = sh, prob = rt / (rt + n))
p_ord <- function(pmf, tol = 1e-8) vapply(pmf, function(v) sum(pmf[pmf <= v * (1 + tol)]), numeric(1))
p_apx <- function(pm, ps, x, n, type, fix = FALSE) {
  om <- x / n
  se <- if (type == "binary") { if (fix) { pa <- (x + .5) / (n + 1); sqrt(pa * (1 - pa) / n) } else sqrt(om * (1 - om) / n) }
        else { if (fix) sqrt(x + .5) / n else sqrt(x) / n }
  2 * pnorm(-abs((om - pm) / sqrt(ps^2 + pmax(se, 1e-8)^2))) }

# randomised-size power for an ordering `stat` (small = more extreme)
rand_power <- function(stat, pmf0, pmf1, alpha) {
  o <- order(stat); s <- stat[o]; m0 <- pmf0[o]; m1 <- pmf1[o]
  grp <- cumsum(c(1, abs(diff(s)) > 1e-9 * pmax(abs(s[-1]), 1e-300)))
  g0 <- tapply(m0, grp, sum); g1 <- tapply(m1, grp, sum)
  cum <- 0; pw <- 0
  for (j in seq_along(g0)) {
    if (cum + g0[j] <= alpha + 1e-15) { cum <- cum + g0[j]; pw <- pw + g1[j] }
    else { w <- (alpha - cum) / g0[j]; pw <- pw + w * g1[j]; cum <- alpha; break }
  }
  unname(pw)
}
rej <- function(p, pmf, al) sum(pmf[p < al])

specs <- rbind(
  expand.grid(family = "Beta-Binomial", pm = c(0.1, 0.3, 0.5), strength = c(2, 5, 10, 20, 50), stringsAsFactors = FALSE),
  expand.grid(family = "Gamma-Poisson", pm = c(0.1, 1), strength = c(1, 2, 5, 20, 50), stringsAsFactors = FALSE))
rows <- list(); chk_pmf <- 0; chk_p <- 0
for (s in seq_len(nrow(specs))) {
  fam <- specs$family[s]; pm <- specs$pm[s]; str <- specs$strength[s]
  if (fam == "Beta-Binomial") {
    a <- pm * str; b <- (1 - pm) * str; type <- "binary"
    pr <- elicit_beta(mean = pm, sd = sqrt(pm * (1 - pm) / (str + 1)), method = "moments", label = "p")
  } else {
    rate <- str / pm; type <- "poisson"
    pr <- elicit_gamma(mean = pm, sd = sqrt(str) / rate, method = "moments", label = "g")
  }
  pmean <- pr$fit_summary$mean; psd <- pr$fit_summary$sd
  for (n in N_GRID) {
    if (type == "binary") { pmf0 <- pmf_bb(a, b, n); K <- n }
    else { K <- qnbinom(1 - 1e-12, size = str, prob = rate / (rate + n)) + 5; pmf0 <- pmf_nb(str, rate, n, K); pmf0 <- pmf0 / sum(pmf0) }
    x <- 0:K
    # check against the package
    chk_pmf <- max(chk_pmf, max(abs(pmf0 - bayprior:::.predictive_pmf(pr, type, n, K)[seq_along(pmf0)])))
    pe <- p_ord(pmf0); pmid <- pe - 0.5 * pmf0
    cdf_lo <- cumsum(pmf0); cdf_hi <- rev(cumsum(rev(pmf0))); ptail <- pmin(1, 2 * pmin(cdf_lo, cdf_hi))
    pa <- p_apx(pmean, psd, x, n, type); pb <- p_apx(pmean, psd, x, n, type, fix = TRUE)
    xs <- sample(x, 1); chk_p <- max(chk_p, abs(pe[xs + 1] -
            suppressMessages(prior_conflict(pr, list(type = type, x = xs, n = n))$box_pvalue)),
            abs(pa[xs + 1] - prior_conflict(pr, list(type = type, x = xs, n = n), exact = FALSE)$box_pvalue))
    for (sh in shifts) {
      t1 <- pmean + sh * psd
      if (type == "binary" && !(t1 > 0.002 && t1 < 0.998)) next
      if (type == "poisson" && !(t1 > 0.002 * pmean)) next
      pmf1 <- if (type == "binary") dbinom(x, n, t1) else dpois(x, t1 * n)
      lr <- ifelse(pmf0 > 0, pmf1 / pmf0, Inf)
      for (al in ALPHAS)
        rows[[length(rows) + 1]] <- data.frame(family = fam, prior_mean = pm, strength = str, n = n,
          shift = sh, alpha = al,
          size_exact = rej(pe, pmf0, al), size_midp = rej(pmid, pmf0, al), size_approx = rej(pa, pmf0, al),
          size_approxB = rej(pb, pmf0, al), size_tail = rej(ptail, pmf0, al), pow_tail = rej(ptail, pmf1, al),
          adj_tail = rand_power(pmin(cdf_lo, cdf_hi), pmf0, pmf1, al),
          pow_exact = rej(pe, pmf1, al), pow_midp = rej(pmid, pmf1, al), pow_approx = rej(pa, pmf1, al),
          pow_approxB = rej(pb, pmf1, al),
          adj_exact = rand_power(pe, pmf0, pmf1, al), adj_approx = rand_power(pa, pmf0, pmf1, al),
          adj_approxB = rand_power(pb, pmf0, pmf1, al), adj_oracle = rand_power(-lr, pmf0, pmf1, al),
          stringsAsFactors = FALSE)
    }
  }
}
res <- do.call(rbind, rows); write.csv(res, "results_power.csv", row.names = FALSE)

# Monte Carlo spot check of an adjusted-free quantity (unadjusted power) + a randomised-test check
mc <- data.frame()
for (cfg in list(list("Beta-Binomial", .3, 10, 20, -2), list("Gamma-Poisson", 1, 5, 20, 2), list("Beta-Binomial", .1, 20, 10, -1))) {
  fam <- cfg[[1]]; pm <- cfg[[2]]; str <- cfg[[3]]; n <- cfg[[4]]; sh <- cfg[[5]]
  r <- res[res$family == fam & res$prior_mean == pm & res$strength == str & res$n == n & res$shift == sh & res$alpha == .05, ]
  if (fam == "Beta-Binomial") { a <- pm * str; b <- (1 - pm) * str; pmf0 <- pmf_bb(a, b, n); psd <- sqrt(pm * (1 - pm) / (str + 1)); t1 <- pm + sh * psd
    xx <- rbinom(2e6, n, t1); xt <- rbinom(2e6, n, rbeta(2e6, a, b)); xs <- 0:n }
  else { rate <- str / pm; psd <- sqrt(str) / rate; t1 <- pm + sh * psd; K <- qnbinom(1 - 1e-12, size = str, prob = rate / (rate + n)) + 5
    pmf0 <- pmf_nb(str, rate, n, K); pmf0 <- pmf0 / sum(pmf0); xx <- pmin(rpois(2e6, t1 * n), K); xt <- pmin(rpois(2e6, rgamma(2e6, str, rate) * n), K); xs <- 0:K }
  pe <- p_ord(pmf0)
  mc <- rbind(mc, data.frame(cell = paste(fam, pm, str, n, sh), pow_mc = mean(pe[xx + 1] < .05), pow_det = r$pow_exact,
                             size_mc = mean(pe[xt + 1] < .05), size_det = r$size_exact))
}
write.csv(mc, "verification_power_mc.csv", row.names = FALSE)
cat("rows:", nrow(res), " max|pmf pkg - script|:", signif(chk_pmf, 3), " max|p pkg - script|:", signif(chk_p, 3), "\n")
print(mc)
