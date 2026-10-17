# Validation Part E: calibration of the prior predictive p-value computed by the
# package's numerical path (equal-probability quadrature) for lognormal, Weibull
# and exponential priors on a rate, Poisson/survival data. Deterministic:
# under theta ~ prior the rejection probability is sum_x P(x) 1{p(x) < alpha}.
# P is computed here INDEPENDENTLY of the package (log-scale trapezoid rule,
# 40,001 points), so agreement with the package is a real check.
# Run: Rscript sim_numerical.R   (BAYPRIOR_SRC selects the package source)
suppressMessages(devtools::load_all(Sys.getenv("BAYPRIOR_SRC", "../bayprior-fix"), quiet = TRUE))
set.seed(20261015)

dens_q <- function(pr) {                       # density and quantile on theta
  p <- pr$params
  switch(pr$dist,
    lognormal   = list(d = function(t) dlnorm(t, p$meanlog, p$sdlog), q = function(u) qlnorm(u, p$meanlog, p$sdlog),
                       r = function(m) rlnorm(m, p$meanlog, p$sdlog)),
    weibull     = list(d = function(t) dweibull(t, p$shape, p$scale), q = function(u) qweibull(u, p$shape, p$scale),
                       r = function(m) rweibull(m, p$shape, p$scale)),
    exponential = list(d = function(t) dexp(t, p$rate), q = function(u) qexp(u, p$rate), r = function(m) rexp(m, p$rate)))
}
indep_pmf <- function(pr, n, K) {
  f <- dens_q(pr)
  s <- seq(log(f$q(1e-13)), log(f$q(1 - 1e-13)), length.out = 40001); h <- s[2] - s[1]
  th <- exp(s); w <- f$d(th) * th * h; w[c(1, length(w))] <- w[c(1, length(w))] / 2
  pm <- numeric(K + 1)
  for (blk in split(0:K, ceiling((0:K + 1) / 25)))
    pm[blk + 1] <- rowSums(matrix(vapply(seq_along(th), function(j) w[j] * dpois(blk, th[j] * n), numeric(length(blk))), nrow = length(blk)))
  pm
}
p_from_pmf <- function(pmf, tol = 1e-6) vapply(pmf, function(v) sum(pmf[pmf <= v * (1 + tol)]), numeric(1))
p_approx <- function(pm, ps, x, n) { se <- pmax(sqrt(x) / n, 1e-8); 2 * pnorm(-abs((x / n - pm) / sqrt(ps^2 + se^2))) }

mk <- function(fam, pm, cv) switch(fam,
  lognormal   = elicit_lognormal(mean = pm, sd = cv * pm, method = "moments", label = "r"),
  weibull     = elicit_weibull(mean = pm, sd = cv * pm, method = "moments", label = "r"),
  exponential = elicit_exponential(mean = pm, method = "moments", label = "r"))
specs <- rbind(expand.grid(fam = c("lognormal", "weibull"), pm = c(0.1, 1), cv = c(0.25, 0.5, 1.5), stringsAsFactors = FALSE),
               data.frame(fam = "exponential", pm = c(0.1, 1), cv = 1))
rows <- list(); maxdiff_pmf <- 0; maxdiff_p <- 0
for (i in seq_len(nrow(specs))) {
  pr <- mk(specs$fam[i], specs$pm[i], specs$cv[i])
  for (n in c(5, 10, 20, 50, 100, 500)) {
    pk <- bayprior:::.predictive_pmf(pr, "poisson", n, 0)
    if (is.null(pk)) { rows[[length(rows) + 1]] <- data.frame(specs[i, ], n = n, status = "package returned NULL (approx fallback)",
                         alpha = NA, rej_prior_predictive = NA, rej_normal_approx = NA, pmf_maxdiff = NA, p_maxdiff = NA); next }
    K <- length(pk) - 1
    ip <- indep_pmf(pr, n, K); mass_out <- 1 - sum(ip)           # mass beyond K (independent)
    pmf_d <- max(abs(ip - pk)); maxdiff_pmf <- max(maxdiff_pmf, pmf_d)
    px_pkg <- p_from_pmf(pk); px_ind <- p_from_pmf(ip / sum(ip))
    p_d <- max(abs(px_pkg - px_ind)); maxdiff_p <- max(maxdiff_p, p_d)
    pa <- p_approx(pr$fit_summary$mean, pr$fit_summary$sd, 0:K, n)
    truth <- ip / sum(ip)
    for (al in c(0.05, 0.10))
      rows[[length(rows) + 1]] <- data.frame(specs[i, ], n = n, status = "ok", alpha = al,
        rej_prior_predictive = sum(truth[px_pkg < al]), rej_normal_approx = sum(truth[pa < al]),
        pmf_maxdiff = pmf_d, p_maxdiff = p_d, stringsAsFactors = FALSE)
  }
}
res <- do.call(rbind, rows); write.csv(res, "results_numerical.csv", row.names = FALSE)

## package-level check: prior_conflict() reproduces the table; Monte Carlo spot check
chk <- 0
for (k in 1:30) { i <- sample(nrow(specs), 1); pr <- mk(specs$fam[i], specs$pm[i], specs$cv[i]); n <- sample(c(5, 10, 20, 50), 1)
  x <- rpois(1, pr$fit_summary$mean * n); pk <- bayprior:::.predictive_pmf(pr, "poisson", n, x)
  r <- prior_conflict(pr, list(type = "poisson", x = x, n = n))
  chk <- max(chk, abs(r$box_pvalue - p_from_pmf(pk)[x + 1]), r$box_pvalue_method != "numerical") }
mc <- data.frame()
for (cfg in list(list("lognormal", 1, 0.5, 10), list("weibull", 0.1, 1.5, 50), list("exponential", 1, 1, 20))) {
  pr <- mk(cfg[[1]], cfg[[2]], cfg[[3]]); n <- cfg[[4]]; f <- dens_q(pr)
  pk <- bayprior:::.predictive_pmf(pr, "poisson", n, 0); px <- p_from_pmf(pk)
  M <- 4e6; x <- pmin(rpois(M, f$r(M) * n), length(pk) - 1); rej <- mean(px[x + 1] < 0.05)
  r0 <- res[res$fam == cfg[[1]] & res$pm == cfg[[2]] & res$cv == cfg[[3]] & res$n == n & res$alpha == 0.05, "rej_prior_predictive"]
  mc <- rbind(mc, data.frame(fam = cfg[[1]], n = n, mc = rej, mc_se = sqrt(rej * (1 - rej) / M), deterministic = r0))
}
write.csv(mc, "verification_numerical_mc.csv", row.names = FALSE)
ok <- res[res$status == "ok", ]
cat("cells:", nrow(res) / 2, " non-ok:", sum(res$status != "ok") / 2, "\n")
cat("max |pmf pkg - independent|:", signif(maxdiff_pmf, 3), " max |p pkg - independent|:", signif(maxdiff_p, 3), "\n")
cat("prior_conflict() vs table, max diff / method flag:", chk, "\n")
for (al in c(0.05, 0.10)) { r <- ok[ok$alpha == al, ]
  cat(sprintf("alpha=%.2f prior predictive: max %.4f mean %.4f | Normal approx: max %.4f mean %.4f, above alpha in %d/%d\n",
      al, max(r$rej_prior_predictive), mean(r$rej_prior_predictive), max(r$rej_normal_approx), mean(r$rej_normal_approx),
      sum(r$rej_normal_approx > al), nrow(r))) }
print(mc)
print(aggregate(cbind(rej_prior_predictive, rej_normal_approx) ~ fam + n, ok[ok$alpha == .05, ], max))
