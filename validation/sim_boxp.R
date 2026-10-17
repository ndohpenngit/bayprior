# Validation study, Parts A and B: calibration of the Box prior predictive
# p-value (exact vs Normal approximation) in bayprior.
# Run from this directory:  Rscript sim_boxp.R
# Reads the package from BAYPRIOR_SRC (default: ../bayprior, the released tree).
suppressMessages({ library(stats) })
src <- Sys.getenv("BAYPRIOR_SRC", "../bayprior")
suppressMessages(devtools::load_all(src, quiet = TRUE))
RNGkind("L'Ecuyer-CMRG"); set.seed(20261013)
REPS   <- as.integer(Sys.getenv("REPS", "20000"))
ALPHAS <- c(0.05, 0.10)

## ---- p-value methods, tabulated over every possible outcome x ------------
p_approx <- function(pm, ps, x, n, type, se_fix = FALSE) {
  om <- x / n
  if (type == "binary") {
    if (se_fix) { pa <- (x + 0.5) / (n + 1); se <- sqrt(pa * (1 - pa) / n) }
    else se <- sqrt(om * (1 - om) / n)
  } else {
    se <- if (se_fix) sqrt(x + 0.5) / n else sqrt(x) / n
  }
  se <- pmax(se, 1e-8)
  2 * pnorm(-abs((om - pm) / sqrt(ps^2 + se^2)))
}
p_exact <- function(prior, type, x, n)
  vapply(x, function(xx) bayprior:::.exact_box_pvalue(prior, type, xx, n), numeric(1))
tab <- function(prior, n, type, xmax) {
  x <- 0:xmax; pm <- prior$fit_summary$mean; ps <- prior$fit_summary$sd
  list(approx   = p_approx(pm, ps, x, n, type),
       approx_B = p_approx(pm, ps, x, n, type, se_fix = TRUE),
       exact    = p_exact(prior, type, x, n))
}

## ---- grids ----------------------------------------------------------------
shifts <- c(-3, -2, -1, -0.5, 0.5, 1, 2, 3)         # in prior SD units
N_GRID <- c(5, 10, 20, 50, 100, 500)
specs <- rbind(
  expand.grid(family = "Beta-Binomial", pm = c(0.1, 0.3, 0.5), strength = c(2, 5, 10, 20, 50), stringsAsFactors = FALSE),
  expand.grid(family = "Gamma-Poisson", pm = c(0.1, 1),        strength = c(1, 2, 5, 20, 50),  stringsAsFactors = FALSE))

rows <- list(); k <- 0
for (s in seq_len(nrow(specs))) {
  fam <- specs$family[s]; pm <- specs$pm[s]; str <- specs$strength[s]
  if (fam == "Beta-Binomial") {
    a <- pm * str; b <- (1 - pm) * str
    pr <- elicit_beta(mean = pm, sd = sqrt(pm * (1 - pm) / (str + 1)), method = "moments", label = "p")
    type <- "binary"; rdraw <- function(th, n) rbinom(length(th), n, th)
    prior_draw <- function(m) rbeta(m, a, b)
  } else {
    rate <- str / pm
    pr <- elicit_gamma(mean = pm, sd = sqrt(str) / rate, method = "moments", label = "g")
    type <- "poisson"; rdraw <- function(th, n) rpois(length(th), th * n)
    prior_draw <- function(m) rgamma(m, str, rate)
  }
  psd <- pr$fit_summary$sd
  for (n in N_GRID) {
    xmax <- if (type == "binary") n else qpois(1 - 1e-9, (pm + 6 * psd) * n) + 5
    tb <- tab(pr, n, type, xmax)
    scen <- c(list(none = NA), as.list(shifts)); names(scen) <- c("none", paste0("shift", shifts))
    for (sc in names(scen)) {
      if (sc == "none") th <- prior_draw(REPS) else {
        t0 <- pm + scen[[sc]] * psd
        valid <- if (type == "binary") t0 > 0.002 && t0 < 0.998 else t0 > 0.002 * pm
        if (!valid) next
        th <- rep(t0, REPS)
      }
      x <- pmin(rdraw(th, n), xmax)
      for (al in ALPHAS) {
        k <- k + 1
        pe <- tb$exact[x + 1]; pa <- tb$approx[x + 1]; pb <- tb$approx_B[x + 1]
        rows[[k]] <- data.frame(family = fam, prior_mean = pm, strength = str, n = n,
          scenario = sc, alpha = al, reps = REPS,
          rej_exact = mean(pe < al), rej_approx = mean(pa < al), rej_approxB = mean(pb < al),
          disc_approx_vs_exact = mean((pe < al) != (pa < al)),
          disc_approxB_vs_exact = mean((pe < al) != (pb < al)),
          s_med_exact = median(-log2(pe)), s_med_approx = median(-log2(pmax(pa, 1e-300))),
          s_q90_exact = quantile(-log2(pe), .9), s_q90_approx = quantile(-log2(pmax(pa, 1e-300)), .9),
          stringsAsFactors = FALSE)
      }
    }
  }
}
out <- do.call(rbind, rows); rownames(out) <- NULL
saveRDS(out, "results_boxp.rds"); write.csv(out, "results_boxp.csv", row.names = FALSE)

## ---- independent verification --------------------------------------------
# (1) harness tables vs prior_conflict() on random cells (both methods)
vchk <- 0; set.seed(1)
for (i in 1:200) {
  fam <- sample(c("Beta-Binomial", "Gamma-Poisson"), 1)
  if (fam == "Beta-Binomial") {
    pm <- sample(c(.1, .3, .5), 1); str <- sample(c(2, 5, 10, 20, 50), 1); n <- sample(N_GRID, 1); x <- sample(0:n, 1)
    pr <- elicit_beta(mean = pm, sd = sqrt(pm * (1 - pm) / (str + 1)), method = "moments", label = "p"); type <- "binary"
  } else {
    pm <- sample(c(.1, 1), 1); str <- sample(c(1, 2, 5, 20, 50), 1); n <- sample(N_GRID, 1)
    x <- rpois(1, pm * n); pr <- elicit_gamma(mean = pm, sd = sqrt(str) / (str / pm), method = "moments", label = "g"); type <- "poisson"
  }
  ds <- list(type = type, x = x, n = n)
  pe <- suppressMessages(prior_conflict(pr, ds, exact = TRUE)$box_pvalue)
  pa <- prior_conflict(pr, ds, exact = FALSE)$box_pvalue
  vchk <- max(vchk, abs(pe - p_exact(pr, type, x, n)), abs(pa - p_approx(pr$fit_summary$mean, pr$fit_summary$sd, x, n, type)))
}
# (2) exact p vs brute-force Monte Carlo from the generative model.
# Outcomes are ordered by an independently coded closed-form pmf (so exact
# ties, e.g. symmetric outcomes, cannot be broken by Monte Carlo noise); the
# tail MASS is then estimated by simulating (theta ~ prior, y ~ data model).
mc <- data.frame()
for (cfg in list(list("Beta-Binomial", 0.1, 50, 10), list("Beta-Binomial", 0.3, 20, 20), list("Beta-Binomial", 0.5, 5, 50),
                 list("Gamma-Poisson", 1, 5, 20), list("Gamma-Poisson", 0.1, 20, 100))) {
  fam <- cfg[[1]]; pm <- cfg[[2]]; str <- cfg[[3]]; n <- cfg[[4]]
  if (fam == "Beta-Binomial") {
    pr <- elicit_beta(mean = pm, sd = sqrt(pm * (1 - pm) / (str + 1)), method = "moments", label = "p"); type <- "binary"
    a_ <- pm * str; b_ <- (1 - pm) * str
    th <- rbeta(4e6, a_, b_); y <- rbinom(4e6, n, th); xs <- 0:min(n, 12)
    kk <- 0:n; pmf <- exp(lchoose(n, kk) + lbeta(kk + a_, n - kk + b_) - lbeta(a_, b_))
  } else {
    pr <- elicit_gamma(mean = pm, sd = sqrt(str) / (str / pm), method = "moments", label = "g"); type <- "poisson"
    r_ <- str / pm
    th <- rgamma(4e6, str, r_); y <- rpois(4e6, th * n); xs <- 0:12
    kk <- 0:max(y); pmf <- exp(lgamma(kk + str) - lgamma(str) - lfactorial(kk) + str * log(r_ / (r_ + n)) + kk * log(n / (r_ + n)))
  }
  pmf <- pmf / sum(pmf)
  for (xx in xs) {
    thr <- pmf[xx + 1] * (1 + 1e-8)
    p_mc <- mean(pmf[y + 1] <= thr)
    mc <- rbind(mc, data.frame(family = fam, pm = pm, strength = str, n = n, x = xx,
                               p_exact = p_exact(pr, type, xx, n), p_mc = p_mc))
  }
}
mc$abs_diff <- abs(mc$p_exact - mc$p_mc)
write.csv(mc, "verification_mc.csv", row.names = FALSE)
cat(sprintf("harness vs prior_conflict(): max |diff| over 200 random cells = %.2e\n", vchk))
cat(sprintf("exact p vs brute-force Monte Carlo (4e6 draws, MC SE <= 0.00025): max |diff| = %.5f over %d outcomes\n", max(mc$abs_diff), nrow(mc)))
cat("rows:", nrow(out), "\n")
writeLines(capture.output(sessionInfo()), "sessionInfo_boxp.txt")
