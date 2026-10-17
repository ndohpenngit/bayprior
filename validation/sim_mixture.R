# Validation Part D: calibration of the Box prior predictive p-value for MIXTURE
# priors (robust prior; linearly pooled consensus). Deterministic: the rejection
# probability under theta ~ prior is sum_x P(x) * 1{p(x) < alpha}, with P the
# prior predictive pmf, so no Monte Carlo error. Run: Rscript sim_mixture.R
suppressMessages(devtools::load_all(Sys.getenv("BAYPRIOR_SRC", "../bayprior-fix"), quiet = TRUE))
pmf_fun <- bayprior:::.predictive_pmf
out <- list()
add <- function(...) out[[length(out) + 1]] <<- data.frame(...)
for (pm in c(0.1, 0.3, 0.5)) for (str in c(5, 10, 20, 50)) {
  b <- elicit_beta(mean = pm, sd = sqrt(pm * (1 - pm) / (str + 1)), method = "moments")
  rp <- suppressWarnings(robust_prior(b, vague_weight = 0.2))
  b2 <- elicit_beta(mean = min(pm + 0.15, 0.9), sd = sqrt(pm * (1 - pm) / (str + 1)), method = "moments")
  pool <- suppressWarnings(elicit_mixture(list(b, b2), weights = c(0.5, 0.5)))
  for (pn in c("robust", "pooled")) {
    pr <- if (pn == "robust") rp else pool
    for (n in c(5, 10, 20, 50, 100)) {
      pmf <- pmf_fun(pr, "binary", n, 0)           # true generative law (= its own predictive)
      px <- vapply(seq_along(pmf), function(i) sum(pmf[pmf <= pmf[i] * (1 + 1e-6)]), numeric(1))
      pa <- vapply(0:n, function(x) prior_conflict(pr, list(type = "binary", x = x, n = n),
                                                   exact = FALSE)$box_pvalue, numeric(1))
      for (al in c(0.05, 0.10))
        add(prior = pn, pm = pm, strength = str, n = n, alpha = al,
            rej_prior_predictive = sum(pmf[px < al]), rej_normal_approx = sum(pmf[pa < al]),
            boundary_mass = pmf[1] + pmf[n + 1])
    }
  }
}
res <- do.call(rbind, out)
write.csv(res, "results_mixture.csv", row.names = FALSE)
cat("cells:", nrow(res), "\n")
for (al in c(0.05, 0.10)) { r <- res[res$alpha == al, ]
  cat(sprintf("alpha=%.2f  prior-predictive: max %.4f mean %.4f | Normal approx: max %.4f mean %.4f; approx > alpha in %d/%d cells\n",
      al, max(r$rej_prior_predictive), mean(r$rej_prior_predictive),
      max(r$rej_normal_approx), mean(r$rej_normal_approx), sum(r$rej_normal_approx > al), nrow(r))) }
print(aggregate(cbind(rej_prior_predictive, rej_normal_approx) ~ prior + n, res[res$alpha == .05, ], max))
