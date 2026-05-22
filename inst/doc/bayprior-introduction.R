## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse   = TRUE,
  comment    = "#>",
  fig.width  = 6,
  fig.height = 3.8,
  out.width  = "95%",
  dpi        = 120,
  warning    = FALSE,
  message    = FALSE
)

## ----beta-quantile------------------------------------------------------------
library(bayprior)

prior_q <- elicit_beta(
  quantiles = c("0.05" = 0.10, "0.50" = 0.30, "0.95" = 0.60),
  expert_id = "Expert_1",
  label     = "Response rate (treatment arm)"
)
print(prior_q)

## ----beta-moments-------------------------------------------------------------
prior_m <- elicit_beta(
  mean      = 0.30,
  sd        = 0.10,
  method    = "moments",
  expert_id = "Expert_1",
  label     = "Response rate (treatment arm)"
)
plot(prior_m)

## ----normal-prior-------------------------------------------------------------
prior_nor <- elicit_normal(
  quantiles = c("0.025" = -0.5, "0.50" = 0.20, "0.975" = 0.90),
  label     = "Log odds ratio"
)
print(prior_nor)

## ----gamma-prior--------------------------------------------------------------
prior_gam <- elicit_gamma(
  mean   = 5,
  sd     = 2,
  method = "moments",
  label  = "Median OS (months)"
)
plot(prior_gam)

## ----lognormal-prior----------------------------------------------------------
prior_ln <- elicit_lognormal(
  quantiles = c("0.05" = 0.40, "0.50" = 0.70, "0.95" = 1.20),
  label     = "Hazard ratio (treatment vs control)"
)
plot(prior_ln)

## ----roulette-----------------------------------------------------------------
prior_rou <- elicit_roulette(
  chips  = c(0L, 2L, 5L, 8L, 5L, 2L, 1L),
  breaks = seq(0, 0.7, by = 0.1),
  family = "beta",
  label  = "Response rate"
)
print(prior_rou)

## ----linear-pooling-----------------------------------------------------------
p1 <- elicit_beta(mean = 0.25, sd = 0.08, method = "moments",
                  expert_id = "E1", label = "Response rate")
p2 <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments",
                  expert_id = "E2", label = "Response rate")
p3 <- elicit_beta(mean = 0.30, sd = 0.09, method = "moments",
                  expert_id = "E3", label = "Response rate")

consensus <- aggregate_experts(
  priors  = list(E1 = p1, E2 = p2, E3 = p3),
  weights = c(0.40, 0.30, 0.30),
  method  = "linear"
)
print(consensus)
plot(consensus)

## ----manual-mixture-----------------------------------------------------------
mix <- elicit_mixture(
  components = list(p1, p2),
  weights    = c(0.5, 0.5),
  label      = "50-50 pooled prior"
)
plot(mix)

## ----conflict-----------------------------------------------------------------
prior <- elicit_beta(
  mean   = 0.30,
  sd     = 0.10,
  method = "moments",
  label  = "Response rate"
)

# Observed: 18 events in 40 patients
cd <- prior_conflict(
  prior        = prior,
  data_summary = list(type = "binary", x = 18, n = 40),  # also: "continuous", "poisson", "survival"
  alpha        = 0.05
)
print(cd)

## ----overlay, fig.height = 4--------------------------------------------------
plot_prior_likelihood(
  prior,
  data_summary   = list(type = "binary", x = 18, n = 40),
  show_posterior = TRUE
)

## ----sensitivity, cache = TRUE------------------------------------------------
sa <- sensitivity_grid(
  prior        = prior,
  data_summary = list(type = "binary", x = 14, n = 40),  # also: "continuous", "poisson", "survival"
  param_grid   = list(alpha = seq(1, 6, 1), beta = seq(2, 14, 2)),
  target       = c("posterior_mean", "prob_efficacy"),
  threshold    = 0.30
)

## ----tornado, fig.height = 3--------------------------------------------------
plot_tornado(sa)

## ----heatmap, fig.height = 4--------------------------------------------------
plot_sensitivity(sa, target = "posterior_mean")

## ----cri-sensitivity, cache = TRUE, fig.height = 4----------------------------
cri_sa <- sensitivity_cri(
  prior        = prior,
  data_summary = list(type = "binary", x = 14, n = 40),  # also: "continuous", "poisson", "survival"
  param_grid   = list(alpha = seq(1, 6, 1), beta = seq(2, 14, 2)),
  cri_level    = 0.95,
  threshold    = 0.30
)
plot_sensitivity(cri_sa, target = "cri_width")

## ----robust, fig.height = 3.5-------------------------------------------------
informative <- elicit_beta(
  mean   = 0.30,
  sd     = 0.08,
  method = "moments",
  label  = "Response rate"
)

rob <- robust_prior(
  informative  = informative,
  vague_weight = 0.20,
  label        = "Robust mixture prior"
)
plot(rob)

## ----sceptical-normal, fig.height = 3.5---------------------------------------
sc_norm <- sceptical_prior(
  null_value = 0,
  family     = "normal",
  strength   = "moderate",
  label      = "Log odds ratio (sceptical)"
)
plot(sc_norm)

## ----sceptical-beta, fig.height = 3.5-----------------------------------------
sc_beta <- sceptical_prior(
  null_value = 0.20,   # null response rate of 20%
  family     = "beta",
  strength   = "moderate",
  label      = "Response rate (sceptical)"
)
plot(sc_beta)

## ----power-prior, cache = TRUE------------------------------------------------
base <- elicit_beta(
  mean   = 0.50,
  sd     = 0.20,
  method = "moments",
  label  = "Response rate"
)

calib <- calibrate_power_prior(
  historical_data = list(type = "binary", x = 12, n = 40),
  current_data    = list(type = "binary", x = 18, n = 50),
  base_prior      = base,
  target_bf       = 3,
  delta_grid      = seq(0.10, 1.0, by = 0.10),  # coarse grid for vignette speed
  method          = "bayes_factor"
)
print(calib)
plot(calib)

## ----mahal--------------------------------------------------------------------
mv <- conflict_mahalanobis(
  prior_means = c(0.35, 0.60),
  prior_cov   = matrix(c(0.01, 0.003, 0.003, 0.015), 2, 2),
  obs_means   = c(0.55, 0.58),
  obs_cov     = matrix(c(0.008, 0.002, 0.002, 0.010), 2, 2) / 50,
  labels      = c("Response rate", "OS rate")
)
print(mv)

## ----report, eval = FALSE-----------------------------------------------------
# # prior_report() calls rmarkdown::render() internally and cannot be run
# # inside a vignette build. Run interactively after loading bayprior.
# 
# prior_report(
#   prior         = prior,
#   conflict      = cd,
#   sensitivity   = sa,
#   output_format = "html",       # or "pdf" or "docx"
#   trial_name    = "TRIAL-001",
#   sponsor       = "BioPharma Ltd",
#   author        = "J. Smith",
#   notes         = paste0(
#     "Prior based on Phase 2 internal data and two external expert ",
#     "elicitations. Sensitivity analysis confirms robustness across a ",
#     "wide range of prior hyperparameter values."
#   )
# )

## ----full-example, cache = TRUE, fig.height = 4-------------------------------
# 1. Elicit priors from three experts using different methods
e1 <- elicit_beta(mean = 0.28, sd = 0.08, method = "moments",
                  expert_id = "E1", label = "ORR")
e2 <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments",
                  expert_id = "E2", label = "ORR")
e3 <- elicit_beta(
  quantiles = c("0.10" = 0.15, "0.50" = 0.30, "0.90" = 0.52),
  expert_id = "E3", label = "ORR"
)

# 2. Pool into a consensus prior
consensus <- aggregate_experts(
  priors  = list(E1 = e1, E2 = e2, E3 = e3),
  weights = c(0.40, 0.35, 0.25),
  method  = "linear"
)

# 3. Check conflict with interim data (20 responses in 55 patients)
data_obs <- list(type = "binary", x = 20, n = 55)
cd_full  <- prior_conflict(consensus, data_obs)
print(cd_full)

# 4. Visualise prior-likelihood-posterior
plot_prior_likelihood(consensus, data_obs, show_posterior = TRUE)

## ----full-sensitivity, cache = TRUE, fig.height = 3---------------------------
# 5. Sensitivity analysis (coarse grid for vignette speed)
sa_full <- sensitivity_grid(
  prior        = consensus,
  data_summary = data_obs,
  param_grid   = list(alpha = seq(1, 6, 1), beta = seq(2, 12, 2)),
  target       = c("posterior_mean", "prob_efficacy"),
  threshold    = 0.25
)
plot_tornado(sa_full)

## ----full-heatmap, cache = TRUE, fig.height = 4-------------------------------
plot_sensitivity(sa_full, target = "prob_efficacy")

## ----full-robust, fig.height = 3.5--------------------------------------------
# 6. Robust alternative in case conflict worsens at final analysis
rob_full <- robust_prior(consensus, vague_weight = 0.20)
plot(rob_full)

