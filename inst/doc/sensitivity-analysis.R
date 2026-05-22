## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width = 7, fig.height = 4,
                      message = FALSE, warning = FALSE)
library(bayprior)

## ----binary-setup-------------------------------------------------------------
prior    <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments",
                        label = "Response rate")
data_obs <- list(type = "binary", x = 14, n = 40)

## ----binary-grid--------------------------------------------------------------
sa <- sensitivity_grid(
  prior        = prior,
  data_summary = data_obs,
  param_grid   = list(alpha = seq(1, 8, 1), beta = seq(2, 20, 2)),
  target       = c("posterior_mean", "prob_efficacy"),
  threshold    = 0.30
)
sa$influence_scores

## ----binary-tornado-----------------------------------------------------------
plot_tornado(sa)

## ----binary-heatmap-----------------------------------------------------------
plot_sensitivity(sa, target = "posterior_mean")

## ----binary-cri---------------------------------------------------------------
cri_sa <- sensitivity_cri(
  prior        = prior,
  data_summary = data_obs,
  param_grid   = list(alpha = seq(1, 8, 1), beta = seq(2, 20, 2)),
  cri_level    = 0.95
)
cri_sa$influence_scores
plot_sensitivity(cri_sa, target = "cri_width")

## ----poisson-grid-------------------------------------------------------------
prior_ae  <- elicit_gamma(mean = 0.15, sd = 0.06, method = "moments",
                           label = "AE rate (per person-year)")
data_pois <- list(type = "poisson", x = 18, n = 120)

sa_pois <- sensitivity_grid(
  prior        = prior_ae,
  data_summary = data_pois,
  param_grid   = list(shape = seq(2, 10, 1), rate = seq(5, 40, 5)),
  target       = c("posterior_mean", "prob_efficacy"),
  threshold    = 0.20
)
sa_pois$influence_scores
plot_tornado(sa_pois)

## ----poisson-heatmap----------------------------------------------------------
plot_sensitivity(sa_pois, target = "posterior_mean")

## ----survival-grid------------------------------------------------------------
prior_hz  <- elicit_exponential(mean = 0.05, method = "moments",
                                 label = "OS hazard rate")
data_surv <- list(type = "survival", x = 30, n = 600)

sa_surv <- sensitivity_grid(
  prior        = prior_hz,
  data_summary = data_surv,
  param_grid   = list(shape = seq(1, 5, 0.5), rate = seq(5, 30, 5)),
  target       = c("posterior_mean", "prob_efficacy"),
  threshold    = 0.10
)
sa_surv$influence_scores
plot_tornado(sa_surv)

## ----survival-cri-------------------------------------------------------------
cri_surv <- sensitivity_cri(
  prior        = prior_hz,
  data_summary = data_surv,
  param_grid   = list(shape = seq(1, 5, 0.5), rate = seq(5, 30, 5)),
  cri_level    = 0.95
)
plot_sensitivity(cri_surv, target = "cri_width")

## ----continuous---------------------------------------------------------------
prior_cont <- elicit_normal(mean = 0.0, sd = 0.3, method = "moments",
                             label = "Log odds ratio")
sa_cont <- sensitivity_grid(
  prior        = prior_cont,
  data_summary = list(type = "continuous", x = 0.20, sd = 0.25, n = 60),
  param_grid   = list(mu = seq(-0.5, 0.5, 0.1), sigma = seq(0.1, 0.8, 0.1)),
  target       = c("posterior_mean", "posterior_sd")
)
plot_tornado(sa_cont)

## ----mixture-sa---------------------------------------------------------------
e1  <- elicit_beta(mean = 0.25, sd = 0.08, method = "moments",
                   expert_id = "E1", label = "ORR")
e2  <- elicit_beta(mean = 0.40, sd = 0.10, method = "moments",
                   expert_id = "E2", label = "ORR")
mix <- aggregate_experts(list(E1 = e1, E2 = e2), weights = c(0.5, 0.5))

sa_mix <- sensitivity_grid(
  prior        = mix,
  data_summary = list(type = "binary", x = 14, n = 40),
  param_grid   = list(alpha = seq(1, 8, 1), beta = seq(2, 16, 2)),
  target       = "posterior_mean"
)
plot_tornado(sa_mix)

