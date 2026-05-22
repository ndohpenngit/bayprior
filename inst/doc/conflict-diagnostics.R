## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width = 7, fig.height = 4,
                      message = FALSE, warning = FALSE)
library(bayprior)

## ----binary-prior-------------------------------------------------------------
prior <- elicit_beta(
  mean = 0.30, sd = 0.10, method = "moments",
  label = "Response rate"
)

## ----binary-none--------------------------------------------------------------
# No-conflict: data consistent with prior
cd_none <- prior_conflict(
  prior        = prior,
  data_summary = list(type = "binary", x = 13, n = 40),
  alpha        = 0.05
)
print(cd_none)

## ----binary-mild--------------------------------------------------------------
# Mild conflict
cd_mild <- prior_conflict(
  prior        = prior,
  data_summary = list(type = "binary", x = 20, n = 40)
)
print(cd_mild)

## ----binary-severe------------------------------------------------------------
# Severe conflict
cd_severe <- prior_conflict(
  prior        = prior,
  data_summary = list(type = "binary", x = 35, n = 40)
)
print(cd_severe)

## ----thresholds, echo=FALSE---------------------------------------------------
knitr::kable(data.frame(
  Diagnostic        = c("Box p-value", "Surprise index",
                        "KL divergence", "Bhattacharyya overlap"),
  `No conflict`     = c(">= 0.05", "< 2",    "< 0.5", "> 0.6"),
  `Mild conflict`   = c("0.01-0.05", "2-3",   "0.5-1", "0.3-0.6"),
  `Severe conflict` = c("< 0.01",   "> 3",    "> 1",   "< 0.3"),
  check.names = FALSE
), align = "lrrr")

## ----overlay-none-------------------------------------------------------------
plot_prior_likelihood(
  prior,
  data_summary   = list(type = "binary", x = 13, n = 40),
  show_posterior = TRUE
)

## ----overlay-severe-----------------------------------------------------------
plot_prior_likelihood(
  prior,
  data_summary   = list(type = "binary", x = 35, n = 40),
  show_posterior = TRUE
)

## ----continuous---------------------------------------------------------------
prior_norm <- elicit_normal(
  mean = 0.0, sd = 0.3, method = "moments",
  label = "Log odds ratio"
)

# No conflict: observed log OR consistent with prior
prior_conflict(
  prior_norm,
  data_summary = list(type = "continuous", x = 0.15, sd = 0.20, n = 80)
)

## ----poisson-setup------------------------------------------------------------
# Gamma prior on adverse event rate
# Expert believes the AE rate is ~0.15 events per person-year (SD 0.06)
prior_ae <- elicit_gamma(
  mean      = 0.15,
  sd        = 0.06,
  method    = "moments",
  label     = "Adverse event rate (per person-year)",
  expert_id = "Safety Expert"
)
print(prior_ae)

## ----poisson-noconflict-------------------------------------------------------
# Observed: 12 AEs over 100 person-years => rate 0.12 — consistent with prior
cd_pois_none <- prior_conflict(
  prior        = prior_ae,
  data_summary = list(type = "poisson", x = 12, n = 100),
  alpha        = 0.05
)
print(cd_pois_none)

## ----poisson-conflict---------------------------------------------------------
# Observed: 40 AEs over 100 person-years => rate 0.40 — much higher than prior
cd_pois_conf <- prior_conflict(
  prior        = prior_ae,
  data_summary = list(type = "poisson", x = 40, n = 100)
)
print(cd_pois_conf)

## ----poisson-overlay----------------------------------------------------------
plot_prior_likelihood(
  prior_ae,
  data_summary   = list(type = "poisson", x = 40, n = 100),
  show_posterior = TRUE
)

## ----survival-setup-----------------------------------------------------------
# Exponential prior on hazard rate
# Expert believes median OS ≈ 20 months => hazard ≈ 0.05
prior_hz <- elicit_exponential(
  mean      = 0.05,
  method    = "moments",
  label     = "OS hazard rate",
  expert_id = "Clinical Expert"
)
print(prior_hz)

## ----survival-noconflict------------------------------------------------------
# Observed: 20 deaths over 400 person-months => hazard 0.05 — consistent
cd_surv_none <- prior_conflict(
  prior        = prior_hz,
  data_summary = list(type = "survival", x = 20, n = 400),
  alpha        = 0.05
)
print(cd_surv_none)

## ----survival-conflict--------------------------------------------------------
# Observed: 60 deaths over 400 person-months => hazard 0.15 — much higher
cd_surv_conf <- prior_conflict(
  prior        = prior_hz,
  data_summary = list(type = "survival", x = 60, n = 400)
)
print(cd_surv_conf)

## ----survival-overlay---------------------------------------------------------
plot_prior_likelihood(
  prior_hz,
  data_summary   = list(type = "survival", x = 60, n = 400),
  show_posterior = TRUE
)

## ----survival-gamma-----------------------------------------------------------
prior_hz_gamma <- elicit_gamma(
  mean = 0.05, sd = 0.02, method = "moments",
  label = "OS hazard rate (Gamma prior)"
)
prior_conflict(
  prior_hz_gamma,
  data_summary = list(type = "survival", x = 20, n = 400)
)

## ----compat-warning-----------------------------------------------------------
# Warning: Beta prior with Poisson data is unusual
prior_beta <- elicit_beta(mean = 0.15, sd = 0.06, method = "moments",
                           label = "Rate")
# The app shows a warning notification; the function still runs with
# a Normal approximation
cd_warn <- prior_conflict(
  prior_beta,
  data_summary = list(type = "poisson", x = 15, n = 100)
)

## ----mixture-conflict---------------------------------------------------------
e1 <- elicit_beta(mean = 0.25, sd = 0.08, method = "moments",
                  expert_id = "E1", label = "ORR")
e2 <- elicit_beta(mean = 0.40, sd = 0.12, method = "moments",
                  expert_id = "E2", label = "ORR")
mix <- aggregate_experts(list(E1 = e1, E2 = e2), weights = c(0.5, 0.5))

prior_conflict(mix, list(type = "binary", x = 18, n = 40))

## ----mahalanobis--------------------------------------------------------------
conflict_mahalanobis(
  prior_means = c(0.35, 0.60),
  prior_cov   = matrix(c(0.010, 0.003, 0.003, 0.015), 2, 2),
  obs_means   = c(0.55, 0.58),
  obs_cov     = matrix(c(0.008, 0.002, 0.002, 0.010), 2, 2) / 50,
  labels      = c("Response rate", "OS rate"),
  alpha       = 0.05
)

