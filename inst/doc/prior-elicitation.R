## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width = 7, fig.height = 4,
                      message = FALSE, warning = FALSE)
library(bayprior)

## ----beta-quantile------------------------------------------------------------
prior_q <- elicit_beta(
  quantiles = c("0.05" = 0.10, "0.50" = 0.30, "0.95" = 0.60),
  expert_id = "Expert_1",
  label     = "ORR (treatment arm)"
)
print(prior_q)
plot(prior_q)

## ----beta-moments-------------------------------------------------------------
prior_m <- elicit_beta(
  mean      = 0.30,
  sd        = 0.10,
  method    = "moments",
  expert_id = "Expert_1",
  label     = "ORR (treatment arm)"
)
print(prior_m)

## ----normal-quantile----------------------------------------------------------
prior_lor <- elicit_normal(
  quantiles = c("0.025" = -0.50, "0.50" = 0.20, "0.975" = 0.90),
  label     = "Log odds ratio"
)
plot(prior_lor)

## ----normal-moments-----------------------------------------------------------
prior_md <- elicit_normal(
  mean = 0.0, sd = 0.5, method = "moments",
  label = "Mean difference (sceptical)"
)
plot(prior_md)

## ----gamma-moments------------------------------------------------------------
prior_os <- elicit_gamma(
  mean = 18, sd = 6, method = "moments",
  label = "Median OS (months)"
)
plot(prior_os)

## ----gamma-quantile-----------------------------------------------------------
prior_rate <- elicit_gamma(
  quantiles = c("0.10" = 2, "0.50" = 5, "0.90" = 10),
  label     = "Event rate (per 100 patient-years)"
)
print(prior_rate)

## ----lognormal-quantile-------------------------------------------------------
prior_hr <- elicit_lognormal(
  quantiles = c("0.05" = 0.40, "0.50" = 0.70, "0.95" = 1.20),
  label     = "Hazard ratio (treatment vs control)"
)
plot(prior_hr)

## ----lognormal-moments--------------------------------------------------------
prior_pk <- elicit_lognormal(
  mean = 25, sd = 10, method = "moments",
  label = "AUC (ng/mL*h)"
)
print(prior_pk)

## ----exp-moments--------------------------------------------------------------
# Mean hazard rate 0.05 => mean survival 1/0.05 = 20 months
prior_hz <- elicit_exponential(
  mean      = 0.05,
  method    = "moments",
  label     = "Hazard rate (constant)",
  expert_id = "Expert_1"
)
print(prior_hz)
plot(prior_hz)

## ----exp-rate-----------------------------------------------------------------
prior_ae <- elicit_exponential(
  rate   = 0.12,
  method = "rate",
  label  = "AE rate per person-year"
)
print(prior_ae)

## ----exp-quantile-------------------------------------------------------------
prior_hz_q <- elicit_exponential(
  quantiles = c("0.25" = 0.02, "0.50" = 0.05, "0.75" = 0.10),
  method    = "quantile",
  label     = "Hazard rate"
)
print(prior_hz_q)
plot(prior_hz_q)

## ----weibull-moments----------------------------------------------------------
prior_wb <- elicit_weibull(
  mean      = 20,
  sd        = 10,
  method    = "moments",
  label     = "OS (months)",
  expert_id = "Expert_1"
)
print(prior_wb)
plot(prior_wb)

## ----weibull-params-----------------------------------------------------------
# k=2: linearly increasing hazard
prior_wb2 <- elicit_weibull(
  shape  = 2,
  scale  = 20,
  method = "params",
  label  = "PFS (months) — increasing hazard"
)
print(prior_wb2)
plot(prior_wb2)

## ----weibull-quantile---------------------------------------------------------
prior_wb3 <- elicit_weibull(
  quantiles = c("0.10" = 5, "0.50" = 18, "0.90" = 40),
  method    = "quantile",
  label     = "OS (months)"
)
print(prior_wb3)
plot(prior_wb3)

## ----roulette-----------------------------------------------------------------
prior_rou <- elicit_roulette(
  chips  = c(0L, 1L, 3L, 7L, 9L, 7L, 4L, 2L, 1L, 1L),
  breaks = seq(0, 1, by = 0.1),
  family = "beta",
  label  = "Response rate"
)
print(prior_rou)
plot(prior_rou)

## ----linear-pooling-----------------------------------------------------------
e1 <- elicit_beta(mean = 0.25, sd = 0.08, method = "moments",
                  expert_id = "Oncologist_1", label = "ORR")
e2 <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments",
                  expert_id = "Oncologist_2", label = "ORR")
e3 <- elicit_beta(
  quantiles = c("0.10" = 0.15, "0.50" = 0.30, "0.90" = 0.52),
  expert_id = "Statistician", label = "ORR"
)
consensus <- aggregate_experts(
  priors  = list(Oncologist_1 = e1, Oncologist_2 = e2, Statistician = e3),
  weights = c(0.40, 0.35, 0.25),
  method  = "linear"
)
plot(consensus)

## ----pooling-compat-valid-----------------------------------------------------
# Same positive support — allowed (Gamma + Exponential)
g1   <- elicit_gamma(mean = 5, sd = 2, method = "moments",
                     expert_id = "E1", label = "Rate")
exp1 <- elicit_exponential(mean = 0.2, method = "moments",
                            expert_id = "E2", label = "Rate")
pool_pos <- aggregate_experts(
  priors  = list(E1 = g1, E2 = exp1),
  weights = c(0.5, 0.5)
)
print(pool_pos)

## ----structure----------------------------------------------------------------
str(prior_q, max.level = 1)

