# bayprior <img src="man/figures/logo.png" align="right" height="195" alt="bayprior logo" />

<!-- [![CRAN status](https://www.r-pkg.org/badges/version/bayprior?color=green)](https://CRAN.R-project.org/package=bayprior)
[![downloads](https://cranlogs.r-pkg.org/badges/bayprior)](https://CRAN.R-project.org/package=bayprior)
[![downloads](https://cranlogs.r-pkg.org/badges/grand-total/bayprior)](https://CRAN.R-project.org/package=bayprior) -->
[![R CMD check](https://github.com/ndohpenngit/bayprior/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ndohpenngit/bayprior/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/ndohpenngit/bayprior/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ndohpenngit/bayprior)
[![Live app](https://img.shields.io/badge/Live_App-shinyapps.io-blueviolet?logo=shiny)](https://npenn.shinyapps.io/bayprior/)

***Bayesian prior elicitation, conflict diagnostics, and regulatory reporting for clinical trials.***

---

## Overview

**bayprior** is an advanced interactive **R package and Shiny application**
designed for biostatisticians and clinical researchers to implement
**Bayesian Prior Elicitation, Conflict Diagnostics, and Sensitivity Analysis**
for clinical trials.

The package addresses the upstream problem that existing Bayesian trial
packages (`trialr`, `RBesT`, `hdbayes`) largely ignore: *how do you construct,
validate, and justify your prior to a regulator?* The FDA's 2026 draft guidance
on Bayesian methods makes this a live, urgent need — with no unified R tool
previously addressing it.

**bayprior** enables users to:

- **Elicit structured priors** — SHELF-style quantile matching, moment matching,
  and the interactive roulette method across Beta, Normal, Gamma, Log-Normal,
  **Exponential**, and **Weibull** families.
- **Aggregate expert opinions** — Linear or logarithmic pooling of multiple
  expert priors with pairwise Bhattacharyya agreement diagnostics and
  cross-family compatibility validation.
- **Diagnose prior-data conflict** — Box's p-value, surprise index,
  Bhattacharyya overlap, and multivariate Mahalanobis distance, supporting
  **binary, continuous, Poisson/count, and survival** data types.
- **Quantify sensitivity** — Posterior conclusions evaluated across
  hyperparameter grids via tornado plots and influence heatmaps, with
  independent data entry.
- **Build robust priors** — Sceptical, robust mixture, and calibrated power
  priors for regulatory sensitivity analyses.
- **Generate regulatory reports** — Self-contained HTML, PDF, or Word (.docx)
  prior justification reports rendered via Quarto.

---

## Features and Modules

| Module | Detail | Primary Output | Goal |
|---|---|---|---|
| **Prior Elicitation** | Quantile matching, moment matching, SHELF roulette for Beta / Normal / Gamma / Log-Normal / **Exponential** / **Weibull** | Fitted density plot + parameter table | Structured expert prior elicitation |
| **Expert Pooling** | Linear and logarithmic opinion pooling with support compatibility validation | Consensus density overlay + Bhattacharyya matrix | Aggregate multi-expert beliefs |
| **Conflict Diagnostics** | Box p-value, surprise index, KL divergence, Bhattacharyya overlap; binary, continuous, **Poisson**, and **survival** data | Prior-Likelihood-Posterior overlay | Detect prior misspecification |
| **Mahalanobis Check** | Two-endpoint multivariate conflict test | Chi-sq p-value + per-parameter z-scores | Co-primary endpoint trials |
| **Sensitivity Analysis** | Hyperparameter grid over posterior mean, SD, CrI width, Pr(efficacy); independent data entry | Tornado plot + influence heatmap | Demonstrate robustness to regulators |
| **Sceptical Prior** | Spiegelhalter-Freedman centred-at-null prior | Prior density + summary statistics | Conservative regulatory sensitivity |
| **Robust Mixture** | Schmidli et al. MAP robust mixture prior | Robust vs informative density overlay | Protection against misspecification |
| **Power Prior** | Ibrahim-Chen calibrated borrowing weight via Bayes Factor | Calibration curves + optimal delta | Principled historical data borrowing |
| **Export Report** | HTML / PDF / Word (.docx) prior justification document via Quarto | Regulatory-ready self-contained report | Submission documentation |

---

## Core Methodology

### Prior Elicitation

Three structured elicitation approaches are implemented.
**Quantile matching** fits a parametric distribution to expert-specified
probability-value pairs via numerical optimisation.
**Moment matching** derives hyperparameters analytically from an expert-supplied
mean and SD.
The **SHELF roulette method** (Oakley & O'Hagan, 2010) lets the expert allocate
chips across histogram bins, fitting a parametric family to the chip allocation
in real time.

All three methods support six distribution families:

| Family | Support | Typical use |
|---|---|---|
| **Beta** | (0, 1) | Response rates, proportions |
| **Normal** | (-Inf, Inf) | Mean differences, log odds ratios |
| **Gamma** | (0, Inf) | Event rates, variances, survival times |
| **Log-Normal** | (0, Inf) | Hazard ratios, PK parameters |
| **Exponential** | (0, Inf) | Constant hazard rates, Poisson rate priors |
| **Weibull** | (0, Inf) | Non-constant hazard survival times (OS, PFS) |

### Prior-Data Conflict Diagnostics

Conflict detection follows Box (1980). Four complementary metrics are computed:

- **Prior predictive p-value** — tests whether observed data is plausible under the prior predictive distribution.
- **Surprise index** — standardised distance between prior mean and observed data.
- **Bhattacharyya overlap** — distributional overlap between prior and normalised likelihood.
- **KL divergence** — information-theoretic distance from prior to likelihood.

Four data types are supported:

| Data type | Conjugate update | Typical endpoint |
|---|---|---|
| **Binary** (x events / n) | Beta-Binomial | Response rate, ORR |
| **Continuous** (mean, SD, n) | Normal-Normal | Mean difference |
| **Poisson / count** (events / exposure) | Gamma-Poisson | Adverse event rate |
| **Survival** (events / follow-up time) | Gamma-Exponential | Hazard rate, OS, PFS |

### Sensitivity Analysis

The sensitivity module is **fully independent of conflict diagnostics** — users
can enter observed data directly without running conflict diagnostics first.
Results are visualised as **tornado plots** and **influence heatmaps**.
A dedicated `sensitivity_cri()` function tracks credible interval width
specifically — a key regulatory quantity.

### Robust and Power Priors

The **robust mixture prior** (Schmidli et al., 2014) mixes the informative
prior with a vague Normal component. The **sceptical prior**
(Spiegelhalter & Freedman, 1994) is centred at the null treatment effect.
The **power prior** (Ibrahim & Chen, 2000) down-weights historical data by
delta in (0, 1], calibrated to achieve a target Bayes Factor.

---

## Installation

[![CRAN status](https://www.r-pkg.org/badges/version/bayprior?color=green)](https://CRAN.R-project.org/package=bayprior)
[![codecov](https://codecov.io/gh/ndohpenngit/bayprior/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ndohpenngit/bayprior)

| Type | Source | Command |
|---|---|---|
| Release | CRAN | `install.packages("bayprior")` |
| Development | GitHub | `remotes::install_github("ndohpenngit/bayprior")` |

> **PDF reports** require [Quarto CLI](https://quarto.org/docs/get-started/)
> and a LaTeX installation:
>
> ```r
> install.packages("tinytex")
> tinytex::install_tinytex()
> ```

---

## Quick Start

```r
library(bayprior)

# Elicit a Beta prior
prior <- elicit_beta(mean = 0.35, sd = 0.10, method = "moments",
                     label = "Response rate", expert_id = "Expert_1")
plot(prior)

# Pool two experts
e1  <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments", expert_id = "E1")
e2  <- elicit_beta(mean = 0.42, sd = 0.12, method = "moments", expert_id = "E2")
agg <- aggregate_experts(list(E1 = e1, E2 = e2), weights = c(0.6, 0.4))

# Conflict diagnostics
cd <- prior_conflict(prior, list(type = "binary", x = 18, n = 40))
print(cd)

# Sensitivity analysis
sa <- sensitivity_grid(
  prior,
  data_summary = list(type = "binary", x = 18, n = 40),
  param_grid   = list(alpha = seq(1, 8, 0.5), beta = seq(2, 20, 1)),
  target       = c("posterior_mean", "prob_efficacy"),
  threshold    = 0.30
)
plot_tornado(sa)

# Robust and sceptical priors
rob  <- robust_prior(prior, vague_weight = 0.20)
scep <- sceptical_prior(null_value = 0.20, family = "beta",
                        strength = "moderate")

# Generate regulatory report
prior_report(
  prior           = prior,
  conflict        = cd,
  sensitivity     = sa,
  robust_prior    = rob,
  sceptical_prior = scep,
  output_format   = "html",
  trial_name      = "TRIAL-001",
  sponsor         = "Example Pharma Ltd",
  author          = "N.P., Biostatistician"
)

# Launch the Shiny app
run_app()
```

---

## Vignettes

| Vignette | Covers |
|---|---|
| `bayprior-introduction` | Full end-to-end workflow overview |
| `prior-elicitation` | All six families and three elicitation methods |
| `conflict-diagnostics` | All four data types; univariate and multivariate |
| `sensitivity-analysis` | Grid sensitivity, tornado plots, CrI tracking |
| `robust-priors` | Robust mixture, sceptical, and power priors |
| `regulatory-reporting` | Report generation and compliance checklist |

```r
browseVignettes("bayprior")
```

---

## Documentation

Full **[documentation](https://ndohpenngit.github.io/bayprior/)** — vignettes, function reference, changelog, and cheat sheet.

---

## Code of Conduct

Please note that the bayprior project is released with a
[Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

---

## References

- O'Hagan, A. et al. (2006). *Uncertain Judgements: Eliciting Experts' Probabilities*. Wiley.
- Box, G. E. P. (1980). Sampling and Bayes' inference in scientific modelling and robustness. *JRSS-A*, 143, 383-430.
- Oakley, J. E. & O'Hagan, A. (2010). *SHELF: the Sheffield Elicitation Framework*. University of Sheffield.
- Schmidli, H. et al. (2014). Robust meta-analytic-predictive priors in clinical trials with historical control information. *Biometrics*, 70, 1023-1032.
- Ibrahim, J. G. & Chen, M.-H. (2000). Power prior distributions for regression models. *Statistical Science*, 15, 46-60.
- Spiegelhalter, D. J., Freedman, L. S. & Parmar, M. K. B. (1994). Bayesian approaches to randomized trials. *JRSS-A*, 157, 357-416.
- FDA (2026). *Draft Guidance: Bayesian Statistical Methods for Drug and Biological Products*.