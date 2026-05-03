<p align="center">
  <img src="man/figures/logo.png" width="150" style="margin-bottom: 10px;"/>
  <h1 align="center">
    bayprior: Bayesian Prior Elicitation for Clinical Trials
    <br>
    <a href="https://www.r-project.org/">
      <img src="https://img.shields.io/badge/R-%3E=4.1.0-blue?logo=r&logoColor=white" alt="R version"/>
    </a>
    <a href="https://shiny.posit.co/">
      <img src="https://img.shields.io/badge/Built%20with-Shiny-009999?logo=rstudio&logoColor=white" alt="Shiny"/>
    </a>
    <a href="https://thinkr-open.github.io/golem/">
      <img src="https://img.shields.io/badge/Framework-golem-6C63FF?logo=r&logoColor=white" alt="golem"/>
    </a>
    <a href="https://www.gnu.org/licenses/gpl-3.0">
      <img src="https://img.shields.io/badge/License-GPLv3-blue.svg" alt="GPL-3"/>
    </a>
    <a href="https://github.com/ndohpenngit/bayprior/actions">
      <img src="https://github.com/ndohpenngit/bayprior/actions/workflows/R-CMD-check.yaml/badge.svg" alt="R CMD check"/>
    </a>
    <a href="https://rstudio.github.io/renv/">
      <img src="https://img.shields.io/badge/renv-enabled-brightgreen" alt="renv"/>
    </a>
    <a href="https://lifecycle.r-lib.org/articles/stages.html#experimental">
      <img src="https://img.shields.io/badge/lifecycle-experimental-orange.svg" alt="Lifecycle"/>
    </a>
    <a href="https://quarto.org">
      <img src="https://img.shields.io/badge/Reports-Quarto-blue?logo=quarto&logoColor=white" alt="Quarto"/>
    </a>
  </h1>
</p>

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
  and the interactive roulette method across Beta, Normal, Gamma, and Log-Normal
  families.
- **Aggregate expert opinions** — Linear or logarithmic pooling of multiple
  expert priors with pairwise Bhattacharyya agreement diagnostics.
- **Diagnose prior-data conflict** — Box's p-value, surprise index,
  Bhattacharyya overlap, and multivariate Mahalanobis distance.
- **Quantify sensitivity** — Posterior conclusions evaluated across
  hyperparameter grids via tornado plots and influence heatmaps.
- **Build robust priors** — Sceptical, robust mixture, and calibrated power
  priors for regulatory sensitivity analyses.
- **Generate regulatory reports** — Self-contained HTML, PDF, or Word (.docx)
  prior justification reports aligned with FDA/EMA submission expectations,
  rendered via Quarto.

---

## Features and Modules

| Module | Detail | Primary Output | Goal |
|---|---|---|---|
| **Prior Elicitation** | Quantile matching, moment matching, SHELF roulette for Beta / Normal / Gamma / Log-Normal | Fitted density plot + parameter table | Structured expert prior elicitation |
| **Expert Pooling** | Linear and logarithmic opinion pooling across multiple experts | Consensus density overlay + Bhattacharyya matrix | Aggregate multi-expert beliefs |
| **Conflict Diagnostics** | Box p-value, surprise index, KL divergence, Bhattacharyya overlap | Prior–Likelihood–Posterior overlay | Detect prior misspecification |
| **Mahalanobis Check** | Two-endpoint multivariate conflict test | Chi-sq p-value + per-parameter z-scores | Co-primary endpoint trials |
| **Sensitivity Analysis** | Hyperparameter grid over posterior mean, SD, CrI width, Pr(efficacy) | Tornado plot + influence heatmap | Demonstrate robustness to regulators |
| **Sceptical Prior** | Spiegelhalter–Freedman centred-at-null prior | Prior density + summary statistics | Conservative regulatory sensitivity |
| **Robust Mixture** | Schmidli et al. MAP robust mixture prior | Robust vs informative density overlay | Protection against misspecification |
| **Power Prior** | Ibrahim–Chen calibrated borrowing weight via Bayes Factor; supports Beta, Normal, Gamma, Log-Normal, and Mixture priors | Calibration curves + optimal δ | Principled historical data borrowing |
| **Export Report** | HTML / PDF / Word (.docx) prior justification document rendered via Quarto | Regulatory-ready self-contained report | FDA/EMA submission documentation |

---

## Core Methodology

### Prior Elicitation

Three structured elicitation approaches are implemented.
**Quantile matching** fits a parametric distribution to expert-specified
probability–value pairs via numerical optimisation.
**Moment matching** derives hyperparameters analytically from an expert-supplied
mean and SD.
The **SHELF roulette method** (Oakley & O'Hagan, 2010) lets the expert allocate
chips across histogram bins, fitting a parametric family to the chip allocation
in real time.

All three methods support four distribution families:

| Family | Support | Typical use |
|---|---|---|
| **Beta** | (0, 1) | Response rates, proportions |
| **Normal** | (−∞, ∞) | Mean differences, log odds ratios |
| **Gamma** | (0, ∞) | Event rates, variances, survival times |
| **Log-Normal** | (0, ∞) | Hazard ratios, PK parameters |

### Prior-Data Conflict Diagnostics

Conflict detection follows Box (1980). The **prior predictive p-value** tests
whether the observed data is plausible under the prior predictive distribution.
The **surprise index** measures standardised distance between the prior mean and
the observed data. The **Bhattacharyya overlap coefficient** quantifies
distributional overlap between the prior and the normalised likelihood.
For two-endpoint trials, a **Mahalanobis distance** provides an omnibus
multivariate conflict statistic with a known chi-squared reference distribution.

All diagnostics work transparently with mixture priors via a Normal
approximation based on the mixture's `fit_summary`.

### Sensitivity Analysis

The sensitivity module evaluates how key posterior quantities — posterior mean,
SD, credible interval width, and Pr(θ > θ₀) — change as prior hyperparameters
vary over a user-specified grid. Results are visualised as **tornado plots**
(bar width = range of influence, ordered from most to least sensitive) and
**influence heatmaps** (two-dimensional colour-coded grid with the reference
prior marked). A dedicated `sensitivity_cri()` function tracks credible
interval width specifically — a key regulatory quantity.

### Robust and Power Priors

The **robust mixture prior** (Schmidli et al., 2014) mixes the informative
elicited prior with a vague Normal component, protecting against prior-data
conflict while preserving the main prior's information.

The **sceptical prior** (Spiegelhalter & Freedman, 1994) is centred at the
null value of the treatment effect and calibrated to weak, moderate, or strong
scepticism. For Beta family priors, the null value must be in (0, 1).

The **power prior** (Ibrahim & Chen, 2000) down-weights historical data by
δ ∈ (0, 1], calibrated to achieve a target Bayes Factor. Conjugate updating
is supported for Beta, Normal, Gamma, Log-Normal, and Mixture priors.

---

## User Interface

The Shiny app includes a **dark / light mode toggle** in the header, with theme
preference persisted across sessions via browser localStorage. All analysis
panels show a placeholder before an analysis is run, replacing empty boxes with
a clear call to action.

### Prior Elicitation Panel

- **Distribution family:** Beta, Normal, Gamma, or Log-Normal
- **Elicitation method:** Quantile matching, moment matching, or roulette
- **Real-time density preview:** Prior density rendered after fitting
- **Expert pool:** Accumulate multiple fitted priors for consensus aggregation
- **Value boxes:** Prior mean, SD, and 95% CrI at a glance

### Conflict Diagnostics Panel

- **Data entry:** Binary (events / n) or continuous (mean, SD, n)
- **Diagnostic statistics:** Box p-value, surprise index, and overlap with
  colour-coded severity (green = none, amber = mild, red = severe)
- **Overlay plot:** Interactive Prior–Likelihood–Posterior density overlay
- **Recommendation:** Automatic severity classification with actionable guidance

### Sensitivity Analysis Panel

- **Hyperparameter grid:** User-defined ranges and grid resolution via sliders
- **Target outcomes:** Posterior mean, SD, CrI width, and Pr(efficacy)
- **Tornado plot:** Influence ordered from largest to smallest
- **Influence heatmap:** Two-dimensional colour map across the parameter grid

### Robust Priors Panel

- **Robust mixture:** Vague weight slider; density overlay of informative vs
  robust prior
- **Sceptical prior:** Family and strength selection
- **Power prior:** Historical and current data entry; calibration curve
  displaying Bayes Factor and Box p-value vs δ

### Report Export Panel

- **Trial metadata:** Protocol number, indication, sponsor, statistician, date,
  and free-text scientific rationale
- **Contents checklist:** Live indicators showing which sections are complete
- **Output format:** HTML (self-contained), PDF (xelatex via Quarto), or Word (.docx)
- **Download:** Single click renders and downloads the complete report

---

## Installation

```r
# Development version from GitHub
# install.packages("devtools")
devtools::install_github("ndohpenngit/bayprior", build_vignettes = TRUE)
```

> **System requirements:** PDF report generation requires
> [Quarto CLI](https://quarto.org/docs/get-started/) and a LaTeX installation.
> HTML and Word reports work without either.
>
> ```r
> # Install TinyTeX (recommended LaTeX distribution)
> install.packages("tinytex")
> tinytex::install_tinytex()
> ```

> **Note:** After installation, use `devtools::install()` (not `devtools::load_all()`)
> before rendering reports — Quarto spawns a fresh R session that requires the
> package to be properly installed.

### Reproducible environment (renv)

This project uses [renv](https://rstudio.github.io/renv/) for reproducible
package management. After cloning:

```r
renv::restore()   # installs exact package versions from renv.lock
```

---

## Quick Start

```r
library(bayprior)

# ── 1. Elicit a Beta prior ────────────────────────────────────────────────────
prior <- elicit_beta(
  mean      = 0.35,
  sd        = 0.10,
  method    = "moments",
  label     = "Response rate (treatment arm)",
  expert_id = "Expert_1"
)
plot(prior)

# ── 2. Pool two expert priors ─────────────────────────────────────────────────
e1  <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments",
                   expert_id = "E1", label = "Response rate")
e2  <- elicit_beta(mean = 0.42, sd = 0.12, method = "moments",
                   expert_id = "E2", label = "Response rate")
agg <- aggregate_experts(list(E1 = e1, E2 = e2), weights = c(0.6, 0.4))
plot(agg)

# ── 3. Diagnose prior-data conflict ───────────────────────────────────────────
data_obs <- list(type = "binary", x = 18, n = 40)
cd <- prior_conflict(prior, data_obs)
print(cd)
plot_prior_likelihood(prior, data_obs, show_posterior = TRUE)

# ── 4. Sensitivity analysis ───────────────────────────────────────────────────
sa <- sensitivity_grid(
  prior,
  data_summary = data_obs,
  param_grid   = list(alpha = seq(1, 8, 0.5), beta = seq(2, 20, 1)),
  target       = c("posterior_mean", "prob_efficacy"),
  threshold    = 0.30
)
plot_tornado(sa)
plot_sensitivity(sa, target = "posterior_mean")

# ── 5. Credible interval sensitivity ─────────────────────────────────────────
cri_sa <- sensitivity_cri(
  prior,
  data_summary = data_obs,
  param_grid   = list(alpha = seq(1, 8, 0.5), beta = seq(2, 20, 1)),
  cri_level    = 0.95
)
plot_sensitivity(cri_sa, target = "cri_width")

# ── 6. Robust and sceptical priors ────────────────────────────────────────────
rob  <- robust_prior(prior, vague_weight = 0.20)
scep <- sceptical_prior(null_value = 0.20, family = "beta",
                        strength   = "moderate",
                        label      = "Response rate (sceptical)")

# ── 7. Power prior (calibrated historical borrowing) ─────────────────────────
calib <- calibrate_power_prior(
  historical_data = list(type = "binary", x = 12, n = 40),
  current_data    = list(type = "binary", x = 18, n = 50),
  base_prior      = elicit_beta(mean = 0.5, sd = 0.2, method = "moments",
                                label = "Response rate"),
  target_bf       = 3
)
plot(calib)

# ── 8. Multivariate conflict (co-primary endpoints) ───────────────────────────
conflict_mahalanobis(
  prior_means = c(0.35, 0.60),
  prior_cov   = matrix(c(0.01, 0.003, 0.003, 0.015), 2, 2),
  obs_means   = c(0.55, 0.58),
  obs_cov     = matrix(c(0.008, 0.002, 0.002, 0.010), 2, 2) / 50,
  labels      = c("Response rate", "OS rate")
)

# ── 9. Generate regulatory report ─────────────────────────────────────────────
# Requires devtools::install() — not just devtools::load_all()
prior_report(
  prior         = prior,
  conflict      = cd,
  sensitivity   = sa,
  output_format = "html",          # or "pdf" or "docx"
  output_file   = "prior_report",
  trial_name    = "TRIAL-001",
  sponsor       = "Example Pharma Ltd",
  author        = "N. Penn, Principal Biostatistician",
  notes         = paste0(
    "Prior based on Phase 2 data and two external expert elicitations. ",
    "Pre-specified in the Bayesian SAP version 2.1."
  )
)

# ── 10. Launch the full Shiny app ─────────────────────────────────────────────
run_app()
```

---

## Vignettes

Six vignettes cover the full analytical workflow:

```r
# Browse all vignettes
browseVignettes("bayprior")

# Or open individually
vignette("bayprior-introduction", package = "bayprior")
vignette("prior-elicitation",     package = "bayprior")
vignette("conflict-diagnostics",  package = "bayprior")
vignette("sensitivity-analysis",  package = "bayprior")
vignette("robust-priors",         package = "bayprior")
vignette("regulatory-reporting",  package = "bayprior")
```

| Vignette | Covers |
|---|---|
| `bayprior-introduction` | Full end-to-end workflow overview |
| `prior-elicitation` | All four families and three elicitation methods in depth |
| `conflict-diagnostics` | Univariate and multivariate conflict detection |
| `sensitivity-analysis` | Grid sensitivity, tornado plots, CrI tracking |
| `robust-priors` | Robust mixture, sceptical, and power priors |
| `regulatory-reporting` | Report generation, FDA/EMA compliance checklist |

---

## References

- O'Hagan, A. et al. (2006). *Uncertain Judgements: Eliciting Experts'
  Probabilities*. Wiley.
- Box, G. E. P. (1980). Sampling and Bayes' inference in scientific modelling
  and robustness. *JRSS-A*, 143, 383–430.
- Oakley, J. E. & O'Hagan, A. (2010). *SHELF: the Sheffield Elicitation
  Framework*. University of Sheffield.
- Schmidli, H. et al. (2014). Robust meta-analytic-predictive priors in
  clinical trials with historical control information. *Biometrics*, 70,
  1023–1032.
- Ibrahim, J. G. & Chen, M.-H. (2000). Power prior distributions for regression
  models. *Statistical Science*, 15, 46–60.
- Spiegelhalter, D. J., Freedman, L. S. & Parmar, M. K. B. (1994). Bayesian
  approaches to randomized trials. *JRSS-A*, 157, 357–416.
- FDA (2026). *Draft Guidance: Bayesian Statistical Methods for Drug and
  Biological Products*.

---

## Citation

```bibtex
@Manual{bayprior2026,
  title  = {bayprior: Bayesian Prior Elicitation, Conflict Diagnostics and
             Sensitivity Analysis for Clinical Trials},
  author = {Ndoh Penn},
  year   = {2026},
  note   = {R package version 0.1.0},
  url    = {https://github.com/ndohpenngit/bayprior}
}
```

---

## License

GPL (>= 3) — see [LICENSE](LICENSE) for details.