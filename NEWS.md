# bayprior 0.1.0

## Initial release

### Prior Elicitation
* Quantile matching, moment matching, and SHELF roulette for Beta, Normal,
  Gamma, and Log-Normal families
* `elicit_beta()`, `elicit_normal()`, `elicit_gamma()`, `elicit_lognormal()`,
  `elicit_roulette()`, `elicit_mixture()`

### Expert Pooling
* Linear and logarithmic pooling with Bhattacharyya agreement diagnostics
* `aggregate_experts()`

### Conflict Diagnostics
* Box p-value, surprise index, KL divergence, Bhattacharyya overlap
* Multivariate Mahalanobis distance for co-primary endpoints
* `prior_conflict()`, `conflict_mahalanobis()`

### Sensitivity Analysis
* Posterior quantity grid (`sensitivity_grid()`) and credible interval
  sensitivity (`sensitivity_cri()`) with tornado plots and influence heatmaps

### Robust Priors
* Robust mixture prior (Schmidli et al., 2014): `robust_prior()`
* Sceptical prior (Spiegelhalter & Freedman, 1994): `sceptical_prior()`
* Calibrated power prior (Ibrahim & Chen, 2000): `calibrate_power_prior()`

### Reporting
* HTML, PDF, and Word prior justification reports via Quarto: `prior_report()`
* FDA/EMA regulatory compliance checklist included in every report

### Shiny App
* Full interactive Shiny application via `run_app()`