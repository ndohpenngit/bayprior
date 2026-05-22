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
library(bayprior)

## ----eval = FALSE-------------------------------------------------------------
# # 1. Elicit the prior
# prior <- elicit_beta(
#   mean      = 0.30,
#   sd        = 0.10,
#   method    = "moments",
#   expert_id = "Expert_1",
#   label     = "Objective response rate"
# )
# 
# # 2. Run conflict diagnostics
# cd <- prior_conflict(
#   prior        = prior,
#   data_summary = list(type = "binary", x = 18, n = 40),
#   alpha        = 0.05
# )
# 
# # 3. Run sensitivity analysis
# sa <- sensitivity_grid(
#   prior        = prior,
#   data_summary = list(type = "binary", x = 14, n = 40),
#   param_grid   = list(alpha = seq(1, 8, 0.5), beta = seq(2, 20, 1)),
#   target       = c("posterior_mean", "prob_efficacy"),
#   threshold    = 0.30
# )
# 
# # 4. Build robust and sceptical priors (optional — appear in report if supplied)
# rob  <- robust_prior(prior, vague_weight = 0.20)
# scep <- sceptical_prior(null_value = 0.20, family = "beta", strength = "moderate")
# 
# # 5. Generate the report
# # NOTE: requires devtools::install(), not just devtools::load_all()
# # Quarto spawns a fresh R session that needs the installed package.
# prior_report(
#   prior           = prior,
#   conflict        = cd,
#   sensitivity     = sa,
#   robust_prior    = rob,      # included in "Robust and Sensitivity Priors" section
#   sceptical_prior = scep,     # ditto
#   output_format   = "pdf",
#   output_file     = "prior_justification_report",
#   trial_name      = "TRIAL-001",
#   sponsor         = "BioPharma Ltd",
#   author          = "J. Smith, Principal Biostatistician",
#   notes         = paste0(
#     "Prior elicited from two independent oncologists (Expert_1, Expert_2) ",
#     "using the SHELF structured elicitation protocol. Experts were blinded ",
#     "to interim data at the time of elicitation. Prior was pre-specified in ",
#     "the Statistical Analysis Plan dated 2025-09-01."
#   )
# )

## ----checklist-demo-----------------------------------------------------------
library(knitr)
kable(data.frame(
  Requirement = c(
    "Prior elicitation method documented",
    "Distribution family and parameters specified",
    "Expert / source of prior identified",
    "Prior density plot provided",
    "Prior-data conflict assessed",
    "Conflict diagnostic statistics reported",
    "Sensitivity analysis performed",
    "Sensitivity visualisations provided",
    "Alternative priors considered",
    "Robust / sceptical prior computed",
    "Regulatory report generated"
  ),
  `FDA Guidance Section` = c(
    "Section IV.B", "Section IV.B", "Section IV.B",
    "Section IV.B", "Section IV.C", "Section IV.C",
    "Section IV.D", "Section IV.D", "Section IV.D",
    "Section IV.D", "Section IV.E"
  ),
  check.names = FALSE
), align = "ll")

