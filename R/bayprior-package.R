#' bayprior: Bayesian Prior Elicitation for Clinical Trials
#'
#' A toolkit for constructing, validating, and justifying Bayesian priors
#' in clinical trial settings. Implements SHELF-style expert elicitation
#' (quantile matching, roulette method, moment matching), linear and
#' logarithmic expert pooling, prior-data conflict diagnostics (Box p-value,
#' surprise index, Mahalanobis check), sensitivity analyses with tornado and
#' influence plots, sceptical/robust/power priors, and automated HTML/PDF
#' regulatory reports aligned with FDA/EMA expectations. Includes a fully
#' modular Shiny application.
#'
#' @section Main workflow:
#' \enumerate{
#'   \item **Elicitation** — \code{\link{elicit_beta}},
#'     \code{\link{elicit_normal}}, \code{\link{elicit_gamma}},
#'     \code{\link{elicit_lognormal}}, \code{\link{elicit_roulette}}
#'   \item **Expert pooling** — \code{\link{aggregate_experts}}
#'   \item **Conflict diagnostics** — \code{\link{prior_conflict}},
#'     \code{\link{conflict_mahalanobis}}
#'   \item **Sensitivity analysis** — \code{\link{sensitivity_grid}},
#'     \code{\link{sensitivity_cri}}
#'   \item **Robust priors** — \code{\link{sceptical_prior}},
#'     \code{\link{robust_prior}}, \code{\link{calibrate_power_prior}}
#'   \item **Reporting** — \code{\link{prior_report}}
#'   \item **Shiny app** — \code{\link{run_app}}
#' }
#'
#' @section References:
#' \itemize{
#'   \item O'Hagan et al. (2006). \emph{Uncertain Judgements}. Wiley.
#'   \item Box (1980). JRSS-A, 143, 383--430.
#'   \item Schmidli et al. (2014). \emph{Biometrics}, 70, 1023--1032.
#'   \item Ibrahim & Chen (2000). \emph{Statistical Science}, 15, 46--60.
#'   \item FDA Draft Guidance: Bayesian Methods in Clinical Trials (2026).
#' }
#'
#' @author Ndoh Penn \email{ndohpenn9@gmail.com}
#'
#' @docType package
#' @name bayprior
#' @aliases bayprior
"_PACKAGE"
