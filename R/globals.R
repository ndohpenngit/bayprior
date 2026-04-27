# R/globals.R
# Suppress R CMD CHECK notes for Shiny functions and ggplot2 variables
# that are used but not explicitly imported at the top level.

# ── Shiny globals ─────────────────────────────────────────────────────────────
#' @importFrom shiny NS tagList tags icon fluidRow column uiOutput
#' @importFrom shiny textInput textAreaInput dateInput numericInput
#' @importFrom shiny selectInput sliderInput checkboxGroupInput
#' @importFrom shiny actionButton downloadButton conditionalPanel
#' @importFrom shiny moduleServer renderUI observeEvent observe reactive
#' @importFrom shiny reactiveVal reactiveValues req isolate
#' @importFrom shiny showNotification withProgress setProgress
#' @importFrom shiny downloadHandler addResourcePath
NULL

# ── Stats / base globals ──────────────────────────────────────────────────────
#' @importFrom stats median setNames density
#' @importFrom graphics curve
NULL

# ── ggplot2 / dplyr tidy-eval variables ──────────────────────────────────────
# These are column names used inside aes() or .data[[]] that R CMD CHECK
# cannot resolve statically. Declaring them here silences the notes.
utils::globalVariables(c(
  # plot.bayprior / plot_prior_likelihood
  "theta", "density",
  # plot.bayprior_conflict
  "curve",
  # plot.bayprior_power_prior
  "delta", "bayes_factor", "box_pvalue",
  # plot_sensitivity
  ".data",
  # plot_tornado
  "target", "lower", "upper", "ref_value"
))