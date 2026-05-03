# R/globals.R
# Suppress R CMD CHECK notes for Shiny functions and ggplot2 aes() variables
# that are used but not explicitly imported at the top level of each file.

# ── Shiny globals ─────────────────────────────────────────────────────────────
#' @importFrom shiny NS tagList tags icon fluidRow column uiOutput HTML
#' @importFrom shiny textInput textAreaInput dateInput numericInput
#' @importFrom shiny selectInput sliderInput checkboxGroupInput
#' @importFrom shiny actionButton downloadButton conditionalPanel
#' @importFrom shiny moduleServer renderUI observeEvent observe reactive
#' @importFrom shiny reactiveVal reactiveValues req isolate
#' @importFrom shiny showNotification withProgress setProgress
#' @importFrom shiny downloadHandler addResourcePath
#' @importFrom shiny updateNumericInput updateSliderInput
NULL

# ── Stats / base globals ──────────────────────────────────────────────────────
#' @importFrom stats median setNames density
#' @importFrom graphics curve
NULL

# ── ggplot2 / tidy-eval aes() column names ───────────────────────────────────
# These are bare column names used inside aes() that R CMD CHECK cannot
# resolve statically. Declaring them here silences the NOTEs.
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
