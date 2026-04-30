#' @noRd
mod_welcome_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    shinydashboard::box(
      width = 8, status = "primary", solidHeader = TRUE,
      title = tagList(icon("house"), " Welcome to bayprior"),
      tags$p(class = "lead",
        "A structured toolkit for Bayesian prior elicitation, conflict
         diagnostics, and sensitivity analysis \u2014 aligned with FDA (2026)
         and EMA guidance on Bayesian clinical trial methods."
      ),
      tags$hr(),
      tags$h5("Workflow"),
      tags$ol(
        tags$li(tags$strong("Prior Elicitation:"),
          " Fit Beta, Normal, Gamma, or Log-Normal priors via quantile
            matching, moment matching, or the SHELF roulette method."),
        tags$li(tags$strong("Expert Pooling:"),
          " Aggregate beliefs from multiple experts via linear or
            logarithmic opinion pooling with Bhattacharyya agreement checks."),
        tags$li(tags$strong("Conflict Diagnostics:"),
          " Detect prior-data conflict using Box's p-value, surprise index,
            overlap coefficient, and multivariate Mahalanobis distance."),
        tags$li(tags$strong("Sensitivity Analysis:"),
          " Tornado and influence heatmap plots across hyperparameter grids,
            including CrI width and Pr(efficacy) tracking."),
        tags$li(tags$strong("Robust Priors:"),
          " Build sceptical (Spiegelhalter-Freedman), robust mixture
            (Schmidli et al.), or calibrated power priors (Ibrahim & Chen)."),
        tags$li(tags$strong("Export Report:"),
          " Generate HTML, PDF, or Word (.docx) prior justification reports
            for regulatory submission (FDA/EMA aligned).")
      ),
      tags$hr(),
      tags$h6("Key references"),
      tags$ul(
        tags$li("O'Hagan et al. (2006). Uncertain Judgements. Wiley."),
        tags$li("Box (1980). JRSS-A 143, 383-430."),
        tags$li("Schmidli et al. (2014). Biometrics 70, 1023-1032."),
        tags$li("Ibrahim & Chen (2000). Statistical Science 15, 46-60."),
        tags$li("FDA Draft Guidance: Bayesian Methods (2026).")
      )
    ),
    column(4,
      shinydashboard::infoBox(
        "Distributions", "4",
        subtitle = "Beta . Normal . Gamma . Log-Normal",
        icon = icon("shapes"), color = "blue", fill = TRUE, width = 12),
      shinydashboard::infoBox(
        "Elicitation methods", "3",
        subtitle = "Quantile . Moment . Roulette",
        icon = icon("sliders"), color = "green", fill = TRUE, width = 12),
      shinydashboard::infoBox(
        "Conflict diagnostics", "4",
        subtitle = "Box p . Surprise . Overlap . Mahalanobis",
        icon = icon("vial"), color = "orange", fill = TRUE, width = 12),
      # FIX: was "2" \u2014 docx was added as a third format
      shinydashboard::infoBox(
        "Report formats", "3",
        subtitle = "HTML . PDF . Word (.docx)",
        icon = icon("file"), color = "red", fill = TRUE, width = 12)
    )
  )
}

#' @noRd
mod_welcome_server <- function(id) {
  moduleServer(id, function(input, output, session) {})
}