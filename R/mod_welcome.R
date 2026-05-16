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
      tags$h5("Analytical Workflow"),

      # -- SVG Workflow Diagram --------------------------------------------
      tags$div(
        style = "overflow-x: auto; padding: 6px 0 16px;",
        HTML('
<svg viewBox="0 0 860 130" xmlns="http://www.w3.org/2000/svg"
     style="width:100%; max-width:860px; font-family:Arial,sans-serif;">
  <defs>
    <marker id="arr" markerWidth="8" markerHeight="8" refX="7" refY="3"
            orient="auto">
      <path d="M0,0 L0,6 L8,3 z" fill="#adb5bd"/>
    </marker>
  </defs>

  <!-- Step boxes -->
  <!-- 1 Elicitation -->
  <rect x="2"   y="30" width="112" height="64" rx="8" fill="#185FA5"/>
  <text x="58"  y="55" text-anchor="middle" fill="white" font-size="11"
        font-weight="bold">1. Elicitation</text>
  <text x="58"  y="70" text-anchor="middle" fill="#b0c8e0" font-size="9">
    Beta . Normal . Gamma</text>
  <text x="58"  y="82" text-anchor="middle" fill="#b0c8e0" font-size="9">
    LogNormal . Exp . Weibull</text>

  <!-- 2 Pooling -->
  <rect x="134" y="30" width="112" height="64" rx="8" fill="#1D9E75"/>
  <text x="190" y="55" text-anchor="middle" fill="white" font-size="11"
        font-weight="bold">2. Pooling</text>
  <text x="190" y="70" text-anchor="middle" fill="#cef5e8" font-size="9">
    Linear . Logarithmic</text>
  <text x="190" y="82" text-anchor="middle" fill="#cef5e8" font-size="9">
    Bhattacharyya checks</text>

  <!-- 3 Conflict -->
  <rect x="266" y="30" width="112" height="64" rx="8" fill="#D85A30"/>
  <text x="322" y="55" text-anchor="middle" fill="white" font-size="11"
        font-weight="bold">3. Conflict</text>
  <text x="322" y="70" text-anchor="middle" fill="#fdd5c6" font-size="9">
    Box p . Surprise . KL</text>
  <text x="322" y="82" text-anchor="middle" fill="#fdd5c6" font-size="9">
    Overlap . Mahalanobis</text>

  <!-- 4 Sensitivity -->
  <rect x="398" y="30" width="112" height="64" rx="8" fill="#6C63FF"/>
  <text x="454" y="55" text-anchor="middle" fill="white" font-size="11"
        font-weight="bold">4. Sensitivity</text>
  <text x="454" y="70" text-anchor="middle" fill="#d8d6ff" font-size="9">
    Grid . Tornado</text>
  <text x="454" y="82" text-anchor="middle" fill="#d8d6ff" font-size="9">
    Heatmap . CrI width</text>

  <!-- 5 Robust -->
  <rect x="530" y="30" width="112" height="64" rx="8" fill="#0F3460"/>
  <text x="586" y="55" text-anchor="middle" fill="white" font-size="11"
        font-weight="bold">5. Robust</text>
  <text x="586" y="70" text-anchor="middle" fill="#b0c8e0" font-size="9">
    Sceptical . Mixture</text>
  <text x="586" y="82" text-anchor="middle" fill="#b0c8e0" font-size="9">
    Power prior</text>

  <!-- 6 Report -->
  <rect x="662" y="30" width="112" height="64" rx="8" fill="#1A7A4A"/>
  <text x="718" y="55" text-anchor="middle" fill="white" font-size="11"
        font-weight="bold">6. Report</text>
  <text x="718" y="70" text-anchor="middle" fill="#cef5e8" font-size="9">
    HTML . PDF . Word</text>
  <text x="718" y="82" text-anchor="middle" fill="#cef5e8" font-size="9">
    FDA / EMA aligned</text>

  <!-- Arrows -->
  <line x1="115" y1="62" x2="132" y2="62" stroke="#adb5bd" stroke-width="2"
        marker-end="url(#arr)"/>
  <line x1="247" y1="62" x2="264" y2="62" stroke="#adb5bd" stroke-width="2"
        marker-end="url(#arr)"/>
  <line x1="379" y1="62" x2="396" y2="62" stroke="#adb5bd" stroke-width="2"
        marker-end="url(#arr)"/>
  <line x1="511" y1="62" x2="528" y2="62" stroke="#adb5bd" stroke-width="2"
        marker-end="url(#arr)"/>
  <line x1="643" y1="62" x2="660" y2="62" stroke="#adb5bd" stroke-width="2"
        marker-end="url(#arr)"/>

  <!-- Step numbers (top) -->
  <text x="58"  y="22" text-anchor="middle" fill="#185FA5"
        font-size="9" font-weight="bold">STEP 1</text>
  <text x="190" y="22" text-anchor="middle" fill="#1D9E75"
        font-size="9" font-weight="bold">STEP 2</text>
  <text x="322" y="22" text-anchor="middle" fill="#D85A30"
        font-size="9" font-weight="bold">STEP 3</text>
  <text x="454" y="22" text-anchor="middle" fill="#6C63FF"
        font-size="9" font-weight="bold">STEP 4</text>
  <text x="586" y="22" text-anchor="middle" fill="#0F3460"
        font-size="9" font-weight="bold">STEP 5</text>
  <text x="718" y="22" text-anchor="middle" fill="#1A7A4A"
        font-size="9" font-weight="bold">STEP 6</text>

  <!-- Footnote -->
  <text x="430" y="120" text-anchor="middle" fill="#adb5bd" font-size="9">
    Steps 2, 3, 4, and 5 are optional - proceed directly to Report when only
    elicitation is required.
  </text>
</svg>
        ')
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
        "Distributions", "6",
        subtitle = "Beta . Normal . Gamma . LogNormal . Exp . Weibull",
        icon = icon("shapes"), color = "blue", fill = TRUE, width = 12),
      shinydashboard::infoBox(
        "Elicitation methods", "3",
        subtitle = "Quantile . Moment . Roulette",
        icon = icon("sliders"), color = "green", fill = TRUE, width = 12),
      shinydashboard::infoBox(
        "Data types", "4",
        subtitle = "Binary . Continuous . Poisson . Survival",
        icon = icon("vial"), color = "orange", fill = TRUE, width = 12),
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
