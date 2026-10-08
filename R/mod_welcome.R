#' @noRd
.welcome_link <- function(href, icon_name, label) {
  tags$a(
    class = "btn btn-default btn-sm", href = href,
    target = "_blank", rel = "noopener noreferrer",
    icon(icon_name), paste0(" ", label)
  )
}

#' @noRd
mod_welcome_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    shinydashboard::box(
      width = 8, status = "primary", solidHeader = TRUE,
      title = tagList(icon("house"), " Welcome to bayprior"),
      tags$p(class = "lead",
        "A structured toolkit for Bayesian prior elicitation, MAP priors from
         historical data, conflict diagnostics, sensitivity analysis, and
         regulatory reporting, informed by the FDA's 2026 draft guidance and
         EMA's 2026 concept paper on Bayesian methods in clinical
         development."
      ),
      tags$hr(),
      tags$h5("Analytical Workflow"),

      # -- SVG Workflow Diagram --------------------------------------------
      tags$div(
        style = "overflow-x: auto; padding: 6px 0 16px;",
        HTML('
<svg viewBox="0 0 920 150" xmlns="http://www.w3.org/2000/svg"
     style="width:100%; max-width:920px; font-family:Arial,sans-serif;">
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

  <!-- 3 MAP prior (alternative route to a base prior) -->
  <rect x="266" y="30" width="112" height="64" rx="8" fill="#8B5A2B"/>
  <text x="322" y="55" text-anchor="middle" fill="white" font-size="11"
        font-weight="bold">3. MAP Prior</text>
  <text x="322" y="70" text-anchor="middle" fill="#f1dfc9" font-size="9">
    Historical trials</text>
  <text x="322" y="82" text-anchor="middle" fill="#f1dfc9" font-size="9">
    Random-effects . tau</text>

  <!-- 4 Conflict -->
  <rect x="398" y="30" width="112" height="64" rx="8" fill="#D85A30"/>
  <text x="454" y="48" text-anchor="middle" fill="white" font-size="11"
        font-weight="bold">4. Conflict</text>
  <text x="454" y="62" text-anchor="middle" fill="#fdd5c6" font-size="9">
    Box p . S-value</text>
  <text x="454" y="73" text-anchor="middle" fill="#fdd5c6" font-size="9">
    Surprise . KL . Overlap</text>
  <text x="454" y="84" text-anchor="middle" fill="#fdd5c6" font-size="9">
    Mahalanobis</text>

  <!-- 5 Sensitivity -->
  <rect x="530" y="30" width="112" height="64" rx="8" fill="#6C63FF"/>
  <text x="586" y="55" text-anchor="middle" fill="white" font-size="11"
        font-weight="bold">5. Sensitivity</text>
  <text x="586" y="70" text-anchor="middle" fill="#d8d6ff" font-size="9">
    Grid . Tornado</text>
  <text x="586" y="82" text-anchor="middle" fill="#d8d6ff" font-size="9">
    Heatmap . CrI width</text>

  <!-- 6 Robust -->
  <rect x="662" y="30" width="112" height="64" rx="8" fill="#0F3460"/>
  <text x="718" y="55" text-anchor="middle" fill="white" font-size="11"
        font-weight="bold">6. Robust</text>
  <text x="718" y="70" text-anchor="middle" fill="#b0c8e0" font-size="9">
    Sceptical . Mixture</text>
  <text x="718" y="82" text-anchor="middle" fill="#b0c8e0" font-size="9">
    Power prior</text>

  <!-- 7 Report -->
  <rect x="794" y="30" width="112" height="64" rx="8" fill="#1A7A4A"/>
  <text x="850" y="55" text-anchor="middle" fill="white" font-size="11"
        font-weight="bold">7. Report</text>
  <text x="850" y="70" text-anchor="middle" fill="#cef5e8" font-size="9">
    HTML . PDF . Word</text>
  <text x="850" y="82" text-anchor="middle" fill="#cef5e8" font-size="9">
    FDA (2026) informed</text>

  <!-- Arrows (1 -> 2, then the base-prior group -> 4 -> 5 -> 6 -> 7) -->
  <line x1="115" y1="62" x2="132" y2="62" stroke="#adb5bd" stroke-width="2"
        marker-end="url(#arr)"/>
  <text x="256" y="65" text-anchor="middle" fill="#6c757d" font-size="9"
        font-style="italic">or</text>
  <line x1="379" y1="62" x2="396" y2="62" stroke="#adb5bd" stroke-width="2"
        marker-end="url(#arr)"/>
  <line x1="511" y1="62" x2="528" y2="62" stroke="#adb5bd" stroke-width="2"
        marker-end="url(#arr)"/>
  <line x1="643" y1="62" x2="660" y2="62" stroke="#adb5bd" stroke-width="2"
        marker-end="url(#arr)"/>
  <line x1="775" y1="62" x2="792" y2="62" stroke="#adb5bd" stroke-width="2"
        marker-end="url(#arr)"/>

  <!-- Bracket: steps 1-3 are routes to a base prior -->
  <path d="M2,98 L2,104 L378,104 L378,98" fill="none" stroke="#adb5bd"
        stroke-width="1.5"/>
  <text x="190" y="118" text-anchor="middle" fill="#6c757d" font-size="9">
    Build the base prior: elicit (and optionally pool experts), or derive
    it from historical trials</text>

  <!-- Step numbers (top) -->
  <text x="58"  y="22" text-anchor="middle" fill="#185FA5"
        font-size="9" font-weight="bold">STEP 1</text>
  <text x="190" y="22" text-anchor="middle" fill="#1D9E75"
        font-size="9" font-weight="bold">STEP 2</text>
  <text x="322" y="22" text-anchor="middle" fill="#8B5A2B"
        font-size="9" font-weight="bold">STEP 3</text>
  <text x="454" y="22" text-anchor="middle" fill="#D85A30"
        font-size="9" font-weight="bold">STEP 4</text>
  <text x="586" y="22" text-anchor="middle" fill="#6C63FF"
        font-size="9" font-weight="bold">STEP 5</text>
  <text x="718" y="22" text-anchor="middle" fill="#0F3460"
        font-size="9" font-weight="bold">STEP 6</text>
  <text x="850" y="22" text-anchor="middle" fill="#1A7A4A"
        font-size="9" font-weight="bold">STEP 7</text>

  <!-- Footnote -->
  <text x="460" y="142" text-anchor="middle" fill="#6c757d" font-size="9">
    Steps 4, 5, and 6 are optional - proceed directly to Report when only
    a prior is required.
  </text>
</svg>
        ')
      ),

      tags$hr(),
      tags$h6("Learn more"),
      tags$div(
        style = "display:flex; flex-wrap:wrap; gap:8px; margin-bottom:10px;",
        .welcome_link("https://ndohpenngit.github.io/bayprior/",
                      "book", "Documentation"),
        .welcome_link(
          "https://ndohpenngit.github.io/bayprior/vignettes/bayprior-introduction.html",
          "rocket", "Getting started"),
        .welcome_link(
          "https://ndohpenngit.github.io/bayprior/vignettes/regulatory-reporting.html",
          "file-lines", "Regulatory reporting guide"),
        .welcome_link(
          "https://ndohpenngit.github.io/bayprior/cheatsheet/bayprior_cheatsheet.html",
          "table-list", "Cheat sheet"),
        .welcome_link("https://github.com/ndohpenngit/bayprior/issues",
                      "bug", "Report an issue")
      ),
      tags$p(class = "text-muted", style = "font-size:12px;",
        "Methods follow O'Hagan et al. (2006), Box (1980), Schmidli et al.
         (2014) and others; the full reference list is in the documentation."
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