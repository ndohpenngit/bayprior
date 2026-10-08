#' @noRd
.welcome_link <- function(href, icon_name, label) {
  tags$a(
    class = "btn btn-default btn-sm", href = href,
    target = "_blank", rel = "noopener noreferrer",
    icon(icon_name), paste0(" ", label)
  )
}

#' @noRd
.welcome_start <- function(title, text, action, done = FALSE) {
  tags$div(class = paste("bp-start-card", if (done) "bp-start-card--done"),
    tags$div(class = "bp-start-title",
             if (done) tags$span(class = "bp-tick", icon("circle-check")),
             title),
    tags$p(class = "bp-start-text", text),
    action)
}

#' @noRd
.welcome_goto <- function(label, tab, primary = TRUE) {
  tags$button(type = "button",
              class = paste("btn btn-sm", if (primary) "btn-primary" else "btn-outline"),
              `data-goto` = tab, label)
}

# "Start here" before a prior exists; "Next steps" once one does.
#' @noRd
.welcome_start_ui <- function(ns, done) {
  has_prior <- isTRUE(done$elicit) || isTRUE(done$map)
  if (!has_prior) {
    return(tagList(
      tags$h5(class = "bp-h", "Start here"),
      .welcome_start(
        "Elicit from experts",
        "Fit a prior from quantiles, moments or roulette chips, then pool
         several experts.",
        .welcome_goto("Open elicitation", "elicitation")),
      .welcome_start(
        "Derive from historical trials",
        "Combine earlier trial results into a MAP prior with an explicit
         prior on between-trial heterogeneity.",
        .welcome_goto("Open MAP prior", "map_prior")),
      .welcome_start(
        "Explore the TRIAL-001 example",
        "A synthetic Phase II oncology trial: two experts, pooled, ready for
         the interim conflict check.",
        actionButton(ns("load_example"), "Load example",
                     class = "btn btn-outline btn-sm"))
    ))
  }
  tagList(
    tags$h5(class = "bp-h", "Next steps"),
    .welcome_start(
      if (isTRUE(done$conflict)) "Conflict checked" else "Check prior-data conflict",
      if (isTRUE(done$conflict))
        "Review the result, or rerun it with updated trial data."
      else
        "Compare the prior with observed data using Box p, S-value, surprise and overlap.",
      .welcome_goto(if (isTRUE(done$conflict)) "Review conflict" else "Open conflict diagnostics",
                    "conflict", primary = !isTRUE(done$conflict)),
      done = isTRUE(done$conflict)),
    .welcome_start(
      if (isTRUE(done$sens)) "Sensitivity analysed" else "Test sensitivity",
      if (isTRUE(done$sens))
        "Revisit the grid, tornado and heatmap, or change the target."
      else
        "See how much the posterior depends on the prior across a parameter grid.",
      .welcome_goto(if (isTRUE(done$sens)) "Review sensitivity" else "Open sensitivity",
                    "sensitivity", primary = !isTRUE(done$sens) && isTRUE(done$conflict)),
      done = isTRUE(done$sens)),
    .welcome_start(
      if (isTRUE(done$report)) "Report exported" else "Export the report",
      "Document the prior, its justification and the diagnostics for regulators.",
      .welcome_goto(if (isTRUE(done$report)) "Open report again" else "Open report",
                    "report", primary = FALSE),
      done = isTRUE(done$report)),
    tags$p(class = "bp-start-more",
           actionLink(ns("load_example"), "Load the TRIAL-001 example instead"))
  )
}

#' @noRd
mod_welcome_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    shinydashboard::box(
      width = 8, status = "primary", solidHeader = TRUE,
      title = tagList(icon("house"), " Welcome to bayprior"),
      tags$div(class = "bp-welcome",
      tags$p(class = "lead",
        "A structured toolkit for Bayesian prior elicitation, MAP priors from
         historical data, conflict diagnostics, sensitivity analysis, and
         regulatory reporting."
      ),
      tags$p(class = "bp-glance",
        "Informed by the FDA's 2026 draft guidance and EMA's 2026 concept
         paper on Bayesian methods in clinical development. Six distribution
         families, three elicitation methods, four data types (binary,
         continuous, Poisson, survival), and HTML, PDF or Word reports."
      ),
      tags$hr(),
      tags$h5(class = "bp-h", "Analytical Workflow"),

      # -- Workflow diagram: live (shows completed steps) and clickable ------
      tags$div(
        style = "overflow-x: auto; padding: 6px 0 16px;",
        uiOutput(ns("flow"))
      ),
      tags$script(HTML("
        $(document).off('click.bpflow').on('click.bpflow',
          '[data-goto]', function (e) {
            e.preventDefault();
            var tab = $(this).attr('data-goto');
            $('.sidebar-menu a[data-value=\"' + tab + '\"]').first().trigger('click');
          });
      ")),

      tags$hr(),
      tags$h5(class = "bp-h", "Learn more"),
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
      )
    ),
    column(4, tags$div(class = "bp-start", uiOutput(ns("start"))))
  )
}

# One entry per workflow step. `key` matches the names in `steps_done`
# (see app_server.R); `tab` is the sidebar tabName the step opens.
.welcome_steps <- function() {
  list(
    list(key = "elicit",   tab = "elicitation", title = "Elicitation",
         sub = "Quantiles, moments or roulette chips"),
    list(key = "pool",     tab = "pooling",     title = "Pooling",
         sub = "Linear or logarithmic, agreement checks"),
    list(key = "map",      tab = "map_prior",   title = "MAP Prior",
         sub = "Historical trials, between-trial tau"),
    list(key = "conflict", tab = "conflict",    title = "Conflict",
         sub = "Box p, S-value, KL, overlap, Mahalanobis"),
    list(key = "sens",     tab = "sensitivity", title = "Sensitivity",
         sub = "Grid, tornado, heatmap, CrI width"),
    list(key = "robust",   tab = "robust",      title = "Robust",
         sub = "Sceptical, mixture, power prior"),
    list(key = "report",   tab = "report",      title = "Report",
         sub = "HTML, PDF or Word; FDA (2026) informed")
  )
}

# Build the workflow diagram as HTML (real text sizes, equal-height boxes).
# Steps already completed in this session are filled (class "is-done"); every
# step is a link into its panel. Layout and colours live in custom.css so
# light and dark mode share one definition.
.welcome_flow_html <- function(done) {
  st  <- .welcome_steps()
  one <- function(i) {
    s      <- st[[i]]
    isdone <- isTRUE(done[[s$key]])
    tags$a(
      class = paste0("bp-step bp-step-", i, if (isdone) " is-done" else ""),
      href = "#", `data-goto` = s$tab, role = "link",
      `aria-label` = sprintf("Step %d, %s%s", i, s$title,
                             if (isdone) ", complete" else ""),
      tags$span(class = "bp-step-num", paste("STEP", i)),
      tags$span(class = "bp-step-title",
                s$title, if (isdone) HTML("&nbsp;&#10003;")),
      tags$span(class = "bp-step-sub", s$sub)
    )
  }
  tags$div(class = "bp-flow", role = "group",
           `aria-label` = "Analytical workflow, seven steps",
    lapply(seq_along(st), one),
    tags$div(class = "bp-bracket", `aria-hidden` = "true"),
    tags$div(class = "bp-bracket-cap",
      "Build the base prior: elicit (and optionally pool experts), or derive
       it from historical trials"),
    tags$div(class = "bp-flow-foot",
      "Steps 4, 5, and 6 are optional. Proceed directly to Report when only a
       prior is required. Click a step to open it.")
  )
}

#' @param steps_done A reactive returning a named list of logicals, one per
#'   workflow step (see `.welcome_steps()`); NULL gives a diagram with no
#'   completed steps.
#' @param shared The app's shared reactiveValues; needed by "Load example".
#' @noRd
mod_welcome_server <- function(id, steps_done = NULL, shared = NULL) {
  moduleServer(id, function(input, output, session) {
    cur <- function() if (is.null(steps_done)) list() else steps_done()
    output$flow  <- renderUI(.welcome_flow_html(cur()))
    output$start <- renderUI(.welcome_start_ui(session$ns, cur()))

    # TRIAL-001: the two-expert consensus prior used in the paper's case
    # study (same inputs as the Pooling module would produce). The interim
    # data (18 responses in 40 patients) are entered on the Conflict page.
    observeEvent(input$load_example, {
      req(shared)
      e1 <- elicit_beta(mean = 0.30, sd = 0.08, method = "moments",
                        label = "Response rate", expert_id = "Expert_1")
      e2 <- elicit_beta(mean = 0.42, sd = 0.10, method = "moments",
                        label = "Response rate", expert_id = "Expert_2")
      pool <- aggregate_experts(list(E1 = e1, E2 = e2),
                                weights = c(0.5, 0.5), method = "linear")
      shared$expert_pool <- list(Expert_1 = e1, Expert_2 = e2)
      shared$consensus   <- pool
      shared$base_prior  <- pool
      showNotification(
        "TRIAL-001 loaded: two pooled experts, interim data pre-filled (18 responses in 40 patients). Click Run Diagnostics.",
        type = "message", duration = 8)
      # Pre-fill the Conflict page with the interim data, then open it.
      shinyjs::runjs(paste0(
        "$('#conflict-bin_x').val(18).trigger('change');",
        "$('#conflict-bin_n').val(40).trigger('change');",
        "$('.sidebar-menu a[data-value=\"conflict\"]').first().click();"
      ))
    })
  })
}