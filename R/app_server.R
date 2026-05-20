#' Application server (golem convention)
#'
#' Wires all module servers via a single shared \code{reactiveValues} object.
#' Modules read and write \code{shared} — no global state.
#'
#' @param input,output,session Internal shiny parameters.
#' @noRd
app_server <- function(input, output, session) {

  # ── Shared state ──────────────────────────────────────────────────────────
  shared <- reactiveValues(
    current_prior   = NULL,  # most recently fitted bayprior (any module)
    base_prior      = NULL,  # elicited or pooled prior only (for sensitivity)
    expert_pool     = list(), # named list of baypriors (one per expert)
    consensus       = NULL,  # output of aggregate_experts()
    conflict        = NULL,  # output of prior_conflict()
    sensitivity     = NULL,  # output of sensitivity_grid() / sensitivity_cri()
    robust_prior    = NULL,  # output of robust_prior()
    sceptical_prior = NULL,  # output of sceptical_prior()
    power_prior     = NULL   # output of calibrate_power_prior()
  )

  # Convenience: resolved prior (consensus preferred, else current)
  # Used by conflict, robust, sceptical, power prior modules.
  active_prior <- reactive({
    shared$consensus %||% shared$current_prior
  })

  # Base prior: only set by elicitation and pooling.
  # Sensitivity analysis uses this to avoid reacting to downstream
  # priors (robust, sceptical, power) which are products of sensitivity,
  # not inputs to it.
  base_prior <- reactive({
    shared$consensus %||% shared$base_prior
  })

  # ── Step completion indicators ────────────────────────────────────────────
  # Small coloured dot next to each sidebar menu item.
  # Green check = step complete; invisible = not yet done.
  .step_badge <- function(done) {
    if (isTRUE(done))
      tags$span(
        style = paste0(
          "display:inline-block; margin-left:4px; vertical-align:middle;",
          "width:9px; height:9px; border-radius:50%;",
          "background:#1D9E75; box-shadow:0 0 3px #1D9E75;"
        )
      )
    else
      tags$span(style = "display:inline-block; width:9px; height:9px;")
  }

  output$step_badge_elicit  <- renderUI(
    .step_badge(!is.null(active_prior())))
  output$step_badge_pool    <- renderUI(
    .step_badge(!is.null(shared$consensus)))
  output$step_badge_conflict <- renderUI(
    .step_badge(!is.null(shared$conflict)))
  output$step_badge_sens    <- renderUI(
    .step_badge(!is.null(shared$sensitivity)))
  output$step_badge_robust  <- renderUI(
    .step_badge(!is.null(shared$robust_prior) ||
                !is.null(shared$sceptical_prior) ||
                !is.null(shared$power_prior))
  )

  # ── Button enable / disable based on active prior ─────────────────────────
  # Downstream analysis buttons are disabled when no prior is fitted,
  # preventing silent failures and guiding users through the workflow.
  observe({
    has <- !is.null(active_prior())
    # Note: mahal-run_btn is NOT disabled — Mahalanobis takes raw prior
    # parameters as direct inputs and does not require an elicited prior.
    btns <- c("conflict-run_btn", "sensitivity-run_btn",
              "robust-fit_btn",   "sceptical-fit_btn", "power-run_btn",
              "pooling-pool_btn")
    for (btn in btns) {
      if (has) shinyjs::enable(btn) else shinyjs::disable(btn)
    }
  })

  # When the BASE prior changes (elicitation / pooling only), downstream
  # results are stale. Robust/sceptical/power priors changing should NOT
  # clear sensitivity -- they are downstream products, not inputs.
  observeEvent(base_prior(), {
    p <- base_prior()
    if (!is.null(p)) {
      shared$conflict       <- NULL
      shared$sensitivity    <- NULL
      shared$robust_prior   <- NULL
      shared$sceptical_prior <- NULL
      shared$power_prior    <- NULL
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # When active_prior changes due to robust/sceptical/power modules,
  # only clear sibling downstream priors (not sensitivity).
  observeEvent(active_prior(), {
    p  <- active_prior()
    bp <- base_prior()
    # Only act when active_prior diverges from base_prior (i.e. a downstream
    # module set current_prior), and there IS a base prior already.
    if (!is.null(p) && !is.null(bp) && !identical(p, bp)) {
      # Don't reset conflict or sensitivity -- they belong to the base prior
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # When expert pool changes (new expert added / pooling rerun),
  # clear conflict and sensitivity since the active prior has changed
  observeEvent(shared$consensus, {
    if (!is.null(shared$consensus)) {
      shared$conflict       <- NULL
      shared$sensitivity    <- NULL
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # ── Sidebar prior badge ───────────────────────────────────────────────────
  # Rendered into uiOutput("sidebar_prior_badge") in app_ui.R sidebar footer.
  output$sidebar_prior_badge <- renderUI({
    p <- active_prior()
    if (is.null(p)) {
      tags$div(
        style = paste0(
          "margin:6px 8px; padding:8px 10px; border-radius:6px;",
          "background:#c0392b; color:#fff; font-size:11px;"
        ),
        tags$div(
          style = "font-weight:700; font-size:12px; letter-spacing:0.5px;",
          icon("circle-xmark"), " NONE"
        ),
        tags$div(
          style = "opacity:0.85; margin-top:2px;",
          "No prior fitted"
        )
      )
    } else {
      s <- p$fit_summary
      tags$div(
        style = paste0(
          "margin:6px 8px; padding:8px 10px; border-radius:6px;",
          "background:#1D9E75; color:#fff; font-size:11px;",
          "box-shadow: 0 2px 6px rgba(0,0,0,0.25);"
        ),
        tags$div(
          style = "font-weight:700; font-size:13px; letter-spacing:0.5px;",
          icon("circle-check"), " ", toupper(p$dist)
        ),
        tags$div(
          style = paste0(
            "opacity:0.75; margin-top:2px; font-size:10px;",
            "white-space:nowrap; overflow:hidden; text-overflow:ellipsis;"
          ),
          p$label
        ),
        tags$hr(style = "border-color:rgba(255,255,255,0.3); margin:6px 0 4px;"),
        {
          # Safe numeric extraction -- fit_summary may contain named vectors
          # or lists; use tryCatch to avoid non-numeric errors
          .fmt <- function(x) tryCatch(round(as.numeric(x)[1L], 3),
                                       error = function(e) "N/A")
          tags$table(
            style = "width:100%; font-size:10px; border-collapse:collapse;",
            tags$tr(
              tags$td(style = "opacity:0.8; padding:1px 0;", "Mean"),
              tags$td(style = "text-align:right; font-weight:600;",
                      .fmt(s$mean))
            ),
            tags$tr(
              tags$td(style = "opacity:0.8; padding:1px 0;", "SD"),
              tags$td(style = "text-align:right; font-weight:600;",
                      .fmt(s$sd))
            ),
            if (!is.null(s$q025) && !is.null(s$q975) &&
                is.numeric(tryCatch(as.numeric(s$q025), error = function(e) NA)))
              tags$tr(
                tags$td(style = "opacity:0.8; padding:1px 0;", "95% CrI"),
                tags$td(style = "text-align:right; font-weight:600;",
                        paste0("[", .fmt(s$q025), ", ", .fmt(s$q975), "]"))
              )
          )
        }
      )
    }
  })

  # ── Module servers ────────────────────────────────────────────────────────
  mod_welcome_server("welcome")
  mod_elicitation_server("elicitation", shared = shared)
  mod_roulette_server("roulette",       shared = shared)
  mod_pooling_server("pooling",         shared = shared, active_prior = active_prior)
  mod_conflict_server("conflict",       shared = shared, active_prior = active_prior)
  mod_mahal_server("mahal")
  mod_sensitivity_server("sensitivity", shared = shared, active_prior = base_prior)
  mod_robust_server("robust",           shared = shared, active_prior = active_prior, base_prior = base_prior)
  mod_power_server("power",             shared = shared, active_prior = active_prior, base_prior = base_prior)
  mod_sceptical_server("sceptical",     shared = shared)
  mod_report_server("report",           shared = shared, active_prior = active_prior)
}