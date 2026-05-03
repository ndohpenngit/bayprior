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
    current_prior = NULL,   # most recently fitted bayprior
    expert_pool   = list(), # named list of baypriors (one per expert)
    consensus     = NULL,   # output of aggregate_experts()
    conflict      = NULL,   # output of prior_conflict()
    sensitivity   = NULL    # output of sensitivity_grid()
  )

  # Convenience: resolved prior (consensus preferred, else current)
  active_prior <- reactive({
    shared$consensus %||% shared$current_prior
  })

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
          style = "opacity:0.9; margin-top:3px;",
          glue::glue("mean = {round(p$fit_summary$mean, 3)}")
        ),
        tags$div(
          style = paste0(
            "opacity:0.75; margin-top:2px; font-size:10px;",
            "white-space:nowrap; overflow:hidden; text-overflow:ellipsis;"
          ),
          p$label
        )
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
  mod_sensitivity_server("sensitivity", shared = shared, active_prior = active_prior)
  mod_sceptical_server("sceptical",     shared = shared)
  mod_robust_server("robust",           shared = shared, active_prior = active_prior)
  mod_power_server("power",             shared = shared, active_prior = active_prior)
  mod_report_server("report",           shared = shared, active_prior = active_prior)
}
