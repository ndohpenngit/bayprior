#' Application server (golem convention)
#'
#' Wires all module servers via a single shared \code{reactiveValues} object.
#' Modules read and write \code{shared} — no global state.
#'
#' @param input,output,session Internal shiny parameters.
#' @noRd
app_server <- function(input, output, session) {

  # ── Shared state ──────────────────────────────────────────────────────
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

  # ── Sidebar status ────────────────────────────────────────────────────
  output$sidebar_status <- shinydashboard::renderValueBox({
    p <- active_prior()
    if (is.null(p)) {
      shinydashboard::valueBox(
        "None", "No prior fitted",
        icon = icon("circle-xmark"), color = "red", width = 12)
    } else {
      shinydashboard::valueBox(
        toupper(p$dist),
        glue::glue("mean = {round(p$fit_summary$mean, 3)}"),
        icon = icon("circle-check"), color = "green", width = 12)
    }
  })

  # ── Module servers ────────────────────────────────────────────────────
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
