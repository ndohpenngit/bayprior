#' Application UI (golem convention)
#'
#' Builds the shinydashboard shell.  Every module gets its own
#' \code{tabItem}; navigation is driven by \code{sidebarMenu}.
#'
#' @param request Internal shiny request object.
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),

    shinydashboard::dashboardPage(
      skin  = "blue",
      title = "bayprior",

      # ── Header ────────────────────────────────────────────────────────
      shinydashboard::dashboardHeader(
        title = tags$span(
          tags$strong("bay", style = "color:#fff;"),
          tags$span("prior", style = "color:#A8CFED;"),
          style = "font-size:20px; letter-spacing:-0.5px;"
        ),
        titleWidth = 240
      ),

      # ── Sidebar ───────────────────────────────────────────────────────
      shinydashboard::dashboardSidebar(
        width = 240,
        shinydashboard::sidebarMenu(
          id = "sidebar_menu",

          shinydashboard::menuItem(
            "Welcome",
            tabName = "tab_welcome",
            icon    = icon("house")
          ),

          shinydashboard::menuItem(
            "Prior Elicitation",
            icon          = icon("sliders"),
            startExpanded = FALSE,
            shinydashboard::menuSubItem(
              "Quantile / Moment", tabName = "tab_elicit",  icon = icon("chart-line")),
            shinydashboard::menuSubItem(
              "Roulette method",   tabName = "tab_roulette", icon = icon("th"))
          ),

          shinydashboard::menuItem(
            "Expert Pooling",
            tabName = "tab_pooling",
            icon    = icon("users")
          ),

          shinydashboard::menuItem(
            "Conflict Diagnostics",
            icon          = icon("triangle-exclamation"),
            startExpanded = FALSE,
            shinydashboard::menuSubItem(
              "Univariate check",       tabName = "tab_conflict", icon = icon("vial")),
            shinydashboard::menuSubItem(
              "Multivariate (Mahal.)",  tabName = "tab_mahal",    icon = icon("border-all"))
          ),

          shinydashboard::menuItem(
            "Sensitivity Analysis",
            tabName = "tab_sensitivity",
            icon    = icon("chart-bar")
          ),

          shinydashboard::menuItem(
            "Robust Priors",
            icon          = icon("shield"),
            startExpanded = FALSE,
            shinydashboard::menuSubItem(
              "Sceptical prior", tabName = "tab_sceptical", icon = icon("scale-balanced")),
            shinydashboard::menuSubItem(
              "Robust mixture",  tabName = "tab_robust",    icon = icon("layer-group")),
            shinydashboard::menuSubItem(
              "Power prior",     tabName = "tab_power",     icon = icon("bolt"))
          ),

          shinydashboard::menuItem(
            "Export Report",
            tabName = "tab_report",
            icon    = icon("file-export")
          ),

          tags$hr(style = "border-color:#3c4a5e; margin:8px 0;"),
          tags$div(
            style = "padding:0 12px; font-size:11px; color:#adb5bd;",
            tags$p("Active prior:", style = "margin:0 0 4px;"),
            shinydashboard::valueBoxOutput("sidebar_status", width = 12)
          )
        )
      ),

      # ── Body ──────────────────────────────────────────────────────────
      shinydashboard::dashboardBody(
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css")
        ),

        shinydashboard::tabItems(
          shinydashboard::tabItem("tab_welcome",     mod_welcome_ui("welcome")),
          shinydashboard::tabItem("tab_elicit",      mod_elicitation_ui("elicitation")),
          shinydashboard::tabItem("tab_roulette",    mod_roulette_ui("roulette")),
          shinydashboard::tabItem("tab_pooling",     mod_pooling_ui("pooling")),
          shinydashboard::tabItem("tab_conflict",    mod_conflict_ui("conflict")),
          shinydashboard::tabItem("tab_mahal",       mod_mahal_ui("mahal")),
          shinydashboard::tabItem("tab_sensitivity", mod_sensitivity_ui("sensitivity")),
          shinydashboard::tabItem("tab_sceptical",   mod_sceptical_ui("sceptical")),
          shinydashboard::tabItem("tab_robust",      mod_robust_ui("robust")),
          shinydashboard::tabItem("tab_power",       mod_power_ui("power")),
          shinydashboard::tabItem("tab_report",      mod_report_ui("report"))
        )
      )
    )
  )
}

#' Bundle external resources (golem convention)
#' @noRd
golem_add_external_resources <- function() {
  addResourcePath("www", app_sys("app/www"))
  tags$head(
    golem::favicon(ico = "favicon"),
    golem::bundle_resources(path = app_sys("app/www"), app_title = "bayprior")
  )
}
