#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),

    shinydashboard::dashboardPage(

      title = "Bayesian Prior Justification with bayprior",

      # ── Header ──────────────────────────────────────────────────────────────
      shinydashboard::dashboardHeader(
        title = tagList(
          tags$span(
            tags$b("bay"),
            tags$span("prior", style = "font-weight:300;"),
            style = "font-size:20px; letter-spacing:0.5px;"
          )
        ),

        # Theme toggle in the header navbar
        tags$li(
          class = "dropdown",
          style = "padding-top:2px;",
          tags$a(
            href        = "#",
            id          = "theme_toggle",
            class       = "theme-toggle-btn",
            onclick     = "toggleTheme(); return false;",
            title       = "Toggle dark / light mode",
            icon("moon"), " Dark"
          )
        )
      ),

      # ── Sidebar ─────────────────────────────────────────────────────────────
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          id = "sidebar_menu",
          shinydashboard::menuItem(
            "Welcome",
            tabName  = "welcome",
            icon     = icon("house")
          ),
          shinydashboard::menuItem(
            "Prior Elicitation",
            icon     = icon("pencil"),
            tabName  = "elicitation",
            shinydashboard::menuSubItem(
              "Parametric", tabName = "elicitation", icon = icon("circle")
            ),
            shinydashboard::menuSubItem(
              "Roulette",   tabName = "roulette",    icon = icon("th")
            )
          ),
          shinydashboard::menuItem(
            "Expert Pooling",
            tabName  = "pooling",
            icon     = icon("users")
          ),
          shinydashboard::menuItem(
            "Conflict Diagnostics",
            icon     = icon("vial"),
            tabName  = "conflict",
            shinydashboard::menuSubItem(
              "Univariate",    tabName = "conflict",  icon = icon("circle")
            ),
            shinydashboard::menuSubItem(
              "Mahalanobis",   tabName = "mahal",     icon = icon("border-all")
            )
          ),
          shinydashboard::menuItem(
            "Sensitivity Analysis",
            tabName  = "sensitivity",
            icon     = icon("chart-bar")
          ),
          shinydashboard::menuItem(
            "Robust Priors",
            icon     = icon("shield-halved"),
            tabName  = "robust",
            shinydashboard::menuSubItem(
              "Robust Mixture", tabName = "robust",    icon = icon("circle")
            ),
            shinydashboard::menuSubItem(
              "Sceptical",      tabName = "sceptical", icon = icon("scale-balanced")
            ),
            shinydashboard::menuSubItem(
              "Power Prior",    tabName = "power",     icon = icon("bolt")
            )
          ),
          shinydashboard::menuItem(
            "Export Report",
            tabName  = "report",
            icon     = icon("file-export")
          )
        ),

        # Active prior badge in sidebar footer
        tags$div(
          style = "position:absolute; bottom:0; width:100%; padding:4px 0;",
          tags$div(
            style = paste0(
              "font-size:10px; color:#aaa; text-transform:uppercase;",
              "letter-spacing:0.8px; padding:0 12px 2px;"
            ),
            "Active prior:"
          ),
          uiOutput("sidebar_prior_badge")
        )
      ),

      # ── Body ─────────────────────────────────────────────────────────────────
      shinydashboard::dashboardBody(

        # Theme toggle JS — plots always rendered dark from R via .bayprior_gg_theme()
        # Light mode uses Plotly.relayout() to switch colours in the browser
        tags$head(
          tags$script(HTML("
            function isDark() {
              return document.body.classList.contains('dark-mode');
            }

            function relayoutPlotly(dark) {
              var bg   = '#ffffff';
              var fg   = dark ? '#e0e0e0' : '#444444';
              var grid = dark ? '#2a3f6f' : '#eeeeee';
              var lbg  = dark ? '#0f3460' : '#f8f8f8';
              document.querySelectorAll('.js-plotly-plot').forEach(function(el) {
                try {
                  Plotly.relayout(el, {
                    paper_bgcolor: bg, plot_bgcolor: bg,
                    'font.color': fg,
                    'xaxis.gridcolor': grid, 'xaxis.linecolor': grid,
                    'xaxis.zerolinecolor': grid, 'xaxis.tickfont.color': fg,
                    'xaxis.titlefont.color': fg,
                    'yaxis.gridcolor': grid, 'yaxis.linecolor': grid,
                    'yaxis.zerolinecolor': grid, 'yaxis.tickfont.color': fg,
                    'yaxis.titlefont.color': fg,
                    'legend.bgcolor': lbg, 'legend.font.color': fg
                  });
                } catch(e) {}
              });
            }

            function toggleTheme() {
              var body = document.body;
              var btn  = document.getElementById('theme_toggle');
              if (isDark()) {
                body.classList.remove('dark-mode');
                localStorage.setItem('bayprior_theme', 'light');
                btn.innerHTML = '<i class=\"fa fa-moon\"></i> Dark';
                relayoutPlotly(false);
                if (window.Shiny) Shiny.setInputValue('app_theme', 'light', {priority:'event'});
              } else {
                body.classList.add('dark-mode');
                localStorage.setItem('bayprior_theme', 'dark');
                btn.innerHTML = '<i class=\"fa fa-sun\"></i> Light';
                if (window.Shiny) Shiny.setInputValue('app_theme', 'dark', {priority:'event'});
              }
            }

            document.addEventListener('DOMContentLoaded', function() {
              var saved = localStorage.getItem('bayprior_theme');
              var btn   = document.getElementById('theme_toggle');
              if (saved === 'dark') {
                document.body.classList.add('dark-mode');
                btn.innerHTML = '<i class=\"fa fa-sun\"></i> Light';
              }
            });

            document.addEventListener('shiny:connected', function() {
              if (window.Shiny)
                Shiny.setInputValue('app_theme', isDark() ? 'dark' : 'light', {priority:'event'});
            });

            // After each Shiny render, fix light mode (plots default dark from R)
            document.addEventListener('shiny:value', function() {
              setTimeout(function() {
                if (!isDark()) relayoutPlotly(false);
              }, 200);
            });
          "))
        ),

        shinyjs::useShinyjs(),

tags$style(HTML("
  body.dark-mode .js-plotly-plot {
            background-color: #ffffff !important;
    filter: invert(1) hue-rotate(180deg);
  }
")),

        shinydashboard::tabItems(

          shinydashboard::tabItem(
            tabName = "welcome",
            mod_welcome_ui("welcome")
          ),
          shinydashboard::tabItem(
            tabName = "elicitation",
            mod_elicitation_ui("elicitation")
          ),
          shinydashboard::tabItem(
            tabName = "roulette",
            mod_roulette_ui("roulette")
          ),
          shinydashboard::tabItem(
            tabName = "pooling",
            mod_pooling_ui("pooling")
          ),
          shinydashboard::tabItem(
            tabName = "conflict",
            mod_conflict_ui("conflict")
          ),
          shinydashboard::tabItem(
            tabName = "mahal",
            mod_mahal_ui("mahal")
          ),
          shinydashboard::tabItem(
            tabName = "sensitivity",
            mod_sensitivity_ui("sensitivity")
          ),
          shinydashboard::tabItem(
            tabName = "robust",
            mod_robust_ui("robust")
          ),
          shinydashboard::tabItem(
            tabName = "sceptical",
            mod_sceptical_ui("sceptical")
          ),
          shinydashboard::tabItem(
            tabName = "power",
            mod_power_ui("power")
          ),
          shinydashboard::tabItem(
            tabName = "report",
            mod_report_ui("report")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' @noRd
golem_add_external_resources <- function() {
  addResourcePath("www", app_sys("app/www"))
  tags$head(
    golem::favicon(),
    tags$link(rel = "stylesheet", type = "text/css",
               href = "www/bayprior-dark.css")
  )
}
