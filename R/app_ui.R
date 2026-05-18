#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),

    shinydashboard::dashboardPage(

      title = "Bayesian Prior Elicitation and Analysis",

      # -- Header --------------------------------------------------------------
      shinydashboard::dashboardHeader(
        title = tagList(
          tags$img(
            src    = "www/favicon.png",
            height = "44px",
            width  = "44px",
            style  = paste0(
              "margin-right:8px;",
              "margin-top:-3px;",
              "vertical-align:middle;",
              "border-radius:6px;",
                            "image-rendering:crisp-edges;"
            )
          ),
          tags$span(
            style = "font-size:20px; letter-spacing:0.5px; display:inline-flex; align-items:baseline;",
            tags$b("bay"),
            tags$span("prior", style = "font-weight:300;")
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

      # -- Sidebar -------------------------------------------------------------
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          id = "sidebar_menu",
          shinydashboard::menuItem(
            "Welcome",
            tabName  = "welcome",
            icon     = icon("house")
          ),
          shinydashboard::menuItem(
            tagList("Prior Elicitation", uiOutput("step_badge_elicit", inline = TRUE)),
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
            tagList("Expert Pooling", uiOutput("step_badge_pool", inline = TRUE)),
            tabName  = "pooling",
            icon     = icon("users")
          ),
          shinydashboard::menuItem(
            tagList("Conflict Diagnostics", uiOutput("step_badge_conflict", inline = TRUE)),
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
            tagList("Sensitivity Analysis", uiOutput("step_badge_sens", inline = TRUE)),
            tabName  = "sensitivity",
            icon     = icon("chart-bar")
          ),
          shinydashboard::menuItem(
            tagList("Robust Priors", uiOutput("step_badge_robust", inline = TRUE)),
            icon     = icon("shield-halved"),
            tabName  = "robust",
            shinydashboard::menuSubItem(
              "Robust Mixture", tabName = "robust",    icon = icon("layer-group")
            ),
            shinydashboard::menuSubItem(
              "Power Prior",    tabName = "power",     icon = icon("bolt")
            ),
            shinydashboard::menuSubItem(
              "Sceptical",      tabName = "sceptical", icon = icon("scale-balanced")
            )
          ),
          shinydashboard::menuItem(
            "Export Report",
            tabName  = "report",
            icon     = icon("file-export")
          )
        ),

        # Active prior badge + version footer
        tags$div(
          style = "position:absolute; bottom:0; width:100%; padding:4px 0;",
          tags$div(
            style = paste0(
              "font-size:10px; color:#aaa; text-transform:uppercase;",
              "letter-spacing:0.8px; padding:0 12px 2px;"
            ),
            "Active prior:"
          ),
          uiOutput("sidebar_prior_badge"),
          tags$div(
            style = paste0(
              "font-size:9px; color:#666; text-align:center;",
              "padding:4px 0 6px; border-top:1px solid #333; margin-top:4px;"
            ),
            paste0("bayprior v",
                   tryCatch(as.character(utils::packageVersion("bayprior")),
                            error = function(e) "dev"))
          )
        )
      ),

      # -- Body -----------------------------------------------------------------
      shinydashboard::dashboardBody(

        # Theme toggle JS
        tags$head(
          tags$script(HTML("
            function isDark() {
              return document.body.classList.contains('dark-mode');
            }

            // Restore Plotly axis/text colours when switching to light mode.
            function relayoutPlotly() {
              var fg   = '#444444';
              var grid = '#eeeeee';
              document.querySelectorAll('.js-plotly-plot').forEach(function(el) {
                try {
                  Plotly.relayout(el, {
                    paper_bgcolor: '#ffffff', plot_bgcolor: '#ffffff',
                    'font.color': fg,
                    'xaxis.gridcolor': grid, 'xaxis.linecolor': grid,
                    'xaxis.zerolinecolor': grid, 'xaxis.tickfont.color': fg,
                    'xaxis.titlefont.color': fg,
                    'yaxis.gridcolor': grid, 'yaxis.linecolor': grid,
                    'yaxis.zerolinecolor': grid, 'yaxis.tickfont.color': fg,
                    'yaxis.titlefont.color': fg,
                    'legend.bgcolor': '#f8f8f8', 'legend.font.color': fg
                  });
                } catch(e) {}
              });
            }

            // -- Three-state theme: auto | dark | light ---------------------
            // State is stored in localStorage:
            //   null / absent = Auto (follows OS prefers-color-scheme)
            //   'dark'        = Manual dark
            //   'light'       = Manual light
            //
            // Clicking the toggle cycles: Auto -> Dark -> Light -> Auto

            function getThemeState() {
              return localStorage.getItem('bayprior_theme') || 'auto';
            }

            function applyThemeState(state) {
              var btn = document.getElementById('theme_toggle');
              if (state === 'dark') {
                document.body.classList.add('dark-mode');
                if (btn) btn.innerHTML =
                  '<i class=\"fa fa-moon\"></i> Dark' +
                  '<span style=\"font-size:9px;opacity:0.6;margin-left:4px;\">(manual)</span>';
              } else if (state === 'light') {
                document.body.classList.remove('dark-mode');
                relayoutPlotly();
                if (btn) btn.innerHTML =
                  '<i class=\"fa fa-sun\"></i> Light' +
                  '<span style=\"font-size:9px;opacity:0.6;margin-left:4px;\">(manual)</span>';
              } else {
                // Auto -- follow OS
                var osDark = window.matchMedia &&
                             window.matchMedia('(prefers-color-scheme: dark)').matches;
                if (osDark) {
                  document.body.classList.add('dark-mode');
                } else {
                  document.body.classList.remove('dark-mode');
                  relayoutPlotly();
                }
                if (btn) btn.innerHTML =
                  '<i class=\"fa fa-circle-half-stroke\"></i> Auto' +
                  '<span style=\"font-size:9px;opacity:0.6;margin-left:4px;\">' +
                  (osDark ? '(dark)' : '(light)') + '</span>';
              }
              if (window.Shiny)
                Shiny.setInputValue('app_theme', isDark() ? 'dark' : 'light',
                                    {priority:'event'});
            }

            function toggleTheme() {
              var current = getThemeState();
              var next = current === 'auto'  ? 'dark'  :
                         current === 'dark'  ? 'light' : 'auto';
              if (next === 'auto') {
                localStorage.removeItem('bayprior_theme');
              } else {
                localStorage.setItem('bayprior_theme', next);
              }
              applyThemeState(next);
            }

            // -- Initialise on page load -------------------------------------
            // Migration: earlier two-state toggle saved 'light' on first
            // load without explicit user action. Clear stale preferences
            // from before the three-state system was introduced.
            (function() {
              var state   = localStorage.getItem('bayprior_theme');
              var version = localStorage.getItem('bayprior_theme_v');
              if (state && !version) {
                // Saved by old two-state toggle -- treat as auto
                localStorage.removeItem('bayprior_theme');
              }
              // Mark that we are using the versioned three-state system
              localStorage.setItem('bayprior_theme_v', '2');
              applyThemeState(getThemeState());
            })();

            // MutationObserver: AdminLTE sometimes resets body class list
            // during its own initialisation, stripping 'dark-mode'.
            // Watch for that and immediately re-apply the saved preference.
            (function() {
              var obs = new MutationObserver(function() {
                var desired = getThemeState();
                var hasDark = document.body.classList.contains('dark-mode');
                if (desired === 'dark' && !hasDark)
                  document.body.classList.add('dark-mode');
                else if (desired === 'light' && hasDark)
                  document.body.classList.remove('dark-mode');
                else if (desired === 'auto') {
                  var osDark = window.matchMedia &&
                               window.matchMedia('(prefers-color-scheme: dark)').matches;
                  if (osDark && !hasDark)
                    document.body.classList.add('dark-mode');
                  else if (!osDark && hasDark)
                    document.body.classList.remove('dark-mode');
                }
              });
              obs.observe(document.body, { attributes: true,
                                           attributeFilter: ['class'] });
              // Disconnect once Shiny is fully ready -- normal toggle takes over
              document.addEventListener('shiny:sessioninitialized', function() {
                obs.disconnect();
                applyThemeState(getThemeState()); // final apply
              }, { once: true });
            })();

            // -- React to OS changes at runtime ------------------------------
            // Only applies when in Auto mode (no manual preference saved)
            if (window.matchMedia) {
              window.matchMedia('(prefers-color-scheme: dark)')
                .addEventListener('change', function(e) {
                  if (getThemeState() === 'auto') {
                    applyThemeState('auto');
                  }
                });
            }

            // -- Set correct button label after Shiny renders the header -----
            document.addEventListener('shiny:sessioninitialized', function() {
              applyThemeState(getThemeState());
            });

            document.addEventListener('shiny:connected', function() {
              if (window.Shiny)
                Shiny.setInputValue('app_theme', isDark() ? 'dark' : 'light',
                                    {priority:'event'});
            });
          "))
        ),
        shinyjs::useShinyjs(),

        # -- Disabled button + tooltip styles ---------------------------------
        tags$style(HTML("
          .btn[disabled], .btn.disabled {
            opacity: 0.45 !important;
            cursor: not-allowed !important;
            pointer-events: auto !important;
          }
          .btn-tip-wrap {
            position: relative; display: block; width: 100%;
          }
          .btn-tip-wrap .btn-tip-text {
            visibility: hidden; opacity: 0;
            background: #333; color: #fff;
            font-size: 11px; border-radius: 4px;
            padding: 4px 8px; white-space: nowrap;
            position: absolute; bottom: 110%; left: 50%;
            transform: translateX(-50%);
            transition: opacity 0.15s;
            z-index: 9999; pointer-events: none;
          }
          .btn-tip-wrap:hover .btn-tip-text {
            visibility: visible; opacity: 1;
          }
        ")),


        # -- Toast notification + clipboard styles ----------------------------
        tags$style(HTML("
          /* Toast notifications */
          #bp-toast-container {
            position: fixed; top: 64px; right: 16px;
            z-index: 99999; display: flex; flex-direction: column; gap: 8px;
          }
          .bp-toast {
            background: #1D9E75; color: #fff;
            padding: 10px 16px; border-radius: 6px;
            font-size: 13px; font-family: Arial, sans-serif;
            box-shadow: 0 4px 12px rgba(0,0,0,0.25);
            display: flex; align-items: center; gap: 8px;
            animation: bp-slide-in 0.25s ease;
            max-width: 300px;
          }
          .bp-toast.bp-toast-error { background: #c0392b; }
          .bp-toast.bp-toast-warn  { background: #e08030; }
          @keyframes bp-slide-in {
            from { opacity: 0; transform: translateX(40px); }
            to   { opacity: 1; transform: translateX(0); }
          }
          @keyframes bp-slide-out {
            from { opacity: 1; transform: translateX(0); }
            to   { opacity: 0; transform: translateX(40px); }
          }
          /* Clipboard value box */
          .bp-clip-box {
            cursor: pointer; position: relative;
          }
          .bp-clip-box::after {
            content: '\\00a0\\1F4CB';
            font-size: 10px; opacity: 0.5;
            position: absolute; bottom: 6px; right: 8px;
          }
          .bp-clip-box:hover::after { opacity: 1; }
        ")),

        tags$script(HTML("
          // -- Toast system -----------------------------------------------
          (function() {
            function ensureContainer() {
              var c = document.getElementById('bp-toast-container');
              if (!c) {
                c = document.createElement('div');
                c.id = 'bp-toast-container';
                document.body.appendChild(c);
              }
              return c;
            }
            window.bpToast = function(msg, type, duration) {
              var container = ensureContainer();
              var t = document.createElement('div');
              t.className = 'bp-toast' +
                (type === 'error' ? ' bp-toast-error' :
                 type === 'warn'  ? ' bp-toast-warn'  : '');
              t.innerHTML = (type === 'error' ? '<i class=\'fa fa-circle-xmark\'></i> ' :
                             type === 'warn'  ? '<i class=\'fa fa-triangle-exclamation\'></i> ' :
                                               '<i class=\'fa fa-circle-check\'></i> ') + msg;
              container.appendChild(t);
              setTimeout(function() {
                t.style.animation = 'bp-slide-out 0.25s ease forwards';
                setTimeout(function() { if (t.parentNode) t.parentNode.removeChild(t); },
                           280);
              }, duration || 3000);
            };
          })();

          // -- Clipboard helper -------------------------------------------
          window.bpCopy = function(value, label) {
            var text = String(value);
            if (navigator.clipboard) {
              navigator.clipboard.writeText(text).then(function() {
                bpToast((label || 'Value') + ' copied: ' + text, 'info', 2000);
              });
            } else {
              // Fallback for non-HTTPS
              var ta = document.createElement('textarea');
              ta.value = text;
              document.body.appendChild(ta);
              ta.select();
              document.execCommand('copy');
              document.body.removeChild(ta);
              bpToast((label || 'Value') + ' copied: ' + text, 'info', 2000);
            }
          };

          // -- Plot save helper -------------------------------------------
          window.bpSavePlot = function(plotId, filename) {
            var el = document.getElementById(plotId);
            if (!el || !el.querySelector('.js-plotly-plot')) {
              bpToast('Plot not ready yet', 'warn', 2000);
              return;
            }
            var plt = el.querySelector('.js-plotly-plot');
            Plotly.downloadImage(plt, {
              format: 'png', width: 1200, height: 700,
              filename: filename || 'bayprior-plot'
            }).catch(function() {
              bpToast('Could not save plot', 'error', 3000);
            });
          };
        ")),

        # Inline dark mode Plotly fix -- invert filter at compositor level
        # This overrides Plotly's inline SVG styles which CSS selectors cannot touch
        tags$style(HTML("
          /* White background on the container div so the CSS filter has
             a solid surface to invert -- transparent SVG fill bleeds the
             dark box colour through, which inverts to lavender.
             White inverted = black = correct dark background. */
          body.dark-mode .js-plotly-plot {
            background-color: #ffffff !important;
            filter: invert(1) hue-rotate(180deg);
          }
          body.dark-mode .js-plotly-plot .modebar {
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
    golem::favicon(ext = "png"),
    tags$link(rel = "stylesheet", type = "text/css",
               href = "www/bayprior-dark.css")
  )
}
