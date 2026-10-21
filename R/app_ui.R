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
            tagList("MAP Prior (Historical)", uiOutput("step_badge_map", inline = TRUE)),
            tabName  = "map_prior",
            icon     = icon("hospital")
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

        # Active prior badge + version footer
        tags$div(
          style = "position:absolute; bottom:0; width:100%; padding:4px 0;",
          tags$div(class = "bp-side-label", "Active prior"),
          uiOutput("sidebar_prior_badge"),
          tags$div(
            class = "bp-side-version",
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

            // Hover text: round long numbers, drop repeated lines (ggplotly repeats the
            // colour and fill mappings, and prints values like 0.5297278173).
            function bpTidyText(s) {
              if (typeof s !== 'string') return s;
              var seen = {}, out = [];
              s.split('<br />').forEach(function(l) {
                if (!seen[l]) { seen[l] = 1; out.push(l); }
              });
              return out.join('<br />').replace(
                /-?\\d+\\.\\d{5,}(?:e[+-]?\\d+)?|-?\\d+(?:\\.\\d+)?e[+-]?\\d+/gi,
                function(m) { return String(Number(parseFloat(m).toPrecision(3))); });
            }
            function bpTidyHover(el) {
              try {
                (el.data || []).forEach(function(t, i) {
                  var tx = t.text;
                  if (!tx) return;
                  var isArr = Array.isArray(tx);
                  var first = isArr ? tx[0] : tx;
                  if (typeof first !== 'string' || bpTidyText(first) === first) return;
                  Plotly.restyle(el, {text: [isArr ? tx.map(bpTidyText) : bpTidyText(tx)]}, [i]);
                });
              } catch (e) {}
            }

            function bpThemePlotly() {
              var dark = isDark();
              var mode = dark ? 'dark' : 'light';
              var c = dark
                ? {paper: 'rgba(0,0,0,0)', fg: '#E3E8F2', grid: '#2C3A5E', leg: 'rgba(0,0,0,0)'}
                : {paper: '#ffffff',       fg: '#444444', grid: '#eeeeee', leg: '#f8f8f8'};
              if (typeof Plotly === 'undefined') return;
              document.querySelectorAll('.js-plotly-plot').forEach(function(el) {
                // Re-theme straight after every draw (no visible light frame).
                if (!el.dataset.bpHook && typeof el.on === 'function') {
                  el.dataset.bpHook = '1';
                  el.on('plotly_afterplot', bpThemePlotly);
                }
                var fl = el._fullLayout;
                if (!fl || el.__bpBusy) return;
                // Inline !important beats any stylesheet, so a stale or cached dark
                // css that still inverts plots cannot turn the panel black.
                el.style.setProperty('filter', 'none', 'important');
                el.style.setProperty('background-color',
                                     dark ? 'transparent' : '#ffffff', 'important');
                // Done already? Compare with what the layout reported right after our
                // last pass (Plotly normalises colour strings, so never compare to c.paper).
                if (el.__bpMode === mode && el.__bpPaper === fl.paper_bgcolor) return;
                el.__bpBusy = true;       // our own relayout fires afterplot: ignore it
                var done = function() {
                  el.__bpMode = mode;
                  el.__bpPaper = el._fullLayout ? el._fullLayout.paper_bgcolor : null;
                  el.__bpBusy = false;
                };
                try {
                  bpTidyHover(el);
                  var u = {
                    paper_bgcolor: c.paper, plot_bgcolor: c.paper,
                    'font.color': c.fg, 'title.font.color': c.fg,
                    'legend.bgcolor': c.leg, 'legend.font.color': c.fg
                  };
                  ['xaxis', 'yaxis'].forEach(function(ax) {
                    if (!fl[ax]) return;
                    u[ax + '.gridcolor'] = c.grid; u[ax + '.linecolor'] = c.grid;
                    u[ax + '.zerolinecolor'] = c.grid; u[ax + '.tickfont.color'] = c.fg;
                    u[ax + '.title.font.color'] = c.fg;
                  });
                  (fl.annotations || []).forEach(function(a, i) {
                    u['annotations[' + i + '].font.color'] = c.fg;
                  });
                  // A horizontal legend at the top sat on top of the title: give the
                  // title its own band and put the legend just above the panel.
                  if (fl.legend && fl.legend.orientation === 'h' &&
                      fl.title && fl.title.text) {
                    u['legend.y'] = 1.02; u['legend.yanchor'] = 'bottom';
                    u['legend.x'] = 0; u['legend.xanchor'] = 'left';
                    u['margin.t'] = Math.max((fl.margin && fl.margin.t) || 0, 92);
                  }
                  var idx = [];
                  (el.data || []).forEach(function(t, i) {
                    if (t.marker && t.marker.colorbar) idx.push(i);
                  });
                  Plotly.relayout(el, u).then(function() {
                    if (!idx.length) return;
                    return Plotly.restyle(el, {
                      'marker.colorbar.tickfont.color': c.fg,
                      'marker.colorbar.title.font.color': c.fg
                    }, idx);
                  }).then(done, done);
                } catch (e) { done(); }
              });
            }
            // Plotly draws on its own surface. Dark mode re-colours it natively (transparent
            // panel, light text, navy grid) instead of inverting a white canvas to black.
            function relayoutPlotly() { bpThemePlotly(); }
            (function() {
              var t = null;
              function watch() {
                new MutationObserver(function() {
                  if (!t) t = requestAnimationFrame(function() { t = null; bpThemePlotly(); });
                }).observe(document.body, {childList: true, subtree: true});
              }
              if (document.body) watch(); else document.addEventListener('DOMContentLoaded', watch);
            })();

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
              bpThemePlotly();
              if (window.Shiny && typeof Shiny.setInputValue === 'function')
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

            // This script sits in <head>, where document.body does not exist
            // yet; touching body.classList there threw and aborted everything
            // below it (OS listener, label sync, app_theme input). Defer the
            // body-dependent set-up until the body is available.
            function bpThemeBoot() {
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
            }
            if (document.body) { bpThemeBoot(); }
            else { document.addEventListener('DOMContentLoaded', bpThemeBoot); }

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
              if (window.Shiny && typeof Shiny.setInputValue === 'function')
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
          /* Longer info-icon tooltips (valueBox metric explanations) reuse
             .btn-tip-wrap as their hover trigger, but need to wrap onto
             multiple lines within a fixed width instead of .btn-tip-text's
             single-line nowrap -- which, for a long sentence, stretched
             far past the viewport instead of just overflowing off-screen. */
          .btn-tip-wrap .info-tip-text {
            visibility: hidden; opacity: 0;
            background: #333; color: #fff;
            font-size: 11px; line-height: 1.4; text-align: left;
            border-radius: 4px; padding: 6px 10px;
            white-space: normal; width: 220px; max-width: 70vw;
            position: absolute; bottom: 125%; left: 50%;
            transform: translateX(-50%);
            transition: opacity 0.15s;
            z-index: 9999; pointer-events: none;
          }
          .btn-tip-wrap:hover .info-tip-text {
            visibility: visible; opacity: 1;
          }
          /* shinydashboard's valueBox() draws a large decorative icon
             (.icon-large) that, in a narrow box (these are width=3 of 12),
             visually and functionally overlaps the small info-icon placed
             in the title text -- and because .icon-large comes later in
             the DOM, it sits on top and swallows all pointer events there,
             so .btn-tip-wrap:hover never fires no matter how precisely the
             cursor is placed over the info icon. Confirmed via a headless
             browser: hovering the info icon times out with 'i.fa-dice ...
             subtree intercepts pointer events'. A purely decorative
             background icon should never block interaction with content
             drawn in front of it. */
          .small-box .icon-large,
          .small-box .icon-large-icon {
            pointer-events: none;
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
              t.innerHTML = (type === 'error' ? '<i class=\"fa fa-circle-xmark\"></i> ' :
                             type === 'warn'  ? '<i class=\"fa fa-triangle-exclamation\"></i> ' :
                                               '<i class=\"fa fa-circle-check\"></i> ') + msg;
              container.appendChild(t);
              setTimeout(function() {
                t.style.animation = 'bp-slide-out 0.25s ease forwards';
                setTimeout(function() { if (t.parentNode) t.parentNode.removeChild(t); },
                           280);
              }, duration || 3000);
            };
          })();

          // -- Clipboard helper -------------------------------------------

          // -- Plot save helper -------------------------------------------
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
            tabName = "map_prior",
            mod_map_prior_ui("map_prior")
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
    tags$link(rel = "icon", type = "image/png",
              href = "www/favicon.png"),
    tags$link(rel = "shortcut icon", type = "image/png",
              href = "www/favicon.png"),
    # Theme tokens first, dark-mode overrides second (same specificity rules,
    # so order matters).
    tags$link(rel = "stylesheet", type = "text/css",
              href = "www/custom.css"),
    tags$link(rel = "stylesheet", type = "text/css",
              href = "www/bp-live.css"),
    tags$link(rel = "stylesheet", type = "text/css",
              href = "www/bayprior-dark.css")
  )
}