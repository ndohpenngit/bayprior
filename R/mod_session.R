#' Session menu (header)
#'
#' A "Session" drop-down in the app header, beside the theme toggle. It is the
#' single place to save the current work to a \code{.rds} file and to load one
#' back, so it is available from every tab.
#'
#' Save writes everything held in the shared state plus the report form.
#' Load restores the priors, the expert pool and the report form (see
#' \code{.bp_session_restored}); analyses are saved but not restored.
#'
#' @param id Module id.
#' @return \code{mod_session_ui()}: an \code{<li class="dropdown">} for
#'   \code{dashboardHeader()}. \code{mod_session_server()}: invisible NULL.
#' @noRd
mod_session_ui <- function(id) {
  ns <- shiny::NS(id)

  tags$li(
    class = "dropdown bp-session",
    style = "padding-top:2px;",

    # Shared helper: opens the hidden picker. Also used by the Welcome page.
    shiny::singleton(tags$script(shiny::HTML("
      window.bpSessionLoad = function() {
        var f = document.querySelector('.bp-session-file input[type=file]');
        if (!f) return;
        f.value = '';   // allows loading the same file twice in a row
        f.click();
      };
    "))),

    tags$a(
      href            = "#",
      class           = "dropdown-toggle theme-toggle-btn bp-session-toggle",
      `data-toggle`   = "dropdown",
      role            = "button",
      `aria-haspopup` = "true",
      `aria-expanded` = "false",
      title           = "Save or load a session",
      icon("floppy-disk"), " Session ", tags$span(class = "caret")
    ),

    tags$ul(
      class = "dropdown-menu dropdown-menu-right bp-session-menu",
      role  = "menu",

      tags$li(role = "none",
        shiny::downloadLink(
          ns("save"),
          label = tagList(
            tags$span(class = "bp-session-item", icon("download"), " Save session"),
            tags$span(class = "bp-session-sub", shiny::textOutput(ns("save_sub"), inline = TRUE))
          ),
          class = "bp-session-link", role = "menuitem"
        )
      ),
      tags$li(role = "none",
        tags$a(
          href    = "#", class = "bp-session-link", role = "menuitem",
          onclick = "bpSessionLoad(); return false;",
          tags$span(class = "bp-session-item", icon("folder-open"), shiny::HTML(" Load session&hellip;")),
          tags$span(class = "bp-session-sub", "Choose a saved .rds file")
        )
      ),
      tags$li(role = "separator", class = "divider"),
      tags$li(role = "none", class = "bp-session-note",
        "Loading restores the prior, expert pool and report details. ",
        "Rerun the analyses afterwards."
      ),

      # Hidden picker; kept in the DOM so Shiny uploads the file as usual.
      tags$li(role = "none", class = "bp-session-file",
        shiny::fileInput(ns("load_file"), label = NULL, accept = ".rds",
                         multiple = FALSE)
      )
    )
  )
}

#' @param shared The app's \code{reactiveValues}.
#' @param report_meta Reactive returning the report form as a named list
#'   (\code{trial_name}, \code{indication}, \code{sponsor}, \code{statistician},
#'   \code{notes}, \code{report_date}); NULL to save no report details.
#' @param restore_meta Function taking such a list and refilling the report
#'   form; NULL to skip.
#' @noRd
mod_session_server <- function(id, shared, report_meta = NULL, restore_meta = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    has_prior <- shiny::reactive({
      !is.null(shared$consensus %||% shared$base_prior %||% shared$current_prior)
    })

    output$save_sub <- shiny::renderText(
      if (has_prior()) "Download as a .rds file" else "Fit or elicit a prior first"
    )

    shiny::observe({
      shinyjs::toggleState("save", condition = has_prior())
    })

    output$save <- shiny::downloadHandler(
      filename = function() paste0("bayprior_session_", Sys.Date(), ".rds"),
      content  = function(file) {
        shiny::req(has_prior())
        state <- list(
          current_prior   = shared$current_prior,
          base_prior      = shared$base_prior,
          expert_pool     = shared$expert_pool,
          consensus       = shared$consensus,
          conflict        = shared$conflict,
          sensitivity     = shared$sensitivity,
          robust_prior    = shared$robust_prior,
          sceptical_prior = shared$sceptical_prior,
          power_prior     = shared$power_prior,
          map_prior       = shared$map_prior
        )
        meta <- if (is.null(report_meta)) list() else shiny::isolate(report_meta())
        saveRDS(.bp_session_pack(state, meta), file)
      }
    )

    shiny::observeEvent(input$load_file, {
      shiny::req(shared)
      .bp_session_apply(.bp_session_read(input$load_file$datapath),
                        shared, restore_meta = restore_meta)
    })

    invisible(NULL)
  })
}