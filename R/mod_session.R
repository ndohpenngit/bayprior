#' Session menu (header)
#'
#' A "Session" drop-down in the app header, beside the theme toggle. It is the
#' single place to save the current work to a \code{.rds} file, to load one
#' back and to start a new session, so it is available from every tab.
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

      # The explanation of each item is its hover text (title) on the <li>, so it
      # still shows while a disabled link ignores the pointer.
      tags$li(role = "none", id = ns("save_li"),
        title = "Download the current work as a .rds file",
        shiny::downloadLink(
          ns("save"),
          label = tags$span(class = "bp-session-item", icon("download"), " Save session"),
          class = "bp-session-link", role = "menuitem"
        )
      ),
      tags$li(role = "none",
        title = "Choose a saved .rds file; restores the prior, expert pool and report details",
        tags$a(
          href    = "#", class = "bp-session-link", role = "menuitem",
          onclick = "bpSessionLoad(); return false;",
          tags$span(class = "bp-session-item", icon("folder-open"), shiny::HTML(" Load session&hellip;"))
        )
      ),
      tags$li(role = "separator", class = "divider"),
      tags$li(role = "none",
        title = "Clear the priors, expert pool, analyses and report details",
        tags$a(
          href = "#", id = ns("new"), class = "bp-session-link action-button",
          role = "menuitem",
          tags$span(class = "bp-session-item", icon("rotate-left"), shiny::HTML(" New session&hellip;"))
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
#' @param reload Function that restarts the app session; defaults to
#'   \code{session$reload()}. Only replaced in tests.
#' @noRd
mod_session_server <- function(id, shared, report_meta = NULL, restore_meta = NULL,
                               reload = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    has_prior <- shiny::reactive({
      !is.null(shared$consensus %||% shared$base_prior %||% shared$current_prior)
    })

    shiny::observe({
      ok <- has_prior()
      shinyjs::toggleState("save", condition = ok)
      tip <- if (ok) "Download the current work as a .rds file"
             else "Nothing to save yet: fit or elicit a prior first"
      shinyjs::runjs(sprintf(
        "document.getElementById('%s').setAttribute('title', '%s');",
        session$ns("save_li"), tip))
    })

    write_session <- function(file) {
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
    session_filename <- function() paste0("bayprior_session_", Sys.Date(), ".rds")

    output$save <- shiny::downloadHandler(
      filename = session_filename,
      content  = function(file) {
        shiny::req(has_prior())
        write_session(file)
      }
    )
    # Same file, offered inside the "New session" dialog.
    output$save_first <- shiny::downloadHandler(
      filename = session_filename,
      content  = function(file) {
        shiny::req(has_prior())
        write_session(file)
      }
    )

    # -- New session: a reload clears shared and module-local state alike -------
    do_reload <- reload %||% function() session$reload()

    has_work <- shiny::reactive({
      has_prior() || length(shared$expert_pool) > 0L || !is.null(shared$map_prior)
    })

    shiny::observeEvent(input$new, {
      if (!has_work()) return(do_reload())
      shiny::showModal(shiny::modalDialog(
        title = "Start a new session?",
        "This clears the priors, expert pool, analyses and report details. ",
        "Anything not saved will be lost.",
        footer = tagList(
          shiny::modalButton("Cancel"),
          if (has_prior())
            shiny::downloadButton(session$ns("save_first"), "Save session first",
                                  class = "btn-default"),
          shiny::actionButton(session$ns("confirm_new"), "Start new session",
                              class = "btn-warning")
        ),
        easyClose = TRUE
      ))
    })

    shiny::observeEvent(input$confirm_new, {
      shiny::removeModal()
      do_reload()
    })

    shiny::observeEvent(input$load_file, {
      shiny::req(shared)
      .bp_session_apply(.bp_session_read(input$load_file$datapath),
                        shared, restore_meta = restore_meta)
    })

    invisible(NULL)
  })
}