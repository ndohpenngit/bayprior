#' @noRd
mod_report_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    shinydashboard::box(
      width = 5, status = "primary", solidHeader = TRUE,
      title = tagList(icon("file-lines"), " Trial Metadata"),
      uiOutput(ns("session_summary")),
      tags$hr(),
      textInput(ns("trial_name"),   "Trial / protocol number", placeholder = "TRIAL-001"),
      textInput(ns("indication"),   "Therapeutic indication",  placeholder = "e.g. NSCLC"),
      textInput(ns("sponsor"),      "Sponsor",                 placeholder = "BioPharma Ltd"),
      textInput(ns("statistician"), "Responsible statistician"),
      dateInput(ns("report_date"),  "Report date", value = Sys.Date()),
      textAreaInput(ns("notes"), "Notes / scientific rationale", rows = 4)
    ),
    column(7,
      shinydashboard::box(
        width = 12, status = "success", solidHeader = TRUE,
        title = tagList(icon("clipboard-check"), " Report contents"),
        uiOutput(ns("contents_checklist"))
      ),
      shinydashboard::box(
        width = 12, status = "success", solidHeader = TRUE,
        title = tagList(icon("file-export"), " Generate"),
        shinyWidgets::radioGroupButtons(
          ns("fmt"), "Output format",
          choices  = c("HTML" = "html", "PDF" = "pdf", "Word (.docx)" = "docx"),
          justified = TRUE, status = "primary"
        ),
        tags$br(),
        downloadButton(ns("dl_report"), "Download Prior Justification Report",
                       class = "btn-success btn-block btn-lg"),
        tags$hr(),
        downloadButton(ns("dl_rdata"), "Save session (.RData)",
                       class = "btn-default btn-block"),
        tags$br(),
        tags$small(class = "text-muted",
          icon("info-circle"), " Report follows FDA (2026) and EMA guidance on
          Bayesian prior documentation.")
      )
    )
  )
}

#' @noRd
mod_report_server <- function(id, shared, active_prior) {
  moduleServer(id, function(input, output, session) {

    output$session_summary <- renderUI({
      p <- active_prior()
      items <- list(
        .status_item(!is.null(p),
          if (!is.null(p)) glue::glue("Prior: {p$label} ({toupper(p$dist)})")
          else "No prior fitted"),
        .status_item(!is.null(shared$conflict),
          if (!is.null(shared$conflict))
            glue::glue("Conflict: severity = {toupper(shared$conflict$conflict_severity)}")
          else "Conflict diagnostics: not run"),
        .status_item(!is.null(shared$sensitivity),
          if (!is.null(shared$sensitivity))
            glue::glue("Sensitivity: {length(shared$sensitivity$target)} target(s)")
          else "Sensitivity analysis: not run")
      )
      tags$ul(class = "list-unstyled", style = "font-size:12px;", items)
    })

    output$contents_checklist <- renderUI({
      p <- active_prior()
      has_p <- !is.null(p)
      has_c <- !is.null(shared$conflict)
      has_s <- !is.null(shared$sensitivity)
      items <- list(
        .check_item(TRUE,  "Elicitation methodology section"),
        .check_item(has_p, "Fitted prior density + parameter table"),
        .check_item(has_c, "Prior-data conflict diagnostics"),
        .check_item(has_c, "Prior-Likelihood-Posterior overlay"),
        .check_item(has_s, "Sensitivity analysis heatmap"),
        .check_item(has_s, "Tornado plot"),
        .check_item(TRUE,  "FDA/EMA regulatory checklist"),
        .check_item(nzchar(isolate(input$notes) %||% ""), "Statistician's narrative")
      )
      tags$ul(class = "list-unstyled", style = "font-size:12px;", items)
    })

    output$dl_report <- downloadHandler(
      filename = function() {
        fmt  <- if (is.null(input$fmt) || !nzchar(input$fmt)) "html" else input$fmt
        stem <- gsub("[^A-Za-z0-9_-]", "_",
                     if (!is.null(input$trial_name) && nzchar(input$trial_name))
                       input$trial_name else "bayprior")
        ext  <- switch(fmt, html = ".html", pdf = ".pdf", docx = ".docx", ".html")
        paste0("prior_justification_", stem, "_", Sys.Date(), ext)
      },
      content = function(file) {
        fmt <- if (is.null(input$fmt) || !nzchar(input$fmt)) "html" else input$fmt
        p   <- active_prior()
        if (is.null(p)) {
          showNotification("No prior available - cannot generate report.",
                           type = "error")
          return(NULL)
        }

        # Capture ggplot objects from the current session so the report shows
        # exactly what the analyst configured, not a generic re-render.
        # Plotly widgets (renderPlotly) are NOT captured — the Rmd falls back
        # to calling the ggplot-returning variant of each plot function.
        prior_plot <- tryCatch(plot(p), error = function(e) NULL)

        overlay_plot <- if (!is.null(shared$conflict))
          tryCatch(
            plot_prior_likelihood(p, shared$conflict$data_summary,
                                  show_posterior = TRUE),
            error = function(e) NULL)
        else NULL

        tornado_plot <- if (!is.null(shared$sensitivity))
          tryCatch(plot_tornado(shared$sensitivity), error = function(e) NULL)
        else NULL

        heatmap_plot <- if (!is.null(shared$sensitivity))
          tryCatch(
            plot_sensitivity(shared$sensitivity,
                             target = shared$sensitivity$target[[1L]]),
            error = function(e) NULL)
        else NULL

        withProgress(message = "Rendering report...", value = 0.5, {
          prior_report(
            prior         = p,
            conflict      = shared$conflict,
            sensitivity   = shared$sensitivity,
            output_format = fmt,
            output_file   = tools::file_path_sans_ext(file),
            trial_name    = if (!is.null(input$trial_name)) input$trial_name else "",
            sponsor       = if (!is.null(input$sponsor))    input$sponsor    else "",
            author        = if (!is.null(input$statistician)) input$statistician else "",
            date          = as.character(
                              if (!is.null(input$report_date)) input$report_date
                              else Sys.Date()),
            notes         = if (!is.null(input$notes)) input$notes else "",
            prior_plot    = prior_plot,
            overlay_plot  = overlay_plot,
            tornado_plot  = tornado_plot,
            heatmap_plot  = heatmap_plot,
            open_after    = FALSE
          )
          setProgress(1)
        })
      }
    )

    output$dl_rdata <- downloadHandler(
      filename = function() paste0("bayprior_session_", Sys.Date(), ".RData"),
      content  = function(file) {
        session_data <- list(
          current_prior = shared$current_prior,
          expert_pool   = shared$expert_pool,
          consensus     = shared$consensus,
          conflict      = shared$conflict,
          sensitivity   = shared$sensitivity
        )
        save(session_data, file = file)
      }
    )
  })
}

# ── Helpers ───────────────────────────────────────────────────────────────────
.status_item <- function(ok, text) {
  ico <- if (ok) tags$span(style = "color:#1D9E75;", icon("check-circle"))
         else    tags$span(style = "color:#aaa;",    icon("dash-circle"))
  tags$li(ico, " ", text, style = "margin-bottom:3px;")
}

.check_item <- function(ok, text) {
  ico <- if (ok) tags$span(style = "color:#1D9E75;", icon("check-square"))
         else    tags$span(style = "color:#aaa;",    icon("square"))
  tags$li(ico, " ", text, style = "margin-bottom:3px;")
}