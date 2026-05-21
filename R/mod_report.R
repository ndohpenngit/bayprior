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
          # docx added alongside html and pdf
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
      p     <- active_prior()
      has_p <- !is.null(p)
      has_c <- !is.null(shared$conflict)
      has_s <- !is.null(shared$sensitivity)
      has_r <- !is.null(shared$robust_prior) ||
               !is.null(shared$sceptical_prior) ||
               !is.null(shared$power_prior)
      has_n <- nzchar(input$notes %||% "")

      .item <- function(done, label, optional = FALSE) {
        col   <- if (done) "#1D9E75" else if (optional) "#f0ad4e" else "#dc3545"
        ico   <- if (done) "circle-check" else if (optional) "circle-minus" else "circle-xmark"
        tip   <- if (done) "Complete" else if (optional) "Optional \u2014 not run" else "Required \u2014 not done"
        tags$li(
          style = "margin-bottom:5px; display:flex; align-items:center; gap:8px;",
          icon(ico, style = paste0("color:", col, "; font-size:13px;")),
          tags$span(style = paste0("font-size:12px; color:", if (done) "#333" else "#777", ";"),
                    label),
          tags$span(style = paste0("font-size:10px; color:", col, "; margin-left:auto;"),
                    tip)
        )
      }

      tagList(
        tags$ul(
          class = "list-unstyled",
          style = "margin:0;",
          .item(has_p,  "Prior fitted and density plotted"),
          .item(has_p,  "Expert / source identified"),
          .item(has_c,  "Prior-data conflict assessed",    optional = !has_c),
          .item(has_c,  "Prior-Likelihood-Posterior overlay", optional = !has_c),
          .item(has_s,  "Sensitivity analysis performed",  optional = !has_s),
          .item(has_s,  "Tornado plot and heatmap",        optional = !has_s),
          .item(has_r,  "Robust / sceptical prior computed", optional = !has_r),
          .item(has_n,  "Statistician's narrative notes",  optional = !has_n),
          .item(TRUE,   "FDA/EMA compliance checklist")
        ),
        tags$div(
          style = "margin-top:10px; padding:8px 10px; border-radius:4px; font-size:11px;",
          style = if (has_p)
            "background:#EFF8F0; border-left:3px solid #1D9E75; color:#1A7A4A;"
          else
            "background:#FFF3F3; border-left:3px solid #dc3545; color:#C0392B;",
          if (has_p)
            tagList(icon("circle-check"), " Ready to generate report")
          else
            tagList(icon("circle-xmark"), " Fit a prior before generating report")
        )
      )
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

        prior_plot   <- tryCatch(plot(p), error = function(e) NULL)
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

        # Capture robust / sceptical / power prior plots
        robust_plot   <- if (!is.null(shared$robust_prior))
          tryCatch(suppressWarnings(plot(shared$robust_prior)),
                   error = function(e) NULL) else NULL
        sceptical_plot <- if (!is.null(shared$sceptical_prior))
          tryCatch(plot(shared$sceptical_prior), error = function(e) NULL)
          else NULL
        power_plot    <- if (!is.null(shared$power_prior))
          tryCatch(plot(shared$power_prior), error = function(e) NULL)
          else NULL

        withProgress(message = "Rendering report...", value = 0.5, {
          prior_report(
            prior           = p,
            conflict        = shared$conflict,
            sensitivity     = shared$sensitivity,
            robust_prior    = shared$robust_prior,
            sceptical_prior = shared$sceptical_prior,
            power_prior     = shared$power_prior,
            output_format   = fmt,
            output_file     = tools::file_path_sans_ext(file),
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
            robust_plot   = robust_plot,
            sceptical_plot = sceptical_plot,
            power_plot    = power_plot,
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

# -- Helpers -------------------------------------------------------------------
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
