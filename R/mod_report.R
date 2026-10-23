#' @noRd
mod_report_ui <- function(id) {
  ns <- NS(id)
  req_label <- function(text) tagList(text, tags$span(class = "bp-req", " *"))
  fluidRow(
    shinydashboard::box(
      width = 5, status = "primary", solidHeader = TRUE,
      title = tagList(icon("file-lines"), " Trial Metadata"),
      textInput(ns("trial_name"),   req_label("Trial / protocol number"),
                placeholder = "TRIAL-001"),
      textInput(ns("indication"),   req_label("Therapeutic indication"),
                placeholder = "e.g. NSCLC"),
      textInput(ns("sponsor"),      "Sponsor", placeholder = "BioPharma Ltd"),
      textInput(ns("statistician"), req_label("Responsible statistician")),
      dateInput(ns("report_date"),  "Report date", value = Sys.Date()),
      textAreaInput(ns("notes"), "Notes / scientific rationale", rows = 4),
      tags$p(class = "bp-note", "* Required for a complete record.")
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
                       class = "bp-btn-primary btn-block btn-lg"),
        tags$hr(),
        downloadButton(ns("dl_script"), "Export R script",
                       class = "bp-btn-secondary btn-block"),
        uiOutput(ns("script_hint")),
        tags$br(),
        tags$small(class = "text-muted",
          icon("info-circle"), " Report is structured with reference to the FDA's 2026 draft
          guidance and EMA's 2026 concept paper on Bayesian methods in
          clinical development.")
      )
    )
  )
}

#' @noRd
mod_report_server <- function(id, shared, active_prior) {
  moduleServer(id, function(input, output, session) {

    required_fields <- c(trial_name   = "trial / protocol number",
                         indication   = "therapeutic indication",
                         statistician = "responsible statistician")
    example_values  <- c("TRIAL-001", "BioPharma Ltd")

    missing_fields <- function() {
      filled <- vapply(names(required_fields), function(f) {
        nzchar(trimws(input[[f]] %||% ""))
      }, logical(1))
      unname(required_fields[!filled])
    }

    has_example_values <- function() {
      any(trimws(c(input$trial_name %||% "", input$sponsor %||% "")) %in%
            example_values)
    }

    output$contents_checklist <- renderUI({
      p     <- active_prior()
      has_p <- !is.null(p)
      has_c <- !is.null(shared$conflict)
      has_s <- !is.null(shared$sensitivity)
      has_r <- !is.null(shared$robust_prior) ||
               !is.null(shared$sceptical_prior) ||
               !is.null(shared$power_prior)
      has_map    <- has_p && identical(p$prior_type, "map")
      map_unused <- !is.null(shared$map_prior) && !has_map
      has_n <- nzchar(input$notes %||% "")

      spec <- list(
        list(has_p,   "Prior fitted and density plotted",      FALSE),
        list(has_p,   "Expert / source identified",            FALSE),
        list(has_map, "MAP prior derived from historical trials", TRUE),
        list(has_map, "Heterogeneity (tau) posterior plotted",    TRUE),
        list(has_c,   "Prior-data conflict assessed",          TRUE),
        list(has_c,   "Prior-Likelihood-Posterior overlay",    TRUE),
        list(has_s,   "Sensitivity analysis performed",        TRUE),
        list(has_s,   "Tornado plot and heatmap",              TRUE),
        list(has_r,   "Robust / sceptical prior computed",     TRUE),
        list(has_n,   "Statistician's narrative notes",        TRUE),
        list(TRUE,    "Guidance-alignment checklist (FDA/EMA)", FALSE)
      )

      item <- function(done, label, optional) {
        state <- if (done) "done" else if (optional) "optional" else "required"
        ico   <- switch(state, done = "circle-check", optional = "circle-minus",
                        required = "circle-xmark")
        tag   <- switch(state, done = "Complete", optional = "Not included",
                        required = "Required \u2014 not done")
        tags$li(class = paste0("bp-chk bp-chk-", state),
                icon(ico),
                tags$span(class = "bp-chk-label", label),
                tags$span(class = "bp-chk-tag", tag))
      }
      items <- lapply(spec, function(r) item(r[[1L]], r[[2L]], r[[3L]]))
      n_opt <- sum(vapply(spec, function(r) isTRUE(r[[3L]]) && !r[[1L]],
                          logical(1)))

      missing <- missing_fields()
      banner <- if (!has_p) {
        list("error", "circle-xmark", "Fit a prior before generating report")
      } else if (length(missing)) {
        list("warn", "circle-exclamation",
             paste0("Metadata incomplete: ", paste(missing, collapse = ", "),
                    ". The report can still be generated."))
      } else if (has_example_values()) {
        list("warn", "circle-exclamation",
             "Trial metadata contains example values; replace them before using the report.")
      } else {
        list("ok", "circle-check", "Ready to generate report")
      }

      tagList(
        if (has_p) {
          label <- p$label %||% "Prior"
          tags$p(class = "bp-note",
                 tags$strong("Prior documented: "),
                 paste0(label, " (", toupper(p$dist), ")",
                        if (has_map)
                          paste0(", derived from ", p$n_trials,
                                 " historical trial(s) (MAP)")))
        },
        if (map_unused) {
          tags$p(class = "bp-note",
                 icon("circle-info"),
                 paste0(" A MAP prior from ", shared$map_prior$n_trials,
                        " historical trial(s) exists but is not the active ",
                        "prior, so it is not included in the report."))
        },
        tags$ul(class = "list-unstyled", style = "margin:0;", items),
        if (n_opt > 0L) {
          tags$p(class = "bp-note", style = "margin-top:6px;",
                 paste0(n_opt, " optional section(s) not included."))
        },
        tags$div(class = paste0("bp-ready bp-ready-", banner[[1L]]),
                 icon(banner[[2L]]), tags$span(banner[[3L]]))
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
        if (length(missing_fields()))
          showNotification(
            paste0("Metadata incomplete: ",
                   paste(missing_fields(), collapse = ", "), "."),
            type = "warning", duration = 8)
        if (has_example_values())
          showNotification("Trial metadata contains example values.",
                           type = "warning", duration = 8)

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

        # MAP prior heterogeneity (tau) posterior plot -- only meaningful
        # when the active prior was itself derived via map_prior(); a MAP
        # prior sitting unused in shared$map_prior while a different prior
        # is active (e.g. after a later robust/sceptical step) should not
        # be plotted here, hence the check against the ACTIVE prior `p`,
        # not against shared$map_prior directly.
        tau_posterior_plot <- if (!is.null(p) && identical(p$prior_type, "map"))
          tryCatch(plot_tau_posterior(p), error = function(e) NULL)
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
            tau_posterior_plot = tau_posterior_plot,
            open_after    = FALSE
          )
          setProgress(1)
        })
        shared$report_exported <- TRUE
      }
    )

    script_info <- reactive({
      .bp_export_script(shared$consensus %||% shared$base_prior,
                       conflict    = shared$conflict,
                       sensitivity = shared$sensitivity,
                       robust      = shared$robust_prior,
                       sceptical   = shared$sceptical_prior,
                       power       = shared$power_prior,
                       active      = active_prior())
    })

    observe(shinyjs::toggleState("dl_script", condition = isTRUE(script_info()$ok)))

    output$script_hint <- renderUI({
      i <- script_info()
      if (!isTRUE(i$ok))
        tags$small(class = "text-muted", style = "display:block; margin-top:4px;", i$reason)
    })

    output$dl_script <- downloadHandler(
      filename = function() paste0("bayprior_analysis_", format(Sys.Date(), "%Y%m%d"), ".R"),
      content  = function(file) {
        i <- script_info()
        writeLines(if (isTRUE(i$ok)) i$text else paste0("# Not exported: ", i$reason),
                   file, useBytes = TRUE)
      }
    )

    # Used by the header Session menu (mod_session): read the form to save it,
    # refill it after a session is loaded.
    list(
      meta = shiny::reactive(list(
        trial_name   = input$trial_name,
        indication   = input$indication,
        sponsor      = input$sponsor,
        statistician = input$statistician,
        notes        = input$notes,
        report_date  = as.character(input$report_date)
      )),
      restore = function(m) {
        for (f in intersect(c("trial_name", "indication", "sponsor", "statistician"),
                            names(m)))
          shiny::updateTextInput(session, f, value = m[[f]])
        if (!is.null(m$notes))
          shiny::updateTextAreaInput(session, "notes", value = m$notes)
        if (!is.null(m$report_date))
          shiny::updateDateInput(session, "report_date", value = as.Date(m$report_date))
        invisible(NULL)
      }
    )
  })
}

# -- Helpers -------------------------------------------------------------------
.status_item <- function(ok, text) {
  ico <- if (ok) tags$span(style = "color:#1D9E75;", icon("circle-check"))
         else    tags$span(style = "color:#aaa;",    icon("circle-minus"))
  tags$li(ico, " ", text, style = "margin-bottom:3px;")
}

.check_item <- function(ok, text) {
  ico <- if (ok) tags$span(style = "color:#1D9E75;", icon("check-square"))
         else    tags$span(style = "color:#aaa;",    icon("square"))
  tags$li(ico, " ", text, style = "margin-bottom:3px;")
}