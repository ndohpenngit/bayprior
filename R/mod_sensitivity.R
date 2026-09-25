#' @noRd
mod_sensitivity_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    shinydashboard::box(
      width = 4, status = "primary", solidHeader = TRUE,
      title = tagList(icon("chart-bar"), " Grid Settings"),
      uiOutput(ns("prior_banner")),
      tags$hr(),
      uiOutput(ns("sens_compat_alert")),

      # -- Observed data (independent of conflict diagnostics) ----------------
      tags$p(tags$strong("Observed data"),
             style = "font-size:13px; margin-bottom:4px;"),
      uiOutput(ns("data_entry_ui")),
      tags$hr(),

      uiOutput(ns("param1_ui")),
      uiOutput(ns("param2_ui")),
      sliderInput(ns("grid_size"), "Grid points per axis", 5, 50, 20, 5),
      tags$hr(),
      shinyWidgets::radioGroupButtons(
        ns("analysis_type"), "Analysis type",
        choices  = c("Posterior quantities" = "grid",
                     "Credible interval"    = "cri"),
        selected = "grid", justified = TRUE, status = "primary"
      ),
      tags$br(),
      # Posterior quantity targets -- shown for grid analysis
      conditionalPanel(
        condition = sprintf("input['%s'] === 'grid'", ns("analysis_type")),
        numericInput(ns("threshold"), "Efficacy threshold (theta_0)",
                     0.30, step = 0.01),
        checkboxGroupInput(ns("targets"), "Compute for:",
          choices  = c("Posterior mean"        = "posterior_mean",
                       "Posterior SD"          = "posterior_sd",
                       "Pr(theta > threshold)" = "prob_efficacy"),
          selected = c("posterior_mean", "prob_efficacy")
        )
      ),
      # CrI options -- shown for CrI analysis
      conditionalPanel(
        condition = sprintf("input['%s'] === 'cri'", ns("analysis_type")),
        sliderInput(ns("cri_level"), "Credible interval level",
                    0.80, 0.99, 0.95, 0.01)
      ),
      tags$hr(),
      tags$div(
        class = "btn-tip-wrap",
        actionButton(ns("run_btn"), "Run Sensitivity Analysis",
                     icon = icon("play"), class = "btn-primary btn-block"),
        tags$span(class = "btn-tip-text", "Fit a prior in Prior Elicitation first")
      )
    ),
    column(8,
      uiOutput(ns("results_or_placeholder"))
    )
  )
}

#' @noRd
mod_sensitivity_server <- function(id, shared, active_prior) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Suppress mixture density warning
    .suppress_mix <- function(expr) {
      withCallingHandlers(expr, warning = function(w) {
        if (grepl("different distribution families", conditionMessage(w),
                  fixed = TRUE))
          invokeRestart("muffleWarning")
      })
    }

    output$prior_banner <- renderUI({
      p   <- active_prior()
      cls <- if (is.null(p)) "alert-warning" else "alert-success"
      msg <- if (is.null(p)) "No prior available." else
        glue::glue("{p$label} ({toupper(p$dist)})")
      tags$div(class = paste("alert", cls),
               style = "font-size:12px; padding:6px;",
               if (is.null(p)) icon("exclamation-triangle") else icon("check"),
               " ", msg)
    })

    # -- Sensitivity compatibility check --------------------------------------
    output$sens_compat_alert <- renderUI({
      p <- active_prior()
      if (is.null(p)) return(NULL)
      chk <- .check_sensitivity_compat(p)
      .validation_alert(chk$msg, chk$severity)
    })

    pnames <- reactive({
      p <- active_prior()
      if (is.null(p)) return(list(p1 = "param1", p2 = "param2"))
      working <- .mixture_working_prior(p, quiet = TRUE)
      nms <- names(working$params)
      list(
        p1   = if (length(nms) >= 1) nms[[1]] else "param1",
        p2   = if (length(nms) >= 2) nms[[2]] else nms[[1]],  # fallback to p1
        vals = working$params,
        single_param = length(nms) == 1  # Exponential has only 'rate'
      )
    })

    output$param1_ui <- renderUI({
      nm <- pnames()$p1
      v  <- max(0.01, abs(pnames()$vals[[nm]] %||% 1))  # abs+floor avoids 0/negative
      lo  <- max(0.001, round(v * 0.2, 3))
      hi  <- max(lo + 0.01, round(v * 4, 2))
      val <- c(max(lo, round(v * 0.5, 2)), min(hi, round(v * 2, 2)))
      sliderInput(ns("p1_range"), glue::glue("Range for {nm}"),
        lo, hi, val, max(0.001, round(v * 0.05, 3)))
    })

    output$param2_ui <- renderUI({
      nm <- pnames()$p2
      v  <- max(0.01, abs(pnames()$vals[[nm]] %||% 1))
      lo  <- max(0.001, round(v * 0.2, 3))
      hi  <- max(lo + 0.01, round(v * 4, 2))
      val <- c(max(lo, round(v * 0.5, 2)), min(hi, round(v * 2, 2)))
      sliderInput(ns("p2_range"), glue::glue("Range for {nm}"),
        lo, hi, val, max(0.001, round(v * 0.05, 3)))
    })

    observeEvent(active_prior()$dist, {
      p <- active_prior(); req(!is.null(p))
      nm <- pnames()
      v1 <- nm$vals[[nm$p1]] %||% 1
      v2 <- nm$vals[[nm$p2]] %||% 1

      # Guard: ensure all slider values are finite, non-NULL, and value >= min
      # abs() + floor of 0.01 handles Normal priors with mean = 0 or
      # any parameter that is zero/negative (which breaks the multiplier logic)
      .safe_slider <- function(session, id, label, v) {
        v   <- max(0.01, abs(v))   # same floor as renderUI definitions
        lo  <- max(0.001, round(v * 0.2, 3))
        hi  <- max(lo + 0.01, round(v * 4, 2))
        val <- c(max(lo, round(v * 0.5, 2)), min(hi, round(v * 2, 2)))
        stp <- max(0.001, round(v * 0.05, 3))
        if (any(is.null(c(lo, hi, val, stp))) ||
            any(is.na(c(lo, hi, val, stp))) ||
            any(!is.finite(c(lo, hi, val, stp)))) return(invisible(NULL))
        updateSliderInput(session, id, label = label,
                          min = lo, max = hi, value = val, step = stp)
      }

      .safe_slider(session, "p1_range",
                   glue::glue("Range for {nm$p1}"), v1)
      .safe_slider(session, "p2_range",
                   glue::glue("Range for {nm$p2}"), v2)
    }, ignoreInit = TRUE)

    # -- Data entry UI -- independent of conflict diagnostics -----------------
    output$data_entry_ui <- renderUI({
      p  <- active_prior(); req(p)
      cd <- shared$conflict
      if (!is.null(cd)) {
        ds <- cd$data_summary
        summary_text <- if (identical(ds$type, "binary")) {
          sprintf("Binary: x = %s, n = %s", ds$x, ds$n)
        } else {
          sprintf("Continuous: mean = %s, SD = %s, n = %s", ds$x, ds$sd, ds$n)
        }
        tags$div(
          tags$small(style = "color:#888; font-style:italic;",
                     "Using data from Conflict Diagnostics:"),
          tags$div(
            style = "background:#f8f9fa; border-radius:4px; padding:6px 10px; margin-top:4px;",
            tags$small(summary_text)
          )
        )
      } else {
        is_beta <- identical(p$dist, "beta") ||
          (identical(p$dist, "mixture") &&
             identical(p$components[[1]]$dist, "beta"))
        tagList(
          tags$small(style = "color:#888; font-style:italic;",
                     "Enter observed data for posterior computation:"),
          tags$br(),
          selectInput(ns("sens_data_type"), "Data type",
            choices  = c(
              "Binary (events/n)"                = "binary",
              "Continuous (mean/SD/n)"           = "continuous",
              "Count / Poisson (events/exposure)"= "poisson",
              "Survival (events/follow-up)"      = "survival"
            ),
            selected = if (is_beta) "binary" else "continuous"
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] === 'binary'", ns("sens_data_type")),
            fluidRow(
              column(6, numericInput(ns("sens_x"), "Events (x)", 15, min = 0, step = 1)),
              column(6, numericInput(ns("sens_n"), "Total (n)",  50, min = 1, step = 1))
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] === 'continuous'", ns("sens_data_type")),
            numericInput(ns("sens_mean"), "Observed mean", 0,   step = 0.01),
            numericInput(ns("sens_sd"),   "Observed SD",   1,   min = 0.001, step = 0.01),
            numericInput(ns("sens_n2"),   "Sample size n", 50L, min = 1, step = 1)
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] === 'poisson'", ns("sens_data_type")),
            numericInput(ns("sens_pois_x"), "Event count (x)", 12, min = 0, step = 1),
            numericInput(ns("sens_pois_n"), "Exposure (person-time)", 100, min = 0.001, step = 1)
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] === 'survival'", ns("sens_data_type")),
            numericInput(ns("sens_surv_x"), "Events (d)", 20, min = 0, step = 1),
            numericInput(ns("sens_surv_n"), "Total follow-up time", 400, min = 0.001, step = 1)
          )
        )
      }
    })

    # Reset results whenever ANY input changes -- prevents stale results
    observeEvent(
      list(active_prior(), input$p1_range, input$p2_range,
           input$grid_size, input$analysis_type,
           input$threshold, input$targets, input$cri_level,
           input$sens_data_type,
           input$sens_x, input$sens_n, input$sens_mean,
           input$sens_sd, input$sens_n2,
           input$sens_pois_x, input$sens_pois_n,
           input$sens_surv_x, input$sens_surv_n),
      { shared$sensitivity <- NULL },
      ignoreInit = TRUE
    )

    observeEvent(input$run_btn, {
      p <- active_prior(); req(p, input$p1_range, input$p2_range)

      # Compatibility check -- block if error-level incompatibility
      compat <- .check_sensitivity_compat(p)
      if (!compat$ok) {
        showNotification(compat$msg, type = "error", duration = 10)
        return(invisible(NULL))
      }

      cd <- shared$conflict

      # Build data_summary: prefer conflict diagnostics, fall back to own inputs.
      # Critically: match data type to prior family to avoid conjugate update failure.
      data_sum <- if (!is.null(cd)) {
        cd$data_summary
      } else {
        switch(input$sens_data_type,
          binary   = list(type = "binary",
                          x    = input$sens_x   %||% 15L,
                          n    = input$sens_n   %||% 50L),
          continuous = list(type = "continuous",
                          x    = input$sens_mean %||% unlist(p$fit_summary$mean),
                          sd   = input$sens_sd   %||% unlist(p$fit_summary$sd),
                          n    = as.integer(input$sens_n2 %||% 50L)),
          poisson  = list(type = "poisson",
                          x    = input$sens_pois_x %||% 12L,
                          n    = input$sens_pois_n %||% 100),
          survival = list(type = "survival",
                          x    = input$sens_surv_x %||% 20L,
                          n    = input$sens_surv_n %||% 400),
          list(type = "binary", x = 15L, n = 50L)
        )
      }
      nm <- pnames()
      pg <- setNames(
        list(
          seq(input$p1_range[1], input$p1_range[2], length.out = input$grid_size),
          seq(input$p2_range[1], input$p2_range[2], length.out = input$grid_size)
        ),
        c(nm$p1, nm$p2)
      )

      run_fn <- if (isTRUE(input$analysis_type == "cri")) {
        function() sensitivity_cri(p, data_sum, pg,
                                   cri_level = input$cri_level %||% 0.95)
      } else {
        function() sensitivity_grid(p, data_sum, pg,
                                    target    = input$targets,
                                    threshold = input$threshold)
      }

      res <- withCallingHandlers(
        tryCatch(run_fn(),
          error = function(e) {
            showNotification(paste("Error:", conditionMessage(e)), type = "error")
            NULL
          }
        ),
        message = function(m) {
          if (grepl("\\[bayprior\\]", conditionMessage(m)))
            invokeRestart("muffleMessage")
        }
      )
      shared$sensitivity <- res
      shinyjs::runjs("bpToast('Sensitivity analysis complete &#10003;', 'info', 3000);")
    })

    # -- Main output: placeholder before run, plots after --------------------
    output$results_or_placeholder <- renderUI({
      if (is.null(shared$sensitivity)) {
        return(
          tags$div(
            class = "text-center",
            style = paste0("padding: 60px 20px; color: #aaa;",
                           "border: 2px dashed #ddd; border-radius: 8px;",
                           "margin-top: 10px;"),
            icon("chart-bar", style = "font-size: 48px; margin-bottom: 16px;"),
            tags$h4("No sensitivity analysis run yet", style = "color: #bbb;"),
            tags$p("Configure the grid settings and click",
                   tags$b("Run Sensitivity Analysis"), "to see results.")
          )
        )
      }

      tagList(
        shinydashboard::box(
          width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
          title = tagList(
            icon("bar-chart-steps"), " Tornado plot"
          ),
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("tornado_plot"), height = "220px"),
            color = "#1D9E75"
          )
        ),
        shinydashboard::box(
          width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
          title = tagList(
            icon("map"), " Influence heatmap"
          ),
          uiOutput(ns("outcome_picker")),
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("influence_plot"), height = "300px"),
            color = "#1D9E75"
          )
        )
      )
    })

    output$outcome_picker <- renderUI({
      req(shared$sensitivity)
      raw_targets   <- shared$sensitivity$target
      pretty_labels <- vapply(raw_targets, .target_label, character(1))
      # named vector: label shown -> raw value sent to input$outcome
      choices <- setNames(raw_targets, pretty_labels)
      shinyWidgets::radioGroupButtons(
        ns("outcome"), NULL,
        choices  = choices,
        selected = raw_targets[1],
        justified = TRUE, status = "info")
    })

    output$tornado_plot <- plotly::renderPlotly({
      req(shared$sensitivity)
      gp <- .suppress_mix(plot_tornado(shared$sensitivity))
      plotly::ggplotly(gp) |> .apply_plotly_theme()
    })

    output$influence_plot <- plotly::renderPlotly({
      req(shared$sensitivity, input$outcome)
      gp <- .suppress_mix(
        plot_sensitivity(shared$sensitivity, target = input$outcome)
      )
      plotly::ggplotly(gp) |> .apply_plotly_theme()
    })
  })
}