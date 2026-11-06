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

      uiOutput(ns("design_ui")),
      # Prior-strength grid (default for beta, gamma and normal priors)
      conditionalPanel(
        condition = sprintf("input['%s'] === 'strength'", ns("grid_design")),
        sliderInput(ns("strength_range"),
                    "Prior strength (x elicited value)",
                    0.25, 4, c(0.5, 2), 0.05, ticks = FALSE),
        checkboxInput(ns("vary_mean"), "Also vary the prior mean", TRUE),
        conditionalPanel(
          condition = sprintf("input['%s']", ns("vary_mean")),
          sliderInput(ns("shift_sd"), "Prior mean shift (+/- prior SDs)",
                      0.25, 3, 1, 0.25, ticks = FALSE)
        )
      ),
      # Hyperparameter ranges (other families, or chosen explicitly)
      conditionalPanel(
        condition = sprintf("input['%s'] !== 'strength'", ns("grid_design")),
        uiOutput(ns("param1_ui")),
        uiOutput(ns("param2_ui"))
      ),
      sliderInput(ns("grid_size"), "Grid points per axis", 5, 50, 20, 5),
      uiOutput(ns("grid_note")),
      uiOutput(ns("scale_note")),
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
        uiOutput(ns("threshold_ui")),
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
      # Run, with a button that resets every setting above to its default
      tags$div(
        style = "display:flex; gap:8px; align-items:stretch;",
        tags$div(
          class = "btn-tip-wrap", style = "flex:1;",
          actionButton(ns("run_btn"), "Run Sensitivity Analysis",
                       icon = icon("play"), class = "btn-primary btn-block"),
          uiOutput(ns("run_tip"), inline = TRUE)
        ),
        actionButton(ns("reset_grid"), "Reset", icon = icon("undo"),
                     class = "btn-default",
                     title = "Reset all grid settings to their defaults")
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

    # Hover hint on the Run button only while there is no prior
    output$run_tip <- renderUI(.no_prior_tip(!is.null(active_prior())))

    output$prior_banner <- renderUI({
      p   <- active_prior()
      cls <- if (is.null(p)) "alert-warning" else "alert-success"
      msg <- if (is.null(p)) "No prior available." else
        glue::glue("{p$label} ({toupper(p$dist)})")
      tagList(
        tags$div(class = paste("alert", cls),
                 style = "font-size:12px; padding:6px;",
                 if (is.null(p)) icon("exclamation-triangle") else icon("check"),
                 " ", msg),
        if (!is.null(p) && identical(p$dist, "mixture"))
          tags$p(class = "bp-note",
                 icon("info-circle"),
                 " Mixture prior: the grid varies the parameters of a",
                 " single-distribution working approximation of the mixture,",
                 " not of each component.")
      )
    })

    # Single-family working prior (moment-matched for mixtures)
    working <- reactive({
      p <- active_prior()
      if (is.null(p)) return(NULL)
      tryCatch(suppressMessages(suppressWarnings(
        .mixture_working_prior(p, quiet = TRUE))), error = function(e) NULL)
    })

    design_ok   <- reactive(.design_supported(working()))
    design_mode <- reactive(
      if (isTRUE(design_ok()) && identical(input$grid_design, "strength"))
        "strength" else "hyper")

    # Strength grid for the current settings (pure arithmetic, cheap)
    strength_pg <- function() {
      n <- input$grid_size %||% 20L
      .strength_grid(working(),
                     scale    = input$strength_range %||% c(0.5, 2),
                     n_scale  = n,
                     shift_sd = if (isTRUE(input$vary_mean)) input$shift_sd %||% 1 else 0,
                     n_shift  = n)
    }

    output$design_ui <- renderUI({
      req(design_ok())
      shinyWidgets::radioGroupButtons(
        ns("grid_design"), "Grid design",
        choices  = c("Prior strength" = "strength", "Hyperparameters" = "hyper"),
        selected = "strength", justified = TRUE, status = "primary", size = "sm")
    })

    # What the grid covers, in prior terms (mean and effective sample size)
    output$grid_note <- renderUI({
      w <- working(); req(w, design_ok())
      txt <- if (identical(design_mode(), "strength")) {
        g <- strength_pg()
        .grid_note_text(w, g, attr(g, "design"), source = active_prior())
      } else {
        req(input$p1_range, input$p2_range)
        nm <- pnames()
        g  <- expand.grid(input$p1_range, input$p2_range)
        names(g) <- c(nm$p1, nm$p2)
        .grid_note_text(w, g, source = active_prior())
      }
      if (is.null(txt)) return(NULL)
      tags$p(class = "bp-note", icon("info-circle"), " ", txt)
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
      sp <- .sens_slider_spec(pnames()$vals[[nm]] %||% 1)
      sliderInput(ns("p1_range"), glue::glue("Range for {nm}"),
        sp$min, sp$max, sp$value, sp$step, ticks = FALSE)
    })

    output$param2_ui <- renderUI({
      nm <- pnames()$p2
      sp <- .sens_slider_spec(pnames()$vals[[nm]] %||% 1)
      sliderInput(ns("p2_range"), glue::glue("Range for {nm}"),
        sp$min, sp$max, sp$value, sp$step, ticks = FALSE)
    })

    observeEvent(active_prior()$dist, {
      p <- active_prior(); req(!is.null(p))
      nm <- pnames()
      v1 <- nm$vals[[nm$p1]] %||% 1
      v2 <- nm$vals[[nm$p2]] %||% 1

      # Guard: all slider values must be finite and value >= min (the shared
      # spec floors v at 0.01, which handles Normal means of 0 or negative values)
      .safe_slider <- function(session, id, label, v) {
        sp <- .sens_slider_spec(v)
        if (any(!is.finite(c(sp$min, sp$max, sp$value, sp$step))))
          return(invisible(NULL))
        updateSliderInput(session, id, label = label,
                          min = sp$min, max = sp$max, value = sp$value,
                          step = sp$step)
      }

      .safe_slider(session, "p1_range",
                   glue::glue("Range for {nm$p1}"), v1)
      .safe_slider(session, "p2_range",
                   glue::glue("Range for {nm$p2}"), v2)
    }, ignoreInit = TRUE)

    # -- Reset the grid settings to their defaults ------------------------------
    # The observed data are left alone. Changing the inputs clears stale results
    # through the observer further down.
    observeEvent(input$reset_grid, {
      if (isTRUE(design_ok()))
        shinyWidgets::updateRadioGroupButtons(session, "grid_design",
                                              selected = "strength")
      shiny::updateSliderInput(session, "strength_range", value = c(0.5, 2))
      shiny::updateCheckboxInput(session, "vary_mean", value = TRUE)
      shiny::updateSliderInput(session, "shift_sd", value = 1)
      shiny::updateSliderInput(session, "grid_size", value = 20)

      if (!is.null(active_prior())) {
        nm <- pnames()
        for (k in c("p1", "p2")) {
          sp <- .sens_slider_spec(nm$vals[[nm[[k]]]] %||% 1)
          if (all(is.finite(c(sp$min, sp$max, sp$value, sp$step))))
            shiny::updateSliderInput(session, paste0(k, "_range"), min = sp$min,
                              max = sp$max, value = sp$value, step = sp$step)
        }
      }
      shinyWidgets::updateRadioGroupButtons(session, "analysis_type",
                                            selected = "grid")
      shiny::updateNumericInput(session, "threshold", value = 0.30)
      shiny::updateCheckboxGroupInput(session, "targets",
                               selected = c("posterior_mean", "prob_efficacy"))
      shiny::updateSliderInput(session, "cri_level", value = 0.95)
    })

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
            class = "bp-datachip",
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

    # Observed data: from Conflict Diagnostics when available, otherwise this
    # tab's own inputs. The data type must suit the prior family.
    sens_data <- reactive({
      p <- active_prior(); req(p)
      cd <- shared$conflict
      if (!is.null(cd)) {
        cd$data_summary
      } else {
        switch(input$sens_data_type %||% "binary",
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
    })

    # Scale of a Normal prior meeting binary data ("log_odds", "proportion"),
    # or NA. With log-odds the posterior quantities are on the log-odds scale.
    bin_scale <- reactive({
      w <- working(); req(w)
      .binary_scale(w, sens_data())
    })
    on_logit <- reactive(identical(bin_scale(), "log_odds"))

    output$threshold_ui <- renderUI({
      keep <- isolate(input$threshold) %||% 0.30
      if (isTRUE(on_logit())) {
        numericInput(session$ns("threshold"),
                     "Response-rate threshold (used as log-odds)",
                     keep, min = 0.001, max = 0.999, step = 0.01)
      } else {
        numericInput(session$ns("threshold"), "Efficacy threshold (theta_0)",
                     keep, step = 0.01)
      }
    })

    output$scale_note <- renderUI({
      sc <- tryCatch(bin_scale(), error = function(e) NA_character_)
      if (is.na(sc)) return(NULL)
      txt <- if (identical(sc, "log_odds"))
        paste0("The Normal prior is read on the log-odds scale, so posterior",
               " means and SDs below are log-odds. The threshold is a",
               " response rate and is converted with the logit. The scale can",
               " be changed on the Conflict Diagnostics tab.")
      else
        paste0("The Normal prior is read as a prior on the proportion itself.",
               " If it is on the log-odds scale, choose that on the Conflict",
               " Diagnostics tab and rerun.")
      tags$p(class = "bp-note", icon("info-circle"), " ", txt)
    })

    # Reset results whenever ANY input changes -- prevents stale results
    observeEvent(
      list(active_prior(), input$p1_range, input$p2_range,
           input$grid_design, input$strength_range, input$vary_mean,
           input$shift_sd,
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
      p <- active_prior(); req(p)
      if (identical(design_mode(), "hyper")) req(input$p1_range, input$p2_range)

      # Compatibility check -- block if error-level incompatibility
      compat <- .check_sensitivity_compat(p)
      if (!compat$ok) {
        showNotification(compat$msg, type = "error", duration = 10)
        return(invisible(NULL))
      }

      data_sum <- sens_data()
      nm <- pnames()
      pg <- if (identical(design_mode(), "strength")) {
        strength_pg()
      } else {
        setNames(
          list(
            seq(input$p1_range[1], input$p1_range[2], length.out = input$grid_size),
            seq(input$p2_range[1], input$p2_range[2], length.out = input$grid_size)
          ),
          c(nm$p1, nm$p2)
        )
      }

      # With a log-odds reading the threshold is entered as a response rate
      thr <- input$threshold
      if (isTRUE(on_logit())) {
        if (is.null(thr) || !is.finite(thr) || thr <= 0 || thr >= 1) {
          showNotification("Enter a response-rate threshold between 0 and 1.",
                           type = "error")
          return(invisible(NULL))
        }
        thr <- stats::qlogis(thr)
      }

      run_fn <- if (isTRUE(input$analysis_type == "cri")) {
        function() sensitivity_cri(p, data_sum, pg,
                                   cri_level = input$cri_level %||% 0.95)
      } else {
        function() sensitivity_grid(p, data_sum, pg,
                                    target    = input$targets,
                                    threshold = thr)
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
      if (!is.null(res)) attr(res, "bp_args") <- list(data_summary = data_sum)
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
            plotly::plotlyOutput(ns("tornado_plot"), height = "260px"),
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
            plotly::plotlyOutput(ns("influence_plot"), height = "340px"),
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
      .suppress_mix(plot_tornado(shared$sensitivity, interactive = TRUE)) |>
        .apply_plotly_theme()
    })

    output$influence_plot <- plotly::renderPlotly({
      req(shared$sensitivity, input$outcome)
      .suppress_mix(plot_sensitivity(shared$sensitivity, target = input$outcome,
                                     interactive = TRUE)) |>
        .apply_plotly_theme()
    })
  })
}