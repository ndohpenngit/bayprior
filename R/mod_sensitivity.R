#' @noRd
mod_sensitivity_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    shinydashboard::box(
      width = 4, status = "primary", solidHeader = TRUE,
      title = tagList(icon("chart-bar"), " Grid Settings"),
      uiOutput(ns("prior_banner")),
      tags$hr(),
      uiOutput(ns("param1_ui")),
      uiOutput(ns("param2_ui")),
      sliderInput(ns("grid_size"), "Grid points per axis", 5, 50, 20, 5),
      tags$hr(),
      numericInput(ns("threshold"), "Efficacy threshold (theta_0)", 0.30, step = 0.01),
      checkboxGroupInput(ns("targets"), "Compute for:",
        choices  = c("Posterior mean"        = "posterior_mean",
                     "Posterior SD"          = "posterior_sd",
                     "Pr(theta > threshold)" = "prob_efficacy"),
        selected = c("posterior_mean", "prob_efficacy")
      ),
      tags$hr(),
      actionButton(ns("run_btn"), "Run Sensitivity Analysis",
                   icon = icon("play"), class = "btn-primary btn-block")
    ),
    column(8,
      shinydashboard::box(
        width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
        title = tagList(icon("bar-chart-steps"), " Tornado plot"),
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("tornado_plot"), height = "220px"),
          color = "#1D9E75"
        )
      ),
      shinydashboard::box(
        width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
        title = tagList(icon("map"), " Influence heatmap"),
        uiOutput(ns("outcome_picker")),
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("influence_plot"), height = "300px"),
          color = "#1D9E75"
        )
      )
    )
  )
}

#' @noRd
mod_sensitivity_server <- function(id, shared, active_prior) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Helper: suppress the expected "different distribution families" warning
    # from mixture prior density computations so it never surfaces as a Shiny
    # orange warning banner.
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
               if (is.null(p)) icon("exclamation-triangle") else icon("check"), " ", msg)
    })

    # ── Reactive: resolve actual hyperparameter names from the active prior.
    # For mixture priors, use the dominant component's params. This ensures the
    # slider labels and param_grid names always match working_prior$params inside
    # sensitivity_grid(), avoiding positional remap messages.
    pnames <- reactive({
      p <- active_prior()
      if (is.null(p)) return(list(p1 = "param1", p2 = "param2"))

      working <- if (p$dist == "mixture") {
        p$components[[which.max(p$weights)]]
      } else {
        p
      }

      nms <- names(working$params)
      list(
        p1   = if (length(nms) >= 1) nms[[1]] else "param1",
        p2   = if (length(nms) >= 2) nms[[2]] else "param2",
        vals = working$params
      )
    })

    output$param1_ui <- renderUI({
      p  <- active_prior()
      nm <- pnames()$p1
      v  <- pnames()$vals[[nm]] %||% 1
      sliderInput(ns("p1_range"), glue::glue("Range for {nm}"),
        max(0.001, round(v * 0.2, 3)), round(v * 4, 2),
        c(round(v * 0.5, 2), round(v * 2, 2)), round(v * 0.05, 3))
    })

    output$param2_ui <- renderUI({
      p  <- active_prior()
      nm <- pnames()$p2
      v  <- pnames()$vals[[nm]] %||% 1
      sliderInput(ns("p2_range"), glue::glue("Range for {nm}"),
        max(0.001, round(v * 0.2, 3)), round(v * 4, 2),
        c(round(v * 0.5, 2), round(v * 2, 2)), round(v * 0.05, 3))
    })

    # ── Reset sliders when the prior's distribution family changes so the
    # ranges remain meaningful (e.g., switching from Beta [0,1] to Normal
    # where mu can be negative).
    observeEvent(active_prior()$dist, {
      p <- active_prior()
      req(!is.null(p))
      nm <- pnames()
      v1 <- nm$vals[[nm$p1]] %||% 1
      v2 <- nm$vals[[nm$p2]] %||% 1
      updateSliderInput(session, "p1_range",
        label = glue::glue("Range for {nm$p1}"),
        min   = max(0.001, round(v1 * 0.2, 3)),
        max   = round(v1 * 4, 2),
        value = c(round(v1 * 0.5, 2), round(v1 * 2, 2)))
      updateSliderInput(session, "p2_range",
        label = glue::glue("Range for {nm$p2}"),
        min   = max(0.001, round(v2 * 0.2, 3)),
        max   = round(v2 * 4, 2),
        value = c(round(v2 * 0.5, 2), round(v2 * 2, 2)))
    }, ignoreInit = TRUE)

    observeEvent(input$run_btn, {
      p <- active_prior(); req(p, input$p1_range, input$p2_range)
      cd <- shared$conflict
      data_sum <- if (!is.null(cd)) cd$data_summary else
        list(type = "continuous", x = p$fit_summary$mean,
             sd = p$fit_summary$sd, n = 50L)
      nm <- pnames()

      # Build param_grid with names matching working_prior$params — this
      # avoids the positional remap path in sensitivity_grid() entirely.
      pg <- setNames(
        list(
          seq(input$p1_range[1], input$p1_range[2], length.out = input$grid_size),
          seq(input$p2_range[1], input$p2_range[2], length.out = input$grid_size)
        ),
        c(nm$p1, nm$p2)
      )

      # Suppress the positional remap message — it is expected behaviour when
      # the prior changes mid-session and slider labels lag one render behind.
      res <- withCallingHandlers(
        tryCatch(
          sensitivity_grid(p, data_sum, pg,
                           target    = input$targets,
                           threshold = input$threshold),
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
    })

    output$outcome_picker <- renderUI({
      req(shared$sensitivity)
      shinyWidgets::radioGroupButtons(
        ns("outcome"), NULL, choices = shared$sensitivity$target,
        selected = shared$sensitivity$target[1], justified = TRUE, status = "info")
    })

    output$tornado_plot <- plotly::renderPlotly({
      req(shared$sensitivity)
      # Suppress mixture density warning — expected for mixed-family components
      gp <- .suppress_mix(plot_tornado(shared$sensitivity))
      plotly::ggplotly(gp) |>
        plotly::layout(paper_bgcolor = "rgba(0,0,0,0)",
                       plot_bgcolor  = "rgba(0,0,0,0)")
    })

    output$influence_plot <- plotly::renderPlotly({
      req(shared$sensitivity, input$outcome)
      gp <- .suppress_mix(
        plot_sensitivity(shared$sensitivity, target = input$outcome)
      )
      plotly::ggplotly(gp) |>
        plotly::layout(paper_bgcolor = "rgba(0,0,0,0)",
                       plot_bgcolor  = "rgba(0,0,0,0)")
    })
  })
}