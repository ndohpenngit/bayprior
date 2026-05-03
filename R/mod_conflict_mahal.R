# ── Module: univariate conflict ───────────────────────────────────────────────

#' @noRd
mod_conflict_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    shinydashboard::box(
      width = 4, status = "primary", solidHeader = TRUE,
      title = tagList(icon("vial"), " Observed Data"),
      uiOutput(ns("prior_banner")),
      tags$hr(),
      selectInput(ns("data_type"), "Data type",
        choices = c("Binary (events / n)"     = "binary",
                    "Continuous (mean, SD, n)" = "continuous")),
      conditionalPanel(
        condition = sprintf("input['%s'] === 'binary'", ns("data_type")),
        numericInput(ns("bin_x"), "Events (x)", 14, 0),
        numericInput(ns("bin_n"), "Sample size (n)", 40, 1)
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] === 'continuous'", ns("data_type")),
        numericInput(ns("cont_mean"), "Observed mean", 0.45, step = 0.01),
        numericInput(ns("cont_sd"),   "Observed SD",   0.18, step = 0.01),
        numericInput(ns("cont_n"),    "Sample size (n)", 50, 1)
      ),
      tags$hr(),
      numericInput(ns("alpha"), "Significance level (alpha)",
                   0.05, 0.001, 0.2, 0.005),
      actionButton(ns("run_btn"), "Run Diagnostics",
                   icon = icon("stethoscope"), class = "btn-primary btn-block")
    ),
    column(8,
      # Placeholder shown before analysis is run
      uiOutput(ns("results_or_placeholder"))
    )
  )
}

#' @noRd
mod_conflict_server <- function(id, shared, active_prior,
                                plotly_layout = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    output$prior_banner <- renderUI({
      p   <- active_prior()
      cls <- if (is.null(p)) "alert-warning" else "alert-success"
      msg <- if (is.null(p)) "No prior fitted yet." else
        glue::glue("{p$label} ({toupper(p$dist)})")
      tags$div(class = paste("alert", cls),
               style = "font-size:12px; padding:6px;",
               if (is.null(p)) icon("exclamation-triangle") else icon("check"),
               " ", msg)
    })

    data_sum <- reactive({
      if (input$data_type == "binary")
        list(type = "binary", x = input$bin_x, n = input$bin_n)
      else
        list(type = "continuous", x = input$cont_mean,
             sd = input$cont_sd, n = input$cont_n)
    })

    res <- reactiveVal(NULL)

    observeEvent(input$run_btn, {
      p <- active_prior(); req(p)
      r <- tryCatch(
        prior_conflict(p, data_sum(), alpha = input$alpha),
        error = function(e) {
          showNotification(paste("Error:", conditionMessage(e)), type = "error")
          NULL
        })
      res(r)
      shared$conflict <- r
    })

    # ── Single uiOutput: placeholder before run, full results after ──────────
    output$results_or_placeholder <- renderUI({
      if (is.null(res())) {
        # Placeholder — shown before "Run Diagnostics" is clicked
        return(
          tags$div(
            class = "text-center",
            style = paste0("padding: 60px 20px; color: #aaa;",
                           "border: 2px dashed #ddd; border-radius: 8px;",
                           "margin-top: 10px;"),
            icon("vial", style = "font-size: 48px; margin-bottom: 16px;"),
            tags$h4("No diagnostics run yet", style = "color: #bbb;"),
            tags$p("Enter observed data and click",
                   tags$b("Run Diagnostics"), "to see results.")
          )
        )
      }

      r <- res()
      ns <- session$ns

      tagList(
        # Value boxes
        fluidRow(
          shinydashboard::valueBox(
            round(r$box_pvalue, 4), "Box p-value",
            icon  = icon("vial"),
            color = if (r$conflict_flag) "red" else "green",
            width = 4
          ),
          shinydashboard::valueBox(
            round(r$surprise_index, 3), "Surprise index",
            icon  = icon("bolt"), color = "yellow", width = 4
          ),
          shinydashboard::valueBox(
            round(r$overlap, 3), "Overlap coeff.",
            icon  = icon("circle-half-stroke"), color = "blue", width = 4
          )
        ),
        # Alert
        tags$div(
          class = if (r$conflict_flag) "alert alert-danger" else "alert alert-success",
          style = "margin:10px 0;",
          if (r$conflict_flag) icon("triangle-exclamation")
          else icon("circle-check"),
          " ",
          tags$strong(
            glue::glue("Severity: {toupper(r$conflict_severity)}. ")),
          r$recommendation
        ),
        # Overlay plot box
        shinydashboard::box(
          width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
          title = tagList(icon("chart-area"),
                          " Prior - Likelihood - Posterior overlay"),
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("overlay_plot"), height = "300px"),
            color = "#1D9E75"
          )
        )
      )
    })

    output$overlay_plot <- plotly::renderPlotly({
      req(res(), active_prior())
      gp <- withCallingHandlers(
        plot_prior_likelihood(active_prior(), data_sum(), show_posterior = TRUE),
        warning = function(w) {
          if (grepl("different distribution families", conditionMessage(w),
                    fixed = TRUE))
            invokeRestart("muffleWarning")
        }
      )
      plotly::ggplotly(gp) |> .apply_plotly_theme(plotly_layout())
    })
  })
}


# ── Module: multivariate Mahalanobis ─────────────────────────────────────────

#' @noRd
mod_mahal_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    shinydashboard::box(
      width = 4, status = "primary", solidHeader = TRUE,
      title = tagList(icon("border-all"), " Multivariate Conflict Setup"),
      tags$small(class = "text-muted",
        "Two-endpoint check. Enter prior and observed-data parameters for
         each endpoint."),
      tags$br(), tags$br(),
      tags$b("Prior specification"),
      fluidRow(
        column(6, numericInput(ns("pm1"), "Mean - ep.1", 0.35, step = 0.01)),
        column(6, numericInput(ns("pm2"), "Mean - ep.2", 0.60, step = 0.01))
      ),
      fluidRow(
        column(6, numericInput(ns("pv1"), "Var - ep.1",  0.010, step = 0.001)),
        column(6, numericInput(ns("pv2"), "Var - ep.2",  0.015, step = 0.001))
      ),
      numericInput(ns("pcov"), "Covariance (off-diag)", 0.003, step = 0.001),
      tags$hr(),
      tags$b("Observed data"),
      fluidRow(
        column(6, numericInput(ns("om1"), "Mean - ep.1", 0.55, step = 0.01)),
        column(6, numericInput(ns("om2"), "Mean - ep.2", 0.58, step = 0.01))
      ),
      fluidRow(
        column(6, numericInput(ns("ov1"), "Var/n - ep.1", 0.0002, step = 0.00005)),
        column(6, numericInput(ns("ov2"), "Var/n - ep.2", 0.0002, step = 0.00005))
      ),
      numericInput(ns("ocov"), "Covariance/n", 0.00004, step = 0.000005),
      tags$hr(),
      textInput(ns("lbl1"), "Endpoint 1 label", "Response rate"),
      textInput(ns("lbl2"), "Endpoint 2 label", "OS rate"),
      numericInput(ns("alpha"), "Alpha", 0.05, step = 0.005),
      actionButton(ns("run_btn"), "Run Mahalanobis Check",
                   icon = icon("border-all"), class = "btn-primary btn-block")
    ),
    column(8,
      uiOutput(ns("results_or_placeholder"))
    )
  )
}

#' @noRd
mod_mahal_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    res <- reactiveVal(NULL)

    observeEvent(input$run_btn, {
      pm   <- c(input$pm1, input$pm2)
      pcov <- matrix(c(input$pv1, input$pcov, input$pcov, input$pv2), 2, 2)
      om   <- c(input$om1, input$om2)
      ocov <- matrix(c(input$ov1, input$ocov, input$ocov, input$ov2), 2, 2)
      r    <- tryCatch(
        conflict_mahalanobis(pm, pcov, om, ocov,
                             alpha  = input$alpha,
                             labels = c(input$lbl1, input$lbl2)),
        error = function(e) {
          showNotification(paste("Error:", conditionMessage(e)), type = "error")
          NULL
        })
      res(r)
    })

    output$results_or_placeholder <- renderUI({
      if (is.null(res())) {
        return(
          tags$div(
            class = "text-center",
            style = paste0("padding: 60px 20px; color: #aaa;",
                           "border: 2px dashed #ddd; border-radius: 8px;",
                           "margin-top: 10px;"),
            icon("border-all", style = "font-size: 48px; margin-bottom: 16px;"),
            tags$h4("No check run yet", style = "color: #bbb;"),
            tags$p("Enter prior and observed data, then click",
                   tags$b("Run Mahalanobis Check"), "to see results.")
          )
        )
      }

      r <- res()
      col <- if (r$conflict_flag) "red" else "green"

      tagList(
        fluidRow(
          shinydashboard::valueBox(
            round(r$mahal_distance, 3), "Mahalanobis D",
            icon = icon("ruler"), color = "blue", width = 4),
          shinydashboard::valueBox(
            round(r$pvalue, 4), "Chi-sq p-value",
            icon = icon("chart-pie"), color = col, width = 4),
          shinydashboard::valueBox(
            if (r$conflict_flag) "CONFLICT" else "OK", "Status",
            icon = icon("flag"), color = col, width = 4)
        ),
        shinydashboard::box(
          width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
          title = tagList(icon("table"), " Per-parameter marginal z-scores"),
          DT::dataTableOutput(session$ns("z_tbl"))
        ),
        tags$div(
          class = if (r$conflict_flag) "alert alert-danger" else "alert alert-success",
          style = "margin:10px 0;",
          if (r$conflict_flag) icon("triangle-exclamation") else icon("check"),
          " ", r$interpretation
        )
      )
    })

    output$z_tbl <- DT::renderDataTable({
      req(res())
      df <- data.frame(
        Endpoint     = res()$labels,
        `Marginal z` = round(res()$marginal_z, 3),
        check.names  = FALSE
      )
      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "t"), class = "compact stripe")
    })
  })
}
