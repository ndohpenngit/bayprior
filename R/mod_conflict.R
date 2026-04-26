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
        choices = c("Binary (events / n)" = "binary",
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
      fluidRow(
        shinydashboard::valueBoxOutput(ns("vb_boxp"),    width = 4),
        shinydashboard::valueBoxOutput(ns("vb_surprise"), width = 4),
        shinydashboard::valueBoxOutput(ns("vb_overlap"),  width = 4)
      ),
      uiOutput(ns("conflict_alert")),
      shinydashboard::box(
        width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
        title = tagList(icon("chart-area"), " Prior - Likelihood - Posterior overlay"),
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("overlay_plot"), height = "300px"),
          color = "#1D9E75"
        )
      )
    )
  )
}

#' @noRd
mod_conflict_server <- function(id, shared, active_prior) {
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
          showNotification(paste("Error:", conditionMessage(e)), type = "error"); NULL
        })
      res(r); shared$conflict <- r
    })

    output$vb_boxp <- shinydashboard::renderValueBox({
      val <- if (!is.null(res())) round(res()$box_pvalue, 4) else "-"
      col <- if (!is.null(res()) && res()$conflict_flag) "red" else "green"
      shinydashboard::valueBox(val, "Box p-value", icon = icon("vial"), color = col)
    })
    output$vb_surprise <- shinydashboard::renderValueBox({
      val <- if (!is.null(res())) round(res()$surprise_index, 3) else "-"
      shinydashboard::valueBox(val, "Surprise index", icon = icon("bolt"), color = "yellow")
    })
    output$vb_overlap <- shinydashboard::renderValueBox({
      val <- if (!is.null(res())) round(res()$overlap, 3) else "-"
      shinydashboard::valueBox(val, "Overlap coeff.", icon = icon("circle-half-stroke"), color = "blue")
    })

    output$conflict_alert <- renderUI({
      req(res())
      cls <- if (res()$conflict_flag) "alert-danger" else "alert-success"
      ico <- if (res()$conflict_flag) icon("triangle-exclamation") else icon("circle-check")
      tags$div(class = paste("alert", cls), style = "margin:10px 0;",
               ico, " ",
               tags$strong(glue::glue("Severity: {toupper(res()$conflict_severity)}. ")),
               res()$recommendation)
    })

    output$overlay_plot <- plotly::renderPlotly({
      req(res(), active_prior())
      gp <- plot_prior_likelihood(active_prior(), data_sum(), show_posterior = TRUE)
      plotly::ggplotly(gp) |>
        plotly::layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
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
        column(6, numericInput(ns("pv1"), "Var - ep.1", 0.01,  step = 0.001)),
        column(6, numericInput(ns("pv2"), "Var - ep.2", 0.015, step = 0.001))
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
      fluidRow(
        shinydashboard::valueBoxOutput(ns("vb_D"),    width = 4),
        shinydashboard::valueBoxOutput(ns("vb_pval"), width = 4),
        shinydashboard::valueBoxOutput(ns("vb_flag"), width = 4)
      ),
      shinydashboard::box(
        width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
        title = tagList(icon("table"), " Per-parameter marginal z-scores"),
        DT::dataTableOutput(ns("z_tbl"))
      ),
      uiOutput(ns("mv_alert"))
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
        conflict_mahalanobis(pm, pcov, om, ocov, alpha = input$alpha,
                             labels = c(input$lbl1, input$lbl2)),
        error = function(e) {
          showNotification(paste("Error:", conditionMessage(e)), type = "error"); NULL
        })
      res(r)
    })

    output$vb_D <- shinydashboard::renderValueBox({
      val <- if (!is.null(res())) round(res()$mahal_distance, 3) else "-"
      shinydashboard::valueBox(val, "Mahalanobis D", icon = icon("ruler"), color = "blue")
    })
    output$vb_pval <- shinydashboard::renderValueBox({
      val <- if (!is.null(res())) round(res()$pvalue, 4) else "-"
      col <- if (!is.null(res()) && res()$conflict_flag) "red" else "green"
      shinydashboard::valueBox(val, "Chi-sq p-value", icon = icon("chart-pie"), color = col)
    })
    output$vb_flag <- shinydashboard::renderValueBox({
      val <- if (!is.null(res())) if (res()$conflict_flag) "CONFLICT" else "OK" else "-"
      col <- if (!is.null(res()) && res()$conflict_flag) "red" else "green"
      shinydashboard::valueBox(val, "Status", icon = icon("flag"), color = col)
    })

    output$z_tbl <- DT::renderDataTable({
      req(res())
      df <- data.frame(Endpoint = res()$labels,
                       `Marginal z` = round(res()$marginal_z, 3),
                       check.names = FALSE)
      DT::datatable(df, rownames = FALSE, options = list(dom = "t"), class = "compact stripe")
    })

    output$mv_alert <- renderUI({
      req(res())
      cls <- if (res()$conflict_flag) "alert-danger" else "alert-success"
      ico <- if (res()$conflict_flag) icon("triangle-exclamation") else icon("check")
      tags$div(class = paste("alert", cls), style = "margin:10px 0;",
               ico, " ", res()$interpretation)
    })
  })
}
