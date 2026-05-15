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
