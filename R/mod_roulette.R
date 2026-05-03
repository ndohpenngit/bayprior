#' @noRd
mod_roulette_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    shinydashboard::box(
      width = 4, status = "primary", solidHeader = TRUE,
      title = tagList(icon("th"), " Roulette Setup"),
      tags$small(class = "text-muted",
        "Allocate chips across bins representing your quantity's plausible
         range. The distribution is fitted to the chip histogram in real time.
         Based on the SHELF roulette methodology (Oakley & O'Hagan, 2010)."),
      tags$br(), tags$br(),
      textInput(ns("expert_id"), "Expert ID", value = "Expert_1"),
      textInput(ns("label"), "Quantity label", value = "Response rate"),
      selectInput(ns("family"), "Distribution to fit",
        choices = c("Beta"      = "beta",
                    "Normal"    = "normal",
                    "Gamma"     = "gamma",
                    "Log-Normal"= "lognormal")),
      tags$hr(),
      fluidRow(
        column(6, numericInput(ns("range_min"), "Range min", 0.0, step = 0.05)),
        column(6, numericInput(ns("range_max"), "Range max", 1.0, step = 0.05))
      ),
      numericInput(ns("n_bins"), "Number of bins", 10, min = 4, max = 20),
      tags$hr(),
      actionButton(ns("reset_btn"), "Reset chips",
                   icon = icon("rotate-left"), class = "btn-warning btn-block"),
      tags$br(),
      actionButton(ns("fit_btn"), "Fit Prior from chips",
                   icon = icon("magic"), class = "btn-success btn-block"),
      tags$br(),
      actionButton(ns("add_btn"), "Add to Expert Pool",
                   icon = icon("plus"), class = "btn-primary btn-block"),
      uiOutput(ns("fit_msg"))
    ),
    column(8,
      shinydashboard::box(
        width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
        title = tagList(icon("chart-column"), " Chip allocation histogram"),
        tags$small(class = "text-muted",
          "Use the +/- controls below to place chips. The green curve shows
           the fitted distribution scaled to chip height."),
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("roulette_plot"), height = "260px"),
          color = "#1D9E75"
        )
      ),
      shinydashboard::box(
        width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
        title = tagList(icon("sliders"), " Chip controls"),
        uiOutput(ns("chip_ui"))
      ),
      shinydashboard::box(
        width = 12, status = "success", solidHeader = TRUE,
        collapsible = TRUE, collapsed = TRUE,
        title = tagList(icon("table"), " Fitted parameters"),
        DT::dataTableOutput(ns("params_tbl"))
      )
    )
  )
}

#' @noRd
mod_roulette_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    chips <- reactiveVal(rep(0L, 10L))

    observeEvent(list(input$n_bins, input$reset_btn), {
      chips(rep(0L, as.integer(input$n_bins %||% 10L)))
    }, ignoreInit = FALSE)

    breaks <- reactive({
      req(input$range_min, input$range_max, input$n_bins)
      seq(input$range_min, input$range_max,
          length.out = as.integer(input$n_bins) + 1L)
    })

    output$chip_ui <- renderUI({
      brks <- breaks()
      n    <- length(brks) - 1L
      cv   <- chips()
      if (length(cv) != n) cv <- rep(0L, n)
      fluidRow(
        lapply(seq_len(n), function(i) {
          lbl <- glue::glue("[{round(brks[i],2)},\n{round(brks[i+1],2)})")
          column(
            width = max(1L, floor(12L / min(n, 12L))),
            tags$div(style = "text-align:center; margin-bottom:6px;",
              tags$small(lbl, style = "font-size:10px; display:block;"),
              actionButton(ns(paste0("p_", i)), "+",
                           class = "btn-xs btn-success",
                           style = "padding:1px 5px;"),
              tags$span(cv[i], id = ns(paste0("cnt_", i)),
                        style = paste0("display:inline-block; width:22px;",
                                       "font-weight:700; text-align:center;")),
              actionButton(ns(paste0("m_", i)), "-",
                           class = "btn-xs btn-danger",
                           style = "padding:1px 5px;")
            )
          )
        })
      )
    })

    observe({
      n <- as.integer(input$n_bins %||% 10L)
      lapply(seq_len(n), function(i) {
        observeEvent(input[[paste0("p_", i)]], {
          cv <- chips(); if (length(cv) < i) return()
          cv[i] <- cv[i] + 1L; chips(cv)
        }, ignoreInit = TRUE)
        observeEvent(input[[paste0("m_", i)]], {
          cv <- chips(); if (length(cv) < i) return()
          cv[i] <- max(0L, cv[i] - 1L); chips(cv)
        }, ignoreInit = TRUE)
      })
    })

    output$roulette_plot <- plotly::renderPlotly({
      brks <- breaks()
      cv   <- chips()
      n    <- length(brks) - 1L
      if (length(cv) != n) cv <- rep(0L, n)
      mids <- (brks[-length(brks)] + brks[-1]) / 2

      fig <- plotly::plot_ly() |>
        plotly::add_bars(x = mids, y = cv, name = "Chips",
          marker = list(color = "#A8CFED",
                        line  = list(color = "#185FA5", width = 1)))

      if (sum(cv) >= 2L) {
        pr <- tryCatch(
          elicit_roulette(cv, brks, family    = input$family,
                          expert_id = input$expert_id,
                          label     = input$label),
          error = function(e) NULL)
        if (!is.null(pr)) {
          dg <- .density_grid(pr)
          sf <- max(cv) / max(dg$y, na.rm = TRUE)
          fig <- fig |>
            plotly::add_lines(x = dg$x, y = dg$y * sf,
              name = glue::glue("Fitted {toupper(pr$dist)} (scaled)"),
              line = list(color = "#1D9E75", width = 2.5))
        }
      }
      plotly::layout(fig,
        xaxis = list(title = input$label),
        yaxis = list(title = "Chips")) |>
        .apply_plotly_theme()
    })

    fitted <- reactiveVal(NULL)

    observeEvent(input$fit_btn, {
      brks <- breaks(); cv <- chips()
      if (sum(cv) < 2L) {
        showNotification("Place at least 2 chips before fitting.",
                         type = "warning")
        return()
      }
      pr <- tryCatch(
        elicit_roulette(cv, brks, family    = input$family,
                        expert_id = input$expert_id,
                        label     = input$label),
        error = function(e) {
          showNotification(paste("Error:", conditionMessage(e)),
                           type = "error")
          NULL
        })
      fitted(pr)
      shared$current_prior <- pr
    })

    observeEvent(input$add_btn, {
      req(fitted())
      shared$expert_pool[[input$expert_id]] <- fitted()
      showNotification(
        glue::glue("'{input$expert_id}' added ",
                   "({length(shared$expert_pool)} in pool)."),
        type = "message")
    })

    output$fit_msg <- renderUI({
      req(fitted())
      p <- fitted()
      tags$div(class = "alert alert-success",
               style = "margin-top:8px; padding:6px; font-size:12px;",
               icon("check"), " ",
               glue::glue("{toupper(p$dist)}: ",
                          "mean={round(p$fit_summary$mean,3)}, ",
                          "sd={round(p$fit_summary$sd,3)}"))
    })

    output$params_tbl <- DT::renderDataTable({
      req(fitted())
      s  <- fitted()$fit_summary
      df <- data.frame(
        Statistic = c("Mean", "SD", "2.5th pctile", "Median", "97.5th pctile"),
        Value     = round(c(s$mean, s$sd, s$q025, s$q500, s$q975), 5))
      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "t"), class = "compact stripe")
    })
  })
}
