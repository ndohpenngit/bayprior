#' @noRd
mod_elicitation_ui <- function(id) {
  ns <- NS(id)
  fluidRow(

    # ── Left: inputs ─────────────────────────────────────────────────────────
    shinydashboard::box(
      width = 4, status = "primary", solidHeader = TRUE,
      title = tagList(icon("pencil"), " Expert Input"),

      textInput(ns("expert_id"), "Expert ID", value = "Expert_1"),
      textInput(ns("label"), "Quantity label", value = "Response rate"),
      tags$hr(),

      selectInput(ns("family"), "Distribution family",
        choices = c(
          "Beta (proportions)"  = "beta",
          "Normal (unbounded)"  = "normal",
          "Gamma (positive)"    = "gamma",
          "Log-Normal (ratios)" = "lognormal"
        )
      ),

      shinyWidgets::radioGroupButtons(
        ns("method"), "Elicitation method",
        choices  = c("Quantile" = "quantile", "Moments" = "moments"),
        selected = "quantile", justified = TRUE, status = "primary"
      ),
      tags$hr(),

      # Quantile inputs
      conditionalPanel(
        condition = sprintf("input['%s'] === 'quantile'", ns("method")),
        tags$small(class = "text-muted",
          "Enter probability (%) and the corresponding expert value."),
        tags$br(), tags$br(),
        fluidRow(
          column(5, tags$b("Prob (%)")),
          column(7, tags$b("Value"))
        ),
        fluidRow(
          column(5, numericInput(ns("q1p"), NULL, 5,  1, 49, 1)),
          column(7, numericInput(ns("q1v"), NULL, 0.10, step = 0.01))
        ),
        fluidRow(
          column(5, numericInput(ns("q2p"), NULL, 50, 1, 99, 1)),
          column(7, numericInput(ns("q2v"), NULL, 0.35, step = 0.01))
        ),
        fluidRow(
          column(5, numericInput(ns("q3p"), NULL, 95, 51, 99, 1)),
          column(7, numericInput(ns("q3v"), NULL, 0.65, step = 0.01))
        )
      ),

      # Moment inputs
      conditionalPanel(
        condition = sprintf("input['%s'] === 'moments'", ns("method")),
        numericInput(ns("mom_mean"), "Prior mean",  value = 0.35, step = 0.01),
        numericInput(ns("mom_sd"),   "Prior SD",    value = 0.10, step = 0.01,
                     min = 0.001)
      ),
      tags$hr(),

      actionButton(ns("fit_btn"), "Fit Prior",
                   icon = icon("play"), class = "btn-primary btn-block"),
      tags$br(),
      actionButton(ns("add_btn"), "Add to Expert Pool",
                   icon = icon("plus"), class = "btn-success btn-block"),
      uiOutput(ns("fit_msg"))
    ),

    # ── Right: outputs ────────────────────────────────────────────────────────
    column(8,
      uiOutput(ns("results_or_placeholder"))
    )
  )
}

#' @noRd
mod_elicitation_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {

    fitted <- reactiveVal(NULL)

    observeEvent(input$fit_btn, {
      pr <- tryCatch({
        if (input$method == "quantile") {
          qs <- setNames(c(input$q1v, input$q2v, input$q3v),
                         as.character(c(input$q1p, input$q2p, input$q3p) / 100))
          switch(input$family,
            beta      = elicit_beta(quantiles = qs, method = "quantile",
                                    expert_id = input$expert_id,
                                    label     = input$label),
            normal    = elicit_normal(quantiles = qs, method = "quantile",
                                      expert_id = input$expert_id,
                                      label     = input$label),
            gamma     = elicit_gamma(quantiles = qs, method = "quantile",
                                     expert_id = input$expert_id,
                                     label     = input$label),
            lognormal = elicit_lognormal(quantiles = qs, method = "quantile",
                                         expert_id = input$expert_id,
                                         label     = input$label)
          )
        } else {
          switch(input$family,
            beta      = elicit_beta(mean = input$mom_mean, sd = input$mom_sd,
                                    method    = "moments",
                                    expert_id = input$expert_id,
                                    label     = input$label),
            normal    = elicit_normal(mean = input$mom_mean, sd = input$mom_sd,
                                      method    = "moments",
                                      expert_id = input$expert_id,
                                      label     = input$label),
            gamma     = elicit_gamma(mean = input$mom_mean, sd = input$mom_sd,
                                     method    = "moments",
                                     expert_id = input$expert_id,
                                     label     = input$label),
            lognormal = elicit_lognormal(mean = input$mom_mean, sd = input$mom_sd,
                                         method    = "moments",
                                         expert_id = input$expert_id,
                                         label     = input$label)
          )
        }
      }, error = function(e) {
        showNotification(paste("Fitting error:", conditionMessage(e)),
                         type = "error", duration = 8)
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
               glue::glue("Fitted {toupper(p$dist)}: ",
                          "mean={round(p$fit_summary$mean, 3)}, ",
                          "SD={round(p$fit_summary$sd, 3)}"))
    })

    # ── Placeholder before fit, full results after ───────────────────────────
    output$results_or_placeholder <- renderUI({
      if (is.null(fitted())) {
        return(tags$div(
          class = "text-center",
          style = paste0("padding:60px 20px; color:#aaa;",
                         "border:2px dashed #ddd; border-radius:8px;",
                         "margin-top:10px;"),
          icon("chart-line", style = "font-size:48px; margin-bottom:16px;"),
          tags$h4("No prior fitted yet", style = "color:#bbb;"),
          tags$p("Configure the inputs and click",
                 tags$b("Fit Prior"), "to see results.")
        ))
      }

      p  <- fitted()
      s  <- p$fit_summary
      ns <- session$ns

      tagList(
        fluidRow(
          shinydashboard::valueBox(
            round(s$mean, 3), "Prior mean",
            icon = icon("dot-circle"), color = "blue", width = 4),
          shinydashboard::valueBox(
            round(s$sd, 3), "Prior SD",
            icon = icon("arrows-left-right"), color = "green", width = 4),
          shinydashboard::valueBox(
            glue::glue("[{round(s$q025 %||% (s$mean - 1.96*s$sd), 3)}, ",
                       "{round(s$q975 %||% (s$mean + 1.96*s$sd), 3)}]"),
            "95% CrI",
            icon = icon("ruler-horizontal"), color = "purple", width = 4)
        ),
        shinydashboard::box(
          width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
          title = tagList(icon("chart-line"), " Fitted prior density"),
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("prior_plot"), height = "300px"),
            color = "#1D9E75"
          )
        ),
        shinydashboard::box(
          width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
          title = tagList(icon("table"), " Parameter summary"),
          DT::dataTableOutput(ns("params_tbl"))
        )
      )
    })

    output$prior_plot <- plotly::renderPlotly({
      req(fitted())
      # Force white gg backgrounds before ggplotly() conversion.
      # plot.bayprior() may set a coloured background internally; this
      # overrides it so the CSS filter invert(1) produces black, not lavender.
      gp <- plot(fitted()) +
        ggplot2::theme(
          plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
          panel.background = ggplot2::element_rect(fill = "white", colour = NA)
        )
      plotly::ggplotly(gp) |> .apply_plotly_theme()
    })

    output$params_tbl <- DT::renderDataTable({
      req(fitted())
      s  <- fitted()$fit_summary
      df <- data.frame(
        Statistic = c("Mean", "SD", "2.5th pctile", "Median", "97.5th pctile"),
        Value     = round(c(s$mean, s$sd, s$q025, s$q500, s$q975), 5)
      )
      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "t"), class = "compact stripe")
    })
  })
}