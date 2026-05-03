# ── Module: sceptical prior ───────────────────────────────────────────────────

#' @noRd
mod_sceptical_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    shinydashboard::box(
      width = 4, status = "primary", solidHeader = TRUE,
      title = tagList(icon("scale-balanced"), " Sceptical Prior"),
      tags$small(class = "text-muted",
        "Centred at the null value of the treatment effect. Implements the
         Spiegelhalter-Freedman (1994) approach recommended as a regulatory
         sensitivity analysis companion."),
      tags$br(), tags$br(),
      selectInput(ns("family"), "Distribution family",
        choices = c("Normal"     = "normal",
                    "Beta"       = "beta",
                    "Log-Normal" = "lognormal")),
      numericInput(ns("null_val"), "Null value", value = 0, step = 0.05),
      uiOutput(ns("null_val_hint")),
      selectInput(ns("strength"), "Scepticism strength",
        choices  = c("Weak"     = "weak",
                     "Moderate" = "moderate",
                     "Strong"   = "strong"),
        selected = "moderate"),
      textInput(ns("label"), "Label", value = "Sceptical prior"),
      tags$hr(),
      actionButton(ns("fit_btn"), "Build sceptical prior",
                   icon = icon("shield"), class = "btn-primary btn-block"),
      uiOutput(ns("fit_msg"))
    ),
    column(8,
      uiOutput(ns("results_or_placeholder"))
    )
  )
}

#' @noRd
mod_sceptical_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    fitted <- reactiveVal(NULL)

    observeEvent(input$family, {
      if (input$family == "beta") {
        updateNumericInput(session, "null_val",
                           label = "Null value (must be in (0, 1))",
                           value = 0.20, min = 0.01, max = 0.99, step = 0.01)
      } else if (input$family == "lognormal") {
        updateNumericInput(session, "null_val",
                           label = "Null value (log scale; 0 = HR of 1)",
                           value = 0, min = -5, max = 5, step = 0.05)
      } else {
        updateNumericInput(session, "null_val",
                           label = "Null value",
                           value = 0, min = -Inf, max = Inf, step = 0.05)
      }
    }, ignoreInit = TRUE)

    output$null_val_hint <- renderUI({
      switch(input$family,
        beta      = tags$small(class = "text-muted",
                      "Beta: null value must be strictly in (0, 1),",
                      "e.g. 0.20 for a 20% null response rate."),
        lognormal = tags$small(class = "text-muted",
                      "Log-Normal: log scale. Use 0 to centre at HR = 1."),
        NULL
      )
    })

    observeEvent(input$fit_btn, {
      if (input$family == "beta" &&
          (is.na(input$null_val) ||
           input$null_val <= 0 || input$null_val >= 1)) {
        showNotification(
          paste0("For Beta family, null value must be strictly between 0 and 1. ",
                 "Example: 0.20 for a 20% null response rate."),
          type = "error", duration = 8)
        return()
      }
      pr <- tryCatch(
        sceptical_prior(null_value = input$null_val, family = input$family,
                        strength = input$strength, label = input$label),
        error = function(e) {
          showNotification(paste("Error:", conditionMessage(e)),
                           type = "error", duration = 8)
          NULL
        })
      fitted(pr)
      if (!is.null(pr)) shared$current_prior <- pr
    })

    output$fit_msg <- renderUI({
      req(fitted())
      p <- fitted()
      tags$div(class = "alert alert-success",
               style = "margin-top:8px; padding:6px; font-size:12px;",
               icon("check"), " ",
               glue::glue("Sceptical {toupper(p$dist)}: ",
                          "mean = {round(p$fit_summary$mean, 3)}, ",
                          "sd = {round(p$fit_summary$sd, 3)}"))
    })

    # ── Placeholder before fit, results after ───────────────────────────────
    output$results_or_placeholder <- renderUI({
      if (is.null(fitted())) {
        return(tags$div(
          class = "text-center",
          style = paste0("padding:60px 20px; color:#aaa;",
                         "border:2px dashed #ddd; border-radius:8px;",
                         "margin-top:10px;"),
          icon("scale-balanced", style = "font-size:48px; margin-bottom:16px;"),
          tags$h4("No sceptical prior built yet", style = "color:#bbb;"),
          tags$p("Configure settings and click",
                 tags$b("Build sceptical prior"), "to see results.")
        ))
      }
      p  <- fitted()
      s  <- p$fit_summary
      ns <- session$ns
      q025 <- s$q025 %||% (s$mean - 1.96 * s$sd)
      q975 <- s$q975 %||% (s$mean + 1.96 * s$sd)

      tagList(
        fluidRow(
          shinydashboard::valueBox(
            round(s$mean, 3), "Prior mean",
            icon = icon("dot-circle"), color = "blue", width = 4),
          shinydashboard::valueBox(
            round(s$sd, 3), "Prior SD",
            icon = icon("arrows-left-right"), color = "green", width = 4),
          shinydashboard::valueBox(
            glue::glue("[{round(q025,3)}, {round(q975,3)}]"), "95% CrI",
            icon = icon("ruler-horizontal"), color = "purple", width = 4)
        ),
        shinydashboard::box(
          width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
          title = tagList(icon("chart-line"), " Sceptical prior density"),
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("prior_plot"), height = "280px"),
            color = "#1D9E75"
          )
        )
      )
    })

    output$prior_plot <- plotly::renderPlotly({
      req(fitted())
      plotly::ggplotly(plot(fitted())) |> .apply_plotly_theme()
    })
  })
}


# ── Module: robust mixture prior ─────────────────────────────────────────────

#' @noRd
mod_robust_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    shinydashboard::box(
      width = 4, status = "primary", solidHeader = TRUE,
      title = tagList(icon("layer-group"), " Robust Mixture Prior"),
      tags$small(class = "text-muted",
        "Mixes an informative prior with a vague component (Schmidli et al.,
         2014). Protects against prior misspecification. Requires a fitted
         prior from the Elicitation tab."),
      tags$br(), tags$br(),
      uiOutput(ns("prior_banner")),
      tags$hr(),
      # FIX: original had min/max/value in wrong order (value=0.05, min=0.50)
      sliderInput(ns("vague_weight"), "Vague component weight",
                  min = 0.05, max = 0.50, value = 0.20, step = 0.05),
      numericInput(ns("vague_sd_mult"), "Vague SD = informative SD x",
                   value = 10, min = 2, max = 50, step = 1),
      textInput(ns("label"), "Label", "Robust mixture prior"),
      tags$hr(),
      actionButton(ns("fit_btn"), "Build robust prior",
                   icon = icon("layer-group"), class = "btn-primary btn-block"),
      uiOutput(ns("fit_msg"))
    ),
    column(8,
      uiOutput(ns("results_or_placeholder"))
    )
  )
}

#' @noRd
mod_robust_server <- function(id, shared, active_prior) {
  moduleServer(id, function(input, output, session) {
    fitted <- reactiveVal(NULL)

    output$prior_banner <- renderUI({
      p   <- active_prior()
      cls <- if (is.null(p)) "alert-warning" else "alert-success"
      msg <- if (is.null(p)) "No informative prior yet." else
        glue::glue("{p$label} ({toupper(p$dist)})")
      tags$div(class = paste("alert", cls),
               style = "font-size:12px; padding:6px;",
               if (is.null(p)) icon("exclamation-triangle") else icon("check"),
               " ", msg)
    })

    observeEvent(input$fit_btn, {
      inf <- active_prior(); req(inf)
      vsd <- input$vague_sd_mult * inf$fit_summary$sd
      pr  <- tryCatch(
        robust_prior(inf, vague_weight = input$vague_weight,
                     vague_sd = vsd, label = input$label),
        error = function(e) {
          showNotification(paste("Error:", conditionMessage(e)),
                           type = "error")
          NULL
        })
      fitted(pr)
      if (!is.null(pr)) shared$current_prior <- pr
    })

    output$fit_msg <- renderUI({
      req(fitted())
      p <- fitted()
      tags$div(class = "alert alert-success",
               style = "margin-top:8px; padding:6px; font-size:12px;",
               icon("check"), " ",
               glue::glue("{round((1 - p$vague_weight) * 100)}% informative + ",
                          "{round(p$vague_weight * 100)}% vague"))
    })

    # ── Placeholder before fit, results after ───────────────────────────────
    output$results_or_placeholder <- renderUI({
      if (is.null(fitted())) {
        return(tags$div(
          class = "text-center",
          style = paste0("padding:60px 20px; color:#aaa;",
                         "border:2px dashed #ddd; border-radius:8px;",
                         "margin-top:10px;"),
          icon("layer-group", style = "font-size:48px; margin-bottom:16px;"),
          tags$h4("No robust prior built yet", style = "color:#bbb;"),
          tags$p("Fit a prior in the Elicitation tab, then click",
                 tags$b("Build robust prior."))
        ))
      }
      p  <- fitted()
      ns <- session$ns

      tagList(
        fluidRow(
          shinydashboard::valueBox(
            round(p$fit_summary$mean, 3), "Mixture mean",
            icon = icon("dot-circle"), color = "blue", width = 4),
          shinydashboard::valueBox(
            round(p$fit_summary$sd, 3), "Mixture SD",
            icon = icon("arrows-left-right"), color = "green", width = 4),
          shinydashboard::valueBox(
            glue::glue("{round((1-p$vague_weight)*100)}% / ",
                       "{round(p$vague_weight*100)}%"), "Info / Vague %",
            icon = icon("percent"), color = "orange", width = 4)
        ),
        shinydashboard::box(
          width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
          title = tagList(icon("chart-line"), " Robust vs informative density"),
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("prior_plot"), height = "280px"),
            color = "#1D9E75"
          )
        )
      )
    })

    output$prior_plot <- plotly::renderPlotly({
      req(fitted())
      inf   <- active_prior()
      x     <- .density_grid(fitted())$x
      y_mix <- .eval_density_vec(fitted(), x)
      fig   <- plotly::plot_ly() |>
        plotly::add_lines(x = x, y = y_mix, name = "Robust mixture",
                          line = list(color = "#1D9E75", width = 2.5))
      if (!is.null(inf)) {
        y_inf <- .eval_density_vec(inf, x)
        fig   <- plotly::add_lines(fig, x = x, y = y_inf, name = "Informative",
                                   line = list(color = "#185FA5", width = 1.5,
                                               dash = "dot"))
      }
      plotly::layout(fig, legend = list(orientation = "h")) |>
        .apply_plotly_theme()
    })
  })
}


# ── Module: power prior calibration ──────────────────────────────────────────

#' @noRd
mod_power_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    shinydashboard::box(
      width = 4, status = "primary", solidHeader = TRUE,
      title = tagList(icon("bolt"), " Power Prior Calibration"),
      tags$small(class = "text-muted",
        "Selects the power prior weight delta that down-weights historical
         data before incorporating it into the current analysis.
         (Ibrahim & Chen, 2000; Gravestock & Held, 2017)"),
      tags$br(), tags$br(),
      uiOutput(ns("prior_banner")),
      tags$hr(),
      selectInput(ns("data_type"), "Data type",
        choices = c("Binary"     = "binary",
                    "Continuous" = "continuous")),
      tags$h6("Historical data"),
      conditionalPanel(
        condition = sprintf("input['%s'] === 'binary'", ns("data_type")),
        fluidRow(
          column(6, numericInput(ns("hx"), "Events", 12, 0)),
          column(6, numericInput(ns("hn"), "n",      40, 1))
        )
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] === 'continuous'", ns("data_type")),
        numericInput(ns("hmean"), "Mean", 0.35, step = 0.01),
        numericInput(ns("hsd"),   "SD",   0.15, step = 0.01),
        numericInput(ns("hn_c"), "n",     40,  1)
      ),
      tags$h6("Current data"),
      conditionalPanel(
        condition = sprintf("input['%s'] === 'binary'", ns("data_type")),
        fluidRow(
          column(6, numericInput(ns("cx"), "Events", 18, 0)),
          column(6, numericInput(ns("cn"), "n",      50, 1))
        )
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] === 'continuous'", ns("data_type")),
        numericInput(ns("cmean"), "Mean", 0.45, step = 0.01),
        numericInput(ns("csd"),   "SD",   0.18, step = 0.01),
        numericInput(ns("cn_c"), "n",     50,  1)
      ),
      tags$hr(),
      selectInput(ns("method"), "Calibration method",
        choices = c("Bayes Factor"          = "bayes_factor",
                    "Box p-value (compat.)" = "compatibility")),
      numericInput(ns("target_bf"), "Target Bayes Factor", 3, 1, step = 0.5),
      actionButton(ns("run_btn"), "Calibrate",
                   icon = icon("bolt"), class = "btn-primary btn-block"),
      uiOutput(ns("fit_msg"))
    ),
    column(8,
      uiOutput(ns("results_or_placeholder"))
    )
  )
}

#' @noRd
mod_power_server <- function(id, shared, active_prior) {
  moduleServer(id, function(input, output, session) {
    res <- reactiveVal(NULL)

    output$prior_banner <- renderUI({
      p   <- active_prior()
      cls <- if (is.null(p)) "alert-warning" else "alert-success"
      msg <- if (is.null(p)) "Fit a base prior in Elicitation first." else
        glue::glue("{p$label} ({toupper(p$dist)})")
      tags$div(class = paste("alert", cls),
               style = "font-size:12px; padding:6px;",
               if (is.null(p)) icon("exclamation-triangle") else icon("check"),
               " ", msg)
    })

    observeEvent(input$run_btn, {
      base   <- active_prior(); req(base)
      hist_d <- if (input$data_type == "binary")
        list(type = "binary",     x = input$hx,    n = input$hn)
      else
        list(type = "continuous", x = input$hmean, sd = input$hsd,
             n = input$hn_c)
      curr_d <- if (input$data_type == "binary")
        list(type = "binary",     x = input$cx,    n = input$cn)
      else
        list(type = "continuous", x = input$cmean, sd = input$csd,
             n = input$cn_c)
      r <- tryCatch(
        calibrate_power_prior(hist_d, curr_d, base,
                              target_bf = input$target_bf,
                              method    = input$method),
        error = function(e) {
          showNotification(paste("Error:", conditionMessage(e)),
                           type = "error")
          NULL
        })
      res(r)
      if (!is.null(r)) shared$current_prior <- r$power_prior
    })

    output$fit_msg <- renderUI({
      req(res())
      tags$div(class = "alert alert-success",
               style = "margin-top:8px; padding:6px; font-size:12px;",
               icon("check"), " ",
               glue::glue("Optimal delta = {res()$delta_opt}"))
    })

    # ── Placeholder before calibration, results after ────────────────────────
    output$results_or_placeholder <- renderUI({
      if (is.null(res())) {
        return(tags$div(
          class = "text-center",
          style = paste0("padding:60px 20px; color:#aaa;",
                         "border:2px dashed #ddd; border-radius:8px;",
                         "margin-top:10px;"),
          icon("bolt", style = "font-size:48px; margin-bottom:16px;"),
          tags$h4("Not calibrated yet", style = "color:#bbb;"),
          tags$p("Enter historical and current data, then click",
                 tags$b("Calibrate"), "to see results.")
        ))
      }

      r  <- res()
      ns <- session$ns

      tagList(
        fluidRow(
          shinydashboard::valueBox(
            r$delta_opt, "Optimal delta",
            icon = icon("sliders"), color = "blue", width = 4),
          shinydashboard::valueBox(
            round(r$power_prior$fit_summary$mean, 3), "Power prior mean",
            icon = icon("dot-circle"), color = "green", width = 4),
          shinydashboard::valueBox(
            round(r$power_prior$fit_summary$sd, 3), "Power prior SD",
            icon = icon("arrows-left-right"), color = "purple", width = 4)
        ),
        shinydashboard::box(
          width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
          title = tagList(icon("chart-line"), " Calibration curves"),
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("calib_plot"), height = "280px"),
            color = "#1D9E75"
          )
        )
      )
    })

    output$calib_plot <- plotly::renderPlotly({
      req(res())
      r      <- res()
      df     <- r$results
      d_opt  <- r$delta_opt
      bf_max <- max(df$bayes_factor, na.rm = TRUE)
      plotly::plot_ly(df, x = ~delta) |>
        plotly::add_lines(y = ~bayes_factor, name = "Bayes Factor",
                          line = list(color = "#185FA5", width = 2)) |>
        plotly::add_lines(y = ~box_pvalue * bf_max, name = "Box p (scaled)",
                          line = list(color = "#D85A30", width = 2,
                                      dash = "dot")) |>
        plotly::add_segments(x = d_opt, xend = d_opt, y = 0, yend = bf_max,
                             line = list(color = "#1D9E75", dash = "dash",
                                         width = 1.5),
                             name = "Optimal delta") |>
        plotly::layout(
          xaxis = list(title = "delta"),
          yaxis = list(title = "Bayes Factor")) |>
        .apply_plotly_theme()
    })
  })
}
