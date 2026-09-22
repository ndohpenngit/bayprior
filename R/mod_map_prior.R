#' @noRd
mod_map_prior_ui <- function(id) {
  ns <- NS(id)
  fluidRow(

    # -- Left: inputs ---------------------------------------------------------
    shinydashboard::box(
      width = 4, status = "primary", solidHeader = TRUE,
      title = tagList(icon("hospital"), " Historical Trials"),

      textInput(ns("label"), "Quantity label", value = "Historical control"),
      selectInput(ns("outcome_type"), "Outcome scale",
        choices = c(
          "Single-arm log-odds (e.g. historical control rate)" = "single_arm_log_odds",
          "Log-odds ratio (two-arm)"       = "log_or",
          "Standardised mean difference"   = "smd",
          "Log incidence-rate ratio"       = "log_irr",
          "Mean difference (raw units)"    = "mean_difference",
          "Correlation"                    = "correlation"
        )
      ),
      tags$p(class = "text-muted", style = "font-size:11px; margin-top:-6px;",
             icon("circle-info"),
             " Sets the tau prior scale below to Roever et al. (2021)'s",
             " recommended default for this outcome. It does",
             tags$b(" not"), " rescale your trial estimates -- enter",
             tags$b(" estimate"), " and", tags$b(" SE"),
             " already on this scale. \"Mean difference\" has no default",
             " (its units are endpoint-specific): set the tau scale",
             " yourself for that case."),
      tags$hr(),

      tags$b("Historical trial estimates"),
      tags$p(class = "text-muted", style = "font-size:11px; margin-top:2px;",
             "One historical trial per line, as", tags$code("estimate, se"),
             "(both on the outcome scale selected above). At least 2 trials",
             " are required to estimate heterogeneity."),
      textAreaInput(ns("trials_txt"), NULL, rows = 6,
                    value = paste(
                      "-0.85, 0.25",
                      "-0.62, 0.30",
                      "-1.10, 0.28",
                      sep = "\n"
                    ),
                    placeholder = "estimate, se\n-0.85, 0.25\n-0.62, 0.30"),
      uiOutput(ns("parse_msg")),
      tags$hr(),

      tags$b("Heterogeneity (tau) prior"),
      shinyWidgets::radioGroupButtons(
        ns("tau_family"), NULL,
        choices  = c("Half-Normal" = "half_normal", "Half-Cauchy" = "half_cauchy"),
        selected = "half_normal", justified = TRUE, status = "primary", size = "sm"
      ),
      numericInput(ns("tau_scale"), "Scale", value = 1.0, min = 0.01, step = 0.05),
      tags$p(class = "text-muted", style = "font-size:11px;",
             "Weakly-informative defaults follow Roever et al. (2021); ",
             "adjust the scale to match your outcome's typical between-trial ",
             "variability."),
      tags$hr(),

      actionButton(ns("fit_btn"), "Derive MAP Prior",
                   icon = icon("play"), class = "btn-primary btn-block"),
      uiOutput(ns("fit_msg"))
    ),

    # -- Right: outputs --------------------------------------------------------
    column(8,
      uiOutput(ns("results_or_placeholder"))
    )
  )
}

#' @noRd
mod_map_prior_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    fitted <- reactiveVal(NULL)

    # Parses the "estimate, se" textarea into a numeric data.frame.
    # Returns NULL (with a UI message set) on any malformed line rather
    # than erroring, so the user gets feedback without a crashed app.
    .parse_trials <- function(txt) {
      lines <- trimws(strsplit(txt, "\n")[[1]])
      lines <- lines[nzchar(lines)]
      if (length(lines) == 0) return(NULL)
      parts <- strsplit(lines, ",")
      if (any(vapply(parts, length, integer(1)) != 2)) return(NULL)
      vals <- suppressWarnings(lapply(parts, function(p) as.numeric(trimws(p))))
      if (any(vapply(vals, function(v) anyNA(v), logical(1)))) return(NULL)
      mat <- do.call(rbind, vals)
      data.frame(estimate = mat[, 1], se = mat[, 2])
    }

    trials_data <- reactive(.parse_trials(input$trials_txt %||% ""))

    # Auto-populate the tau prior fields to Roever et al. (2021)'s preset
    # for the newly-selected outcome_type. "mean_difference" has no preset
    # (its units are endpoint-specific) -- resolve_tau_prior() errors for
    # it, so the fields are deliberately left untouched, forcing the analyst
    # to set a value themselves rather than silently inheriting whatever the
    # previous outcome_type happened to leave behind.
    observeEvent(input$outcome_type, {
      preset <- tryCatch(resolve_tau_prior(input$outcome_type),
                          error = function(e) NULL)
      if (!is.null(preset)) {
        shinyWidgets::updateRadioGroupButtons(session, "tau_family",
                                               selected = preset$family)
        updateNumericInput(session, "tau_scale", value = preset$scale)
      }
    }, ignoreInit = TRUE)

    output$parse_msg <- renderUI({
      df <- trials_data()
      if (is.null(df)) {
        tags$p(class = "text-danger", style = "font-size:11px; margin-top:2px;",
               icon("triangle-exclamation"),
               " Could not parse trial rows -- each line must be",
               " \"estimate, se\" with two numeric values.")
      } else {
        tags$p(class = "text-muted", style = "font-size:11px; margin-top:2px;",
               icon("check", style = "color:#1D9E75;"),
               sprintf(" %d trial row(s) parsed.", nrow(df)))
      }
    })

    # Reset fitted prior whenever inputs change, matching mod_elicitation
    observeEvent(
      list(input$trials_txt, input$tau_family, input$tau_scale,
           input$outcome_type, input$label),
      { fitted(NULL) },
      ignoreInit = TRUE
    )

    observeEvent(input$fit_btn, {
      pr <- withCallingHandlers(
        tryCatch({
          df <- trials_data()
          if (is.null(df)) {
            stop("Trial rows could not be parsed -- check the \"estimate, se\" format.")
          }
          if (nrow(df) < 2) {
            stop("At least 2 historical trial rows (estimate and SE) are required.")
          }
          map_prior(
            y  = df$estimate,
            se = df$se,
            # Always passed explicitly from the numeric inputs (which are
            # auto-populated per outcome_type, see the observeEvent above),
            # so map_prior()'s outcome_type-driven preset resolution is
            # never actually exercised from this path -- the UI's own
            # auto-populate/no-op-on-mean_difference behaviour is what does
            # the equivalent safety job here.
            tau_prior    = list(family = input$tau_family, scale = input$tau_scale),
            outcome_type = input$outcome_type,
            label        = input$label
          )
        }, error = function(e) {
          showNotification(paste("MAP fitting error:", conditionMessage(e)),
                           type = "error", duration = 8)
          NULL
        }),
        # General safety net: surface any R warning as a Shiny notification
        # rather than letting it vanish into server logs, since a plain
        # warning() is invisible in a deployed app otherwise.
        warning = function(w) {
          showNotification(conditionMessage(w), type = "warning", duration = NULL)
          invokeRestart("muffleWarning")
        }
      )
      fitted(pr)
      if (!is.null(pr)) {
        shared$current_prior <- pr
        shared$base_prior    <- pr   # sensitivity uses base_prior only
        shared$map_prior     <- pr
        shinyjs::runjs("bpToast('MAP prior derived successfully &#10003;', 'info', 3000);")
      }
    })

    output$fit_msg <- renderUI({
      req(fitted())
      p <- fitted()
      tags$div(class = "alert alert-success",
               style = "margin-top:8px; padding:6px; font-size:12px;",
               icon("check"), " ",
               glue::glue("MAP prior from {p$n_trials} trials: ",
                          "mean={round(p$fit_summary$mean, 3)}, ",
                          "SD={round(p$fit_summary$sd, 3)}, ",
                          "tau (median)={round(p$tau_summary$median, 3)}"))
    })

    output$results_or_placeholder <- renderUI({
      if (is.null(fitted())) {
        return(tags$div(
          class = "text-center",
          style = paste0("padding:60px 20px; color:#aaa;",
                         "border:2px dashed #ddd; border-radius:8px;",
                         "margin-top:10px;"),
          icon("hospital", style = "font-size:48px; margin-bottom:16px;"),
          tags$h4("No MAP prior derived yet", style = "color:#bbb;"),
          tags$p("Enter historical trial estimates, set the tau prior, and click",
                 tags$b("Derive MAP Prior"), ".")
        ))
      }

      p <- fitted()
      s <- p$fit_summary

      tagList(
        fluidRow(
          shinydashboard::valueBox(
            round(s$mean, 3), "MAP prior mean",
            icon = icon("dot-circle"), color = "blue", width = 4),
          shinydashboard::valueBox(
            round(s$sd, 3), "MAP prior SD",
            icon = icon("arrows-left-right"), color = "green", width = 4),
          shinydashboard::valueBox(
            round(p$tau_summary$median, 3), "Tau (posterior median)",
            icon = icon("chart-simple"), color = "purple", width = 4)
        ),
        shinydashboard::box(
          width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
          title = tagList(icon("chart-line"), " Fitted MAP prior density"),
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("prior_plot"), height = "280px"),
            color = "#1D9E75"
          )
        ),
        shinydashboard::box(
          width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
          title = tagList(icon("chart-area"), " Heterogeneity (tau) posterior"),
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("tau_plot"), height = "260px"),
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
      gp <- plot(fitted()) +
        ggplot2::theme(
          plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
          panel.background = ggplot2::element_rect(fill = "white", colour = NA)
        )
      plotly::ggplotly(gp) |> .apply_plotly_theme()
    })

    output$tau_plot <- plotly::renderPlotly({
      req(fitted())
      gp <- plot_tau_posterior(fitted()) +
        ggplot2::theme(
          plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
          panel.background = ggplot2::element_rect(fill = "white", colour = NA)
        )
      plotly::ggplotly(gp) |> .apply_plotly_theme()
    })

    output$params_tbl <- DT::renderDataTable({
      req(fitted())
      p <- fitted()
      s <- p$fit_summary
      df <- data.frame(
        Statistic = c("Trials (k)", "Mean", "SD", "Tau prior family", "Tau prior scale",
                      "Tau (mean)", "Tau (median)"),
        Value     = c(p$n_trials,
                      round(c(s$mean, s$sd), 5),
                      p$tau_prior$family,
                      round(p$tau_prior$scale, 3),
                      round(p$tau_summary$mean, 5),
                      round(p$tau_summary$median, 5))
      )
      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "t"), class = "compact stripe")
    })
  })
}