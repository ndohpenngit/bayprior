#' @noRd
mod_pooling_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    shinydashboard::box(
      width = 4, status = "primary", solidHeader = TRUE,
      title = tagList(icon("users"), " Pool Settings"),
      uiOutput(ns("pool_banner")),
      tags$hr(),
      selectInput(ns("method"), "Pooling method",
        choices = c("Linear (mixture)" = "linear", "Logarithmic" = "logarithmic")),
      sliderInput(ns("disagree_thresh"), "Disagreement alert threshold",
                  0.1, 0.9, 0.5, 0.05),
      tags$h5("Expert weights"),
      uiOutput(ns("weight_sliders")),
      tags$hr(),
      actionButton(ns("pool_btn"), "Compute Consensus Prior",
                   icon = icon("compress"), class = "btn-primary btn-block"),
      uiOutput(ns("pool_msg"))
    ),
    column(8,
      shinydashboard::box(
        width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE,
        title = tagList(icon("table"), " Pairwise Bhattacharyya agreement"),
        DT::dataTableOutput(ns("bc_tbl"))
      ),
      shinydashboard::box(
        width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
        title = tagList(icon("chart-line"), " Individual vs consensus priors"),
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("consensus_plot"), height = "320px"),
          color = "#1D9E75"
        )
      )
    )
  )
}

#' @noRd
mod_pooling_server <- function(id, shared, active_prior) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$pool_banner <- renderUI({
      n   <- length(shared$expert_pool)
      cls <- if (n >= 2) "alert-success" else "alert-warning"
      msg <- if (n >= 2)
        glue::glue("{n} expert(s): {paste(names(shared$expert_pool), collapse=', ')}")
      else "Add >= 2 experts from the Elicitation tab first."
      tags$div(class = paste("alert", cls),
               style = "font-size:12px; padding:6px;",
               if (n >= 2) icon("check") else icon("exclamation-triangle"), " ", msg)
    })

    output$weight_sliders <- renderUI({
      pool <- shared$expert_pool
      if (!length(pool)) return(tags$p("No experts yet.", class = "text-muted"))
      k <- length(pool)
      lapply(seq_along(pool), function(i)
        sliderInput(ns(paste0("w_", i)), names(pool)[i],
                    0, 1, round(1/k, 2), 0.05))
    })

    observeEvent(input$pool_btn, {
      pool <- shared$expert_pool
      req(length(pool) >= 2)
      k   <- length(pool)
      wts <- vapply(seq_len(k), function(i) input[[paste0("w_",i)]] %||% (1/k), numeric(1))
      wts <- wts / sum(wts)
      res <- tryCatch(
        aggregate_experts(pool, weights = wts, method = input$method,
                          disagreement_threshold = input$disagree_thresh),
        error = function(e) {
          showNotification(paste("Pooling error:", conditionMessage(e)), type = "error"); NULL
        })
      shared$consensus <- res
    })

    output$pool_msg <- renderUI({
      req(shared$consensus)
      con <- shared$consensus
      tags$div(class = "alert alert-success",
               style = "margin-top:8px; font-size:12px; padding:6px;",
               icon("check"), " ",
               glue::glue("Consensus ({con$aggregation$method}): ",
                          "mean = {round(con$fit_summary$mean,3)}, ",
                          "SD = {round(con$fit_summary$sd,3)}"))
    })

    output$bc_tbl <- DT::renderDataTable({
      req(shared$consensus)
      bc <- as.data.frame(round(shared$consensus$aggregation$disagreement, 3))
      DT::datatable(cbind(Expert = rownames(bc), bc),
                    rownames = FALSE, options = list(dom = "t"), class = "compact stripe")
    })

    output$consensus_plot <- plotly::renderPlotly({
      req(shared$consensus)
      con  <- shared$consensus
      pool <- con$components
      nms  <- names(pool) %||% paste0("Expert_", seq_along(pool))
      fig  <- plotly::plot_ly()
      for (i in seq_along(pool)) {
        dg  <- .density_grid(pool[[i]])
        fig <- plotly::add_lines(fig, x = dg$x, y = dg$y, name = nms[i],
                                 line = list(dash = "dot", width = 1.5))
      }
      x_rng <- range(unlist(lapply(pool, function(p)
        c(p$fit_summary$q025, p$fit_summary$q975))), na.rm = TRUE)
      x_seq <- seq(x_rng[1], x_rng[2], length.out = 500)
      wts   <- con$aggregation$weights
      y_mix <- Reduce("+", lapply(seq_along(pool), function(i)
        wts[i] * .eval_density_vec(pool[[i]], x_seq)))
      fig <- plotly::add_lines(fig, x = x_seq, y = y_mix, name = "Consensus",
                               line = list(color = "#1D9E75", width = 3))
      plotly::layout(fig,
        xaxis = list(title = "theta"), yaxis = list(title = "Density"),
        legend = list(orientation = "h"),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
    })
  })
}
