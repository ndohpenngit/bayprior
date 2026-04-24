#' Run the bayprior Shiny Application
#'
#' Launches the golem-structured shinydashboard application.
#'
#' @param onStart A function called before the app runs.
#' @param options List of options passed to \code{shiny::shinyApp()}.
#' @param enableBookmarking Passed to \code{shiny::shinyApp()}.
#' @param uiPattern Passed to \code{shiny::shinyApp()}.
#' @param ... Additional arguments passed to \code{golem::with_golem_options()}.
#'
#' @return A \code{shiny.appobj}, invisibly.
#' @export
run_app <- function(onStart = NULL, options = list(),
                    enableBookmarking = NULL, uiPattern = "/", ...) {
  golem::with_golem_options(
    app = shiny::shinyApp(
      ui                = app_ui,
      server            = app_server,
      onStart           = onStart,
      options           = options,
      enableBookmarking = enableBookmarking,
      uiPattern         = uiPattern
    ),
    golem_opts = list(...)
  )
}
