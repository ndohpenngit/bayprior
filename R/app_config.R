# nocov start
#' Access files in the current app
#' @param ... Path components inside inst/
#' @noRd
app_sys <- function(...) system.file(..., package = "bayprior")

#' Read golem config value
#' @noRd
get_golem_config <- function(value,
                              config = Sys.getenv("R_CONFIG_ACTIVE", "default"),
                              use_parent = TRUE, ...) {
  config::get(value = value, config = config,
              file = app_sys("golem-config.yml"),
              use_parent = use_parent, ...)
}
# nocov end
