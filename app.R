# Install bayprior from the bundled source
if (!requireNamespace("bayprior", quietly = TRUE)) {
  install.packages(".", repos = NULL, type = "source")
}
bayprior::run_app()
