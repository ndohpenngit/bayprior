#' Generate a Prior Justification Report for regulatory submission
#'
#' Renders the bayprior Quarto template to HTML or PDF using
#' \code{quarto::quarto_render()}.  Requires Quarto >= 1.4 on PATH.
#'
#' @param prior A \code{bayprior} object.
#' @param conflict Optional \code{bayprior_conflict} from \code{prior_conflict()}.
#' @param sensitivity Optional \code{bayprior_sensitivity} from \code{sensitivity_grid()}.
#' @param output_format \code{"html"} (default) or \code{"pdf"}.
#' @param output_file Output path (without extension). Default \code{"prior_justification_report"}.
#' @param trial_name Trial / protocol identifier.
#' @param sponsor Sponsor name.
#' @param date Report date string. Default \code{Sys.Date()}.
#' @param author Responsible statistician.
#' @param notes Optional narrative / justification text.
#' @param open_after Open in browser after rendering. Default \code{TRUE} interactively.
#'
#' @return Path to the rendered report, invisibly.
#' @export
prior_report <- function(prior,
                         conflict      = NULL,
                         sensitivity   = NULL,
                         output_format = c("html", "pdf"),
                         output_file   = NULL,
                         trial_name    = "Clinical Trial",
                         sponsor       = "Sponsor",
                         date          = as.character(Sys.Date()),
                         author        = "Biostatistics",
                         notes         = "",
                         open_after    = interactive()) {

  output_format <- match.arg(output_format)

  # Quarto must be installed
  rlang::check_installed("quarto",
    reason = "required to render the prior justification report.")
  if (!quarto::quarto_available()) {
    rlang::abort(c(
      "Quarto is not installed or not found on PATH.",
      "i" = "Install from <https://quarto.org> and restart R."))
  }

  ext         <- if (output_format == "html") ".html" else ".pdf"
  output_file <- output_file %||% paste0("prior_justification_report", ext)
  if (!grepl(paste0("\\", ext, "$"), output_file))
    output_file <- paste0(output_file, ext)
  output_file <- normalizePath(output_file, mustWork = FALSE)

  # Locate bundled .qmd
  qmd_src <- system.file(
    "rmarkdown", "templates", "prior_report", "skeleton", "prior_report.qmd",
    package = "bayprior")
  if (!nzchar(qmd_src))
    rlang::abort("Quarto template not found. Please reinstall bayprior.")

  # Copy to writable temp dir (quarto_render needs write access next to input)
  tmp_dir <- tempfile("bayprior_report_")
  dir.create(tmp_dir, recursive = TRUE)
  tmp_qmd <- file.path(tmp_dir, "prior_report.qmd")
  file.copy(qmd_src, tmp_qmd, overwrite = TRUE)

  # Persist R objects as RDS; paths passed as execute params
  saveRDS(prior,       file.path(tmp_dir, "prior.rds"))
  saveRDS(conflict,    file.path(tmp_dir, "conflict.rds"))
  saveRDS(sensitivity, file.path(tmp_dir, "sensitivity.rds"))

  params <- list(
    prior_rds       = file.path(tmp_dir, "prior.rds"),
    conflict_rds    = file.path(tmp_dir, "conflict.rds"),
    sensitivity_rds = file.path(tmp_dir, "sensitivity.rds"),
    trial_name = trial_name, sponsor = sponsor,
    author = author, date = date, notes = notes
  )

  cli::cli_progress_step("Rendering {output_format} report with Quarto...")

  quarto::quarto_render(
    input          = tmp_qmd,
    output_format  = if (output_format == "html") "html" else "pdf",
    output_file    = basename(output_file),
    execute_params = params,
    quiet          = TRUE
  )

  rendered <- file.path(tmp_dir, basename(output_file))
  if (file.exists(rendered))
    file.copy(rendered, output_file, overwrite = TRUE)

  cli::cli_alert_success("Report written to: {.path {output_file}}")
  if (open_after) utils::browseURL(output_file)
  invisible(output_file)
}
