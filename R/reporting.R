#' Generate a Prior Justification Report
#'
#' Renders an HTML or PDF report using rmarkdown::render().
#' No external Quarto installation required.
#'
#' @param prior A bayprior object.
#' @param conflict Optional bayprior_conflict from prior_conflict().
#' @param sensitivity Optional bayprior_sensitivity from sensitivity_grid().
#' @param output_format "html" (default) or "pdf".
#' @param output_file Output path without extension.
#' @param trial_name Trial / protocol identifier.
#' @param sponsor Sponsor name.
#' @param date Report date string. Default Sys.Date().
#' @param author Responsible statistician.
#' @param notes Optional narrative text.
#' @param open_after Open in browser after rendering. Default TRUE interactively.
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

  rlang::check_installed("rmarkdown",
    reason = "required to render the prior justification report.")
  rlang::check_installed("knitr",
    reason = "required to render the prior justification report.")

  # Output path
  ext <- if (output_format == "html") ".html" else ".pdf"
  if (is.null(output_file))
    output_file <- paste0("prior_justification_report", ext)
  if (!grepl(paste0("\\", ext, "$"), output_file))
    output_file <- paste0(output_file, ext)
  output_file <- normalizePath(output_file, mustWork = FALSE)

  # Locate bundled Rmd template
  rmd_src <- system.file(
    "rmarkdown", "templates", "prior_report", "skeleton", "skeleton.Rmd",
    package = "bayprior"
  )

  # Fall back to inline template if not found
  if (!nzchar(rmd_src)) {
    cli::cli_alert_warning(
      "Bundled template not found — using built-in fallback template.")
    rmd_src <- .write_fallback_template()
  }

  # Copy to temp dir
  tmp_dir <- tempfile("bayprior_report_")
  dir.create(tmp_dir, recursive = TRUE)
  tmp_rmd <- file.path(tmp_dir, "prior_report.Rmd")
  file.copy(rmd_src, tmp_rmd, overwrite = TRUE)

  # Output format object
  out_fmt <- if (output_format == "html") {
    rmarkdown::html_document(
      toc             = TRUE,
      toc_float       = TRUE,
      toc_depth       = 3,
      theme           = "flatly",
      highlight       = "tango",
      number_sections = TRUE,
      code_folding    = "hide",
      self_contained  = TRUE
    )
  } else {
    rmarkdown::pdf_document(
      toc             = TRUE,
      number_sections = TRUE,
      latex_engine    = "xelatex"
    )
  }

  # Params
  params <- list(
    prior       = prior,
    conflict    = conflict,
    sensitivity = sensitivity,
    trial_name  = trial_name,
    sponsor     = sponsor,
    author      = author,
    date        = date,
    notes       = notes
  )

  cli::cli_progress_step("Rendering {output_format} report...")

  rendered <- rmarkdown::render(
    input         = tmp_rmd,
    output_format = out_fmt,
    output_file   = basename(output_file),
    output_dir    = tmp_dir,
    params        = params,
    envir         = new.env(parent = globalenv()),
    quiet         = TRUE
  )

  file.copy(rendered, output_file, overwrite = TRUE)
  cli::cli_alert_success("Report written to: {.path {output_file}}")
  if (open_after) utils::browseURL(output_file)
  invisible(output_file)
}


# Internal fallback template written inline — used if inst/ template missing
.write_fallback_template <- function() {
  tmp <- tempfile(fileext = ".Rmd")
  writeLines(con = tmp, text = c(
    '---',
    'title: "Bayesian Prior Justification Report"',
    'subtitle: "`r params$trial_name`"',
    'author: "`r params$author`"',
    'date: "`r params$date`"',
    'output:',
    '  html_document:',
    '    toc: true',
    '    toc_float: true',
    '    number_sections: true',
    '    theme: flatly',
    'params:',
    '  prior:       NULL',
    '  conflict:    NULL',
    '  sensitivity: NULL',
    '  trial_name:  "Clinical Trial"',
    '  sponsor:     "Sponsor"',
    '  author:      "Biostatistics"',
    '  date:        ""',
    '  notes:       ""',
    '---',
    '',
    '```{r setup, include=FALSE}',
    'knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)',
    'library(bayprior); library(ggplot2); library(knitr)',
    'pr <- params$prior; cd <- params$conflict; sa <- params$sensitivity',
    'has_prior <- !is.null(pr); has_conf <- !is.null(cd); has_sens <- !is.null(sa)',
    '```',
    '',
    '# Prior Specification',
    '',
    '```{r params-table}',
    'if (has_prior) {',
    '  s <- pr$fit_summary',
    '  kable(data.frame(',
    '    Item  = c("Distribution","Method","Expert","Mean","SD","95% CrI"),',
    '    Value = c(toupper(pr$dist), pr$method, pr$expert_id,',
    '              round(s$mean,4), round(s$sd,4),',
    '              paste0("[",round(s$q025,4),", ",round(s$q975,4),"]"))',
    '  ), col.names=c("Item","Value"), align="ll")',
    '}',
    '```',
    '',
    '```{r prior-plot, fig.cap="Elicited prior. Shaded = 95% CrI."}',
    'if (has_prior) plot(pr)',
    '```',
    '',
    '# Conflict Diagnostics',
    '',
    '```{r conflict-table}',
    'if (has_conf) {',
    '  kable(data.frame(',
    '    Statistic = c("Box p-value","Surprise index","Overlap","Severity"),',
    '    Value = c(round(cd$box_pvalue,4), round(cd$surprise_index,3),',
    '              round(cd$overlap,3), toupper(cd$conflict_severity))',
    '  ), caption="Conflict diagnostics", align="ll")',
    '} else { cat("No conflict diagnostics computed.") }',
    '```',
    '',
    '```{r overlay-plot}',
    'if (has_conf && has_prior)',
    '  plot_prior_likelihood(pr, cd$data_summary, show_posterior=TRUE)',
    '```',
    '',
    '# Sensitivity Analysis',
    '',
    '```{r tornado}',
    'if (has_sens) plot_tornado(sa) else cat("No sensitivity analysis computed.")',
    '```',
    '',
    '# Session Info',
    '',
    '```{r session}',
    'si <- sessionInfo()',
    'kable(data.frame(',
    '  Item=c("R version","bayprior","Platform","Date"),',
    '  Value=c(paste(si$R.version$major,si$R.version$minor,sep="."),',
    '          as.character(utils::packageVersion("bayprior")),',
    '          si$platform, params$date)',
    '), align="ll")',
    '```'
  ))
  tmp
}