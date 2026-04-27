#' Generate a Prior Justification Report
#'
#' Renders an HTML, PDF, or Word (.docx) report using rmarkdown::render().
#' No external Quarto installation required.
#'
#' @param prior A bayprior object.
#' @param conflict Optional bayprior_conflict from prior_conflict().
#' @param sensitivity Optional bayprior_sensitivity from sensitivity_grid().
#' @param output_format "html" (default), "pdf", or "docx".
#' @param output_file Output path without extension.
#' @param trial_name Trial / protocol identifier.
#' @param sponsor Sponsor name.
#' @param date Report date string. Default Sys.Date().
#' @param author Responsible statistician.
#' @param notes Optional narrative text.
#' @param open_after Open after rendering. Default TRUE interactively.
#'
#' @return Path to the rendered report, invisibly.
#' @export
prior_report <- function(prior,
                         conflict      = NULL,
                         sensitivity   = NULL,
                         output_format = c("html", "pdf", "docx"),
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
  ext <- switch(output_format, html = ".html", pdf = ".pdf", docx = ".docx")
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

  if (!nzchar(rmd_src)) {
    cli::cli_alert_warning(
      "Bundled template not found - using built-in fallback template.")
    rmd_src <- .write_fallback_template()
  }

  # Copy Rmd and any supporting files to temp dir
  tmp_dir <- tempfile("bayprior_report_")
  dir.create(tmp_dir, recursive = TRUE)
  tmp_rmd <- file.path(tmp_dir, "prior_report.Rmd")
  file.copy(rmd_src, tmp_rmd, overwrite = TRUE)

  # For docx: locate or generate a reference_doc for styling
  ref_docx <- NULL
  if (output_format == "docx") {
    ref_docx_src <- system.file(
      "rmarkdown", "templates", "prior_report", "skeleton",
      "bayprior_reference.docx",
      package = "bayprior"
    )
    if (nzchar(ref_docx_src)) {
      # Copy bundled reference doc alongside the Rmd so rmarkdown can find it
      file.copy(ref_docx_src,
                file.path(tmp_dir, "bayprior_reference.docx"),
                overwrite = TRUE)
      ref_docx <- "bayprior_reference.docx"
    } else {
      cli::cli_alert_info(paste0(
        "No bundled Word reference template found. ",
        "Using default Word styles. ",
        "To customise, place bayprior_reference.docx in ",
        "inst/rmarkdown/templates/prior_report/skeleton/."
      ))
    }
  }

  # Output format object
  out_fmt <- switch(output_format,
    html = rmarkdown::html_document(
      toc             = TRUE,
      toc_float       = TRUE,
      toc_depth       = 3,
      theme           = "flatly",
      highlight       = "tango",
      number_sections = TRUE,
      code_folding    = "hide",
      self_contained  = TRUE
    ),
    pdf = rmarkdown::pdf_document(
      toc             = TRUE,
      number_sections = TRUE,
      latex_engine    = "xelatex"
    ),
    docx = {
      wd_args <- list(
        toc         = TRUE,
        toc_depth   = 3,
        fig_width   = 6.5,
        fig_height  = 4,
        fig_caption = TRUE
      )
      # Only pass reference_docx when we actually have one — "default" is not
      # a valid value and causes rmarkdown::word_document() to error.
      if (!is.null(ref_docx)) wd_args$reference_docx <- ref_docx
      do.call(rmarkdown::word_document, wd_args)
    }
  )

  params <- list(
    prior       = prior,
    conflict    = conflict,
    sensitivity = sensitivity,
    trial_name  = trial_name,
    sponsor     = sponsor,
    author      = author,
    date        = date,
    notes       = notes
    # output_format is intentionally excluded: rmarkdown::render() rejects
    # params not declared in the Rmd YAML when passed alongside a format object.
    # The Rmd detects the format via knitr::opts_knit$get("rmarkdown.pandoc.to").
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


# ── Internal helpers ──────────────────────────────────────────────────────────

# Safe round: coerces to numeric first; non-convertible values become NA
# rather than throwing "non-numeric argument to mathematical function".
.safe_round <- function(x, digits = 4) {
  x <- suppressWarnings(as.numeric(x))
  round(x, digits)
}

# Safe scalar accessor: returns a character fallback when a field is
# NULL, NA, or length-0 (common for mixture priors or partially-built objects).
.safe_chr <- function(x, fallback = "—") {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) {
    return(fallback)
  }
  as.character(x[[1]])
}

# Flatten prior params safely for a kable row, handling mixture priors
# whose $params is list(weights = c(...)) rather than named scalars.
.params_df <- function(pr) {
  p <- pr$params
  if (is.null(p) || length(p) == 0) {
    return(data.frame(Parameter = character(0), Value = character(0)))
  }
  # Mixture: show rounded weights rather than a nested list
  if (pr$dist == "mixture") {
    w <- pr$weights %||% p$weights
    return(data.frame(
      Parameter = paste0("weight[", seq_along(w), "]"),
      Value     = as.character(round(w, 6)),
      stringsAsFactors = FALSE
    ))
  }
  vals <- tryCatch(unlist(p), error = function(e) NULL)
  if (is.null(vals)) {
    return(data.frame(Parameter = character(0), Value = character(0)))
  }
  data.frame(
    Parameter = names(vals),
    Value     = as.character(.safe_round(vals, 6)),
    stringsAsFactors = FALSE
  )
}

# Flatten fit_summary safely: returns NA for any missing field.
.fit_summary_safe <- function(s) {
  get_field <- function(nm) {
    v <- s[[nm]]
    if (is.null(v) || length(v) == 0) NA_real_ else as.numeric(v[[1]])
  }
  list(
    mean = get_field("mean"),
    sd   = get_field("sd"),
    q025 = get_field("q025"),
    q500 = get_field("q500"),
    q975 = get_field("q975")
  )
}


# Internal fallback template written inline - used if inst/ template missing
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
    '',
    '# Self-contained helpers — no dependency on %||% or rlang in render env',
    '.null_or <- function(x, default) if (is.null(x) || length(x) == 0L) default else x',
    '.safe_round <- function(x, digits=4) { x <- suppressWarnings(as.numeric(x)); round(x, digits) }',
    '.safe_chr <- function(x, fallback="\u2014") {',
    '  if (is.null(x) || length(x)==0L || (length(x)==1L && is.na(x))) return(fallback)',
    '  as.character(x[[1L]])',
    '}',
    '.fs <- function(s, nm) { v <- s[[nm]]; if (is.null(v)||length(v)==0L) NA_real_ else suppressWarnings(as.numeric(v[[1L]])) }',
    '.params_df <- function(prior_obj) {',
    '  p <- prior_obj$params',
    '  if (is.null(p)||length(p)==0L) return(data.frame(Parameter=character(0L),Value=character(0L),stringsAsFactors=FALSE))',
    '  if (identical(prior_obj$dist,"mixture")) {',
    '    w <- prior_obj$weights; if (is.null(w)) w <- p$weights; if (is.null(w)) w <- numeric(0L)',
    '    return(data.frame(Parameter=paste0("weight[",seq_along(w),"]"),Value=as.character(round(w,6L)),stringsAsFactors=FALSE))',
    '  }',
    '  vals <- tryCatch(unlist(p),error=function(e) NULL)',
    '  if (is.null(vals)||!is.numeric(vals)) return(data.frame(Parameter=character(0L),Value=character(0L),stringsAsFactors=FALSE))',
    '  data.frame(Parameter=names(vals),Value=as.character(.safe_round(vals,6L)),stringsAsFactors=FALSE)',
    '}',
    '.safe_plot <- function(expr_fn, msg="Plot could not be rendered") {',
    '  tryCatch(expr_fn(), error=function(e) cat(msg,":",conditionMessage(e),"\n"))',
    '}',
    '```',
    '',
    '# Prior Specification',
    '',
    '```{r params-table}',
    'if (has_prior) {',
    '  s <- pr$fit_summary',
    '  kable(data.frame(',
    '    Item  = c("Distribution","Method","Expert","Mean","SD","95% CrI"),',
    '    Value = c(',
    '      toupper(.safe_chr(pr$dist)),',
    '      .safe_chr(pr$method),',
    '      .safe_chr(pr$expert_id),',
    '      as.character(.safe_round(.fs(s,"mean"),4)),',
    '      as.character(.safe_round(.fs(s,"sd"),4)),',
    '      paste0("[", .safe_round(.fs(s,"q025"),4), ",  ", .safe_round(.fs(s,"q975"),4), "]")',
    '    ), stringsAsFactors=FALSE',
    '  ), col.names=c("Item","Value"), align="ll")',
    '}',
    '```',
    '',
    '```{r hyperparams-table}',
    'if (has_prior) {',
    '  df <- .params_df(pr)',
    '  if (nrow(df) > 0L)',
    '    kable(df, col.names=c("Parameter","Value"), caption="Fitted hyperparameters", align="lr")',
    '}',
    '```',
    '',
    '```{r prior-plot, fig.cap="Elicited prior. Shaded = 95% CrI."}',
    'if (has_prior) .safe_plot(function() plot(pr), "Prior density plot could not be rendered")',
    '```',
    '',
    '# Conflict Diagnostics',
    '',
    '```{r conflict-table}',
    'if (has_conf) {',
    '  kable(data.frame(',
    '    Statistic = c("Box p-value","Surprise index","KL divergence","Overlap","Severity"),',
    '    Value = c(',
    '      as.character(.safe_round(cd$box_pvalue, 4)),',
    '      as.character(.safe_round(cd$surprise_index, 3)),',
    '      as.character(.safe_round(cd$kl_prior_likelihood, 3)),',
    '      as.character(.safe_round(cd$overlap, 3)),',
    '      toupper(.safe_chr(cd$conflict_severity))',
    '    ), stringsAsFactors=FALSE',
    '  ), caption="Conflict diagnostics", align="ll")',
    '} else { cat("No conflict diagnostics computed.") }',
    '```',
    '',
    '```{r overlay-plot}',
    'if (has_conf && has_prior)',
    '  .safe_plot(function() plot_prior_likelihood(pr, cd$data_summary, show_posterior=TRUE),',
    '             "Overlay plot could not be rendered")',
    '```',
    '',
    '# Sensitivity Analysis',
    '',
    '```{r sensitivity-influence}',
    'if (has_sens) {',
    '  sc  <- sa$influence_scores',
    '  nms <- names(sc)',
    '  if (is.null(nms) || length(nms)==0L) nms <- paste0("target_", seq_along(sc))',
    '  kable(data.frame(',
    '    Target    = nms,',
    '    Range     = as.character(.safe_round(sc, 5)),',
    '    Sensitive = ifelse(is.na(sc) | sc > 0.05, "Yes", "No"),',
    '    stringsAsFactors = FALSE',
    '  ), caption="Posterior sensitivity to prior hyperparameters", align="lrr")',
    '} else { cat("No sensitivity analysis computed.") }',
    '```',
    '',
    '```{r tornado}',
    'if (has_sens) .safe_plot(function() plot_tornado(sa), "Tornado plot could not be rendered")',
    '```',
    '',
    '# Session Info',
    '',
    '```{r session}',
    'si <- sessionInfo()',
    'kable(data.frame(',
    '  Item=c("R version","bayprior","Platform","Date"),',
    '  Value=c(',
    '    paste(si$R.version$major, si$R.version$minor, sep="."),',
    '    tryCatch(as.character(utils::packageVersion("bayprior")), error=function(e) "unknown"),',
    '    si$platform,',
    '    .null_or(params$date, as.character(Sys.Date()))',
    '  ), stringsAsFactors=FALSE',
    '), align="ll")',
    '```'
  ))
  tmp
}
