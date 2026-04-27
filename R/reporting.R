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
#' @param prior_plot Optional pre-captured ggplot from plot(prior).
#' @param overlay_plot Optional pre-captured ggplot from plot_prior_likelihood().
#' @param tornado_plot Optional pre-captured ggplot from plot_tornado().
#' @param heatmap_plot Optional pre-captured ggplot from plot_sensitivity().
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
                         prior_plot    = NULL,
                         overlay_plot  = NULL,
                         tornado_plot  = NULL,
                         heatmap_plot  = NULL,
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

  # For docx: locate reference_doc for styling
  ref_docx <- NULL
  if (output_format == "docx") {
    ref_docx_src <- system.file(
      "rmarkdown", "templates", "prior_report", "skeleton",
      "bayprior_reference.docx",
      package = "bayprior"
    )
    if (nzchar(ref_docx_src)) {
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
      if (!is.null(ref_docx)) wd_args$reference_docx <- ref_docx
      do.call(rmarkdown::word_document, wd_args)
    }
  )

  params <- list(
    prior        = prior,
    conflict     = conflict,
    sensitivity  = sensitivity,
    trial_name   = trial_name,
    sponsor      = sponsor,
    author       = author,
    date         = date,
    notes        = notes,
    prior_plot   = prior_plot,
    overlay_plot = overlay_plot,
    tornado_plot = tornado_plot,
    heatmap_plot = heatmap_plot
    # output_format intentionally excluded: rmarkdown::render() rejects params
    # not declared in the Rmd YAML when passed alongside a format object.
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
    '  prior:        !r NULL',
    '  conflict:     !r NULL',
    '  sensitivity:  !r NULL',
    '  trial_name:   "Clinical Trial"',
    '  sponsor:      "Sponsor"',
    '  author:       "Biostatistics"',
    '  date:         ""',
    '  notes:        ""',
    '  prior_plot:   !r NULL',
    '  overlay_plot: !r NULL',
    '  tornado_plot: !r NULL',
    '  heatmap_plot: !r NULL',
    '---',
    '',
    '```{r setup, include=FALSE}',
    'knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)',
    'library(bayprior); library(ggplot2); library(knitr)',
    'pr <- params$prior; cd <- params$conflict; sa <- params$sensitivity',
    'has_prior <- !is.null(pr); has_conf <- !is.null(cd); has_sens <- !is.null(sa)',
    'pp_prior_plot   <- params$prior_plot',
    'pp_overlay_plot <- params$overlay_plot',
    'pp_tornado_plot <- params$tornado_plot',
    'pp_heatmap_plot <- params$heatmap_plot',
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
    '  withCallingHandlers(',
    '    tryCatch(expr_fn(), error=function(e) cat(msg,":",conditionMessage(e),"\n")),',
    '    warning=function(w) {',
    '      if (grepl("different distribution families",conditionMessage(w),fixed=TRUE))',
    '        invokeRestart("muffleWarning")',
    '    }',
    '  )',
    '}',
    '.callout <- function(text, type="note") {',
    '  label <- switch(type, note="Note", warning="Warning", important="Important", tip="Tip", "Note")',
    '  cat(paste0("> **", label, ":** ", text, "\n\n"))',
    '}',
    '```',
    '',
    '# Prior Specification',
    '',
    '```{r params-table}',
    'if (has_prior) {',
    '  s <- pr$fit_summary',
    '  df_e <- data.frame(',
    '    Item  = c("Distribution","Method","Expert","Mean","SD","95% CrI"),',
    '    Value = c(',
    '      toupper(.safe_chr(pr$dist)), .safe_chr(pr$method), .safe_chr(pr$expert_id),',
    '      as.character(.safe_round(.fs(s,"mean"),4)), as.character(.safe_round(.fs(s,"sd"),4)),',
    '      paste0("[", .safe_round(.fs(s,"q025"),4), ",  ", .safe_round(.fs(s,"q975"),4), "]")',
    '    ), stringsAsFactors=FALSE)',
    '  rownames(df_e) <- NULL',
    '  kable(df_e, col.names=c("Item","Value"), align="ll", row.names=FALSE)',
    '}',
    '```',
    '',
    '```{r prior-plot, fig.cap="Elicited prior. Shaded = 95% CrI."}',
    'if (has_prior) {',
    '  if (!is.null(pp_prior_plot) && inherits(pp_prior_plot,"gg")) print(pp_prior_plot)',
    '  else .safe_plot(function() plot(pr), "Prior density plot could not be rendered")',
    '}',
    '```',
    '',
    '# Conflict Diagnostics',
    '',
    '```{r conflict-table}',
    'if (has_conf) {',
    '  df_c <- data.frame(',
    '    Statistic = c("Box p-value","Surprise index","KL divergence","Overlap","Severity"),',
    '    Value = c(',
    '      as.character(.safe_round(cd$box_pvalue,4)), as.character(.safe_round(cd$surprise_index,3)),',
    '      as.character(.safe_round(cd$kl_prior_likelihood,3)), as.character(.safe_round(cd$overlap,3)),',
    '      toupper(.safe_chr(cd$conflict_severity))',
    '    ), stringsAsFactors=FALSE)',
    '  rownames(df_c) <- NULL',
    '  kable(df_c, caption="Conflict diagnostics", align="ll", row.names=FALSE)',
    '} else { cat("No conflict diagnostics computed.") }',
    '```',
    '',
    '```{r overlay-plot}',
    'if (has_conf && has_prior) {',
    '  if (!is.null(pp_overlay_plot) && inherits(pp_overlay_plot,"gg")) print(pp_overlay_plot)',
    '  else .safe_plot(function() plot_prior_likelihood(pr,cd$data_summary,show_posterior=TRUE),"Overlay plot could not be rendered")',
    '}',
    '```',
    '',
    '# Sensitivity Analysis',
    '',
    '```{r sensitivity-influence}',
    'if (has_sens) {',
    '  sc <- sa$influence_scores; nms <- names(sc)',
    '  if (is.null(nms)||length(nms)==0L) nms <- paste0("target_",seq_along(sc))',
    '  df_s <- data.frame(Target=nms, Range=as.character(.safe_round(sc,5)),',
    '    Sensitive=ifelse(is.na(sc)|sc>0.05,"Yes","No"), stringsAsFactors=FALSE)',
    '  rownames(df_s) <- NULL',
    '  kable(df_s, caption="Posterior sensitivity to prior hyperparameters", align="lrr", row.names=FALSE)',
    '} else { cat("No sensitivity analysis computed.") }',
    '```',
    '',
    '```{r tornado}',
    'if (has_sens) {',
    '  if (!is.null(pp_tornado_plot) && inherits(pp_tornado_plot,"gg")) print(pp_tornado_plot)',
    '  else .safe_plot(function() plot_tornado(sa), "Tornado plot could not be rendered")',
    '}',
    '```',
    '',
    '```{r heatmap}',
    'if (has_sens) {',
    '  if (!is.null(pp_heatmap_plot) && inherits(pp_heatmap_plot,"gg")) print(pp_heatmap_plot)',
    '  else .safe_plot(function() plot_sensitivity(sa,target=sa$target[[1L]]),"Heatmap could not be rendered")',
    '}',
    '```',
    '',
    '# Session Info',
    '',
    '```{r session}',
    'si <- sessionInfo()',
    'df_si <- data.frame(',
    '  Item=c("R version","bayprior","Platform","Date"),',
    '  Value=c(paste(si$R.version$major,si$R.version$minor,sep="."),',
    '    tryCatch(as.character(utils::packageVersion("bayprior")),error=function(e)"unknown"),',
    '    si$platform, .null_or(params$date,as.character(Sys.Date()))',
    '  ), stringsAsFactors=FALSE)',
    'rownames(df_si) <- NULL',
    'kable(df_si, align="ll", row.names=FALSE)',
    '```'
  ))
  tmp
}