#' Generate a Prior Justification Report
#'
#' Renders an HTML, PDF, or Word (.docx) report using quarto::quarto_render().
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
#' @param prior_plot    Optional pre-captured ggplot from \code{plot(prior)}.
#' @param overlay_plot  Optional pre-captured ggplot from \code{plot_prior_likelihood()}.
#' @param tornado_plot  Optional pre-captured ggplot from \code{plot_tornado()}.
#' @param heatmap_plot  Optional pre-captured ggplot from \code{plot_sensitivity()}.
#' @param robust_prior   Optional output of robust_prior().
#' @param sceptical_prior Optional output of sceptical_prior().
#' @param power_prior    Optional output of calibrate_power_prior().
#' @param robust_plot    Optional pre-captured ggplot of the robust prior.
#' @param sceptical_plot Optional pre-captured ggplot of the sceptical prior.
#' @param power_plot     Optional pre-captured ggplot of the power prior.
#' @param open_after Open after rendering. Default TRUE interactively.
#'
#' @return Path to the rendered report, invisibly.
#' @export
prior_report <- function(prior,
                         conflict        = NULL,
                         sensitivity     = NULL,
                         robust_prior    = NULL,
                         sceptical_prior = NULL,
                         power_prior     = NULL,
                         output_format   = c("html", "pdf", "docx"),
                         output_file     = NULL,
                         trial_name      = "Clinical Trial",
                         sponsor         = "Sponsor",
                         date            = as.character(Sys.Date()),
                         author          = "Biostatistics",
                         notes           = "",
                         prior_plot      = NULL,
                         overlay_plot    = NULL,
                         tornado_plot    = NULL,
                         heatmap_plot    = NULL,
                         robust_plot     = NULL,
                         sceptical_plot  = NULL,
                         power_plot      = NULL,
                         open_after      = interactive()) {

  output_format <- match.arg(output_format)

  rlang::check_installed("quarto",
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

  # Locate bundled Quarto template
  qmd_src <- system.file(
    "quarto", "templates", "prior_report", "prior_report.qmd",
    package = "bayprior"
  )
  if (!nzchar(qmd_src)) {
    cli::cli_alert_warning(
      "Bundled Quarto template not found - using built-in fallback template.")
    qmd_src <- .write_fallback_qmd()
  }

  # Copy qmd to temp dir
  tmp_dir <- tempfile("bayprior_report_")
  dir.create(tmp_dir, recursive = TRUE)
  tmp_qmd <- file.path(tmp_dir, "prior_report.qmd")
  file.copy(qmd_src, tmp_qmd, overwrite = TRUE)

  # -- Serialize R objects to RDS -------------------------------------
  rds_path <- file.path(tmp_dir, "bayprior_session.rds")
  saveRDS(
    list(
      prior           = prior,
      conflict        = conflict,
      sensitivity     = sensitivity,
      prior_plot      = prior_plot,
      overlay_plot    = overlay_plot,
      tornado_plot    = tornado_plot,
      heatmap_plot    = heatmap_plot,
      robust_prior    = robust_prior,
      robust_plot     = robust_plot,
      sceptical_prior = sceptical_prior,
      sceptical_plot  = sceptical_plot,
      power_prior     = power_prior,
      power_plot      = power_plot
    ),
    file = rds_path
  )

  # Only scalar/character params go through execute_params (YAML-safe)
  execute_params <- list(
    rds_path   = rds_path,
    trial_name = trial_name,
    sponsor    = sponsor,
    author     = author,
    date       = date,
    notes      = notes
  )

  quarto_format <- switch(output_format,
    html = "html", pdf = "pdf", docx = "docx"
  )

  # -- Ensure Quarto CLI is on PATH -------------------------------------
  qp <- tryCatch(quarto::quarto_path(), error = function(e) NULL)
  if (!is.null(qp) && nzchar(qp)) {
    qdir <- dirname(qp)
    cur_path <- Sys.getenv("PATH")
    if (!grepl(qdir, cur_path, fixed = TRUE))
      Sys.setenv(PATH = paste(qdir, cur_path, sep = .Platform$path.sep))
  } else {
    for (candidate in c("/usr/local/bin", "/opt/homebrew/bin",
                        "~/.local/share/quarto/bin",
                        "/Applications/quarto/bin")) {
      candidate <- path.expand(candidate)
      if (file.exists(file.path(candidate, "quarto"))) {
        Sys.setenv(PATH = paste(candidate, Sys.getenv("PATH"),
                               sep = .Platform$path.sep))
        break
      }
    }
  }

  # -- Two-step render: knitr in current session + Quarto for formatting --------
  #
  # Core problem on all remote platforms (shinyapps.io, Posit Connect, etc.):
  # quarto::quarto_render() spawns an Rscript subprocess via the system PATH.
  # That subprocess has no access to packrat/renv libraries, so bayprior is
  # not found regardless of where in the Shiny code the call is placed.
  #
  # Solution (pure Quarto, works everywhere):
  #
  # Step 1 -- knitr::knit() executes all R code IN THE CURRENT R session,
  #   which has the correct library paths. Output: a pre-executed .md file
  #   with all tables, text, and base64-encoded plots already embedded.
  #   Figures are encoded as base64 inline rather than saved as files,
  #   so there are no relative path issues in the output markdown.
  #
  # Step 2 -- quarto::quarto_render(execute=FALSE) on the .md file.
  #   execute=FALSE tells Quarto CLI to skip R entirely (no subprocess spawned)
  #   and only run pandoc to apply the Quarto theme, TOC, and document format.
  #
  # This preserves 100% of Quarto output quality (themes, embed-resources,
  # callout blocks, numbered sections) while avoiding the library path problem.

  cli::cli_progress_step("Executing report chunks in current session...")

  knitr_env        <- new.env(parent = globalenv())
  knitr_env$params <- execute_params

  # Save and restore knitr state
  old_chunk <- knitr::opts_chunk$get(c("echo", "warning", "message",
                                        "fig.path", "dev", "fig.ext",
                                        "dpi", "out.width"))
  old_knit  <- knitr::opts_knit$get(c("base.dir", "upload.fun"))

  # Use relative fig.path for PDF so knitr writes relative paths in .md
  # Absolute fig.path causes Quarto's Lua filter to fail resolving images.
  # Save figures to tmp_dir/figures/ but reference them as figures/ (relative).
  fig_dir_abs <- file.path(tmp_dir, "figures", "")
  dir.create(fig_dir_abs, recursive = TRUE, showWarnings = FALSE)
  fig_dir <- if (output_format == "pdf") "figures/" else fig_dir_abs

  # upload.fun strategy per format:
  # HTML: image_uri encodes as base64 data URIs (fully self-contained)
  # docx: image_uri - pandoc Word handles base64 URIs fine
  # pdf:  identity function (return path as-is) - Quarto/xelatex CANNOT
  #       process base64 data URIs; needs actual PNG files on disk.
  #       upload.fun=NULL causes "attempt to apply non-function"; use identity.
  knitr::opts_knit$set(
    base.dir   = tmp_dir,
    upload.fun = if (output_format == "pdf") function(x, ...) x
                 else knitr::image_uri
  )

  fmt_opts <- switch(output_format,
    html = list(fig.width=6,   fig.height=3.5, out.width="100%", dpi=120),
    docx = list(fig.width=6.5, fig.height=4,   out.width=NULL,   dpi=150),
    pdf  = list(fig.width=7,   fig.height=4,   out.width=NULL,   dpi=150)
  )
  knitr::opts_chunk$set(
    echo      = FALSE, warning = FALSE, message = FALSE,
    fig.path  = fig_dir, dev = "png", fig.ext = "png",
    fig.width = fmt_opts$fig.width,
    fig.height= fmt_opts$fig.height,
    out.width = fmt_opts$out.width,
    dpi       = fmt_opts$dpi
  )

  on.exit({
    knitr::opts_chunk$set(old_chunk)
    knitr::opts_knit$set(old_knit)
  }, add = TRUE)

  tmp_md <- file.path(tmp_dir, "prior_report.md")

  tryCatch(
    knitr::knit(
      input  = tmp_qmd,
      output = tmp_md,
      envir  = knitr_env,
      quiet  = TRUE
    ),
    error = function(e) {
      rlang::abort(paste0(
        "Report chunk execution failed (knitr::knit step).\n",
        "Error: ", conditionMessage(e)
      ))
    }
  )

  if (!file.exists(tmp_md))
    rlang::abort("knitr::knit() did not produce expected output file.")

  # For PDF: inject fig-format: png into YAML so Quarto does not look for
  # .pdf vector figures (its default for LaTeX). Our figures are PNG files.
  if (output_format == "pdf") {
    md_lines <- readLines(tmp_md, warn = FALSE)
    yaml_end <- which(md_lines == "---")[2L]
    if (!is.na(yaml_end)) {
      md_lines <- c(
        md_lines[seq_len(yaml_end - 1L)],
        "fig-format: png",
        md_lines[yaml_end:length(md_lines)]
      )
      writeLines(md_lines, tmp_md)
    }
  }

  # -- Strip {=html} raw blocks for non-HTML formats ------------------------
  # The QMD contains a ```{=html} CSS block for table striping.
  # This is valid Quarto syntax for HTML but breaks LaTeX/Word rendering
  # because pandoc passes the raw block through unprocessed.
  if (output_format != "html") {
    md_lines <- readLines(tmp_md, warn = FALSE)
    in_html_block <- FALSE
    clean_lines   <- character(0)
    for (ln in md_lines) {
      if (grepl("^```\\{=html\\}", ln)) { in_html_block <- TRUE; next }
      if (in_html_block && grepl("^```\\s*$", ln)) { in_html_block <- FALSE; next }
      if (!in_html_block) clean_lines <- c(clean_lines, ln)
    }
    writeLines(clean_lines, tmp_md)
  }

  cli::cli_progress_step("Converting to {output_format} via Quarto (no R subprocess)...")

  # Quarto processes the pre-executed .md using only pandoc - no R execution.
  # The YAML front matter in the .md (preserved from the .qmd) tells Quarto
  # which theme, TOC depth, and format options to apply.
  tryCatch(
    quarto::quarto_render(
      input         = tmp_md,
      output_format = quarto_format,
      output_file   = basename(output_file),
      execute       = FALSE,
      quiet         = TRUE,
      pandoc_args   = character(0)  # Quarto resolves images from .md directory
    ),
    error = function(e) {
      # Retry with quiet=FALSE to surface the full pandoc/xelatex error
      cli::cli_alert_warning("Quarto conversion failed - retrying with verbose output...")
      tryCatch(
        quarto::quarto_render(
          input         = tmp_md,
          output_format = quarto_format,
          output_file   = paste0("retry_", basename(output_file)),
          execute       = FALSE,
          quiet         = FALSE,
          pandoc_args   = if (output_format != "html")
            c("--resource-path", tmp_dir) else character(0)
        ),
        error = function(e2) {
          rlang::abort(paste0(
            "Quarto pandoc conversion failed.\n",
            "Error: ", conditionMessage(e2), "\n",
            "quarto_path(): ",
            tryCatch(quarto::quarto_path(), error = function(e) "ERROR")
          ))
        }
      )
    }
  )

  # Locate rendered file and copy to final destination
  rendered_path <- file.path(tmp_dir, basename(output_file))
  if (!file.exists(rendered_path)) {
    candidates <- list.files(tmp_dir,
                             pattern = paste0("\\", ext, "$"),
                             full.names = TRUE)
    if (length(candidates) > 0L)
      rendered_path <- candidates[[1L]]
    else
      rlang::abort("Rendered output file not found in temp directory.")
  }

  file.copy(rendered_path, output_file, overwrite = TRUE)
  cli::cli_alert_success("Report written to: {.path {output_file}}")
  if (interactive() && isTRUE(open_after))
    tryCatch(get("browseURL", envir = asNamespace("utils"))(output_file),
             error = function(e) invisible(NULL))
  invisible(output_file)
}


# -- Internal helpers -------------------------------------

.safe_round <- function(x, digits = 4) {
  x <- suppressWarnings(as.numeric(x))
  round(x, digits)
}

.safe_chr <- function(x, fallback = "\u2014") {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x)))
    return(fallback)
  as.character(x[[1L]])
}

.fit_summary_safe <- function(s) {
  get_field <- function(nm) {
    v <- s[[nm]]
    if (is.null(v) || length(v) == 0L) NA_real_
    else suppressWarnings(as.numeric(v[[1L]]))
  }
  list(mean = get_field("mean"), sd   = get_field("sd"),
       q025 = get_field("q025"), q500 = get_field("q500"),
       q975 = get_field("q975"))
}


# -- Fallback QMD template -------------------------------------
# Used when inst/quarto/templates/prior_report/prior_report.qmd is not found.
# Written as a character vector (one element per line) so no single line
# exceeds roxygen's 10 000-character limit and no Unicode escapes appear
# inside string literals (which fail in non-UTF-8 locales at parse time).
# Unicode characters are injected at runtime via .u() below.
.u <- function(code) intToUtf8(as.integer(paste0("0x", code)))

.write_fallback_qmd <- function() {
  em  <- .u("2014")   # em-dash
  apo <- .u("2019")   # right single quotation mark (apostrophe)

  lines <- c(
    "---",
    'title: "Bayesian Prior Justification Report"',
    'subtitle: "`r params$trial_name`"',
    'author: "`r params$author`"',
    'date: "`r params$date`"',
    "params:",
    "  rds_path:    ''",
    '  trial_name:  "Clinical Trial"',
    '  sponsor:     "Sponsor"',
    '  author:      "Biostatistics"',
    '  date:        ""',
    '  notes:       ""',
    "format:",
    "  html:",
    "    toc: true",
    "    toc-depth: 3",
    "    number-sections: true",
    "    theme: flatly",
    "    code-fold: true",
    "    embed-resources: true",
    "    fig-width: 8",
    "    fig-height: 4.5",
    "  pdf:",
    "    toc: true",
    "    number-sections: true",
    "    pdf-engine: xelatex",
    "    fig-width: 7",
    "    fig-height: 4",
    "  docx:",
    "    toc: true",
    "    toc-depth: 3",
    "    fig-width: 6.5",
    "    fig-height: 4",
    "execute:",
    "  echo: false",
    "  warning: false",
    "  message: false",
    "---",
    "",
    "```{r setup}",
    "if (!requireNamespace('bayprior', quietly=TRUE)) {",
    "  stop(paste0(",
    "    'bayprior is not installed in the Quarto R library.\\n',",
    "    'Run devtools::install() then retry.'",
    "  ))",
    "}",
    "library(bayprior); library(ggplot2); library(knitr)",
    "if (nzchar(params$rds_path) && file.exists(params$rds_path)) {",
    "  .s <- readRDS(params$rds_path)",
    "} else {",
    "  .s <- list(prior=NULL,conflict=NULL,sensitivity=NULL,",
    "             prior_plot=NULL,overlay_plot=NULL,",
    "             tornado_plot=NULL,heatmap_plot=NULL)",
    "}",
    "pr  <- .s$prior; cd <- .s$conflict; sa <- .s$sensitivity",
    "has_prior <- !is.null(pr); has_conf <- !is.null(cd); has_sens <- !is.null(sa)",
    "pp_prior_plot   <- .s$prior_plot",
    "pp_overlay_plot <- .s$overlay_plot",
    "pp_tornado_plot <- .s$tornado_plot",
    "pp_heatmap_plot <- .s$heatmap_plot",
    ".null_or <- function(x, d) if (is.null(x)||length(x)==0L) d else x",
    ".sr <- function(x, dg=4) { x <- suppressWarnings(as.numeric(x)); round(x,dg) }",
    ".sc <- function(x, fb='-') {",
    "  if (is.null(x)||length(x)==0L||(length(x)==1L&&is.na(x))) return(fb)",
    "  as.character(x[[1L]])",
    "}",
    ".fs <- function(s,nm) {",
    "  v <- s[[nm]]",
    "  if (is.null(v)||length(v)==0L) NA_real_",
    "  else suppressWarnings(as.numeric(v[[1L]]))",
    "}",
    ".cri <- function(s) {",
    "  lo<-.fs(s,'q025'); hi<-.fs(s,'q975')",
    "  mn<-.fs(s,'mean'); sd_v<-.fs(s,'sd')",
    "  if(is.na(lo)) lo <- mn-1.96*sd_v",
    "  if(is.na(hi)) hi <- mn+1.96*sd_v",
    "  paste0('[', .sr(lo,4), ',  ', .sr(hi,4), ']')",
    "}",
    ".pm <- function(p) {",
    "  if (!identical(p$dist,'mixture')) return(.sc(p$method))",
    "  dom <- p$components[[which.max(p$weights)]]",
    "  paste0(.sc(dom$method), ' (pooled)')",
    "}",
    ".pe <- function(p) {",
    "  if (!identical(p$dist,'mixture')) return(.sc(p$expert_id))",
    "  nms <- vapply(p$components, function(x) .sc(x$expert_id), character(1))",
    "  paste(unique(nms[nms!='-']), collapse=', ')",
    "}",
    ".pdf <- function(po) {",
    "  p <- po$params",
    "  if (is.null(p)||length(p)==0L)",
    "    return(data.frame(Parameter=character(0L),Value=character(0L),",
    "                      stringsAsFactors=FALSE))",
    "  if (identical(po$dist,'mixture')) {",
    "    w <- po$weights; if(is.null(w)) w <- p$weights",
    "    if(is.null(w)) w <- numeric(0L)",
    "    return(data.frame(",
    "      Parameter=paste0('weight[',seq_along(w),']'),",
    "      Value=as.character(round(w,6L)),stringsAsFactors=FALSE))",
    "  }",
    "  vals <- tryCatch(unlist(p),error=function(e) NULL)",
    "  if (is.null(vals)||!is.numeric(vals))",
    "    return(data.frame(Parameter=character(0L),Value=character(0L),",
    "                      stringsAsFactors=FALSE))",
    "  data.frame(Parameter=names(vals),",
    "             Value=as.character(.sr(vals,6L)),stringsAsFactors=FALSE)",
    "}",
    ".sp <- function(fn, msg='Plot could not be rendered') {",
    "  withCallingHandlers(",
    "    tryCatch(fn(), error=function(e) cat(msg,':',conditionMessage(e),'\\n')),",
    "    warning=function(w) {",
    "      if (grepl('different distribution families',",
    "                conditionMessage(w),fixed=TRUE))",
    "        invokeRestart('muffleWarning')",
    "    })",
    "}",
    "```",
    "",
    "## Executive Summary",
    "",
    paste0("This report documents the Bayesian prior specification for study ",
           "**`r params$trial_name`**, prepared by **`r params$author`** ",
           "(`r params$sponsor`) and dated `r format(Sys.Date(), '%d %B %Y')`."),
    "",
    "```{r exec-table}",
    "if (has_prior) {",
    "  s <- pr$fit_summary",
    "  df_e <- data.frame(",
    "    Item=c('Distribution family','Elicitation method',",
    "           'Expert / source','Prior mean','Prior SD',",
    "           '95% Credible interval'),",
    "    Value=c(toupper(.sc(pr$dist)),.pm(pr),.pe(pr),",
    "            as.character(.sr(.fs(s,'mean'),4)),",
    "            as.character(.sr(.fs(s,'sd'),4)),",
    "            .cri(s)),stringsAsFactors=FALSE)",
    "  rownames(df_e) <- NULL",
    "  kable(df_e,col.names=c('Item','Value'),align='ll',",
    "        caption='Prior specification summary',row.names=FALSE)",
    "}",
    "```",
    "",
    "```{r notes-block}",
    "#| results: asis",
    "note_text <- .null_or(params$notes, '')",
    "if (nzchar(note_text)) {",
    "  cat('::: {.callout-note}\\n')",
    paste0("  cat(paste0('**Statistician", apo, "s notes:** ', note_text, '\\n'))"),
    "  cat(':::  \\n')",
    "}",
    "```",
    "",
    "## Trial Information",
    "",
    "```{r trial-info}",
    "df_t <- data.frame(",
    "  Field=c('Trial / Protocol','Sponsor','Statistician','Report date'),",
    "  Value=c(.sc(params$trial_name),.sc(params$sponsor),",
    "          .sc(params$author),format(Sys.Date(),'%d %B %Y')),",
    "  stringsAsFactors=FALSE)",
    "rownames(df_t) <- NULL",
    "kable(df_t,col.names=c('Field','Value'),align='ll',row.names=FALSE)",
    "```",
    "",
    "## Prior Specification",
    "",
    "### Elicitation Methodology",
    "",
    "```{r elicitation-method}",
    "if (has_prior) {",
    "  df_el <- data.frame(",
    "    Attribute=c('Quantity','Distribution','Method','Expert / Source'),",
    "    Value=c(.sc(pr$label),toupper(.sc(pr$dist)),.pm(pr),.pe(pr)),",
    "    stringsAsFactors=FALSE)",
    "  rownames(df_el) <- NULL",
    "  kable(df_el,col.names=c('Attribute','Value'),align='ll',row.names=FALSE)",
    "}",
    "```",
    "",
    "### Fitted Parameters",
    "",
    "```{r fitted-params}",
    "if (has_prior) {",
    "  df <- .pdf(pr); rownames(df) <- NULL",
    "  if (nrow(df)>0L)",
    "    kable(df,col.names=c('Parameter','Value'),",
    "          caption='Fitted hyperparameters',align='lr',row.names=FALSE)",
    "  else cat('Hyperparameters not available for this prior type.\\n')",
    "}",
    "```",
    "",
    "### Prior Summary Statistics",
    "",
    "```{r prior-summary}",
    "if (has_prior) {",
    "  s <- pr$fit_summary",
    "  mn<-.fs(s,'mean'); sd_v<-.fs(s,'sd')",
    "  lo<-.fs(s,'q025'); if(is.na(lo)) lo <- mn-1.96*sd_v",
    "  md<-.fs(s,'q500'); if(is.na(md)) md <- mn",
    "  hi<-.fs(s,'q975'); if(is.na(hi)) hi <- mn+1.96*sd_v",
    "  df_s <- data.frame(",
    "    Statistic=c('Mean','SD','2.5th pctile','Median','97.5th pctile'),",
    "    Value=as.character(.sr(c(mn,sd_v,lo,md,hi),5)),",
    "    stringsAsFactors=FALSE)",
    "  rownames(df_s) <- NULL",
    "  kable(df_s,caption='Prior distribution summary statistics',",
    "        align='lr',row.names=FALSE)",
    "}",
    "```",
    "",
    "### Prior Density",
    "",
    "```{r prior-density}",
    "#| fig-cap: 'Elicited prior. Shaded = 95% credible interval.'",
    "if (has_prior) {",
    "  if (!is.null(pp_prior_plot)&&inherits(pp_prior_plot,'gg')) print(pp_prior_plot)",
    "  else .sp(function() plot(pr),'Prior density plot could not be rendered')",
    "}",
    "```",
    "",
    "## Prior-Data Conflict Assessment",
    "",
    "```{r conflict-check}",
    "#| results: asis",
    "if (!has_conf) {",
    "  cat('::: {.callout-warning}\\n')",
    "  cat('No prior-data conflict diagnostics were computed.\\n')",
    "  cat(':::  \\n')",
    "}",
    "```",
    "",
    "```{r conflict-table}",
    "if (has_conf) {",
    "  df_c <- data.frame(",
    "    Diagnostic=c('Box prior predictive p-value',",
    "                 'Surprise index (standardised distance)',",
    "                 'KL divergence (prior || likelihood)',",
    "                 'Bhattacharyya overlap coefficient',",
    "                 'Conflict severity'),",
    "    Value=c(as.character(.sr(cd$box_pvalue,4)),",
    "            as.character(.sr(cd$surprise_index,4)),",
    "            as.character(.sr(cd$kl_prior_likelihood,4)),",
    "            as.character(.sr(cd$overlap,4)),",
    "            toupper(.sc(cd$conflict_severity))),",
    "    stringsAsFactors=FALSE)",
    "  rownames(df_c) <- NULL",
    "  kable(df_c,caption='Prior-data conflict diagnostics',",
    "        align='ll',row.names=FALSE)",
    "}",
    "```",
    "",
    "```{r conflict-rec}",
    "#| results: asis",
    "if (has_conf) {",
    "  sev <- .sc(cd$conflict_severity,fb='none')",
    "  type <- switch(sev,none='tip',mild='warning',severe='important','note')",
    "  cat(paste0('::: {.callout-',type,'}\\n'))",
    "  cat(.sc(cd$recommendation),'\\n')",
    "  cat(':::  \\n')",
    "}",
    "```",
    "",
    "### Prior - Likelihood - Posterior Overlay",
    "",
    "```{r overlay-plot}",
    "#| fig-cap: 'Prior density, scaled likelihood, and posterior.'",
    "if (has_conf&&has_prior) {",
    "  if (!is.null(pp_overlay_plot)&&inherits(pp_overlay_plot,'gg'))",
    "    print(pp_overlay_plot)",
    "  else .sp(function() plot_prior_likelihood(pr,cd$data_summary,",
    "             show_posterior=TRUE),'Overlay could not be rendered')",
    "}",
    "```",
    "",
    "## Sensitivity Analysis",
    "",
    "```{r sens-check}",
    "#| results: asis",
    "if (!has_sens) {",
    "  cat('::: {.callout-warning}\\n')",
    "  cat('No sensitivity analysis was conducted.\\n')",
    "  cat(':::  \\n')",
    "}",
    "```",
    "",
    "```{r sensitivity-influence}",
    "if (has_sens) {",
    "  sc <- sa$influence_scores; nms <- names(sc)",
    "  if (is.null(nms)||length(nms)==0L) nms <- paste0('target_',seq_along(sc))",
    "  df_s <- data.frame(Target=nms,Range=as.character(.sr(sc,5)),",
    "    Sensitive=ifelse(is.na(sc)|sc>0.05,'Yes','No'),stringsAsFactors=FALSE)",
    "  rownames(df_s) <- NULL",
    "  kable(df_s,caption='Posterior sensitivity to prior hyperparameters',",
    "        align='lrr',row.names=FALSE)",
    "}",
    "```",
    "",
    "### Tornado Plot",
    "",
    "```{r tornado}",
    "#| fig-cap: 'Range of each posterior quantity across the hyperparameter grid.'",
    "if (has_sens) {",
    "  if (!is.null(pp_tornado_plot)&&inherits(pp_tornado_plot,'gg'))",
    "    print(pp_tornado_plot)",
    "  else .sp(function() plot_tornado(sa),'Tornado plot could not be rendered')",
    "}",
    "```",
    "",
    "### Hyperparameter Influence Heatmap",
    "",
    "```{r heatmap}",
    "#| fig-cap: 'Posterior mean across the two-dimensional hyperparameter grid.'",
    "if (has_sens) {",
    "  if (!is.null(pp_heatmap_plot)&&inherits(pp_heatmap_plot,'gg'))",
    "    print(pp_heatmap_plot)",
    "  else .sp(function() plot_sensitivity(sa,target=sa$target[[1L]]),",
    "           'Heatmap could not be rendered')",
    "}",
    "```",
    "",
    "## Regulatory Compliance Checklist",
    "",
    "```{r compliance}",
    "df_comp <- data.frame(",
    "  Requirement=c(",
    "    'Prior elicitation method documented',",
    "    'Distribution family and parameters specified',",
    "    'Expert / source of prior identified',",
    "    'Prior density plot provided',",
    "    'Prior-data conflict assessed',",
    "    'Conflict diagnostic statistics reported',",
    "    'Sensitivity analysis performed',",
    "    'Sensitivity visualisations provided',",
    "    'Alternative priors considered',",
    "    'Regulatory report generated'),",
    "  Status=c(",
    "    ifelse(has_prior,'Complete','Missing'),",
    "    ifelse(has_prior,'Complete','Missing'),",
    "    ifelse(has_prior,'Complete','Missing'),",
    "    ifelse(has_prior,'Complete','Missing'),",
    "    ifelse(has_conf,'Complete','Not run'),",
    "    ifelse(has_conf,'Complete','Not run'),",
    "    ifelse(has_sens,'Complete','Not run'),",
    "    ifelse(has_sens,'Complete','Not run'),",
    "    ifelse(has_sens,'See above','Not run'),",
    "    'Complete'),",
    "  stringsAsFactors=FALSE)",
    "rownames(df_comp) <- NULL",
    "kable(df_comp,align='ll',",
    "      caption='FDA/EMA regulatory compliance checklist',row.names=FALSE)",
    "```",
    "",
    "::: {.callout-note}",
    "This checklist maps to considerations in the FDA's 2026 draft guidance,",
    "Use of Bayesian Methodology in Clinical Trials of Drug and Biological",
    "Products, and EMA's 2026 concept paper on the development of a reflection",
    "paper on the use of Bayesian methods in clinical development",
    "(EMA/CHMP/1813/2026).",
    ":::",
    "",
    "## Appendix: Session Information",
    "",
    "```{r session}",
    "si <- sessionInfo()",
    "df_si <- data.frame(",
    "  Item=c('R version','bayprior version',",
    "         'quarto R package','Quarto CLI version',",
    "         'Platform','Date'),",
    "  Value=c(",
    "    paste(si$R.version$major,si$R.version$minor,sep='.'),",
    "    tryCatch(as.character(utils::packageVersion('bayprior')),",
    "             error=function(e)'unknown'),",
    "    tryCatch(as.character(utils::packageVersion('quarto')),",
    "             error=function(e)'unknown'),",
    "    tryCatch(as.character(quarto::quarto_version()),",
    "             error=function(e)'not found'),",
    "    si$platform,",
    "    .null_or(params$date,as.character(Sys.Date()))),",
    "  stringsAsFactors=FALSE)",
    "rownames(df_si) <- NULL",
    "kable(df_si,align='ll',row.names=FALSE)",
    "```",
    "",
    "*Report generated by the **bayprior** R package using `prior_report()`.*"
  )

  tmp <- tempfile(fileext = ".qmd")
  writeLines(lines, con = tmp, useBytes = FALSE)
  tmp
}
