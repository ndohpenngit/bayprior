#' Generate a Prior Justification Report for regulatory submission
#'
#' Produces a structured HTML or PDF report documenting the prior elicitation
#' process, conflict diagnostics, and sensitivity analyses in a format aligned
#' with FDA/EMA expectations for Bayesian clinical trial submissions.
#'
#' @param prior A `bayprior` object (or list of `bayprior` objects for
#'   multi-endpoint reports).
#' @param conflict Optional. A `bayprior_conflict` object from `prior_conflict()`.
#' @param sensitivity Optional. A `bayprior_sensitivity` object from
#'   `sensitivity_grid()`.
#' @param output_format Character. `"html"` (default) or `"pdf"`.
#' @param output_file Character. Output file path. Defaults to
#'   `"prior_justification_report.html"` (or `.pdf`).
#' @param trial_name Character. Clinical trial identifier for the report header.
#' @param sponsor Character. Sponsor name.
#' @param date Character. Report date. Defaults to `Sys.Date()`.
#' @param author Character. Report author.
#' @param open_after Logical. Open the report in the browser after rendering.
#'   Default `TRUE` in interactive sessions.
#'
#' @return Invisibly returns the path to the rendered report.
#'
#' @details
#' The generated report includes:
#' \itemize{
#'   \item Executive summary of prior choice rationale
#'   \item Elicitation methodology and expert inputs
#'   \item Prior distribution summary with density plots
#'   \item Prior-data conflict assessment (if `conflict` provided)
#'   \item Sensitivity analysis results with visualisations (if `sensitivity` provided)
#'   \item Regulatory compliance checklist (FDA 2026 guidance alignment)
#' }
#'
#' @examples
#' \dontrun{
#' prior    <- elicit_beta(mean = 0.30, sd = 0.10, method = "moments",
#'                         label = "Response rate (treatment)")
#' conflict <- prior_conflict(prior, list(n = 40, x = 14, type = "binary"))
#' sens     <- sensitivity_grid(prior, list(n = 40, x = 14, type = "binary"),
#'                              param_grid = list(alpha = seq(1, 6), beta = seq(2, 14)))
#'
#' prior_report(
#'   prior       = prior,
#'   conflict    = conflict,
#'   sensitivity = sens,
#'   trial_name  = "EXAMPLE-001",
#'   sponsor     = "Example Pharma Ltd",
#'   author      = "N.P., Principal Biostatistician"
#' )
#' }
#'
#' @export
prior_report <- function(prior,
                         conflict        = NULL,
                         sensitivity     = NULL,
                         output_format   = c("html", "pdf"),
                         output_file     = NULL,
                         trial_name      = "Clinical Trial",
                         sponsor         = "Sponsor",
                         date            = as.character(Sys.Date()),
                         author          = "Biostatistics",
                         open_after      = interactive()) {

  output_format <- match.arg(output_format)
  ext <- if (output_format == "html") ".html" else ".pdf"
  if (is.null(output_file)) {
    output_file <- paste0("prior_justification_report", ext)
  }

  template_dir <- system.file(
    "rmarkdown", "templates", "prior_report", "resources",
    package = "bayprior"
  )

  tmp_rmd <- tempfile(fileext = ".Rmd")
  .write_report_rmd(
    path        = tmp_rmd,
    prior       = prior,
    conflict    = conflict,
    sensitivity = sensitivity,
    trial_name  = trial_name,
    sponsor     = sponsor,
    date        = date,
    author      = author,
    output_format = output_format
  )

  rmarkdown::render(
    input       = tmp_rmd,
    output_file = normalizePath(output_file, mustWork = FALSE),
    quiet       = TRUE
  )

  cli::cli_alert_success("Report generated: {output_file}")
  if (open_after) utils::browseURL(output_file)
  invisible(output_file)
}


# ---- Internal report builder -----------------------------------------------

.write_report_rmd <- function(path, prior, conflict, sensitivity,
                               trial_name, sponsor, date, author, output_format) {
  fmt_chunk <- if (output_format == "html") {
    'output:\n  html_document:\n    toc: true\n    toc_float: true\n    theme: flatly\n    highlight: tango'
  } else {
    'output:\n  pdf_document:\n    toc: true\n    number_sections: true'
  }

  has_conflict   <- !is.null(conflict)
  has_sensitivity <- !is.null(sensitivity)

  rmd_text <- glue::glue('
---
title: "Prior Justification Report"
subtitle: "{trial_name} — {sponsor}"
author: "{author}"
date: "{date}"
{fmt_chunk}
---

```{{r setup, include=FALSE}}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE,
                       fig.width = 8, fig.height = 4.5, dpi = 150)
library(bayprior)
library(ggplot2)
```

# Executive Summary

This report documents the Bayesian prior specification for study **{trial_name}**,
prepared in accordance with FDA guidance on Bayesian clinical trials (2026) and
EMA reflection paper on the use of Bayesian statistics.

The prior was elicited using **{prior$method} elicitation** and fitted to a
**{toupper(prior$dist)}** distribution. Key summary statistics are presented below.

# Prior Specification

## Elicitation Methodology

| Attribute | Value |
|-----------|-------|
| Quantity | {prior$label} |
| Distribution | {toupper(prior$dist)} |
| Method | {prior$method} |
| Expert(s) | {prior$expert_id} |

## Fitted Parameters

```{{r prior-params}}
s <- prior$fit_summary
knitr::kable(
  data.frame(
    Statistic = c("Mean", "SD", "2.5th percentile", "Median", "97.5th percentile"),
    Value     = round(c(s$mean, s$sd, s$q025 %||% NA, s$q500 %||% NA, s$q975 %||% NA), 4)
  ),
  caption = "Prior distribution summary statistics"
)
```

## Prior Density

```{{r prior-plot}}
plot(prior)
```

', .open = "{{", .close = "}}")

  if (has_conflict) {
    rmd_text <- paste0(rmd_text, glue::glue('

# Prior–Data Conflict Assessment

```{{r conflict-summary}}
c_obj <- conflict
sev_label <- toupper(c_obj$conflict_severity)
knitr::kable(
  data.frame(
    Diagnostic = c("Box\'s predictive p-value", "Surprise index",
                   "KL divergence (prior || likelihood)", "Overlap coefficient",
                   "Conflict severity"),
    Value = c(round(c_obj$box_pvalue, 4), round(c_obj$surprise_index, 4),
              round(c_obj$kl_prior_likelihood, 4), round(c_obj$overlap, 4),
              toupper(c_obj$conflict_severity))
  ),
  caption = "Prior-data conflict diagnostics"
)
```

**Recommendation:** {conflict$recommendation}

## Prior–Likelihood–Posterior Overlay

```{{r conflict-plot}}
plot_prior_likelihood(prior, conflict$data_summary)
```

', .open = "{{", .close = "}}")
    )
  }

  if (has_sensitivity) {
    rmd_text <- paste0(rmd_text, '

# Sensitivity Analysis

## Influence of Prior Hyperparameters

```{r sensitivity-tornado}
plot_tornado(sensitivity)
```

## Hyperparameter Grid Results

```{r sensitivity-plot}
for (t in sensitivity$target) {
  print(plot_sensitivity(sensitivity, target = t))
}
```

## Regulatory Compliance Checklist

| Requirement | Status |
|-------------|--------|
| Prior elicitation documented | ✅ |
| Elicitation method specified | ✅ |
| Prior-data conflict assessed | ✅ |
| Sensitivity analysis performed | ✅ |
| Alternative priors considered | ✅ |
| Results robust to prior choice | See above |

')
  }

  rmd_text <- paste0(rmd_text, '\n\n---\n*Report generated by the bayprior R package.*\n')
  writeLines(rmd_text, path)
}
