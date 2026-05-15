# ── Validation utilities ──────────────────────────────────────────────────────
# Centralised compatibility checks used across conflict, sensitivity,
# pooling and elicitation modules.

# ── Distribution support classification ──────────────────────────────────────
.dist_support <- function(dist) {
  switch(dist,
    beta        = "unit",      # (0, 1)
    normal      = "real",      # (-Inf, Inf)
    gamma       = "positive",  # (0, Inf)
    lognormal   = "positive",
    exponential = "positive",
    weibull     = "positive",
    mixture     = "mixed",     # determined by components
    "unknown"
  )
}

# Compute effective support for a prior (handles mixtures)
.prior_support <- function(prior) {
  if (prior$dist != "mixture") return(.dist_support(prior$dist))
  supports <- vapply(prior$components, function(c) .dist_support(c$dist),
                     character(1))
  if (length(unique(supports)) == 1) return(supports[[1]])
  "mixed"
}

# ── Prior-data type compatibility ─────────────────────────────────────────────
# Returns a list: list(ok = TRUE/FALSE, msg = "...")

#' @noRd
.check_prior_data_compat <- function(prior, data_summary) {

  type <- data_summary$type
  dist <- if (prior$dist == "mixture")
    prior$components[[which.max(prior$weights)]]$dist
  else
    prior$dist

  # Compatibility matrix
  compat <- list(
    beta        = c("binary"),
    normal      = c("binary", "continuous"),
    gamma       = c("poisson", "survival", "continuous"),
    lognormal   = c("continuous"),
    exponential = c("poisson", "survival"),
    weibull     = c("survival", "poisson", "continuous")
  )

  recommended <- compat[[dist]]

  if (is.null(recommended)) {
    return(list(ok = TRUE, msg = NULL, severity = "none"))
  }

  if (type %in% recommended) {
    return(list(ok = TRUE, msg = NULL, severity = "none"))
  }

  # Not in recommended — build a helpful message
  dist_label <- switch(dist,
    beta        = "Beta",
    normal      = "Normal",
    gamma       = "Gamma",
    lognormal   = "Log-Normal",
    exponential = "Exponential",
    weibull     = "Weibull",
    toupper(dist)
  )

  type_label <- switch(type,
    binary     = "binary (events/n)",
    continuous = "continuous (mean/SD/n)",
    poisson    = "Poisson/count (events/exposure)",
    survival   = "survival (events/follow-up)",
    type
  )

  rec_label <- paste(
    vapply(recommended, function(r) switch(r,
      binary     = "binary",
      continuous = "continuous",
      poisson    = "Poisson/count",
      survival   = "survival",
      r
    ), character(1)),
    collapse = " or "
  )

  msg <- paste0(
    "Compatibility warning: a ", dist_label, " prior is typically used with ",
    rec_label, " data, not ", type_label, ". ",
    "The analysis will proceed using a Normal approximation, but results ",
    "may be less reliable. Consider re-eliciting with a more appropriate ",
    "prior family."
  )

  list(ok = TRUE, msg = msg, severity = "warning")
}

# ── Expert pooling compatibility ──────────────────────────────────────────────
# Called by aggregate_experts() and elicit_mixture()

#' @noRd
.check_pooling_compat <- function(priors) {

  dists    <- vapply(priors, function(p) p$dist, character(1))
  supports <- vapply(dists, .dist_support, character(1))

  results <- list(ok = TRUE, msgs = character(0), severity = "none")

  # Check 1: identical families (ideal)
  if (length(unique(dists)) == 1) return(results)

  # Check 2: incompatible supports (ERROR)
  unique_supports <- unique(supports[supports != "unknown"])
  if (length(unique_supports) > 1) {
    results$ok       <- FALSE
    results$severity <- "error"

    # Build per-family support descriptions
    support_desc <- c(
      unit     = "(0, 1) — proportions only",
      real     = "(-Inf, Inf) — any real value",
      positive = "(0, Inf) — positive values only"
    )

    dist_support_pairs <- paste(
      vapply(seq_along(dists), function(i)
        paste0(toupper(dists[[i]]), " [", support_desc[supports[[i]]], "]"),
        character(1)),
      collapse = ", "
    )

    results$msgs <- paste0(
      "Cannot pool distributions with incompatible supports: ",
      dist_support_pairs, ". ",
      "Pooling requires all distributions to share the same parameter space. ",
      "For example:\n",
      "  - Pooling Beta priors is valid (all defined on (0,1)).\n",
      "  - Pooling Gamma and Exponential is valid (both positive).\n",
      "  - Pooling Beta and Normal is NOT valid (different supports)."
    )
    return(results)
  }

  # Check 3: same support but different families (WARNING)
  if (length(unique(dists)) > 1 && length(unique_supports) == 1) {
    sup <- unique_supports[[1]]

    if (sup == "positive") {
      # Gamma, Lognormal, Weibull, Exponential can be mixed with caution
      results$severity <- "warning"
      results$msgs <- paste0(
        "Pooling distributions from different families on the same support: ",
        paste(toupper(unique(dists)), collapse = " + "), ". ",
        "This is technically valid but the mixture density will be computed ",
        "numerically. Sensitivity analysis on the pooled prior will use the ",
        "dominant component's parameters. Ensure this reflects your intent."
      )
    }
  }

  results
}

# ── Sensitivity analysis compatibility ───────────────────────────────────────
# Returns list(ok, msg, severity)

#' @noRd
.check_sensitivity_compat <- function(prior) {

  # Single-param families: Exponential (only 'rate')
  # These can only produce 1-dimensional grids
  single_param_dists <- c("exponential")

  if (prior$dist %in% single_param_dists) {
    return(list(
      ok       = TRUE,
      msg      = paste0(
        "Note: The ", toupper(prior$dist), " distribution has a single ",
        "parameter (rate). Only one hyperparameter axis will be varied. ",
        "The second slider will be ignored."
      ),
      severity = "info"
    ))
  }

  # Cross-family mixture: sensitivity is ambiguous
  if (prior$dist == "mixture") {
    dists    <- vapply(prior$components, function(c) c$dist, character(1))
    supports <- vapply(dists, .dist_support, character(1))

    if (length(unique(dists)) > 1 && length(unique(supports)) == 1) {
      return(list(
        ok  = TRUE,
        msg = paste0(
          "Sensitivity analysis on a mixture of ", toupper(dists[[1]]),
          " + ", toupper(dists[[2]]),
          " will vary the dominant component's parameters. ",
          "Results reflect sensitivity of the dominant prior, not the mixture."
        ),
        severity = "warning"
      ))
    }

    if (length(unique(supports)) > 1) {
      return(list(
        ok  = FALSE,
        msg = paste0(
          "Sensitivity analysis is not supported for mixtures of distributions ",
          "with incompatible supports (", paste(toupper(unique(dists)),
          collapse = " + "), "). ",
          "Please re-elicit a single-family prior or pool within a single family."
        ),
        severity = "error"
      ))
    }
  }

  list(ok = TRUE, msg = NULL, severity = "none")
}

# ── UI helpers for validation feedback ───────────────────────────────────────
# Renders a coloured alert box for use inside renderUI()

#' @noRd
.validation_alert <- function(msg, severity = "warning") {
  if (is.null(msg) || !nzchar(msg)) return(NULL)

  cfg <- switch(severity,
    info    = list(class = "alert-info",    icon = "circle-info"),
    warning = list(class = "alert-warning", icon = "triangle-exclamation"),
    error   = list(class = "alert-danger",  icon = "circle-xmark"),
    list(class = "alert-info", icon = "circle-info")
  )

  tags$div(
    class = paste("alert", cfg$class),
    style = "font-size:12px; margin:6px 0; padding:8px 10px;",
    icon(cfg$icon), " ", msg
  )
}
