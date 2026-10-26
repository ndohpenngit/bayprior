.bp_chr <- function(x) encodeString(as.character(x), quote = "\"")

.bp_num <- function(x) {
  vapply(as.numeric(x), function(v) {
    if (!is.finite(v)) return(deparse(v))
    s <- format(v, digits = 15, trim = TRUE)
    if (isTRUE(as.numeric(s) == v)) s else format(v, digits = 17, trim = TRUE)
  }, character(1))
}

.bp_c <- function(x) paste0("c(", paste(.bp_num(x), collapse = ", "), ")")

.bp_named_c <- function(x) {
  paste0("c(", paste0(.bp_chr(names(x)), " = ", .bp_num(x), collapse = ", "), ")")
}

.bp_list <- function(x) {
  one <- function(v) if (is.character(v)) .bp_chr(v) else .bp_num(v)
  nm <- names(x)
  nm <- ifelse(make.names(nm) == nm, nm, paste0("`", nm, "`"))
  paste0("list(", paste0(nm, " = ", vapply(x, one, character(1)), collapse = ", "), ")")
}

.bp_call <- function(var, fn, args) {
  n <- length(args)
  c(paste0(var, " <- ", fn, "("),
    paste0("  ", names(args), " = ", args, c(rep(",", n - 1L), "")),
    ")")
}

.bp_fail <- function(why) list(ok = FALSE, lines = character(), reason = why)

.bp_prior_code <- function(p, var = "prior") {
  if (!inherits(p, "bayprior")) return(.bp_fail("No prior is available."))
  lab <- .bp_chr(p$label %||% "Prior")
  eid <- .bp_chr(p$expert_id %||% "Expert")
  done <- function(fn, args) list(ok = TRUE, lines = .bp_call(var, fn, args), reason = NULL)

  if (identical(p$prior_type, "map")) {
    h <- p$historical_data
    tp <- p$tau_prior
    if (is.null(h$y) || is.null(h$se) || is.null(tp$family) || is.null(tp$scale) ||
        is.null(p$outcome_type))
      return(.bp_fail("The MAP prior does not record its inputs."))
    return(done("map_prior", c(
      y = .bp_c(h$y), se = .bp_c(h$se),
      outcome_type = .bp_chr(p$outcome_type),
      tau_prior = paste0("list(family = ", .bp_chr(tp$family),
                         ", scale = ", .bp_num(tp$scale), ")"),
      label = lab)))
  }

  if (p$dist %in% c("mixture", "log_pool")) {
    ag <- p$aggregation
    w <- ag$weights
    comps <- p$components
    if (is.null(ag) || is.null(w) || length(w) != length(comps))
      return(.bp_fail(paste0("Only pooled expert priors are exported; ",
                             "other mixtures (for example robust mixtures) are not yet supported.")))
    parts <- lapply(seq_along(comps), function(i)
      .bp_prior_code(comps[[i]], paste0("expert_", i)))
    bad <- Filter(function(z) !z$ok, parts)
    if (length(bad)) return(bad[[1]])
    pooled <- .bp_call(var, "aggregate_experts", c(
      priors = paste0("list(", paste0(.bp_chr(names(w)), " = expert_", seq_along(w),
                                      collapse = ", "), ")"),
      weights = .bp_c(unname(w)),
      method = .bp_chr(ag$method)))
    return(list(ok = TRUE, lines = c(unlist(lapply(parts, `[[`, "lines")), pooled),
                reason = NULL))
  }

  if (!is.null(p$roulette)) {
    if (!p$dist %in% c("beta", "normal", "gamma", "lognormal"))
      return(.bp_fail("Unsupported roulette family."))
    return(done("elicit_roulette", c(
      chips = .bp_c(p$roulette$chips), breaks = .bp_c(p$roulette$breaks),
      family = .bp_chr(p$dist), expert_id = eid, label = lab)))
  }

  m <- p$method
  inp <- p$input
  fn <- switch(p$dist,
    beta = "elicit_beta", normal = "elicit_normal", gamma = "elicit_gamma",
    lognormal = "elicit_lognormal", exponential = "elicit_exponential",
    weibull = "elicit_weibull", NULL)

  if (identical(m, "direct")) {
    if (!p$dist %in% c("beta", "normal", "gamma"))
      return(.bp_fail("Unsupported prior family."))
    return(done("as_prior", c(
      dist = .bp_chr(p$dist),
      params = paste0("list(", paste0(names(p$params), " = ", .bp_num(unlist(p$params)),
                                      collapse = ", "), ")"),
      label = lab, expert_id = eid)))
  }

  if (is.null(fn) || is.null(m) || is.null(inp))
    return(.bp_fail("The prior does not record how it was elicited."))

  arg <- function(nm) {
    v <- inp[[nm]]
    if (is.null(v)) NULL else if (nm == "quantiles") .bp_named_c(v) else .bp_num(v)
  }
  spec <- switch(m,
    quantile = c(quantiles = arg("quantiles")),
    moments = if (p$dist == "exponential") c(mean = arg("mean"))
              else c(mean = arg("mean"), sd = arg("sd")),
    rate = c(rate = arg("rate")),
    params = c(shape = arg("shape"), scale = arg("scale")),
    NULL)
  need <- switch(m, quantile = 1L, moments = if (p$dist == "exponential") 1L else 2L,
                 rate = 1L, params = 2L, 0L)
  if (is.null(spec) || length(spec) != need)
    return(.bp_fail("The prior does not record its elicitation inputs."))
  done(fn, c(spec, method = .bp_chr(m), expert_id = eid, label = lab))
}

.bp_grid_code <- function(v) {
  n <- length(v)
  if (n >= 2L && identical(seq(v[1], v[n], length.out = n), v))
    paste0("seq(", .bp_num(v[1]), ", ", .bp_num(v[n]), ", length.out = ", n, ")")
  else
    .bp_c(v)
}

.bp_robust_code <- function(r, base) {
  vsd <- r$components$vague$params$sigma
  if (!inherits(r, "bayprior") || !identical(r$prior_type, "robust") ||
      !identical(r$informative, base) || is.null(vsd) || is.null(r$vague_weight))
    return(NULL)
  .bp_call("robust", "robust_prior", c(
    informative = "prior", vague_weight = .bp_num(r$vague_weight),
    vague_sd = .bp_num(vsd), label = .bp_chr(r$label)))
}

.bp_sceptical_code <- function(s) {
  if (!inherits(s, "bayprior") || !identical(s$prior_type, "sceptical") ||
      is.null(s$null_value) || is.null(s$strength))
    return(NULL)
  .bp_call("sceptical", "sceptical_prior", c(
    null_value = .bp_num(s$null_value), family = .bp_chr(s$dist),
    strength = .bp_chr(s$strength), label = .bp_chr(s$label),
    expert_id = .bp_chr(s$expert_id)))
}

.bp_power_code <- function(pw, base) {
  a <- attr(pw, "bp_args")
  if (!inherits(pw, "bayprior_power_prior") || is.null(a$historical_data) ||
      is.null(a$current_data))
    return(NULL)
  same <- tryCatch(
    isTRUE(all.equal(.power_prior_update(base, a$historical_data, pw$delta_opt)$fit_summary,
                     pw$power_prior$fit_summary)),
    error = function(e) FALSE)
  if (!same) return(NULL)
  args <- c(historical_data = "hist_data", current_data = "current_data",
            base_prior = "prior", target_bf = .bp_num(pw$target_bf),
            method = .bp_chr(pw$method))
  if (!identical(as.numeric(pw$delta_grid), seq(0.05, 1.0, by = 0.05)))
    args <- c(args, delta_grid = .bp_c(pw$delta_grid))
  call_lines <- .bp_call("power_calib", "calibrate_power_prior", args)
  c(paste0("hist_data <- ", .bp_list(a$historical_data)),
    paste0("current_data <- ", .bp_list(a$current_data)),
    call_lines,
    "power_prior <- power_calib$power_prior")
}

#' Build a reproducible R script from the app session
#'
#' Rebuilds the base prior from its recorded inputs and, where they were run on
#' it, the robust, sceptical and power priors, the conflict diagnostics and the
#' sensitivity analysis. Anything that cannot be reproduced is left out and
#' noted in the script.
#'
#' @param base The base \code{bayprior} object (pooled consensus or base prior).
#' @param conflict,sensitivity Results from \code{prior_conflict()} and
#'   \code{sensitivity_grid()} / \code{sensitivity_cri()}, carrying a
#'   \code{"bp_args"} attribute with the settings used, or \code{NULL}.
#' @param robust,sceptical,power Results from \code{robust_prior()},
#'   \code{sceptical_prior()} and \code{calibrate_power_prior()}, or \code{NULL}.
#' @param active The prior active in the app; noted when it is not exported.
#' @param date,version Used in the script header.
#' @return A list with \code{ok}, \code{reason} (when not \code{ok}) and
#'   \code{text}, the script as a single string.
#' @noRd
.bp_export_script <- function(base, conflict = NULL, sensitivity = NULL,
                             robust = NULL, sceptical = NULL, power = NULL,
                             active = NULL, date = Sys.Date(), version = NULL) {
  pr <- .bp_prior_code(base, "prior")
  if (!pr$ok) return(list(ok = FALSE, reason = pr$reason, text = NULL))

  version <- version %||% tryCatch(as.character(utils::packageVersion("bayprior")),
                                   error = function(e) "dev")
  out <- c(
    "# bayprior analysis script",
    paste0("# Generated by bayprior ", version, " on ", format(date), " (",
           R.version.string, ")"),
    "",
    "library(bayprior)",
    "",
    pr$lines
  )

  vars <- list(prior = base)
  var_of <- function(obj) {
    for (nm in names(vars)) if (identical(vars[[nm]], obj)) return(nm)
    NULL
  }
  skip <- function(what) c("", paste0("# ", what, " could not be reproduced and is not included."))

  if (!is.null(robust)) {
    z <- .bp_robust_code(robust, base)
    if (is.null(z)) out <- c(out, skip("The robust prior")) else {
      out <- c(out, "", z); vars$robust <- robust
    }
  }
  if (!is.null(sceptical)) {
    z <- .bp_sceptical_code(sceptical)
    if (is.null(z)) out <- c(out, skip("The sceptical prior")) else {
      out <- c(out, "", z); vars$sceptical <- sceptical
    }
  }
  if (!is.null(power)) {
    z <- .bp_power_code(power, base)
    if (is.null(z)) out <- c(out, skip("The power prior")) else {
      out <- c(out, "", z); vars$power_prior <- power$power_prior
    }
  }

  if (!is.null(active) && is.null(var_of(active))) {
    out <- c(out, "", "# The active prior in the app is not included.")
  }

  if (!is.null(conflict)) {
    v <- var_of(conflict$prior)
    if (!is.null(v) && !is.null(conflict$data_summary)) {
      a <- attr(conflict, "bp_args")
      args <- c(v, "data_summary")
      if (!is.null(a$alpha)) args <- c(args, paste0("alpha = ", .bp_num(a$alpha)))
      if (identical(a$exact, FALSE)) args <- c(args, "exact = FALSE")
      out <- c(out, "",
               paste0("data_summary <- ", .bp_list(conflict$data_summary)),
               paste0("conflict <- prior_conflict(", paste(args, collapse = ", "), ")"),
               "print(conflict)")
    } else {
      out <- c(out, skip("The conflict diagnostics"))
    }
  }

  if (!is.null(sensitivity)) {
    ds <- attr(sensitivity, "bp_args")$data_summary
    v <- var_of(sensitivity$prior)
    if (!is.null(v) && !is.null(ds) && !is.null(sensitivity$param_grid)) {
      g <- sensitivity$param_grid
      # A data-frame grid (hyperparameters moving together) must be exported as
      # one, or the script would cross the columns into a different grid.
      is_df <- is.data.frame(g)
      g <- as.list(g)
      grid <- c(if (is_df) "param_grid <- data.frame(" else "param_grid <- list(",
                paste0("  ", names(g), " = ",
                       vapply(g, if (is_df) .bp_c else .bp_grid_code, character(1)),
                       c(rep(",", length(g) - 1L), "")),
                ")")
      cri <- !is.null(sensitivity$cri_level)
      extra <- if (cri) {
        paste0("cri_level = ", .bp_num(sensitivity$cri_level))
      } else {
        paste0("target = ", paste0("c(", paste(.bp_chr(sensitivity$target), collapse = ", "), ")"),
               ", threshold = ", .bp_num(sensitivity$threshold))
      }
      out <- c(out, "",
               paste0("sens_data <- ", .bp_list(ds)),
               grid,
               paste0("sensitivity <- ", if (cri) "sensitivity_cri" else "sensitivity_grid",
                      "(", v, ", sens_data, param_grid, ", extra, ")"),
               "plot_tornado(sensitivity)")
    } else {
      out <- c(out, skip("The sensitivity analysis"))
    }
  }

  list(ok = TRUE, reason = NULL, text = paste0(paste(out, collapse = "\n"), "\n"))
}