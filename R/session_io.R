.bp_session_schema <- 1L

.bp_session_restored <- c("expert_pool", "consensus", "base_prior", "current_prior",
                          "map_prior")

.bp_session_meta_fields <- c("trial_name", "indication", "sponsor", "statistician",
                             "notes", "report_date")

.bp_has_code <- function(x, depth = 0L) {
  if (depth > 40L) return(TRUE)
  if (is.function(x) || is.environment(x) || is.language(x) || isS4(x) ||
      typeof(x) %in% c("externalptr", "weakref", "promise", "bytecode"))
    return(TRUE)
  a <- attributes(x)
  if (length(a) && any(vapply(a, .bp_has_code, logical(1), depth = depth + 1L)))
    return(TRUE)
  is.list(x) && length(x) > 0L &&
    any(vapply(x, .bp_has_code, logical(1), depth = depth + 1L))
}

.bp_valid_prior <- function(p, depth = 0L) {
  if (depth > 3L || !inherits(p, "bayprior") || !is.list(p)) return(FALSE)
  if (!is.character(p$dist) || length(p$dist) != 1L) return(FALSE)
  if (!is.null(p$label) && !(is.character(p$label) && length(p$label) == 1L)) return(FALSE)
  if (!is.list(p$fit_summary)) return(FALSE)
  if (p$dist %in% c("mixture", "log_pool")) {
    k <- length(p$components)
    return(k >= 2L && is.numeric(p$weights) && length(p$weights) == k &&
             all(vapply(p$components, .bp_valid_prior, logical(1), depth = depth + 1L)))
  }
  is.list(p$params) && length(p$params) > 0L && all(vapply(p$params, is.numeric, logical(1)))
}

.bp_session_pack <- function(state, meta = list(), version = NULL) {
  list(
    schema = .bp_session_schema,
    format = "bayprior-session",
    bayprior_version = version %||% tryCatch(as.character(utils::packageVersion("bayprior")),
                                             error = function(e) "dev"),
    r_version = R.version.string,
    saved = format(Sys.time(), tz = "UTC", usetz = TRUE),
    state = state,
    meta = meta
  )
}

.bp_session_fail <- function(msg) list(ok = FALSE, msg = msg, state = NULL, meta = NULL, note = NULL)

.bp_session_check <- function(x, version = NULL) {
  if (!is.list(x) || !identical(x$format, "bayprior-session"))
    return(.bp_session_fail("This is not a bayprior session file."))
  if (!is.numeric(x$schema) || length(x$schema) != 1L || x$schema > .bp_session_schema)
    return(.bp_session_fail(paste0(
      "This session was saved by a newer version of bayprior",
      if (is.character(x$bayprior_version)) paste0(" (", x$bayprior_version[1L], ")"), "."
    )))
  st <- x$state
  if (!is.list(st)) return(.bp_session_fail("The session file has no saved state."))

  keep <- st[intersect(names(st), .bp_session_restored)]
  if (.bp_has_code(keep))
    return(.bp_session_fail("The session file contains unexpected objects and was not loaded."))

  single <- c("consensus", "base_prior", "current_prior", "map_prior")
  for (f in intersect(single, names(keep))) {
    if (!is.null(keep[[f]]) && !.bp_valid_prior(keep[[f]]))
      return(.bp_session_fail(paste0("The saved '", f, "' is not a valid prior.")))
  }
  pool <- keep$expert_pool
  if (!is.null(pool)) {
    nm <- names(pool)
    if (!is.list(pool) || (length(pool) > 0L &&
        (is.null(nm) || anyNA(nm) || any(!nzchar(nm)) || anyDuplicated(nm))) ||
        !all(vapply(pool, .bp_valid_prior, logical(1))))
      return(.bp_session_fail("The saved expert pool is not valid."))
  }
  if (is.null(keep$consensus) && is.null(keep$base_prior) && is.null(keep$current_prior))
    return(.bp_session_fail("The session file contains no prior."))

  meta <- list()
  m <- x$meta
  if (is.list(m)) {
    for (f in intersect(.bp_session_meta_fields, names(m))) {
      v <- m[[f]]
      if (!is.character(v) || length(v) != 1L || is.na(v) || nchar(v) > 5000L) next
      if (f == "report_date" && is.na(suppressWarnings(as.Date(v, optional = TRUE)))) next
      meta[[f]] <- v
    }
  }

  note <- NULL
  cur <- tryCatch(as.character(utils::packageVersion("bayprior")), error = function(e) NULL)
  cur <- version %||% cur
  sv <- x$bayprior_version
  if (is.character(sv) && length(sv) == 1L && !is.null(cur) && !identical(sv, cur))
    note <- paste0("Saved with bayprior ", sv, "; you are running ", cur, ".")

  state <- list(
    expert_pool   = pool %||% list(),
    consensus     = keep$consensus,
    base_prior    = keep$base_prior,
    current_prior = keep$current_prior,
    map_prior     = keep$map_prior
  )
  list(ok = TRUE, msg = NULL, state = state, meta = meta, note = note)
}

.bp_session_read <- function(path, max_bytes = 2e7, version = NULL) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path))
    return(.bp_session_fail("The file could not be read."))
  if (file.size(path) > max_bytes)
    return(.bp_session_fail("The file is too large to be a bayprior session."))
  x <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(x)) return(.bp_session_fail("The file could not be read as a bayprior session."))
  .bp_session_check(x, version)
}

.bp_session_apply <- function(res, shared, restore_meta = NULL) {
  if (!isTRUE(res$ok)) {
    shiny::showNotification(res$msg, type = "error", duration = 10)
    return(invisible(FALSE))
  }
  s <- res$state
  shared$consensus     <- s$consensus
  shared$base_prior    <- s$base_prior
  shared$current_prior <- s$current_prior
  shared$map_prior     <- s$map_prior
  shared$expert_pool   <- s$expert_pool

  if (is.function(restore_meta) && length(res$meta)) restore_meta(res$meta)

  shiny::showNotification(
    paste0("Session loaded: ", length(s$expert_pool), " expert(s) in the pool. ",
           "Conflict, sensitivity and robust / power results are not restored; rerun them.",
           if (!is.null(res$note)) paste0(" ", res$note)),
    type = "message", duration = 10)
  invisible(TRUE)
}