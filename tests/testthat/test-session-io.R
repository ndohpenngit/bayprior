mk_state <- function() {
  e1 <- elicit_beta(mean = 0.30, sd = 0.08, method = "moments", expert_id = "E1")
  e2 <- elicit_beta(mean = 0.42, sd = 0.10, method = "moments", expert_id = "E2")
  pool <- suppressMessages(aggregate_experts(list(E1 = e1, E2 = e2),
                                             weights = c(0.5, 0.5)))
  ds <- list(type = "binary", x = 18, n = 40)
  list(
    current_prior = pool, base_prior = pool,
    expert_pool = list(E1 = e1, E2 = e2), consensus = pool,
    conflict = suppressWarnings(prior_conflict(pool, ds)),
    sensitivity = NULL, robust_prior = suppressWarnings(robust_prior(pool)),
    sceptical_prior = sceptical_prior(), power_prior = NULL,
    map_prior = map_prior(y = c(-0.85, -0.62, -1.10), se = c(0.25, 0.30, 0.28),
                          outcome_type = "single_arm_log_odds")
  )
}

save_obj <- function(x) {
  f <- tempfile(fileext = ".rds")
  saveRDS(x, f)
  f
}

test_that("a saved session reads back with its priors and pool", {
  st <- mk_state()
  meta <- list(trial_name = "TRIAL-001", report_date = "2026-10-23")
  r <- .bp_session_read(save_obj(.bp_session_pack(st, meta)))
  expect_true(r$ok)
  expect_equal(r$state$consensus, st$consensus)
  expect_equal(r$state$base_prior, st$base_prior)
  expect_equal(names(r$state$expert_pool), c("E1", "E2"))
  expect_equal(r$state$map_prior, st$map_prior)
  expect_equal(r$meta, meta)
  expect_null(r$note)
})

test_that("a session with only a standalone active prior loads", {
  st <- list(current_prior = sceptical_prior(), expert_pool = list())
  r <- .bp_session_read(save_obj(.bp_session_pack(st)))
  expect_true(r$ok)
  expect_null(r$state$base_prior)
  expect_length(r$state$expert_pool, 0L)
})

test_that("invalid files are rejected with a message", {
  st <- mk_state()
  bad <- function(x) {
    r <- .bp_session_read(save_obj(x))
    expect_false(r$ok)
    expect_true(nzchar(r$msg))
    r
  }
  bad("not a session")
  bad(list(a = 1))
  p <- .bp_session_pack(st); p$schema <- 99L
  expect_match(bad(p)$msg, "newer version")
  p <- .bp_session_pack(st); p$state <- "x"
  bad(p)
  p <- .bp_session_pack(list(expert_pool = list()))
  expect_match(bad(p)$msg, "no prior")

  p <- .bp_session_pack(st); p$state$consensus$params <- function() system("echo")
  expect_match(bad(p)$msg, "unexpected objects")
  p <- .bp_session_pack(st); p$state$base_prior$extra <- new.env()
  expect_match(bad(p)$msg, "unexpected objects")
  p <- .bp_session_pack(st); p$state$current_prior$extra <- quote(system("x"))
  expect_match(bad(p)$msg, "unexpected objects")

  p <- .bp_session_pack(st); p$state$base_prior <- list(dist = "beta")
  expect_match(bad(p)$msg, "base_prior")
  p <- .bp_session_pack(st); names(p$state$expert_pool) <- c("A", "A")
  expect_match(bad(p)$msg, "expert pool")
  p <- .bp_session_pack(st); p$state$expert_pool <- list(1, 2)
  expect_match(bad(p)$msg, "expert pool")

  f <- tempfile(); writeLines("garbage", f)
  expect_false(.bp_session_read(f)$ok)
  expect_false(.bp_session_read(tempfile())$ok)
  expect_false(.bp_session_read(save_obj(.bp_session_pack(st)), max_bytes = 10)$ok)
})

test_that("code in fields that are not restored does not block loading", {
  st <- mk_state(); st$conflict$fn <- function() 1
  r <- .bp_session_read(save_obj(.bp_session_pack(st)))
  expect_true(r$ok)
  expect_false("conflict" %in% names(r$state))
})

test_that("metadata is filtered and a version difference is noted", {
  st <- mk_state()
  meta <- list(trial_name = "ok", sponsor = 5, notes = strrep("x", 6000),
               report_date = "not-a-date", extra = "drop", indication = "NSCLC")
  r <- .bp_session_read(save_obj(.bp_session_pack(st, meta, version = "0.4.0")),
                        version = "0.5.0")
  expect_equal(r$meta, list(trial_name = "ok", indication = "NSCLC"))
  expect_match(r$note, "0.4.0")
  expect_match(r$note, "0.5.0")
})

test_that("saved real objects pass the code check", {
  st <- mk_state()
  keep <- st[c("expert_pool", "consensus", "base_prior", "current_prior", "map_prior")]
  expect_false(.bp_has_code(keep))
  expect_true(.bp_has_code(list(a = list(b = function() 1))))
  expect_true(.bp_has_code(structure(list(), f = globalenv())))
})

mk_shared_from <- function(st) shiny::reactiveValues(
  current_prior = st$current_prior, base_prior = st$base_prior,
  expert_pool = st$expert_pool, consensus = st$consensus,
  conflict = st$conflict, sensitivity = NULL, robust_prior = st$robust_prior,
  sceptical_prior = NULL, power_prior = NULL, map_prior = st$map_prior,
  report_exported = FALSE)

empty_shared <- function() shiny::reactiveValues(
  current_prior = NULL, base_prior = NULL, expert_pool = list(), consensus = NULL,
  map_prior = NULL)

test_that("the header Session menu saves and loads, including the report form", {
  skip_if_not_installed("shinyjs")
  st <- mk_state()
  form <- list(trial_name = "T-9", indication = "NSCLC", sponsor = "S",
               statistician = "N", notes = "n", report_date = "2026-10-23")

  saved <- NULL
  shiny::testServer(mod_session_server,
    args = list(shared = mk_shared_from(st), report_meta = shiny::reactive(form)), {
      expect_true(file.exists(output$save))
      saved <<- tempfile(fileext = ".rds")
      file.copy(output$save, saved)
    })

  restored <- NULL
  empty <- empty_shared()
  shiny::testServer(mod_session_server,
    args = list(shared = empty, restore_meta = function(m) restored <<- m), {
      session$setInputs(load_file = list(datapath = saved, name = "s.rds",
                                         size = file.size(saved), type = ""))
      expect_equal(empty$consensus, st$consensus)
      expect_equal(empty$base_prior, st$base_prior)
      expect_equal(empty$current_prior, st$current_prior)
      expect_equal(empty$map_prior, st$map_prior)
      expect_equal(names(empty$expert_pool), c("E1", "E2"))
      expect_equal(restored, form)

      bad <- tempfile(fileext = ".rds"); saveRDS(list(a = 1), bad)
      empty$consensus <- NULL
      restored <<- NULL
      session$setInputs(load_file = list(datapath = bad, name = "b.rds",
                                         size = file.size(bad), type = ""))
      expect_null(empty$consensus)
      expect_null(restored)
    })
})

test_that("Save is unavailable until there is a prior", {
  skip_if_not_installed("shinyjs")
  shiny::testServer(mod_session_server, args = list(shared = empty_shared()), {
    expect_match(output$save_sub, "first")
    expect_error(output$save)
  })
})

test_that("the report module returns its form and refills it", {
  skip_if_not_installed("shinyjs")
  shiny::testServer(mod_report_server,
    args = list(shared = mk_shared_from(mk_state()),
                active_prior = shiny::reactive(NULL)), {
      session$setInputs(trial_name = "A", indication = "B", sponsor = "C",
                        statistician = "D", notes = "E", report_date = as.Date("2026-10-23"))
      m <- session$returned$meta()
      expect_equal(m$trial_name, "A")
      expect_equal(m$report_date, "2026-10-23")
      expect_silent(session$returned$restore(list(trial_name = "Z", report_date = "2026-01-02")))
    })
})

test_that("the Session menu renders the expected controls", {
  html <- as.character(mod_session_ui("session"))
  expect_match(html, "bp-session-toggle", fixed = TRUE)
  expect_match(html, "data-toggle=\"dropdown\"", fixed = TRUE)
  expect_match(html, "session-save", fixed = TRUE)
  expect_match(html, "session-load_file", fixed = TRUE)
  expect_match(html, "bpSessionLoad", fixed = TRUE)
})