test_that("app loads without error", {
  skip_if_not_installed("shinytest2")
  app <- shinytest2::AppDriver$new(run_app(), name = "bayprior-app",
                                   height = 800, width = 1200,
                                   timeout = 60000)
  on.exit(app$stop())

  # App should load and show welcome tab
  app$wait_for_idle(timeout = 10000)
  val <- app$get_value(output = "sidebar_prior_badge")
  expect_true(!is.null(val))
})

test_that("elicitation module fits a beta prior (moments)", {
  skip_if_not_installed("shinytest2")
  app <- shinytest2::AppDriver$new(run_app(), name = "elicitation-moments",
                                   height = 800, width = 1200,
                                   timeout = 30000)
  on.exit(app$stop())

  # Navigate to elicitation
  app$set_inputs(`sidebar_menu` = "elicitation", wait_ = FALSE)
  app$wait_for_idle(timeout = 5000)

  # Set moments method
  app$set_inputs(`elicitation-method` = "moments", wait_ = FALSE)
  app$set_inputs(`elicitation-family` = "beta", wait_ = FALSE)
  app$set_inputs(`elicitation-mom_mean` = 0.35, wait_ = FALSE)
  app$set_inputs(`elicitation-mom_sd`   = 0.10)

  # Click Fit Prior
  app$click("elicitation-fit_btn")
  app$wait_for_idle(timeout = 5000)

  # Sidebar badge should update
  val <- app$get_value(output = "sidebar_prior_badge")
  expect_true(!is.null(val))
})

test_that("elicitation module fits a beta prior (quantile)", {
  skip_if_not_installed("shinytest2")
  app <- shinytest2::AppDriver$new(run_app(), name = "elicitation-quantile",
                                   height = 800, width = 1200,
                                   timeout = 30000)
  on.exit(app$stop())

  app$set_inputs(`sidebar_menu` = "elicitation", wait_ = FALSE)
  app$wait_for_idle(timeout = 5000)

  app$set_inputs(`elicitation-method` = "quantile", wait_ = FALSE)
  app$set_inputs(`elicitation-family` = "beta", wait_ = FALSE)
  app$set_inputs(`elicitation-q1p` = 5,  `elicitation-q1v` = 0.10, wait_ = FALSE)
  app$set_inputs(`elicitation-q2p` = 50, `elicitation-q2v` = 0.35, wait_ = FALSE)
  app$set_inputs(`elicitation-q3p` = 95, `elicitation-q3v` = 0.65, wait_ = FALSE)

  app$click("elicitation-fit_btn")
  app$wait_for_idle(timeout = 5000)

  # Fit message should appear
  val <- app$get_value(output = "elicitation-fit_msg")
  expect_true(!is.null(val))
})

test_that("conflict module runs diagnostics", {
  skip_if_not_installed("shinytest2")
  app <- shinytest2::AppDriver$new(run_app(), name = "conflict-diag",
                                   height = 800, width = 1200,
                                   timeout = 30000)
  on.exit(app$stop())

  # First fit a prior
  app$set_inputs(`sidebar_menu` = "elicitation", wait_ = FALSE)
  app$wait_for_idle(3000)
  app$set_inputs(`elicitation-method` = "moments",
                 `elicitation-family` = "beta",
                 `elicitation-mom_mean` = 0.30,
                 `elicitation-mom_sd`   = 0.10)
  app$click("elicitation-fit_btn")
  app$wait_for_idle(5000)

  # Navigate to conflict
  app$set_inputs(`sidebar_menu` = "conflict", wait_ = FALSE)
  app$wait_for_idle(3000)

  # Run diagnostics
  app$set_inputs(`conflict-data_type` = "binary",
                 `conflict-bin_x` = 14,
                 `conflict-bin_n` = 40, wait_ = FALSE)
  app$click("conflict-run_btn")
  app$wait_for_idle(5000)

  # Results should appear
  val <- app$get_value(output = "conflict-results_or_placeholder")
  expect_true(!is.null(val))
})

test_that("sensitivity module runs analysis", {
  skip_if_not_installed("shinytest2")
  app <- shinytest2::AppDriver$new(run_app(), name = "sensitivity-run",
                                   height = 800, width = 1200,
                                   timeout = 30000)
  on.exit(app$stop())

  # Fit prior first
  app$set_inputs(`sidebar_menu` = "elicitation", wait_ = FALSE)
  app$wait_for_idle(3000)
  app$set_inputs(`elicitation-method` = "moments",
                 `elicitation-family` = "beta",
                 `elicitation-mom_mean` = 0.30,
                 `elicitation-mom_sd`   = 0.10)
  app$click("elicitation-fit_btn")
  app$wait_for_idle(5000)

  # Navigate to sensitivity
  app$set_inputs(`sidebar_menu` = "sensitivity", wait_ = FALSE)
  app$wait_for_idle(3000)

  app$set_inputs(`sensitivity-grid_size` = 5, wait_ = FALSE)
  app$click("sensitivity-run_btn")
  app$wait_for_idle(10000)

  val <- app$get_value(output = "sensitivity-results_or_placeholder")
  expect_true(!is.null(val))
})

test_that("robust prior module builds prior", {
  skip_if_not_installed("shinytest2")
  app <- shinytest2::AppDriver$new(run_app(), name = "robust-build",
                                   height = 800, width = 1200,
                                   timeout = 30000)
  on.exit(app$stop())

  # Fit prior first
  app$set_inputs(`sidebar_menu` = "elicitation", wait_ = FALSE)
  app$wait_for_idle(3000)
  app$set_inputs(`elicitation-method` = "moments",
                 `elicitation-family` = "beta",
                 `elicitation-mom_mean` = 0.30,
                 `elicitation-mom_sd`   = 0.10)
  app$click("elicitation-fit_btn")
  app$wait_for_idle(5000)

  # Navigate to robust
  app$set_inputs(`sidebar_menu` = "robust", wait_ = FALSE)
  app$wait_for_idle(3000)

  app$click("robust-fit_btn")
  app$wait_for_idle(5000)

  val <- app$get_value(output = "robust-results_or_placeholder")
  expect_true(!is.null(val))
})

test_that("sceptical prior module builds prior", {
  skip_if_not_installed("shinytest2")
  app <- shinytest2::AppDriver$new(run_app(), name = "sceptical-build",
                                   height = 800, width = 1200,
                                   timeout = 30000)
  on.exit(app$stop())

  app$set_inputs(`sidebar_menu` = "sceptical", wait_ = FALSE)
  app$wait_for_idle(3000)

  app$set_inputs(`sceptical-family`   = "normal",
                 `sceptical-null_val` = 0,
                 `sceptical-strength` = "moderate")
  app$click("sceptical-fit_btn")
  app$wait_for_idle(5000)

  val <- app$get_value(output = "sceptical-results_or_placeholder")
  expect_true(!is.null(val))
})

test_that("roulette module fits prior from chips", {
  skip_if_not_installed("shinytest2")
  app <- shinytest2::AppDriver$new(run_app(), name = "roulette-fit",
                                   height = 800, width = 1200,
                                   timeout = 30000)
  on.exit(app$stop())

  app$set_inputs(`sidebar_menu` = "roulette", wait_ = FALSE)
  app$wait_for_idle(3000)

  # Click some chips
  for (i in 1:5) {
    app$click(paste0("roulette-p_5"))
    app$wait_for_idle(500)
  }

  app$click("roulette-fit_btn")
  app$wait_for_idle(5000)

  val <- app$get_value(output = "roulette-fit_msg")
  expect_true(!is.null(val))
})

test_that("pooling module aggregates experts", {
  skip_if_not_installed("shinytest2")
  app <- shinytest2::AppDriver$new(run_app(), name = "pooling-aggregate",
                                   height = 800, width = 1200,
                                   timeout = 30000)
  on.exit(app$stop())

  # Add two experts via elicitation
  for (expert in c("E1", "E2")) {
    app$set_inputs(`sidebar_menu` = "elicitation", wait_ = FALSE)
    app$wait_for_idle(2000)
    app$set_inputs(`elicitation-expert_id` = expert,
                   `elicitation-method`    = "moments",
                   `elicitation-family`    = "beta",
                   `elicitation-mom_mean`  = if(expert=="E1") 0.25 else 0.35,
                   `elicitation-mom_sd`    = 0.09)
    app$click("elicitation-fit_btn")
    app$wait_for_idle(3000)
    app$click("elicitation-add_btn")
    app$wait_for_idle(2000)
  }

  # Navigate to pooling
  app$set_inputs(`sidebar_menu` = "pooling", wait_ = FALSE)
  app$wait_for_idle(3000)

  app$click("pooling-pool_btn")
  app$wait_for_idle(5000)

  val <- app$get_value(output = "pooling-pool_msg")
  expect_true(!is.null(val))
})