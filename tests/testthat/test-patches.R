test_that(".target_label maps known targets", {
  expect_equal(.target_label("posterior_mean"), "Posterior mean")
  expect_equal(.target_label("posterior_sd"),   "Posterior SD")
  expect_equal(.target_label("cri_lower"),      "95% CrI lower bound")
  expect_equal(.target_label("cri_upper"),      "95% CrI upper bound")
  expect_equal(.target_label("cri_width"),      "95% CrI width")
})

test_that(".target_label returns input unchanged for unknown targets", {
  expect_equal(.target_label("unknown_target"), "unknown_target")
  expect_equal(.target_label("custom_metric"),  "custom_metric")
})

test_that(".relabel_sensitivity handles NULL input", {
  expect_null(.relabel_sensitivity(NULL))
})