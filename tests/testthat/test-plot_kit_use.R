library(dplyr)



test_that("plot four layers if included data", {
  plt <- plot_kit_use(rw_1 = fit, rw_2 = fit, data=d)
  nlayers <- length(plt$layers)
  testthat::expect_equal(nlayers,4)
})

test_that("plot three layers if not included data", {
  plt <- plot_kit_use(rw_1 = fit, rw_2 = fit,data=select(d,-p_use))
  nlayers <- length(plt$layers)
  testthat::expect_equal(nlayers,3)
})

test_that("Creates ggplot object", {
  plt <- plot_kit_use(rw_1 = fit, rw_2 = fit, data=d)
  testthat::expect_true(ggplot2::is.ggplot(plt))
})
