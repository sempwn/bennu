library(dplyr)



test_that("plot four layers if included data", {
  plt <- plot_kit_use(rw_1 = fit, rw_2 = fit, data = d)
  nlayers <- length(plt$layers)
  testthat::expect_equal(nlayers, 4)
})

test_that("plot three layers if not included data", {
  plt <- plot_kit_use(rw_1 = fit, rw_2 = fit, data = select(d, -p_use))
  nlayers <- length(plt$layers)
  testthat::expect_equal(nlayers, 3)
})

test_that("Creates ggplot object if using reported argument", {
  plt <- plot_kit_use(rw_1 = fit, rw_2 = fit, reported = TRUE, data = d)
  testthat::expect_true(ggplot2::is.ggplot(plt))
})

test_that("Only plots reported data if have missing data", {
  plt <- plot_kit_use(
    rw_1 = fit_missing, reported = TRUE,
    data = d_missing
  )
  testthat::expect_equal(
    nrow(plt$data),
    nrow(tidyr::drop_na(d_missing))
  )
})

test_that("Creates ggplot object", {
  plt <- plot_kit_use(rw_1 = fit, rw_2 = fit, data = d)
  testthat::expect_true(ggplot2::is.ggplot(plt))
})
