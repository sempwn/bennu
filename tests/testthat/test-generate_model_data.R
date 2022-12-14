test_that("generate_model_data produces tibble", {
  d <- generate_model_data()
  testthat::expect_type(d,"list")
})


test_that("check input errors with different lengths for regions", {
  testthat::expect_error(
      generate_model_data(region_coeffs = c(1,2,3),c_region = c(1))
  )
})
