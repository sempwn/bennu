
test_that("reporting_frequency produces correct
          number of non-empty distributed and used",{
  d <- generate_model_data(reporting_freq = 3,N_t = 90)
  non_missing_used <- sum(!is.na(d[["Reported_Used"]]))
  non_missing_distributed <- sum(!is.na(d[["Reported_Distributed"]]))

  testthat::expect_equal(non_missing_used,2*30,ignore_attr = TRUE)
  testthat::expect_equal(non_missing_distributed,2*30,ignore_attr = TRUE)

})

test_that("generate_model_data produces tibble", {
  d <- generate_model_data()
  testthat::expect_type(d,"list")
})


test_that("check input errors with different lengths for regions", {
  testthat::expect_error(
      generate_model_data(region_coeffs = c(1,2,3),c_region = c(1))
  )
})
