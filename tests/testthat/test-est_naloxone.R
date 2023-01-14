
test_that("Accepts input with missing distribution frequency", {
  # generate distribution data with frequency of every three months
  d <- generate_model_data(reporting_freq = 3)
  # note iter should be at least 2000 to generate a reasonable posterior sample
  fit <- est_naloxone(d, iter = 200, chains = 2)
  expect_s3_class(fit, "stanfit")
})

test_that("Error if Reported_Distributed and Reported_Used number of missing
          don't match", {
  d <- generate_model_data(reporting_freq = 3)
  d["Reported_Distributed"] <- NA
  testthat::expect_error(
    est_naloxone(d, iter = 200, chains = 2),
    "Number of missing `Reported_Distributed` should match number
   of missing `Reported_Used", ignore.case = TRUE
  )
})
