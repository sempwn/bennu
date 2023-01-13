
test_that("Accepts input with missing distribution frequency", {
  # generate distribution data with frequency of every three months
  d <- generate_model_data(reporting_freq = 3)
  # note iter should be at least 2000 to generate a reasonable posterior sample
  fit <- est_naloxone(d,iter=200,chains=2,algorithm="optimizing")
  expect_s3_class(fit,"stanfit")
  })
