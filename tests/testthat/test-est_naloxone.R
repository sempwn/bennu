
test_that("Accepts input with missing distribution frequency", {
  # generate distribution data with frequency of every three months
  d <- generate_model_data(reporting_freq = 3)
  # note iter should be at least 2000 to generate a reasonable posterior sample
  fit <- est_naloxone(d, iter = 1, chains = 1)
  expect_s4_class(fit, "stanfit")
})

test_that("Error if Reported_Distributed and Reported_Used number of missing
          don't match", {
  d <- generate_model_data(reporting_freq = 3)
  d["Reported_Distributed"] <- NA
  testthat::expect_error(
    est_naloxone(d, iter = 200, chains = 2)
  )
})

test_that("Accepts non-default priors", {

  priors <- list(
    c = list(mu = 0, sigma = 1),
    ct0 = list(mu = 0, sigma = 1),
    zeta = list(mu = 0, sigma = 1),
    mu0 = list(mu = 0, sigma = 1),
    sigma = list(mu = 0, sigma = 1)
  )

  d <- model_random_walk_data()

  fit <- est_naloxone(d, priors = priors,
                      iter = 1, chains = 1)
  expect_s4_class(fit, "stanfit")
})

test_that("Errors on missing priors", {

  priors <- list(
    c = list(mu = 0, sigma = 1)
  )

  d <- model_random_walk_data()
  testthat::expect_error(
    est_naloxone(d, priors = priors),
    "Not all prior values defined. Missing priors are: ct0, zeta, mu0, sigma"
  )
})
