
#' Run Bayesian estimation of naloxone number under-reporting
#'
#' @description
#' Samples from Bayesian model
#' @param N_region Number of regions
#' @param N_t number of time steps
#' @param regions vector (time, region) of regions (coded 1 to N_region)
#' @param times vector (time, region) of regions (coded 1 to N_t)
#' @param Orders2D vector (time, region) of orders
#' @param Reported_Distributed vector (time, region) reported as distributed
#' @param Reported_Used vector (time, region) reported as used
#' @param region_name bring in region names
#' @param psi_vec reporting delay distribution
#' @param run_estimation if `TRUE` will sample from posterior otherwise will
#' sample from prior only
#' @export
est_naloxone <- function(N_region, N_t, regions,
                         times, Orders2D, Reported_Distributed,
                         Reported_Used,
                         region_name,
                         psi_vec = c(0.7, 0.2, 0.1),
                         run_estimation = TRUE) {
  Orders <- as.vector(t(Orders2D))

  stan_data <-
    list(
      # // a switch to evaluate the likelihood
      run_estimation = as.numeric(run_estimation),
      #
      # // number of regions
      N_region = N_region,
      # // number of time steps
      N_t = N_t,
      # // total number of rows in data
      N = N_region * N_t,
      #
      # //parameters for delay distribution
      alpha = 2,
      beta = 1,
      # //max number for delay distribution
      max_delays = 3,
      #
      # // vector (time, region) of regions (coded 1 to N_region)
      regions = regions,
      #
      # // vector (time, region) of regions (coded 1 to N_t)
      times = times,
      #
      # // vector (time, region) of orders
      Orders = Orders,
      #
      # // create 2D version of Orders data
      Orders2D = Orders2D,
      #
      # // vector (time, region) reported as distributed
      Reported_Distributed = Reported_Distributed,
      #
      # // vector (time, region) reported as used
      Reported_Used = Reported_Used,

      # // reporting delay distribution
      N_psi = 3,
      psi = psi_vec,

      # // bring in region / site type names
      Region_name = region_name,

      # // hyperpiors
      mu0_mu = 0,
      mu0_sigma = 1,
      sigma_mu = 0,
      sigma_sigma = 1
    )

  fit <- rstan::sampling(
    stanmodels$distribution_covariate_model,
    data = stan_data,
    iter = 1000,
    seed = 42, # fix seed to recreate results
    control = list(adapt_delta = 0.85),
    chains = 4
  )
  return(out)
}
