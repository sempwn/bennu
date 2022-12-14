
#' Run estimation
#' @export
est_naloxone <- function() {
  stan_data <-
    list(
      # // a switch to evaluate the likelihood
      run_estimation = 1,
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
      # // vector (time, HSDA) of regions (coded 1 to N_region)
      regions = regions,
      #
      # // vector (time, HSDA) of regions (coded 1 to N_t)
      times = times,
      #
      # // vector (time, HSDA) of orders
      Orders = Orders,
      #
      # // create 2D version of Orders data
      Orders2D = Orders2D,
      #
      # // vector (time, HSDA) reported as distributed
      Reported_Distributed = Reported_Distributed,
      #
      # // vector (time, HSDA) reported as used
      Reported_Used = Reported_Used,

      # // reporting delay distribution
      N_psi = 3,
      psi = psi_vec,

      # // bring in HSDA / site type names
      Region_name = Region_name,

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
