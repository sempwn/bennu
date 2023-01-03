
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
#' @param rw_type `1` - random walk of order one. `2` - random walk of order 2.
#' @param chains A positive integer specifying the number of Markov chains.
#' The default is 4.
#' @param iter A positive integer specifying the number of iterations
#' for each chain (including warmup). The default is 2000.
#' @param seed Seed for random number generation
#' @param adapt_delta (double, between 0 and 1, defaults to 0.8)
#' @param ... other parameters to pass to [rstan::sampling]
#' @family inference
#' @export
est_naloxone_vec <- function(N_region, N_t, regions,
                             times, Orders2D, Reported_Distributed,
                             Reported_Used,
                             region_name,
                             psi_vec = c(0.7, 0.2, 0.1),
                             run_estimation = TRUE,
                             rw_type = 1,
                             chains = 4,
                             iter = 2000,
                             seed = 42,
                             adapt_delta = 0.85,
                             ...) {
  Orders <- as.vector(t(Orders2D))

  stan_data <-
    list(
      # // a switch to evaluate the likelihood
      run_estimation = as.numeric(run_estimation),
      # // choose which form of a random walk to use (order 1 or order 2)
      rw_type = rw_type,
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
    iter = iter,
    seed = seed, # fix seed to recreate results
    control = list(adapt_delta = adapt_delta),
    chains = chains,
    ...
  )
  return(fit)
}


#' Run Bayesian estimation of naloxone number under-reporting
#'
#' @description
#' Samples from Bayesian model using input from data frame
#' @param d data frame with format
#' \describe{
#'   \item{regions}{unique id for region}
#'   \item{times}{time in months}
#'   \item{Orders}{Kits ordered}
#'   \item{Reported_Used}{Kits reported as used}
#'   \item{Reported_Distributed}{Kits reported as distributed}
#'   \item{region_name}{Optional label for region}
#' }
#' @examples
#' library(rstan)
#' library(bayesplot)
#'
#' rstan_options(auto_write = TRUE)
#' options(mc.cores = parallel::detectCores(logical = FALSE))
#'
#' d <- generate_model_data()
#' fit <- est_naloxone(d, iter = 100, chains = 1)
#' mcmc_pairs(fit,
#'   pars = c("sigma", "mu0"),
#'   off_diag_args = list(size = 1, alpha = 0.5)
#' )
#' @inheritParams est_naloxone_vec
#' @family inference
#' @export
est_naloxone <- function(d,
                         psi_vec = c(0.7, 0.2, 0.1),
                         run_estimation = TRUE,
                         rw_type = 1,
                         chains = 4,
                         iter = 2000,
                         seed = 42,
                         adapt_delta = 0.85,
                         ...) {
  Orders <- NULL

  # checks for data
  d <- d %>%
    dplyr::arrange(regions, times)

  N_region <- length(unique(d[["regions"]]))
  N_t <- length(unique(d[["times"]]))
  regions <- d[["regions"]]
  times <- d[["times"]]

  Reported_Distributed <- d[["Reported_Distributed"]]
  Reported_Used <- d[["Reported_Used"]]

  region_name_label <- intersect(c("region_name", "regions"), names(d))[1]
  region_name <- d[[region_name_label]]

  Orders2D <- d %>%
    tidyr::pivot_wider(regions, names_from = times, values_from = Orders) %>%
    dplyr::select(-regions) %>%
    as.matrix()

  obj <- est_naloxone_vec(N_region, N_t, regions,
    times, Orders2D, Reported_Distributed,
    Reported_Used,
    region_name,
    psi_vec = psi_vec,
    run_estimation = run_estimation,
    rw_type = rw_type,
    chains = chains,
    iter = iter,
    seed = seed,
    adapt_delta = adapt_delta,
    ...
  )

  return(obj)
}
