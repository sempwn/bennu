# default outputs of model to save
the$default_outputs <- c(
  "sim_p", "sim_used", "sim_reported_used", "c", "ct",
  "sigma", "zeta", "mu0", "Distributed"
)

# default prior values
the$default_priors <- list(
  c = list(mu = 0, sigma = 1),
  ct0 = list(mu = 0, sigma = 1),
  zeta = list(mu = 0, sigma = 1),
  mu0 = list(mu = 0, sigma = 1),
  sigma = list(mu = 0, sigma = 1)
)



#' Run Bayesian estimation of naloxone number under-reporting
#'
#' @description
#' Samples from Bayesian model
#' @param N_region Number of regions
#' @param N_t number of time steps
#' @param N_distributed Number of samples of reporting for distribution of kits
#' @param regions vector (time, region) of regions (coded 1 to N_region)
#' @param times vector (time, region) of regions (coded 1 to N_t)
#' @param Orders2D vector (time, region) of orders
#' @param Reported_Distributed vector (time, region) reported as distributed
#' @param Reported_Used vector (time, region) reported as used
#' @param region_name bring in region names
#' @param psi_vec reporting delay distribution
#' @param max_delays maximum delay from kit ordered to kit distributed
#' @param delay_alpha shape parameter for order to distributed delay
#' distribution
#' @param delay_beta shape parameter for order to distributed delay distribution
#' @param priors list of prior values including their mean (mu) and standard
#'   deviation (sigma)
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
#' @inheritParams rstan::sampling
#' @family inference
#' @return An S4 [rstan::stanfit] class object containing the fitted model
#' @export
est_naloxone_vec <- function(N_region, N_t, N_distributed, regions,
                             times, Orders2D, Reported_Distributed,
                             Reported_Used,
                             region_name,
                             psi_vec = c(0.7, 0.2, 0.1),
                             max_delays = 3,
                             delay_alpha = 2,
                             delay_beta = 1,
                             priors = the$default_priors,
                             run_estimation = TRUE,
                             rw_type = 1,
                             chains = 4,
                             iter = 2000,
                             seed = 42,
                             adapt_delta = 0.85,
                             pars = the$default_outputs,
                             include = TRUE,
                             ...) {
  check_prior_format(priors)

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
      # total number of rows in reported distribution data
      N_distributed = N_distributed,
      # //parameters for delay distribution
      alpha = delay_alpha,
      beta = delay_beta,
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
      N_psi = length(psi_vec),
      psi = psi_vec,

      # // bring in region / site type names
      Region_name = region_name,

      # // hyperpiors
      c_mu = priors$c$mu,
      c_sigma = priors$c$sigma,
      ct0_mu = priors$ct0$mu,
      ct0_sigma = priors$ct0$sigma,
      zeta_mu = priors$zeta$mu,
      zeta_sigma = priors$zeta$sigma,
      mu0_mu = priors$mu0$mu,
      mu0_sigma = priors$mu0$sigma,
      sigma_mu = priors$sigma$mu,
      sigma_sigma = priors$sigma$sigma
    )

  fit <- rstan::sampling(
    stanmodels$distribution_covariate_model,
    data = stan_data,
    iter = iter,
    seed = seed, # fix seed to recreate results
    control = list(adapt_delta = adapt_delta),
    chains = chains,
    pars = pars,
    include = include,
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
#' \dontrun{
#' library(rstan)
#' library(bayesplot)
#'
#' rstan_options(auto_write = TRUE)
#' options(mc.cores = parallel::detectCores(logical = FALSE))
#'
#' d <- generate_model_data()
#' priors <- list(
#'   c = list(mu = 0, sigma = 1),
#'   ct0 = list(mu = 0, sigma = 1),
#'   zeta = list(mu = 0, sigma = 1),
#'   mu0 = list(mu = 0, sigma = 1),
#'   sigma = list(mu = 0, sigma = 1)
#'   )
#' fit <- est_naloxone(d, priors = priors, iter = 100, chains = 1)
#' mcmc_pairs(fit,
#'   pars = c("sigma", "mu0"),
#'   off_diag_args = list(size = 1, alpha = 0.5)
#' )
#' }
#' @inheritParams est_naloxone_vec
#' @family inference
#' @return An S4 [rstan::stanfit] class object containing the fitted model
#' @export
est_naloxone <- function(d,
                         psi_vec = c(0.7, 0.2, 0.1),
                         max_delays = 3,
                         delay_alpha = 2,
                         delay_beta = 1,
                         priors = the$default_priors,
                         run_estimation = TRUE,
                         rw_type = 1,
                         chains = 4,
                         iter = 2000,
                         seed = 42,
                         adapt_delta = 0.85,
                         pars = the$default_outputs,
                         include = TRUE,
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

  # check that if missing reporting data then number of distributed and used
  # non-missing match
  if (sum(is.na(Reported_Distributed)) != sum(is.na(Reported_Used))) {
    stop("Number of missing `Reported_Distributed` should match number
         of missing `Reported_Used`")
  }

  # remove missing values
  nonmissing_inds <- !is.na(Reported_Used)

  times <- times[nonmissing_inds]
  regions <- regions[nonmissing_inds]
  Reported_Distributed <- Reported_Distributed[nonmissing_inds]
  Reported_Used <- Reported_Used[nonmissing_inds]
  N_distributed <- length(Reported_Used)


  region_name_label <- intersect(c("region_name", "regions"), names(d))[1]
  region_name <- d[[region_name_label]]

  Orders2D <- d %>%
    tidyr::pivot_wider(regions, names_from = times, values_from = Orders) %>%
    dplyr::select(-regions) %>%
    as.matrix()

  obj <- est_naloxone_vec(
    N_region, N_t, N_distributed,
    regions, times, Orders2D,
    Reported_Distributed, Reported_Used,
    region_name,
    psi_vec = psi_vec,
    max_delays = max_delays,
    delay_alpha = delay_alpha,
    delay_beta = delay_beta,
    priors = priors,
    run_estimation = run_estimation,
    rw_type = rw_type,
    chains = chains,
    iter = iter,
    seed = seed,
    adapt_delta = adapt_delta,
    pars = pars,
    include = include,
    ...
  )

  return(obj)
}

#' check that the format of the priors matches with the default priors
#' @noRd
check_prior_format <- function(priors) {
  if (!generics::setequal(
    names(the$default_priors),
    names(priors)
  )) {
    missing_priors <- generics::setdiff(
      names(the$default_priors),
      names(priors)
    )
    stop(
      "Not all prior values defined. Missing priors are: ",
      paste(missing_priors, collapse = ", ")
    )
  }

  for (prior in names(the$default_priors)) {
    if (!all(c("mu", "sigma") %in% names(priors[[prior]]))) {
      stop(prior, " in priors should contain 'mu' and 'sigma'")
    }
    if (priors[[prior]]$sigma <= 0) {
      stop("sigma for ", prior, " should be positive")
    }
  }
}
