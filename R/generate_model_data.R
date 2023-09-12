
#' generate model data for testing purposes
#' @description
#' `r lifecycle::badge('deprecated')`
#'
#' Simulate kits ordered and kits distributed for a set number of regions and
#' time-points.
#'
#' The kits ordered simulation is a simple square-term multiplied by `region_coeffs`.
#' For example if `region_coeffs = c(1,2)` then the number of kits ordered at
#' month 12 are `c(1,2) * 12^2 = c(144,288)`.
#'
#' The probability of kit use in time is assumed to increase linearly in inverse
#' logit space at a constant rate `0.1`.
#' The probability of reporting for each month and region is iid distributed
#' \eqn{\text{logit}^{-1}(p) \sim N(2,5)} which produces a mean reporting rate
#' of approximately 88%
#' @param N_t number of time-points
#' @param region_coeffs vector of coefficients for regions determining kit orders
#' @param c_region logit probability of kit use per region
#' @param reporting_freq The frequency that distribution data is provided.
#'  If `NULL` distribution frequency matches orders frequency
#' @return A [tibble]
#' \describe{
#'   \item{Orders}{Kit orders per time and region}
#'   \item{regions}{Numeric index indicating region of orders and distributions}
#'   \item{Reported_Used}{Number of kits reported as used}
#'   \item{Reported_Distributed}{Number of kits reported as distributed}
#'   \item{p_use}{Probability that a kit was used}
#'   \item{p_reported}{Probability that a distributed kit was reported}
#'   \item{times}{Index for time}
#'   \item{region_name}{String index for the region}
#' }
#' @export
#' @importFrom stats rbinom rnorm
#' @family data generation
generate_model_data <- function(N_t = 24,
                                region_coeffs = c(5, 0.5),
                                c_region = c(-1, 2),
                                reporting_freq = NULL) {
  lifecycle::deprecate_warn(
    "0.2.1", "generate_model_data()",
    "model_random_walk_data()"
  )

  if (length(region_coeffs) != length(c_region)) {
    stop("Length of region_coeffs should match length of c_region.")
  }

  N_region <- length(region_coeffs)
  Orders <- rep(0, N_region * N_t)
  Orders <- matrix(Orders, nrow = N_region, ncol = N_t)
  # create empty arrays for regions and times
  regions <- Orders
  times <- Orders
  region_name <- Orders


  for (i in 1:N_region) {
    Orders[i, ] <- floor(region_coeffs[i] * (1:N_t)^2)
    regions[i, ] <- i
    times[i, ] <- 1:N_t
    region_name[i, ] <- i
  }


  # flatten 2D arrays
  Orders <- as.vector(t(Orders))
  regions <- as.vector(t(regions))
  times <- as.vector(t(times))
  region_name <- as.vector(t(region_name)) # Add in so code works for real data with names

  # probability of use
  c_time <- 0.1 * (1:N_t)
  logp <- -1 + c_time[times] + c_region[regions]
  p_use <- 1 / (1 + exp(-logp))

  # probability reported
  logp_reported <- rnorm(N_region * N_t, 2, 5)
  p_reported <- inv_logit(-logp_reported)

  # vector (time, region) reported as distributed
  Reported_Distributed <- rbinom(N_region * N_t, Orders, p_reported)

  # vector (time, region) reported as used
  Reported_Used <- rbinom(N_region * N_t, Reported_Distributed, p_use)

  # Remove values from reported data according to reporting frequency
  if (!is.null(reporting_freq)) {
    nonreporting_times <- times %% reporting_freq != 1
    Reported_Distributed[nonreporting_times] <- NA
    Reported_Used[nonreporting_times] <- NA
  }

  example_data <- tidyr::tibble(
    Orders = Orders, regions = regions,
    Reported_Used = Reported_Used,
    Reported_Distributed = Reported_Distributed,
    p_use = p_use, p_reported = p_reported,
    times = times, region_name = region_name,
  )

  return(example_data)
}


#' @description
#' Model generating process using random walk to match data generating model
#' in Bayesian framework
#' @inherit generate_model_data
#' @param sigma standard deviation of error in logit probability of kit use
#' @param zeta standard deviation of random walk in logit space
#' @param mu0 initial condition of random walk in logit space
#' @param Orders A 2D matrix of shape `length(region_coeffs)` by `N_t`
#' @return A tibble
#' \describe{
#'   \item{Orders}{Kit orders per time and region}
#'   \item{regions}{Numeric index indicating region of orders and distributions}
#'   \item{Reported_Used}{Number of kits reported as used}
#'   \item{Reported_Distributed}{Number of kits reported as distributed}
#'   \item{p_use}{Probability that a kit was used}
#'   \item{p_reported}{Probability that a distributed kit was reported}
#'   \item{times}{Index for time}
#'   \item{region_name}{String index for the region}
#' }
#' @export
#' @importFrom stats rbinom rnorm
#' @family data generation
model_random_walk_data <- function(N_t = 24,
                                   region_coeffs = c(5, 0.5),
                                   c_region = c(-1, 2),
                                   sigma = 2,
                                   zeta = 0.5,
                                   mu0 = -1,
                                   Orders = NULL,
                                   reporting_freq = NULL) {

  # set up data
  N_region <- length(region_coeffs)

  # generate orders data
  if (is.null(Orders)) {

    Orders <- rep(0, N_region * N_t)
    Orders <- matrix(Orders, nrow = N_region, ncol = N_t)
    # create orders
    for (i in 1:N_region) {
      Orders[i, ] <- floor(region_coeffs[i] * (1:N_t)^2)
    }

  } else {
    if (!is.matrix(Orders) | dim(Orders) != c(N_region, N_t)) {
      stop(glue::glue("`Orders` should be a {N_region} by {N_t} matrix"))
    }
  }

  # create empty arrays for regions and times
  regions <- Orders
  times <- Orders
  region_name <- Orders


  for (i in 1:N_region) {
    regions[i, ] <- i
    times[i, ] <- 1:N_t
    region_name[i, ] <- i
  }


  # flatten 2D arrays
  Orders <- as.vector(t(Orders))
  regions <- as.vector(t(regions))
  times <- as.vector(t(times))
  region_name <- as.vector(t(region_name))

  # create probability of use and reported

  # probability of use
  c_time <- random_walk_generator(0, zeta, N_t)
  errs <- stats::rnorm(N_region * N_t, sd = sigma)
  logp <- mu0 + c_time[times] + c_region[regions] + errs
  p_use <- inv_logit(logp)

  # probability reported
  logp_reported <- rnorm(N_region * N_t, 2, 5)
  p_reported <- inv_logit(logp_reported)




  # vector (time, region) reported as distributed
  Reported_Distributed <- rbinom(N_region * N_t, Orders, p_reported)

  # vector (time, region) reported as used
  Reported_Used <- rbinom(N_region * N_t, Reported_Distributed, p_use)

  # Remove values from reported data according to reporting frequency
  if (!is.null(reporting_freq)) {
    nonreporting_times <- times %% reporting_freq != 1
    Reported_Distributed[nonreporting_times] <- NA
    Reported_Used[nonreporting_times] <- NA
  }

  example_data <- tidyr::tibble(
    Orders = Orders, regions = regions,
    Reported_Used = Reported_Used,
    Reported_Distributed = Reported_Distributed,
    p_use = p_use, p_reported = p_reported,
    times = times, region_name = region_name,
  )

  return(example_data)
}

#' generate a random walk of length n
#' @param s0 initial point
#' @param zeta standard deviation of random walk increments
#' @param n length of random walk
#' @return vector of length `n`
#' @noRd
random_walk_generator <- function(s0, zeta, n) {
  s0 + cumsum(stats::rnorm(n, mean = 0, sd = zeta))
}

#' inverse logit
#' @param x numeric
#' @return numeric
#' @noRd
inv_logit <- function(x) {
  1 / (1 + exp(-x))
}
