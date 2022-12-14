
#' generate model data for testing purposes
#' @param N_t number of time-points
#' @param region_coeffs vector of coefficients for regions determining kit orders
#' @param c_region logit probability of kit use per region
#' @export
generate_model_data <- function(N_t = 24,
                                region_coeffs = c(5, 0.5),
                                c_region = c(-1, 2)) {

  if(length(region_coeffs) != length(c_region)){
    stop("Length of region_coeffs should match length of c_region.")
  }

  N_region <- length(region_coeffs)
  Orders <- rep(0, N_region * N_t)
  Orders <- matrix(Orders, nrow = N_region, ncol = N_t)
  # create empty arrays for regions and times
  regions <- Orders
  times <- Orders
  HSDA_name <- Orders


  for (i in 1:N_region) {
    Orders[i, ] <- floor(region_coeffs[i] * (1:N_t)^2)
    regions[i, ] <- i
    times[i, ] <- 1:N_t
    HSDA_name[i, ] <- i
  }

  Orders2D <- Orders

  # flatten 2D arrays
  Orders <- as.vector(t(Orders))
  regions <- as.vector(t(regions))
  times <- as.vector(t(times))
  HSDA_name <- as.vector(t(HSDA_name)) # Add in so code works for real data with names

  # probability of use
  c_time <- 0.1 * (1:N_t)
  logp <- -1 + c_time[times] + c_region[regions]
  p_use <- 1 / (1 + exp(-logp))

  # probability reported
  logp_reported <- rnorm(N_region * N_t, 2, 5)
  p_reported <- 1 / (1 + exp(-logp_reported))

  # vector (time, HSDA) reported as distributed
  Reported_Distributed <- rbinom(N_region * N_t, Orders, p_reported)

  # vector (time, HSDA) reported as used
  Reported_Used <- rbinom(N_region * N_t, Reported_Distributed, p_use)

  example_data <- tidyr::tibble(
    Orders = Orders, regions = regions,
    Reported_Used = Reported_Used,
    Reported_Distributed = Reported_Distributed,
    p_use = p_use, p_reported = p_reported,
    times = times, HSDA_name = HSDA_name,
  )

  return(example_data)
}
