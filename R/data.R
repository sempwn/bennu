#' Experimental validation results
#'
#' Generated data from validation experiments of simulated data
#'
#' @format ## `experimental_validation_data`
#' A data frame with 200 rows and 8 columns:
#' \describe{
#'   \item{.variable}{Model variable}
#'   \item{p50}{Median of the posterior}
#'   \item{p25, p75}{2nd and 3rd quartiles of the posterior}
#'   \item{p05, p95}{1st and 19th ventiles of the posterior}
#'   \item{true_value}{The value used to generate the simulation}
#'   \item{experiment}{Experiment number index}
#' }
#' @backref data-raw/create_validation_data.R
"experimental_validation_data"

#' Missing data experimental validation results
#'
#' Generated data from validation experiments of simulated data
#'
#' @format ## `missing_data_validation`
#' A data frame with 10 rows and 6 columns:
#' \describe{
#'   \item{p50}{Median of the posterior}
#'   \item{p25, p75}{2nd and 3rd quartiles of the posterior}
#'   \item{p05, p95}{1st and 19th ventiles of the posterior}
#'   \item{reporting_freq}{The reporting frequency in months}
#' }
#' @backref data-raw/create_missing_data_validation.R
"missing_data_validation"
