

#' Summarize model fit
#'
#' @description
#' Provides a summary of:
#'  - Estimated kits distributed
#'  - Percentage of kits distributed that are reported
#'  - Estimated kits used
#'  - percentage of kits used that are reported
#'  - percentage of kits orders that are used
#'  - probability kit used if distributed
#'
#'
#' @param fit [stanfit] object
#' @param ... variables to group by in estimate
#' @param data data used for model fitting. Can also include `p_use` column
#' which can be used to plot true values if derived from simulated data.
#' @param accuracy A number to round to. Use (e.g.) 0.01 to show 2 decimal
#' places of precision. If NULL, the default, uses a heuristic that should
#' ensure breaks have the minimum number of digits needed to show the
#' difference between adjacent values.
#' @param cri_range The range of the credible interval e.g. 0.95
#' @return A [tibble::tibble]
#' \itemize{
#'   \item{Probability of kit use if distributed}
#'   \item{Estimated as distributed}
#'   \item{Proportion kits distributed that are reported}
#'   \item{Estimated kits used}
#'   \item{Proportion kits used that are reported}
#'   \item{Proportion kits ordered that are used}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#'   fit <- est_naloxone(d)
#'   kit_summary_table(fit,regions,data = d)
#' }
#' @family plots
kit_summary_table <- function(fit, ..., data = NULL,
                              accuracy = 0.01, cri_range = 0.95) {



  i <- sim_p <- times <- estimate <- Distributed <- Reported_Distributed <- NULL
  sim_used <- Reported_Used <- Orders <- NULL

  percent_func <- scales::percent_format(accuracy = accuracy)
  comma_func <- scales::comma_format(accuracy = accuracy)

  # probability of kit used if distributed
  out <- fit %>%
    tidybayes::spread_draws(sim_p[i]) %>%
    dplyr::rename("value" = "sim_p") %>%
    summarise_spread_draws(
      ..., data = data, cri_range = cri_range,
      name_label = "Probability of kit use if distributed",
      sum_func = percent_func
    )

  # total distributed
  data_var_summary <- fit %>%
    tidybayes::spread_draws(Distributed[i]) %>%
    dplyr::rename("value" = "Distributed") %>%
    dplyr::group_by(..., .chain, .iteration, .draw) %>%
    dplyr::summarise(value = sum(value)) %>%
    summarise_spread_draws(
      ..., data = data, cri_range = cri_range,
      name_label = "Estimated as distributed",
      sum_func = comma_func
    )

  out <- out %>%
    dplyr::inner_join(data_var_summary)

  # percentage of kits distributed that are reported
  data_var_summary <- fit %>%
    tidybayes::spread_draws(Distributed[i]) %>%
    dplyr::left_join(
      dplyr::mutate(data, i = dplyr::row_number()),
      by = "i"
    ) %>%
    dplyr::group_by(..., .chain, .iteration, .draw) %>%
    dplyr::summarise(Reported_Distributed = sum(Reported_Distributed),
                     Distributed = sum(Distributed)) %>%
    dplyr::mutate(value = Reported_Distributed / Distributed) %>%
    summarise_spread_draws(
      ..., data = NULL, cri_range = cri_range,
      name_label = "Proportion kits distributed that are reported",
      sum_func = percent_func
    )

  out <- out %>%
    dplyr::inner_join(data_var_summary)

  # estimated kits used
  data_var_summary <- fit %>%
    tidybayes::spread_draws(sim_used[i]) %>%
    dplyr::rename("value" = "sim_used") %>%
    dplyr::group_by(..., .chain, .iteration, .draw) %>%
    dplyr::summarise(value = sum(value)) %>%
    summarise_spread_draws(
      ..., data = data, cri_range = cri_range,
      name_label = "Estimated kits used",
      sum_func = comma_func
    )

  out <- out %>%
    dplyr::inner_join(data_var_summary)

  # percentage of kits used that are reported
  data_var_summary <- fit %>%
    tidybayes::spread_draws(sim_used[i]) %>%
    dplyr::left_join(
      dplyr::mutate(data, i = dplyr::row_number()),
      by = "i"
    ) %>%
    dplyr::group_by(..., .chain, .iteration, .draw) %>%
    dplyr::summarise(Reported_Used = sum(Reported_Used),
                     sim_used = sum(sim_used)) %>%
    dplyr::mutate(value = Reported_Used / sim_used) %>%
    summarise_spread_draws(
      ..., data = NULL, cri_range = cri_range,
      name_label = "Proportion kits used that are reported",
      sum_func = percent_func
    )

  out <- out %>%
    dplyr::inner_join(data_var_summary)

  # percentage of kits ordered that are used
  data_var_summary <- fit %>%
    tidybayes::spread_draws(sim_used[i]) %>%
    dplyr::left_join(
      dplyr::mutate(data, i = dplyr::row_number()),
      by = "i"
    ) %>%
    dplyr::group_by(..., .chain, .iteration, .draw) %>%
    dplyr::summarise(sim_used = sum(sim_used),
                     Orders = sum(Orders)) %>%
    dplyr::mutate(value = sim_used / Orders ) %>%
    summarise_spread_draws(
      ..., data = NULL, cri_range = cri_range,
      name_label = "Proportion kits ordered that are used",
      sum_func = percent_func
    )

  out <- out %>%
    dplyr::inner_join(data_var_summary)

  return(out)
}


#' Summarise the output of [tidybayes::spread_draws]
#'
#' @param out data frame output of [tidybayes::spread_draws]
#' @param ... grouping variables
#' @param data data frame of orders and distribution
#' @param cri_range numeric range for credible interval
#' @param name_label label name for value
#' @param sum_func summary function to convert numeric to string
#'
#' @importFrom rlang :=
#'
#' @return [dplyr::tibble]
#' @noRd
summarise_spread_draws <- function(out, ..., data = NULL, cri_range,
                                   name_label, sum_func) {

  estimate <- .chain <- .iteration <- .draw <- value <- NULL

  # calculate lower and upper bounds of range
  lb <- 0.5 * (1 - cri_range)
  ub <- 1 - lb
  cri_label <- scales::percent(cri_range)

  # add data if included
  if (!is.null(data)) {
    out <- out %>%
      dplyr::left_join(
        dplyr::mutate(data, i = dplyr::row_number()),
        by = "i"
      )
  }

  # summarize for var_name variable
  out %>%
    dplyr::group_by(...) %>%
    summarise_quantiles("value", lb = lb, ub = ub) %>%
    print_as_cri(sum_func = sum_func, cri_label = cri_label) %>%
    dplyr::rename("{name_label}" := estimate)
}

#' Summarize using quantiles for a variable
#'
#' @param d tibble data frame
#' @param var_name string name of variable to summarize
#' @param lb quantile of lower bound
#' @param ub quantile of upper bound
#' @noRd
summarise_quantiles <- function(d, var_name, lb = 0.05, ub = 0.95) {

  .data <- NULL

  d %>%
    dplyr::summarise(
      e = stats::quantile(.data[[var_name]], 0.5, na.rm = TRUE),
      lb = stats::quantile(.data[[var_name]], lb, na.rm = TRUE),
      ub = stats::quantile(.data[[var_name]], ub, na.rm = TRUE)
    )

}

#' create a column named estimate from columns `e`,`lb`, `ub`
#' @param d tibble data frame
#' @param cri_label string label for range of credible interval
#' @param sum_func function to summarize value as string
#' @inheritParams kit_summary_table
#' @noRd
print_as_cri <- function(d, accuracy = 0.01,
                         cri_label = "95%",
                         sum_func = scales::percent){
  e <- lb <- ub <- NULL



  d %>%
    dplyr::mutate(estimate = glue::glue("{sum_func(e)} ",
                                 "({cri_label} CrI: {sum_func(lb)} ",
                                 "- ",
                                 "{sum_func(ub)})")) %>%
    dplyr::select(-e,-lb,-ub)
}
