

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
#' @param fit
#' @param data
#' @param accuracy A number to round to. Use (e.g.) 0.01 to show 2 decimal
#' places of precision. If NULL, the default, uses a heuristic that should
#' ensure breaks have the minimum number of digits needed to show the
#' difference between adjacent values.
#'
#' @return
#' @export
#'
#' @examples
kit_summary_table <- function(fit, ..., data = NULL, accuracy = 0.01) {



  i <- regions <- times <- estimate <- NULL

  out <- fit %>%
    tidybayes::spread_draws(sim_p[i])

  if (!is.null(data)) {
    out <- out %>%
      dplyr::left_join(
        dplyr::mutate(data, i = dplyr::row_number()),
        by = "i"
      )
  }

  name_label <- "Probability of kit use if distributed"

  # summarize for each variable
  out <- out %>%
    dplyr::group_by(...) %>%
    summarise_quantiles(sim_p) %>%
    print_as_cri() %>%
    dplyr::rename("{name_label}" := estimate)

  return(out)
}


#' Title
#'
#' @param d
#' @param var
#' @noRd
summarise_quantiles <- function(d, var) {
  d %>%
    dplyr::summarise(
      p50 = stats::quantile({{ var }}, 0.5),
      p25 = stats::quantile({{ var }}, 0.25),
      p75 = stats::quantile({{ var }}, 0.75),
      p05 = stats::quantile({{ var }}, 0.05),
      p95 = stats::quantile({{ var }}, 0.95)
    )

}

#' create a column named estimate from columns `p50`,`p05`, `p95`
#' @noRd
print_as_cri <- function(d, accuracy = 0.01){
  p05 <- p95 <- p75 <- p25 <- p50 <- NULL

  sum_func <- scales::percent_format(accuracy = accuracy)

  d %>%
    dplyr::mutate(estimate = glue::glue("{sum_func(p50)}",
                                 "(95% CrI: {sum_func(p05)}",
                                 " - ",
                                 "{sum_func(p95)})")) %>%
    dplyr::select(-p50,-p25,-p05,-p95,-p75)
}
