#' Plot of probability of naloxone kit use
#' @description plot can compare between two different model fits or a single
#' model fit by region. If data are simulated then can also include in plot.
#' For more details see the introduction vignette:
#' \code{vignette("Introduction", package = "bennu")}
#' @param ... named list of [stanfit] objects
#' @param data data used for model fitting. Can also include `p_use` column
#' which can be used to plot true values if derived from simulated data.
#' @param reported if `TRUE` then produces a plot of the reported kits
#' which is equivalent to the predictive check.
#' @param regions_to_plot Optional list to filter which regions are
#' plotted
#' @return [ggplot2] object
#' @export
#' @family plots
plot_kit_use <- function(...,
                         data = NULL,
                         reported = FALSE,
                         regions_to_plot = NULL) {
  combined_plot_data <- combine_model_fits(..., data = data,
                                           reported = reported)

  region <- model <- times <- data_var_name <- sim_reported_used <- p_use <- NULL
  region_name <- .data <- NULL
  p50 <- p05 <- p95 <- p25 <- p75 <- NULL

  # data variables that can be plotted if they exist
  if(reported){
    data_var <- "Reported_Used"
    model_var <- "sim_reported_used"
  }else{
    data_var <- "p_use"
    model_var <- "sim_p"
  }

  # add true values as NULL if doesn't exist in data
  if (!data_var %in% names(combined_plot_data)) {
    combined_plot_data[[data_var]] <- NA_real_
  }


  if (!is.null(regions_to_plot)) {
    combined_plot_data <- combined_plot_data %>%
      dplyr::filter(region_name %in% regions_to_plot)
  }

  combined_plot_data <- combined_plot_data %>%
    dplyr::group_by(region_name, model, times) %>%
    dplyr::summarise(
      p50 = stats::quantile(.data[[model_var]], 0.5),
      p25 = stats::quantile(.data[[model_var]], 0.25),
      p75 = stats::quantile(.data[[model_var]], 0.75),
      p05 = stats::quantile(.data[[model_var]], 0.05),
      p95 = stats::quantile(.data[[model_var]], 0.95),
      data_var_name = mean(.data[[data_var]])
    )

  p_use_plot <- combined_plot_data %>%
    dplyr::mutate(region_name = as.factor(region_name)) %>%
    ggplot2::ggplot(ggplot2::aes(x = times)) +
    ggplot2::geom_line(
      ggplot2::aes(y = p50, color = region_name)
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = p05, ymax = p95, fill = region_name),
      alpha = 0.2
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = p25, ymax = p75, fill = region_name),
      alpha = 0.2
    )

  if (!is.null(data) && !all(is.na(combined_plot_data$data_var_name))) {
    p_use_plot <- p_use_plot +
      ggplot2::geom_point(ggplot2::aes(y = data_var_name, color = region_name))
  }

  if (length(list(...)) > 1) {
    p_use_plot <- p_use_plot +
      ggplot2::facet_wrap(ggplot2::vars(model))
  }

  p_use_plot <- p_use_plot +
    ggplot2::labs(x = "Time")

  if(reported){
    p_use_plot <- p_use_plot +
      ggplot2::labs(y = "Reported prior kit use",
                    color = "Region",
                    fill = "Region")
  }else{
    p_use_plot <- p_use_plot +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::labs(y = "Probability of prior kit use")
  }

  return(p_use_plot)
}

#' Combine one of more [stanfit] objects together with data
#' to also include region and time components
#' @param ... list of [rstan] fits
#' @param data data used for model fitting. Can also include `p_use` column
#' which can be used to plot true values if derived from simulated data.
#' @param reported if `TRUE` then produces a plot of the reported kits
#' which is equivalent to the predictive check.
#' @noRd
combine_model_fits <- function(..., data = NULL, reported = FALSE) {
  sim_p <- i <- sim_reported_used <- NULL
  Reported_Used <- Reported_Distributed <- NULL

  fit_list <- list(...)

  comparison_tibble <- dplyr::tibble()
  for (model in names(fit_list)) {
    out <- fit_list[[model]]
    if(reported){
      out <- out %>%
        tidybayes::spread_draws(sim_reported_used[i])
    } else {
      out <- out %>%
        tidybayes::spread_draws(sim_p[i])
    }
    out <- out %>%
      dplyr::mutate(model = model)

    if (!is.null(data)) {
      if(reported){
        out <- out %>%
          dplyr::left_join(
            dplyr::mutate(tidyr::drop_na(data, Reported_Used,
                                         Reported_Distributed),
                          i = dplyr::row_number()),
            by = "i"
          )
      } else {
        out <- out %>%
          dplyr::left_join(
            dplyr::mutate(data, i = dplyr::row_number()),
            by = "i"
          )
      }

    }

    comparison_tibble <- dplyr::bind_rows(
      comparison_tibble,
      out
    )
  }

  return(comparison_tibble)
}


#' plot probability of use from simulated data
#' @param d dataframe
#' @noRd
plot_p_use_data <- function(d) {
  times <- p_use <- regions <- NULL
  d %>%
    ggplot2::ggplot(ggplot2::aes(x = times, y = p_use, color = as.factor(regions))) +
    ggplot2::geom_line() +
    ggplot2::geom_point()
}
