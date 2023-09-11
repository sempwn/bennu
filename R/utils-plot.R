#' Plot of probability of naloxone kit use
#' @description plot can compare between two different model fits or a single
#' model fit by region. If data are simulated then can also include in plot.
#' For more details see the introduction vignette:
#' \code{vignette("Introduction", package = "bennu")}
#' @param ... named list of [stanfit] objects
#' @param data data used for model fitting. Can also include `p_use` column
#' which can be used to plot true values if derived from simulated data.
#' @return [ggplot2] object
#' @export
#' @family plots
plot_kit_use <- function(..., data = NULL) {
  combined_plot_data <- combine_model_fits(..., data = data)

  region <- model <- times <- sim_p <- p_use <- region_name <- NULL
  p50 <- p05 <- p95 <- p25 <- p75 <- NULL

  # add true values as NULL if doesn't exist in data
  if (!"p_use" %in% names(combined_plot_data)) {
    combined_plot_data[["p_use"]] <- NA_real_
  }

  combined_plot_data <- combined_plot_data %>%
    dplyr::group_by(region_name, model, times) %>%
    dplyr::summarise(
      p50 = stats::quantile(sim_p, 0.5),
      p25 = stats::quantile(sim_p, 0.25),
      p75 = stats::quantile(sim_p, 0.75),
      p05 = stats::quantile(sim_p, 0.05),
      p95 = stats::quantile(sim_p, 0.95),
      p_use = mean(p_use)
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

  if ("p_use" %in% names(data)) {
    p_use_plot <- p_use_plot +
      ggplot2::geom_point(ggplot2::aes(y = p_use, color = region_name))
  }

  if (length(list(...)) > 1) {
    p_use_plot <- p_use_plot +
      ggplot2::facet_wrap(ggplot2::vars(model))
  }

  p_use_plot <- p_use_plot +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(x = "Time", y = "Probability of prior kit use")

  return(p_use_plot)
}

#' Combine one of more [stanfit] objects together with data
#' to also include region and time components
#' @noRd
combine_model_fits <- function(..., data = NULL) {
  sim_p <- i <- NULL

  fit_list <- list(...)

  comparison_tibble <- dplyr::tibble()
  for (model in names(fit_list)) {
    out <- fit_list[[model]] %>%
      tidybayes::spread_draws(sim_p[i]) %>%
      dplyr::mutate(model = model)

    if (!is.null(data)) {
      out <- out %>%
        dplyr::left_join(
          dplyr::mutate(data, i = dplyr::row_number()),
          by = "i"
        )
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
