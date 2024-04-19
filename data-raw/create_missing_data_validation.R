library(bennu)
library(rstan)
library(tidybayes)
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(progress)
library(here)

set.seed(43)

rstan::rstan_options(auto_write = TRUE)
options(mc.cores = 4)


experiments <- expand_grid(reporting_freq = seq(1, 10, by = 1))

missing_data_validation <- tibble()

pb <- progress_bar$new(
  format = "  running [:bar] :percent eta: :eta",
  total = nrow(experiments), clear = FALSE, width = 60
)

for (ind in 1:nrow(experiments)) {
  reporting_freq <- as.numeric(experiments[ind, "reporting_freq"])

  # generate complete data
  d_complete <- bennu::model_random_walk_data(
    zeta = 0.25, sigma = 0.25,
    region_coeffs = c(5, 0.5, 1, 2),
    c_region = c(-1, 2, 0, 1), N_t = 48
  )

  # generate partial data based on reporting frequency
  d_reported <- d_complete %>%
    mutate(
      nonreporting_times = times %% reporting_freq != 1,
      Reported_Distributed = if_else(
        times %% reporting_freq != 1,
        NA_real_, Reported_Distributed
      ),
      Reported_Used = if_else(
        times %% reporting_freq != 1,
        NA_real_, Reported_Used
      )
    )

  if (reporting_freq == 1) {
    d_reported <- d_complete
  }

  fit <- est_naloxone(d_reported)

  row_used <- fit %>%
    tidybayes::spread_draws(sim_used[i]) %>%
    dplyr::left_join(
      dplyr::mutate(d_complete, i = dplyr::row_number()),
      by = "i"
    ) %>%
    group_by(.chain, .iteration, .draw) %>%
    dplyr::summarise(
      Actual_Used = sum(Orders * p_use),
      Sim_Used = sum(sim_used)
    ) %>%
    ungroup() %>%
    dplyr::mutate(p_diff = (Actual_Used - Sim_Used) / Actual_Used) %>%
    dplyr::summarise(
      p50 = stats::quantile(p_diff, 0.5),
      p25 = stats::quantile(p_diff, 0.25),
      p75 = stats::quantile(p_diff, 0.75),
      p05 = stats::quantile(p_diff, 0.05),
      p95 = stats::quantile(p_diff, 0.95)
    ) %>%
    mutate(reporting_freq = reporting_freq)

  missing_data_validation <- missing_data_validation %>% bind_rows(row_used)
  pb$tick()
}

usethis::use_data(missing_data_validation)

