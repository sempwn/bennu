##########################################
##                                     ###
##                                     ###
##  Create a series of simulations to  ###
##  Assess ability of model to         ###
##  recover main parameters            ###
##                                     ###
##                                     ###
##########################################


library(bennu)
library(rstan)
library(tidybayes)
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(progress)
library(here)

rstan::rstan_options(auto_write = TRUE)
options(mc.cores = 4)


plot_data <- function(d){
  d %>%
    ggplot(aes(x=times,y=p_use,color=as.factor(regions))) +
    geom_line() +
    geom_point()
}

experiments <- expand_grid(sigma = seq(0.05,0.5,by=0.05),
                           zeta = seq(0.05, 0.5, by=0.05))

experimental_validation_data <- tibble()

pb <- progress_bar$new(
  format = "  running [:bar] :percent eta: :eta",
  total = nrow(experiments), clear = FALSE, width= 60)

for(i in 1:nrow(experiments)){
  sigma <- as.numeric(experiments[i,"sigma"])
  zeta <- as.numeric(experiments[i,"zeta"])

  d <- bennu::model_random_walk_data(zeta=zeta,sigma=sigma,
                                     region_coeffs = c(5, 0.5, 1, 2),
                                     c_region = c(-1, 2, 0, 1), N_t = 48)
  fit <- est_naloxone(d)

  row_results <- fit %>%
    tidybayes::gather_draws(sigma,zeta) %>%
    group_by(.variable) %>%
    dplyr::summarise(
      p50 = stats::quantile(.value, 0.5),
      p25 = stats::quantile(.value, 0.25),
      p75 = stats::quantile(.value, 0.75),
      p05 = stats::quantile(.value, 0.05),
      p95 = stats::quantile(.value, 0.95)
    ) %>%
    left_join(
      tibble(.variable = c("sigma", "zeta"),
             true_value = c(sigma,zeta))
    ) %>%
    mutate(experiment = i)

  experimental_validation_data <- experimental_validation_data %>%
    bind_rows(row_results)
  pb$tick()
}

usethis::use_data(experimental_validation_data)

