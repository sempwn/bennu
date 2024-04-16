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
library(latex2exp)

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

results <- tibble()

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

  results <- results %>% bind_rows(row_results)
  pb$tick()
}

results %>% saveRDS(
  file = here(
    "revised_manuscript", "plots",
    "comprehensive_simulation_results.rds"
  )
)


###############################
## Plot of resulting figures ##
###############################
results <- readRDS(
  file = here(
    "revised_manuscript", "plots",
    "comprehensive_simulation_results.rds"
  )
)
# plot for sigma

p_res_sigma <- results %>%
  group_by(experiment) %>%
  mutate(zeta_val = sum(true_value * as.numeric(.variable == "zeta"))) %>%
  ungroup() %>%
  filter(.variable == "sigma") %>%
  ggplot(aes(x=true_value,y=true_value)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray20") +
  geom_errorbar(aes(y = p50,ymin=p05,ymax=p95, color = zeta_val,
                    group = experiment)) +
  geom_point(aes(y = p50, color = zeta_val)) +
  theme_classic() +
  labs(color = parse(text = TeX("$\\zeta$")),
       x = "True value", y = "Inferred value")


show(p_res_sigma)

p_res_sigma %>% saveRDS(
  file = here(
    "revised_manuscript", "plots",
    "comprehensive_simulation_plot_sigma.rds"
  )
)

p_res_zeta <- results %>%
  group_by(experiment) %>%
  mutate(sigma_val = sum(true_value * as.numeric(.variable == "sigma"))) %>%
  ungroup() %>%
  filter(.variable == "zeta") %>%
  ggplot(aes(x=true_value,y=true_value)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray20") +
  geom_errorbar(aes(y = p50,ymin=p05,ymax=p95, color = sigma_val,
                    group = experiment)) +
  geom_point(aes(y = p50, color = sigma_val)) +
  theme_classic() +
  labs(color = parse(text = TeX("$\\sigma$")),
       x = "True value", y = "Inferred value")

show(p_res_zeta)

p_res_zeta %>% saveRDS(
  file = here(
    "revised_manuscript", "plots",
    "comprehensive_simulation_plot_zeta.rds"
  )
)
