library(rstan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores(logical = FALSE))


## basic example code
d <- generate_model_data()
# note iter should be at least 2000 to generate a reasonable posterior sample
fit <- est_naloxone(d,iter=200,chains=2)

