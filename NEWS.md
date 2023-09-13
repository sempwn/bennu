# bennu 0.3.0

* `kit_summary_table` created to provide a quick way of summarizing model 
output by different strata

* `model_random_walk_data` created to more closely mimic Bayesian data 
generating process

* `generate_model_data` deprecated as `model_random_walk_data` will supplant it
as way of generating simulation data to test properties of the model

* Updates to stan model to make it compliant to rstan 2.26 (#22)

# bennu 0.2.1

# bennu 0.2.0

# bennu 0.1.0

* `est_naloxone` can accept `psi_vector` of variable length and additionally
accepts `delay_alpha` and `delay_beta` (#12).
* `est_naloxone` can accept missing values for `Reported_Distributed` and
`Reported_Used` columns (#6).

# bennu 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
