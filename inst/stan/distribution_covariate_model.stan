//
// This Stan program defines a model of naloxone distribution accounting
// for differences in reported distribution by time and site and delay in
// distribution of kits.
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//


// define functions
functions {

  // convolve a reversed pdf and vector
  vector convolve(vector x, vector rev_pmf) {
      int t = num_elements(x);
      int max_pmf = num_elements(rev_pmf);
      vector[t] conv_cases = rep_vector(1e-5, t);
      for (s in 1:t) {
          conv_cases[s] += dot_product(x[max(1, (s - max_pmf + 1)):s],
                                       tail(rev_pmf, min(max_pmf, s)));
      }
      return(conv_cases);
  }

  // reverse a vector
  vector rev_func(vector x) {
    int n_points = num_elements(x);
    vector [n_points] x_rev;

    for(i in 1:n_points)
      x_rev[i] = x[n_points - i + 1];

    return(x_rev);
  }

  // create a triangular KxK matrix for convolution based on a
  // vector a and dimension number K
  matrix to_triangular_convolution(vector x, int K) {
    matrix[K, K] y;
    int pos = 1;
    for (i in 1:K) {
      for (j in 1:K) {
        pos = i + 1 - j;
        if(pos > 0 && pos <= num_elements(x)){
          y[i, j] = x[pos];
        }else{
          y[i,j] = 0;
        }

      }
    }
    return y;
  }

}

// Input data
data {
  // a switch to evaluate the likelihood
  int<lower=0,upper=1> run_estimation;

  // choose which form of a random walk to use (order 1 or order 2)
  int<lower=1,upper=2> rw_type;

  // number of regions
  int<lower=0> N_region;

  // number of time steps
  int<lower=0> N_t;

  // total number of rows in ordered data
  int<lower=0> N;

  // total number of rows in reported distribution data
  int<lower=0> N_distributed;

  // parameters for delay distribution
  real<lower=0> alpha;
  real<lower=0> beta;

  // max number for delay distribution
  int<lower=0> max_delays;

  // delay distribution for time of kit use to reporting
  int<lower=0> N_psi;
  array[N_psi] real<lower=0> psi;

  // vector (time, HSDA) of regions (coded 1 to N_region)
  array[N_distributed] int<lower=1,upper=N_region> regions;

  // vector (time, HSDA) of regions (coded 1 to N_t)
  array[N_distributed] int<lower=1,upper=N_t> times;

  // vector (time, HSDA) of orders
  array[N] int Orders;

  // create 2D version of Orders data
  array[N_region,N_t] int Orders2D;

  // vector (time, HSDA) reported as distributed
  array[N_distributed] int Reported_Distributed;

  // vector (time, HSDA) reported as used
  array[N_distributed] int Reported_Used;

  //hyper-priors
  real<lower=0> mu0_sigma;
  real<lower=0> sigma_sigma;
  real mu0_mu;
  real sigma_mu;

}

transformed data{
  // create delay distribution for distribution of THN
  vector[max_delays] distribute_pmf;
  vector[max_delays] reverse_distribute_pmf;
  real trunc_lpmf;
  matrix[N_t,N_t] reporting_delay_matrix;


  // calculate discretized  delay distribution
  trunc_lpmf = log_diff_exp(gamma_lcdf(max_delays + 1 | alpha, beta), gamma_lcdf(1 | alpha, beta));
  for (i in 1:max_delays){
    real distribute_lpmf = log_diff_exp(gamma_lcdf(i + 1 | alpha, beta), gamma_lcdf(i | alpha, beta)) - trunc_lpmf;
    distribute_pmf[i] = exp(distribute_lpmf);
  }
  // reverse delay distribution
  reverse_distribute_pmf = rev_func(distribute_pmf);

  // create use to reporting delay distribution matrix
  reporting_delay_matrix = to_triangular_convolution(to_vector(psi),N_t);





}

// The parameters accepted by the model.
parameters {
  array[N_distributed] real logp;
  real<lower=0> sigma;
  real<lower=0> zeta;
  real mu0;

  array[N_region] real c; // region covariates
  array[N_t] real ct; // time covariates



}

//transformed parameters
transformed parameters{
  array[N_distributed] real p;

  p = inv_logit(logp);
}

// The model to be estimated.
model {

  // priors for p
  mu0 ~ normal(mu0_mu,mu0_sigma);
  sigma ~ normal(sigma_mu,sigma_sigma);

  // set priors for covariates
  c ~ normal(0,1);
  ct[1] ~ normal(0,1);
  zeta ~ normal(0,1); // random walk variance

  if(rw_type == 1){
    for(i in 2:N_t){
      ct[i] ~ normal(ct[i-1],zeta);
    }
  }else{
    // RW of order 2
    for(i in 1:(N_t-2)){
      ct[i] - 2*ct[i+1] + ct[i+2] ~ normal(0,zeta);
    }
  }

  // set priors
  logp ~ normal(mu0 + to_vector(c[regions]) + to_vector(ct[times]),sigma);


  // likelihood
  if(run_estimation == 1){
    Reported_Used ~ binomial(Reported_Distributed,p);
  }

}

// simulated quantities based on model
generated quantities {
  array[N] int sim_used;
  //vector[N] sim_used;
  array[N] int Distributed;
  array[N_region, N_t] int Distributed2D;
  array[N] real sim_p;
  array[N_region, N_t] real sim_p2D;
  array[N_distributed] int sim_reported_used;

  vector[N] sim_actual_used;

  array[N_t] int region_distributed;
  array[N_t] int convolve_region_distributed;


  for(i in 1:N_region){
    region_distributed = Orders2D[i,:];



    // convolve a reversed pdf and vector through a binomial random number
    // generator
    for (s in 1:N_t) {
          convolve_region_distributed[s] = sum(binomial_rng(region_distributed[max(1, (s - max_delays + 1)):s],
                                       tail(reverse_distribute_pmf, min(max_delays, s))));
          sim_p2D[i,s] = normal_rng(mu0 + c[i] + ct[s],sigma);
    }

    Distributed2D[i,:] = convolve_region_distributed;





  }

  // flatten in row-major order (important to check this matches Orders)
  Distributed = to_array_1d(Distributed2D);
  sim_p = inv_logit(to_array_1d(sim_p2D));

  sim_used = binomial_rng(Distributed,sim_p);
  sim_reported_used = binomial_rng(Reported_Distributed,p);



  for(i in 1:N_region){
    sim_actual_used[(1 + (i-1)*N_t):i*N_t] = mdivide_left_tri_low(reporting_delay_matrix,
    segment(to_vector(sim_used),1 + (i-1)*N_t,N_t));
  }
}


