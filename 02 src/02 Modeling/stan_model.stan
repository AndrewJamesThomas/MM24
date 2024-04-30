// Very simple bayesian model, more of a POC than anything

data {
  int<lower=0> N; // total sample size
  int<lower=0> k; // number of independent vars
  matrix[N, k] x; // Independent Vector
  array[N] int y; // dependent var
}

parameters {
  vector[k] beta; // coef
}

model {
  // Set Priors
//  beta[1] ~ normal(0.004, 0.002);
//  beta[2] ~ normal(-0.004, 0.002);
//  beta[3] ~ normal(0.004, 0.002);
//  beta[4] ~ normal(-0.004, 0.002);

// set priors on ordinal ranks so they all contribute about the same
  beta[5] ~ normal(-0.004, 0.00001);
  beta[6] ~ normal(0.004, 0.00001);
  beta[7] ~ normal(-0.004, 0.00001);
  beta[8] ~ normal(0.004, 0.00001);
  beta[9] ~ normal(-0.004, 0.00001);
  beta[10] ~ normal(0.004, 0.00001);

  // model
  y ~ bernoulli_logit(x * beta);
}
