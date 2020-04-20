data {
  int<lower = 1> N;  // Total number of answers 
  int<lower = 0, upper = N> k;  // Number of times "umbrella" was answered
}
parameters {
  // theta is a probability, it has to be constrained between 0 and 1
  real<lower = 0, upper = 1> theta;
}
model {
  // Prior on theta:
  target += beta_lpdf(theta | 4, 4); 
  // Likelihood:
  target += binomial_lpmf(k | N, theta); 
}

