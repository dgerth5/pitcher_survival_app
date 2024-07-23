data {
  int<lower=0> N; // number of observations
  int<lower=0> P; // number of predictors + intercept
  vector[N] y; // MLB innings thrown after rookie year
  int<lower=0, upper=1> censor[N];
  matrix[N, P] X; // model matrix
}

parameters {
  real<lower=0> alpha ; //shape
  vector[P] beta ; // set of scale predictors
}

model {
  // priors
  alpha ~ normal(1, 1) ;
  beta[1] ~ normal(5.5, 2) ; // intercept

  for (p in 2:P) {
    beta[p] ~ normal(0, 1);      // Prior for beta[2] to beta[K]
  }

  // model

  for (i in 1:N){
    real lambda = exp(dot_product(X[i], beta)); // scale parameter
    if (censor[i] == 1){
      target += weibull_lpdf(y[i] | alpha, lambda);
    }
    else{
      target += weibull_lccdf(y[i] | alpha, lambda);
    }

  }

}