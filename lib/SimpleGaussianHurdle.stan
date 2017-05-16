data {
int<lower=0> N; // number of data items
int<lower=0> K; // number of predictors
matrix [N,K] x; // predictor matrix
real<lower=0> y[N]; // outcome vector
}
parameters {
real alpha; // intercept
vector[K] beta; // coefficients for predictors
real<lower=0> sigma; // error scale
real alpha_b; // intercept
vector[K] beta_b; // coefficients for predictors
real<lower=1> dfs;

}
transformed parameters {
  vector[N] theta;
  for(n in 1:N){
    theta[n] = x[n,:]*beta_b+alpha_b;
  }
  
}
model {
beta ~ normal(0,1);
alpha ~ normal(0,1);
sigma ~ normal(0,1);
alpha_b ~ normal(0,1);
beta_b ~normal(0,1);
dfs ~ normal(0,100);
for (n in 1:N) {
       if (y[n] == 0)
          target += bernoulli_logit_lpmf(0|theta[n]);
        else {
          target += log_sum_exp(bernoulli_logit_lpmf(1|theta[n]),
           student_t_lpdf(y[n]|dfs,x[n,:] * beta + alpha, sigma));
}
}
}
generated quantities {
    vector[N] log_lik;
    vector[N] y_gen;
  for (n in 1:N){
    if (bernoulli_logit_rng(theta[n])){
          log_lik[n] =  student_t_lpdf(y[n]|dfs,x[n,:] * beta + alpha, sigma)+
                        bernoulli_logit_lpmf(1|theta[n]);
          y_gen[n] =  student_t_rng(dfs,x[n,:] * beta + alpha, sigma);
    }else{
      log_lik[n] = bernoulli_logit_lpmf(0|theta[n]);
      y_gen[n] =  0;
    }
  }
}
