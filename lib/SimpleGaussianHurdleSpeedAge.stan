data {
int<lower=0> NZ; // number of data items equal zero
int<lower=0> NN; // number of data items greater than zero
int<lower=0> K; // number of predictors
int<lower=0> y_zero[NZ]; // outcome vector equal zero
real<lower=0> y_num[NN]; // outcome vector equal zero
matrix [NN,K] x_num; // predictor matrix
matrix [NZ,K] x_zero; // predictor matrix
vector[NZ] age_zero;
vector[NN] age_num;

}

parameters {
real alpha; // intercept
vector[K] beta; // coefficients for predictors
real<lower=0> sigma; // error scale
real alpha_b; // intercept
vector[K] beta_b; // coefficients for predictors
real beta_b_age;
real beta_age;
// real<lower=1> dfs;
}

transformed parameters{
  vector[NZ] theta_NZ;
  vector[NN] theta_NN;
  theta_NZ = x_zero * beta_b + age_zero * beta_b_age + alpha_b ;
  theta_NN = x_num *beta_b +age_num * beta_b_age + alpha_b;
}

model{
beta ~ normal(0,1);
alpha ~ normal(0,1);
sigma ~ normal(0,10);
alpha_b ~ normal(0,1);
beta_b ~normal(0,1);
// dfs ~ normal(0,100);

beta_age ~ normal(0,1);
beta_b_age ~ normal(0,1);

y_zero~ binomial_logit(1,theta_NZ);
1  ~ binomial_logit(1,theta_NN);
y_num ~ normal(x_num * beta+ age_num*beta_age+ alpha ,sigma);


}
generated quantities {
  vector[NN+NZ] log_lik;
  
  for(i in 1:NZ){
    log_lik[i] = binomial_logit_lpmf(0|1,theta_NZ[i]);
  }
  for(i in 1:NN ){
    log_lik[NZ+i] = binomial_logit_lpmf(1|1,theta_NN[i]) + normal_lpdf(y_num[i]|x_num[i,] * beta+ age_num[i]*beta_age+ alpha ,sigma);
  }
  
}
