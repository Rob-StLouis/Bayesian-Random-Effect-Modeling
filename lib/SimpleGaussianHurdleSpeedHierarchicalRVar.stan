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
int J; // number of districts
matrix [NN,J] d_num; // district dummy matrix
matrix [NZ,J] d_zero; // district matrix
vector[K] sum_vec; // hack to get row sum of a matrix
}

parameters {
real alpha; // intercept
vector[K] Beta_Mu;
vector<lower=0>[K] Beta_Sigma;
matrix[J,K] beta; // coefficients for predictors

real<lower=0> sigma_Mu;
real<lower=0> sigma_Sigma;
vector<lower=0>[J] sigma; // error scale
real alpha_b; // intercept
vector[K] beta_b; // coefficients for predictors
real beta_b_age;
real beta_age;
vector[J] gamma;
vector[J] gamma_b;
real Gamma_mu;
real<lower=0> Gamma_sigma;

real Gamma_b_mu;
real<lower=0> Gamma_b_sigma;
}

transformed parameters{
  vector[NZ] theta_NZ;
  vector[NN] theta_NN;
  matrix[J,K] beta_t;
  theta_NZ = x_zero * beta_b +age_zero * beta_b_age +d_zero * gamma_b + alpha_b;
  theta_NN = x_num *beta_b +age_num * beta_b_age+d_num * gamma_b + alpha_b;
  
  for( i in 1:K){
      beta_t[,i] = Beta_Mu[i]+Beta_Sigma[i]*beta[,i];
  }
  
  
}


model{
  
Beta_Mu ~normal(0,10);
Beta_Sigma ~student_t(4,0,10);

// ugly but probably fastest way to do this.
for(i in 1:K){
  beta[,i] ~ normal(0,1);
}

alpha ~ normal(0,1);
sigma_Mu ~ normal(0,10);
sigma_Sigma ~ normal(0,10);
sigma ~ normal(sigma_Mu,sigma_Sigma);


alpha_b ~ normal(0,1);
beta_b ~normal(0,1);
// dfs ~ normal(0,100);

Gamma_mu ~normal(0,.001); //fixing this to be zero because it is over-determined
Gamma_sigma ~normal(0,1);

Gamma_b_mu ~normal(0,.001); //fixing this to be zero because it is over-determined
Gamma_b_sigma ~normal(0,1);

gamma ~normal(Gamma_mu,Gamma_sigma);
gamma_b ~normal(Gamma_b_mu,Gamma_b_sigma);

beta_age ~ normal(0,1);
beta_b_age ~ normal(0,1);


y_zero~ binomial_logit(1,theta_NZ);
1  ~ binomial_logit(1,theta_NN);


// this type of codign of the heirarchical model is a little ugly, but not sure how to speed it up. 
// we could create a giant model matrix, a giant (K*J) vector, and then, do sparse * Dense multiplication to get the output. I should try that and compare. The advantage here is the relative ease of preserving the output values.
 
y_num ~ normal( ((x_num .* (d_num*beta_t)) * sum_vec)+ (age_num*beta_age)+(d_num * gamma)+ alpha ,(d_num*sigma));

}
generated quantities {
  vector[NN+NZ] log_lik;
  vector[NN+NZ] y_pred;

  for(i in 1:NZ){
    log_lik[i] = binomial_logit_lpmf(0|1,theta_NZ[i]);
    
    if (1==binomial_rng(1,inv_logit(theta_NZ[i]))){
      y_pred[i] = normal_rng( (x_zero[i,] .* (d_zero[i,]*beta_t))*sum_vec+ (age_zero[i]*beta_age)
                                + (d_zero[i,] * gamma)+ alpha , d_zero[i,]*sigma);
    }else{
      y_pred[i] = 0;
    }
    
  }
  for(i in 1:NN ){
    log_lik[NZ+i] = binomial_logit_lpmf(1|1,theta_NN[i]) +
                    normal_lpdf(y_num[i]|(x_num[i,] .* (d_num[i,] *beta_t))*sum_vec + 
                                         (age_num[i]*beta_age)+(d_num[i,] * gamma) + alpha ,
                                         d_num[i,]*sigma);
    
    if (1==binomial_rng(1,inv_logit(theta_NN[i]))){
      y_pred[NZ+i] = normal_rng((x_num[i,] .* (d_num[i,]*beta_t))*sum_vec + (age_num[i]*beta_age)
      +(d_num[i,] *
      gamma)+ alpha , d_num[i,]*sigma);
    }else{
      y_pred[NZ+i] = 0;
    }
    
  }

}

