data {
  int<lower=1> J;              // number of goalies
  int<lower=1> P;              // number of polygons ("questioons")
  int<lower=1> N;              // number of shots
  //int<lower=1> B;             // number of predictors for the hierarchical prior on beta (including int)
  //matrix [N, B] S;            // matrix of shot characteristics for hierarchical prior on  beta
  int<lower=1,upper=J> jj[N];  // goalie for observation n
  int<lower=1,upper=P> pp[N];  // polygon for observation n
  int<lower=0,upper=1> y[N];   // correctness for observation n
}

parameters {
  real delta;         // mean student ability
  real alpha[J];      // ability of goalie j - mean ability
  real beta[P];       // difficulty of a save for a shot from a given polygon
  //real<lower=0> sigma_beta;    // variance of the shot difficulty
  //real mu_b[N];       // stores the output of the regression for shot difficulty
  //real mu_beta[N];       // mean of the shot difficulty prior
  //real<lower=0> sigma_b; // variance of the shot difficulty prior
  //vector[B] b;         // coefficients for the hierarchical prior on beta
  //real b_0;          // intercept for the hierarchical prior on beta
}

model {
  //prior
  //b ~ std_normal(); //prior on shot characteristic coefficient
  //sigma_b ~ cauchy(0, 1); //variance of shot prior
  //for (n in 1:N)
  //  mu_b[n] ~ normal(dot_product(S[n], b), sigma_b); //mean of the shot difficulty prior
  ///mu_beta ~ normal(S*b, sigma_b); //mean of the shot difficulty prior
 
  //sigma_beta ~ cauchy(0, 1); //prior on beta standard deviation
  //alpha ~ std_normal();         // informative true prior
  //beta ~ normal(mu_beta, sigma_beta);          // informative true prior
  alpha ~ std_normal();         // informative true prior
  beta ~ std_normal();          // informative true prior
  delta ~ normal(0.75, 1);      // informative true prior
  
  //likelihood
  for (n in 1:N)
    y[n] ~ bernoulli_logit(alpha[jj[n]] - beta[pp[n]] + delta);
}
