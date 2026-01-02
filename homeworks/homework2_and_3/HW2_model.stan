data {
  int<lower=1> N;                  // observations
  int<lower=1> C;                  // countries
  int<lower=1> K;                  // continents

  vector[N] y;                     // happiness score (target variable)
  
  array[N] int<lower=1, upper=C> country;
  array[N] int<lower=1, upper=K> continent;
  array[C] int<lower=1, upper=K> country_continent;

  vector[N] year_s;
  vector[N] gdp_s;
  vector[N] life_s;
}

parameters {
  // global effects
  real alpha_0;
  real beta_t_0;
  real beta_g_0;
  real beta_l_0;

  // continent-level effects
  vector[K] alpha_k;
  vector[K] beta_t_k;
  vector[K] beta_g_k;
  vector[K] beta_l_k;
    
  real<lower=0> sigma_alpha_k;
  real<lower=0> sigma_beta_t;
  real<lower=0> sigma_beta_g;
  real<lower=0> sigma_beta_l;

  // country-level intercepts
  vector[C] alpha_c;
  real<lower=0> sigma_alpha_c;

  // observation noise
  real<lower=0> sigma_y;
}

model {
  // global priors
  alpha_0 ~ normal(5, 2);
  beta_t_0 ~ normal(0, 1);
  beta_g_0 ~ normal(0, 1);
  beta_l_0 ~ normal(0, 1);

  // variance priors
  sigma_y ~ exponential(1);
  sigma_alpha_c ~ exponential(1);
  sigma_alpha_k ~ exponential(1);
  sigma_beta_t ~ exponential(1);
  sigma_beta_g ~ exponential(1);
  sigma_beta_l ~ exponential(1);

  // continent-level
  alpha_k ~ normal(alpha_0, sigma_alpha_k);
  beta_t_k ~ normal(beta_t_0, sigma_beta_t);
  beta_g_k ~ normal(beta_g_0, sigma_beta_g);
  beta_l_k ~ normal(beta_l_0, sigma_beta_l);

  // country-level  
  alpha_c ~ normal(alpha_k[country_continent], sigma_alpha_c);

  // likelihood
  y ~ normal(
    alpha_c[country] +
    beta_t_k[continent] .* year_s +
    beta_g_k[continent] .* gdp_s +
    beta_l_k[continent] .* life_s,
    sigma_y
  );
}


generated quantities {
  vector[N] y_rep;
  for (i in 1:N) {
    y_rep[i] = normal_rng(
      alpha_c[country[i]] +
      beta_t_k[continent[i]] * year_s[i] +
      beta_g_k[continent[i]] * gdp_s[i] +
      beta_l_k[continent[i]] * life_s[i],
      sigma_y
    );
  }
}
