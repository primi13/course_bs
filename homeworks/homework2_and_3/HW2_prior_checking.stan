data {
  int<lower=1> N;
  int<lower=1> C;
  int<lower=1> K;

  array[N] int<lower=1, upper=C> country;
  array[N] int<lower=1, upper=K> continent;
  array[C] int<lower=1, upper=K> country_continent;

  vector[N] year_s;
  vector[N] gdp_s;
  vector[N] life_s;
}

generated quantities {
  // --- global effects ---
  real alpha_0   = normal_rng(5, 2);
  real beta_t_0  = normal_rng(0, 1);
  real beta_g_0  = normal_rng(0, 1);
  real beta_l_0  = normal_rng(0, 1);

  // --- scale parameters ---
  real sigma_y        = exponential_rng(1);
  real sigma_alpha_k  = exponential_rng(1);
  real sigma_alpha_c  = exponential_rng(1);
  real sigma_beta_t   = exponential_rng(1);
  real sigma_beta_g   = exponential_rng(1);
  real sigma_beta_l   = exponential_rng(1);

  // --- continent-level parameters ---
  vector[K] alpha_k;
  vector[K] beta_t_k;
  vector[K] beta_g_k;
  vector[K] beta_l_k;

  for (k in 1:K) {
    alpha_k[k]  = normal_rng(alpha_0,  sigma_alpha_k);
    beta_t_k[k] = normal_rng(beta_t_0, sigma_beta_t);
    beta_g_k[k] = normal_rng(beta_g_0, sigma_beta_g);
    beta_l_k[k] = normal_rng(beta_l_0, sigma_beta_l);
  }

  // --- country-level intercepts ---
  vector[C] alpha_c;
  for (c in 1:C) {
    alpha_c[c] = normal_rng(alpha_k[country_continent[c]], sigma_alpha_c);
  }

  // --- prior predictive observations ---
  vector[N] y_prior;
  for (i in 1:N) {
    y_prior[i] = normal_rng(
      alpha_c[country[i]] +
      beta_t_k[continent[i]] * year_s[i] +
      beta_g_k[continent[i]] * gdp_s[i] +
      beta_l_k[continent[i]] * life_s[i],
      sigma_y
    );
  }
}
