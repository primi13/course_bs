# libraries --------------------------------------------------------------------
library(cmdstanr)
library(posterior)
library(tidyverse)
library(ggplot2)
library(ggdist)

# modeling ---------------------------------------------------------------------
# compile the model
# model <- cmdstan_model("./session_05_the_bayesian_workflow/models/prior.stan")

# for Cauchy(0, 2.5) prior on the intercept use the model below
model <- cmdstan_model("./session_05_the_bayesian_workflow/models/prior_cauchy.stan")

# generate data
n <- 100
weights <- runif(n, 50, 90)
weights <- weights - min(weights)

# stan data
stan_data <- list(n = n, x = weights)

# fit
fit <- model$sample(
  data = stan_data,
  fixed_param = TRUE,
  iter_sampling = 10,
  seed = 1
)

# analysis ---------------------------------------------------------------------
# extract draws
df <- as_draws_df(fit$draws())
df
df <- df %>% select(-alpha, -beta, -sigma, -.chain, -.draw, -.iteration)
df

# prep df for plotting
df_plot <- data.frame(x = numeric(), y = factor())

# plot 10
for (i in 1:10) {
  df_plot <- rbind(df_plot,
                   data.frame(x = as.numeric(df[i, ]), y = as.factor(i)))
}

# simulated distributions of heights
ggplot(df_plot, aes(x = x, y = y)) +
  stat_halfeye(fill = "skyblue", alpha = 0.75) +
  xlim(-300, 300)