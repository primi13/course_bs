# load libraries
install.packages(c("cmdstanr", "ggdist", "tidyverse", "posterior", "bayesplot", "mcmcse", "HDInterval", "shinystan"))
library(cmdstanr)   # for interfacing Stan
library(ggplot2)    # for visualizations
library(ggdist)     # for distribution visualizations
library(tidyverse)  # for data prep
library(posterior)  # for extracting samples
library(bayesplot)  # for some quick MCMC visualizations
library(mcmcse)     # for comparing samples and calculating MCSE
library(HDInterval) # for high density intervals
library(shinystan)  # for visual diagnostics

# compile the model
model <- cmdstan_model("./models/bernoulli_beta.stan")

# prepare the data
data <- read.csv("./data/basketball_shots.csv", sep = ";")

# filter for 1st player, default and special rims
player1_default <- data %>% filter(PlayerID == 1 & SpecialRim == 0)
player1_special <- data %>% filter(PlayerID == 1 & SpecialRim == 1)

# prepare input data
stan_data_default <- list(n = nrow(player1_default), y = player1_default$Made)
stan_data_special <- list(n = nrow(player1_special), y = player1_special$Made)

# fit
fit_default <- model$sample(
  data = stan_data_default,
  seed = 1
)

fit_special <- model$sample(
  data = stan_data_special,
  seed = 1
)