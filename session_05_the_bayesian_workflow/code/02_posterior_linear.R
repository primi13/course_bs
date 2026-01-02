# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(bayesplot)
library(posterior)
library(tidyverse)
library(mcmcse)
library(HDInterval)

# modelling and data prep ------------------------------------------------------
# compile the model
model <- cmdstan_model("./session_05_the_bayesian_workflow/models/simple_linear.stan")

# load the data
data <- read.csv("./session_05_the_bayesian_workflow/data/temperature.csv", sep = ";")

# remove month
data <- data %>% select(-month)
data

# mean per year
data <- data %>%
  group_by(year) %>%
  summarise(temperature = mean(temperature))

# shift to 0
data$year0 <- data$year - min(data$year)
data

# prep the data for Stan
n <- nrow(data)
x <- data$year0
y <- data$temperature
stan_data <- list(n = n, x = x, y = y)

# fit
fit <- model$sample(
  data = stan_data,
  parallel_chains = 4,
  seed = 1
)

# diagnostics ------------------------------------------------------------------
# traceplot
mcmc_trace(fit$draws())

# summary
fit$summary()

# analysis ---------------------------------------------------------------------
# extract draws
df <- as_draws_df(fit$draws())
df

# mcse
mcse(df$b)

# hdi
hdi(df$b, credMass = 0.90)

# visualize data points and regression lines
# plot only 100 random regression lines
df_100 <- data.frame(alpha = df$a, beta = df$b)
df_100 <- sample_n(df_100, 100)

df_1000 <- data.frame(alpha = df$a, beta = df$b)
df_1000 <- sample_n(df_1000, 1000)

# plot
ggplot() +
  geom_point(
    data = data,
    aes(x = year0, y = temperature),
    alpha = 0.3, size = 1.5, shape = 16
  ) +
  geom_abline(
    data = df_1000,
    aes(slope = beta, intercept = alpha),
    alpha = 0.02, linewidth = 1
  ) +
  ylim(6, 12) +
  scale_x_continuous(
    breaks = c(0, 40, 80, 120),
    labels = c(1900, 1940, 1980, 2020),
    limits = c(0, 120)
  ) +
  xlab("Year") +
  ylab("T [°C]")
