# libraries --------------------------------------------------------------------
library(cmdstanr)
library(ggplot2)
library(bayesplot)
library(posterior)
library(tidyverse)

# data prep --------------------------------------------------------------------
# load the data
data <- read.csv("./session_05_the_bayesian_workflow/data/temperature.csv", sep = ";")

# after 2000
data <- data %>% filter(year >= 2000)

# prep the data for stan
stan_data <- list(
  n = nrow(data),
  y = data$temperature
)

# modeling ------- -------------------------------------------------------------
# compile
model <- cmdstan_model("./session_05_the_bayesian_workflow/models/normal.stan")

# fit
fit <- model$sample(
  data = stan_data,
  parallel_chains = 4,
  seed = 1
)

# traceplot
mcmc_trace(fit$draws())


# summary
fit$summary()

# posterior check --------------------------------------------------------------
# extract draws
df <- as_draws_df(fit$draws())

# 16 replicate datasets (4x4 grid for visualizations)
n <- 16
df_16 <- sample_n(df, 16)
df_ppc <- data.frame(temperature = numeric(), iteration = factor())

for (i in 1:n) {
  # generate n_sample observations
  temperatures <- rnorm(nrow(data), mean = df_16[i, ]$mu, sd = df_16[i, ]$sigma)

  # store
  df_ppc <- rbind(df_ppc,
                  data.frame(temperature = temperatures,
                             iteration = as.factor(i)))
}

# means
df_ppc_means <- df_ppc %>%
  group_by(iteration) %>%
  summarize(mean_temperature = mean(temperature))

df_ppc
df_ppc_means

# plot
ggplot(data = df_ppc, aes(x = temperature)) +
  geom_histogram(bins = 30, alpha = 0.5) +
  geom_vline(data = df_ppc_means,
             aes(xintercept = mean_temperature),
             linewidth = 1, alpha = 0.5) +
  geom_vline(xintercept = mean(data$temperature),
             linewidth = 1,
             linetype = "dashed") +
  facet_wrap(iteration ~ ., nrow = 4) +
  xlab("T [°C]")