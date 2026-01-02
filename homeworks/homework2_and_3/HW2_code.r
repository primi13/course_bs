# libraries -------------------------------------------------
library(bayesplot)
library(cowplot)
library(cmdstanr)
library(ggplot2)
library(HDInterval)
library(mcmcse)
library(posterior)
library(tidyverse)
library(ggdist)

dir <- "./homeworks/homework2_and_3/"

# data prep and model compilation ---------------------------
# load data
cantril <- read.csv(paste0(dir, "cantril_ladder.csv"))
countries <- read.csv(paste0(dir, "countries.csv"))

# inner join on country
data <- inner_join(cantril, countries, by = "country")
str(data)

# encode country
data <- data %>%
  mutate(
    country_id = as.integer(factor(country)),
    continent_id = as.integer(factor(continent))
  )

str(data)

# store lookup tables (useful later)
country_lookup <- levels(factor(data$country))
continent_lookup <- levels(factor(data$continent))

# create an array that maps from country to continent
country_continent_id <- data %>%
  distinct(country_id, continent_id) %>%
  arrange(country_id) %>%
  pull(continent_id)
str(country_continent_id)

# standardize continuous predictors
data <- data %>%
  mutate(
    year_s = as.numeric(scale(year)),
    log_gdp_s = as.numeric(scale(log_gdp)),
    life_exp_s = as.numeric(scale(life_expectancy))
  )

# prepare data for stan
stan_data <- list(
  N = nrow(data),
  C = length(unique(data$country_id)),
  K = length(unique(data$continent_id)),
  y = data$score,
  country = data$country_id,
  continent = data$continent_id,
  country_continent = country_continent_id,
  year_s = data$year_s,
  gdp_s = data$log_gdp_s,
  life_s = data$life_exp_s
)

# START PRIOR CHECKING --------------------------------------
# compile prior checking model
prior_model <- cmdstan_model(paste0(dir, "HW2_prior_checking.stan"))
# sample from priors only
# fit model
prior_name <- "prior_fit.rds"
if (!file.exists(paste0(dir, prior_name))) {
  message("Fitting prior model...")
  fit_prior <- prior_model$sample(
    data = stan_data,
    fixed_param = TRUE,
    chains = 4,
    parallel_chains = 4,
    iter_sampling = 2000,
    seed = 42
  )
  # save fitted model object
  fit_prior$save_object(file = paste0(dir, prior_name))
} else {
  fit_prior <- readRDS(paste0(dir, prior_name))
  message("Loaded prior model from file.")
}

# save prior fit object
prior_draws <- as_draws_df(fit_prior$draws())
prior_draws_y <- prior_draws %>%
  select(starts_with("y"))
df_prior_plot <- data.frame(x = numeric(), y = factor())
# plot the distribution of 8000 samples for the first 20 data points
for (i in 1:20) {
  df_prior_plot <- rbind(df_prior_plot,
                         data.frame(x = as.numeric(prior_draws_y[i, ]),
                                    y = as.factor(i)))
}
# simulated distributions of heights
color_vlines <- "black"
g_prior <- ggplot(df_prior_plot, aes(x = x, y = y)) +
  stat_halfeye(fill = "skyblue", alpha = 0.75) +
  labs(
    x = "Simulated happiness score",
    y = "Data point index",
  ) +
  scale_x_continuous(
    breaks = seq(-20, 20, by = 5),
    labels = seq(-20, 20, by = 5),
    limits = c(-20, 20)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = color_vlines) +
  geom_vline(xintercept = 10, linetype = "dashed", color = color_vlines) +
  theme_minimal() +
  theme(  
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12))
# save plot to a pdf vector file
ggsave(
  filename = paste0(dir, "prior_checking.pdf"),
  plot = g_prior,
  width = 8,
  height = 6
)
plot(g_prior)
# priors are reasonable
# END PRIOR CHECKING ----------------------------------------

# compile model
model <- cmdstan_model(paste0(dir, "HW2_model.stan"))

# fit model
model_name <- "model_fit1.rds"
if (!file.exists(paste0(dir, model_name))) {
  message("Fitting model...")
  fit <- model$sample(
    data = stan_data,
    seed = 42,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 2000,
    iter_sampling = 2000
  )
  # save fitted model object
  fit$save_object(file = paste0(dir, model_name))
} else {
  fit <- readRDS(paste0(dir, model_name))
  message("Loaded fitted model from file.")
}


# diagnostics -----------------------------------------------
draws <- fit$draws()

# # get all parameter names except lp__
# param_names <- setdiff(
#   variables(draws),
#   "lp__"
# )
# param_batches <- split(
#   param_names,
#   ceiling(seq_along(param_names) / 16)
# )
# for (i in seq_along(param_batches)) {
#   message("Trace plot batch ", i, " / ", length(param_batches))
#   mcmc_trace(
#     draws,
#     pars = param_batches[[i]]
#   )
# }

diagnostics <- summarise_draws(
  draws,
  rhat,
  ess_bulk,
  ess_tail
)

diagnostics %>%
  arrange(desc(rhat)) %>%
  print(n = 20)

# diagnostics are OK


# START posterior checking ----------------------------------
post_draws <- as_draws_df(draws)
str(post_draws)
# extract y_rep (posterior predictive)
yrep_draws <- post_draws %>%
  select(starts_with("y_rep"))
str(yrep_draws)
# observed data
y_obs <- data$score

# observed quartiles
q_obs <- quantile(y_obs, probs = c(0.25, 0.5, 0.75))

# function to compute quartiles for one replicated dataset
rep_quartiles <- function(yrep_row) {
  quantile(as.numeric(yrep_row), probs = c(0.25, 0.5, 0.75))
}

# compute quartiles for all posterior draws
yrep_mat <- as.matrix(yrep_draws)
yrep_mat[1:5, 1:5]
q_rep <- apply(yrep_mat, 1, rep_quartiles)
# transpose for easier handling
q_rep <- t(q_rep)
q_rep[1:5, 1:3]

colnames(q_rep) <- c("Q25", "Q50", "Q75")
q_rep <- as.data.frame(q_rep)
q_rep[1:5, 1:3]

q_diff <- q_rep %>%
  mutate(
    dQ25 = Q25 - q_obs[1],
    dQ50 = Q50 - q_obs[2],
    dQ75 = Q75 - q_obs[3]
  )
q_diff[1:5, 1:6]

set.seed(42)
num_replications <- 1000
rep_idx <- sample(seq_len(nrow(q_diff)), num_replications)

q_diff_sub <- q_diff[rep_idx, ]

# reshape for plotting
q_diff_long <- q_diff_sub %>%
  select(dQ25, dQ50, dQ75) %>%
  pivot_longer(
    cols = everything(),
    names_to = "quartile",
    values_to = "difference"
  )

g_posterior <- ggplot(q_diff_long, aes(x = quartile, y = difference)) +
  geom_boxplot(
    fill = "skyblue",
    alpha = 0.8,
    outlier.size = 0.6
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black"
  ) +
  labs(
    x = "Quartile",
    y = "Replicated - observed"
  ) +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12)
  )
#   theme_minimal(base_size = 13)
ggsave(
  filename = paste0(dir, "posterior_checking.pdf"),
  plot = g_posterior,
  width = 8,
  height = 6
)

# visualizing the fit w.r.t. one variable at a time
# extract parameters
alpha_c   <- post_draws %>% select(starts_with("alpha_c["))
beta_t_k  <- post_draws %>% select(starts_with("beta_t_k["))
beta_g_k  <- post_draws %>% select(starts_with("beta_g_k["))
beta_l_k  <- post_draws %>% select(starts_with("beta_l_k["))
sigma_y   <- post_draws$sigma_y

ppc_by_predictor <- function(
  x,
  x_name,
  beta_draws,
  alpha_draws,
  x_grid,
  y_obs
) {

  # linear predictor draws
  mu_draws <- sapply(
    x_grid,
    function(x0) alpha_draws + beta_draws * x0
  )

  mu_df <- as.data.frame(t(mu_draws))
  colnames(mu_df) <- paste0("draw_", seq_len(ncol(mu_df)))

  mu_summary <- mu_df %>%
    mutate(x = x_grid) %>%
    pivot_longer(-x, values_to = "mu") %>%
    group_by(x) %>%
    summarise(
      q05 = quantile(mu, 0.05),
      q25 = quantile(mu, 0.25),
      q50 = quantile(mu, 0.50),
      q75 = quantile(mu, 0.75),
      q95 = quantile(mu, 0.95),
      .groups = "drop"
    )

  ggplot() +
    geom_point(
      data = data.frame(x = x, y = y_obs),
      aes(x = x, y = y),
      alpha = 0.3
    ) +
    geom_ribbon(
      data = mu_summary,
      aes(x = x, ymin = q05, ymax = q95),
      fill = "skyblue",
      alpha = 0.3
    ) +
    geom_ribbon(
      data = mu_summary,
      aes(x = x, ymin = q25, ymax = q75),
      fill = "skyblue",
      alpha = 0.6
    ) +
    geom_line(
      data = mu_summary,
      aes(x = x, y = q50),
      linewidth = 1
    ) +
    labs(
      title = paste("Posterior predictive fit vs", x_name),
      x = x_name,
      y = "Score"
    ) +
    theme_minimal(base_size = 13)
}

# plot fit vs year
x_grid_year <- seq(
  min(data$year_s),
  max(data$year_s),
  length.out = 50
)
p_year <- ppc_by_predictor(
  x = data$year_s,
  x_name = "Year (standardized)",
  beta_draws = beta_t_k[[1]],   # choose continent or average if needed
  alpha_draws = alpha_c[[1]],   # choose representative country
  x_grid = x_grid_year,
  y_obs = data$score
)
p_year


# plot fit vs log GDP
x_grid_gdp <- seq(
  min(data$log_gdp_s),
  max(data$log_gdp_s),
  length.out = 50
)
p_gdp <- ppc_by_predictor(
  x = data$log_gdp_s,
  x_name = "GDP (standardized)",
  beta_draws = beta_g_k[[1]],
  alpha_draws = alpha_c[[1]],
  x_grid = x_grid_gdp,
  y_obs = data$score
)
p_gdp

# plot fit vs life expectancy
x_grid_life <- seq(
  min(data$life_exp_s),
  max(data$life_exp_s),
  length.out = 50
)
p_life <- ppc_by_predictor(
  x = data$life_exp_s,
  x_name = "Life expectancy (standardized)",
  beta_draws = beta_l_k[[1]],
  alpha_draws = alpha_c[[1]],
  x_grid = x_grid_life,
  y_obs = data$score
)
p_life

# END posterior checking ------------------------------------

# inference -------------------------------------------------
draws_df <- as_draws_df(fit$draws())

# Q1: Is happiness globally positively 
#     correlated with the year (is it increasing year-by-year)?
beta_year_global <- draws_df$beta_t_0
year_sd <- sd(data$year) # approx 3.97
beta_year_per_year <- beta_year_global / year_sd
mean_beta <- mean(beta_year_per_year)
hdi_beta <- hdi(beta_year_per_year, prob = 0.95)
hdi_beta
mcse(beta_year_per_year > 0)
cat(
  sprintf(
    "%.4f [%.4f, %.4f]\n",
    mean_beta,
    hdi_beta[, 1],
    hdi_beta[, 2]
  )
)
cat(
  sprintf(
    "%.4f [%.4f, %.4f]\n",
    mean_beta,
    quantile(beta_year_per_year, 0.025),
    quantile(beta_year_per_year, 0.975)
  )
)

df_beta <- data.frame(
  beta_year_per_year = beta_year_per_year
)

ggplot(df_beta, aes(x = beta_year_per_year)) +
  geom_density(
    fill = "skyblue",
    alpha = 0.7,
    linewidth = 1
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "black"
  ) +
  labs(
    title = "Posterior distribution of global year effect",
    subtitle = "Effect on happiness per calendar year",
    x = "Change in happiness per year",
    y = "Posterior density"
  ) +
  theme_minimal(base_size = 13)


# Q2: Are humans nowadays (2025) happy (score
#     higher than 5) from the global perspective?
# extrapolation to year 2025 is needed here
# firstly scale year 2025 using the same scaling (z-score)
year_mean <- mean(data$year)
year_sd   <- sd(data$year)
year_2025_s <- (2025 - year_mean) / year_sd
alpha_0 <- draws_df$alpha_0
beta_t  <- draws_df$beta_t_0
mu_2025 <- alpha_0 + beta_t * year_2025_s
mcse(mu_2025 > 5)

mean_mu_2025 <- mean(mu_2025)
hdi_mu_2025  <- hdi(mu_2025, prob = 0.95)
cat(
  sprintf(
    "Global happiness in 2025: %.2f [%.2f, %.2f]\n",
    mean_mu_2025,
    hdi_mu_2025[1],
    hdi_mu_2025[2]
  )
)
cat(
  sprintf(
    "P(happiness > 5 | data) = %.3f\n",
    mean(mu_2025 > 5)
  )
)

ggplot(
  data.frame(mu_2025 = mu_2025),
  aes(x = mu_2025)
) +
  geom_density(
    fill = "skyblue",
    alpha = 0.8,
    linewidth = 1
  ) +
  geom_vline(
    xintercept = 5,
    linetype = "dashed",
    color = "black"
  ) +
  labs(
    title = "Posterior distribution of global happiness in 2025",
    x = "Expected happiness score",
    y = "Posterior density"
  ) +
  theme_minimal(base_size = 13)


# Q3: What is the probability that Slovenia is nowadays (2025)
#     ranked in the top 20 countries happiness wise?
# firstly scale year 2025 using the same scaling (z-score)
year_mean <- mean(data$year)
year_sd   <- sd(data$year)
year_2025_s <- (2025 - year_mean) / year_sd
gdp_s_0  <- 0
life_s_0 <- 0

# continent-level effects
beta_t_k  <- draws_df %>% select(starts_with("beta_t_k")) %>% as.matrix()
beta_t_k[1:7, 1:6]
dim(beta_t_k)
beta_g_k  <- draws_df %>% select(starts_with("beta_g_k")) %>% as.matrix()
beta_l_k  <- draws_df %>% select(starts_with("beta_l_k")) %>% as.matrix()
# country-level intercepts
alpha_c <- draws_df %>% select(starts_with("alpha_c")) %>% as.matrix()

country_lookup <- levels(factor(data$country))
slovenia_idx <- which(country_lookup == "Slovenia")

num_draws <- nrow(draws_df)
num_countries <- length(country_lookup)
mu_matrix <- matrix(NA, nrow = num_draws, ncol = num_countries)
for (i in 1:num_draws) {
  mu_matrix[i, ] <- alpha_c[i, ] +
                    beta_t_k[i, country_continent_id] * year_2025_s
}
dim(mu_matrix)
mu_matrix[1:5, 1:5]
# Check if Slovenia is in top 20
slovenia_top20 <- apply(mu_matrix, 1, function(x) {
  rank_vec <- rank(-x)  # produces: higher happiness -> lower rank
  rank_vec[slovenia_idx] <= 20
})
prob_slovenia_top20 <- mcse(slovenia_top20)
# print the probability with plusminus MCSE
cat(
  sprintf(
    "P(Slovenia in top 20 in 2025 | data) = %.3f",
    prob_slovenia_top20$est),
  sprintf(
    "± %.3f\n",
    prob_slovenia_top20$se)
)

slovenia_ranks <- apply(mu_matrix, 1, function(x) rank(-x)[slovenia_idx])
mean(slovenia_ranks)

g_rank_slo <- ggplot(data.frame(rank = slovenia_ranks), aes(x = rank)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  geom_vline(xintercept = 20, linetype = "dashed", color = "black") +
  labs(
    x = "Rank among countries (1 = happiest)",
    y = "Posterior draws count"
  ) +
  scale_x_continuous(breaks = seq(0, num_countries, by = 20),
                     limits = c(1, num_countries)) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12)
  )
ggsave(
  filename = paste0(dir, "rank_slo.pdf"),
  plot = g_rank_slo,
  width = 8,
  height = 6
)
# Q4: Are there any major differences between the correlation of
#     health and wealth when it comes to continents?

# For each continent, compute the correlation between GDP and life expectancy

# Here we use the posterior slopes as a proxy for the correlation
# Simple approach: compute corr(beta_g_k[,k], beta_l_k[,k])
num_continents <- length(continent_lookup) # 6
continent_corr_summary <- data.frame(
  continent = continent_lookup,
  mean_corr = NA,
  hdi_lower = NA,
  hdi_upper = NA
)
for (k in 1:num_continents) {
    G <- beta_g_k[, k]
    L <- beta_l_k[, k] 
    G_centered <- G - mean(G)
    L_centered <- L - mean(L)
  hdi_k <- hdi(beta_g_k[, k], prob = 0.95)
  # In this simplified approach we report correlation of slopes as proxy
  continent_corr_summary$mean_corr[k] <-
    mean(G_centered * L_centered) / (sd(G) * sd(L))
  continent_corr_summary$hdi_lower[k] <-
    hdi(G * L, prob = 0.95)[1] / (sd(G) * sd(L))
  continent_corr_summary$hdi_upper[k] <-
    hdi(G * L, prob = 0.95)[2] / (sd(G) * sd(L))
}
continent_corr_summary

ggplot(continent_corr_summary, aes(x = continent, y = mean_corr)) +
  geom_point(size = 3, color = "skyblue") +
  geom_errorbar(aes(ymin = hdi_lower, ymax = hdi_upper), width = 0.2) +
  labs(
    title = "Posterior continent-wise correlation between wealth and health",
    x = "Continent",
    y = "Correlation"
  ) +
  theme_minimal()


# now that I understand Q4 correctly (corr of log_gdp and score
# vs corr of life_expectancy and score), I proceed as follows:
# Compute posterior mean and 95% HDI for each continent

# global SD of happiness
sd_y_global <- sd(data$score)

# correlation = slope / global SD
rho_g_k <- beta_g_k / sd_y_global
rho_l_k <- beta_l_k / sd_y_global

# posterior mean and 95% HDI
summary_rho <- tibble(
  continent = continent_lookup,
  rho_g_mean = apply(rho_g_k, 2, mean),
  rho_g_hdi_lower = apply(rho_g_k, 2, function(x) hdi(x, 0.95)[1]),
  rho_g_hdi_upper = apply(rho_g_k, 2, function(x) hdi(x, 0.95)[2]),
  rho_l_mean = apply(rho_l_k, 2, mean),
  rho_l_hdi_lower = apply(rho_l_k, 2, function(x) hdi(x, 0.95)[1]),
  rho_l_hdi_upper = apply(rho_l_k, 2, function(x) hdi(x, 0.95)[2])
)
summary_rho

# reshape for plotting
plot_rho <- summary_rho %>%
  pivot_longer(
    cols = c(rho_g_mean, rho_l_mean),
    names_to = "predictor",
    values_to = "mean_corr"
  ) %>%
  mutate(
    lower = ifelse(predictor == "rho_g_mean", rho_g_hdi_lower, rho_l_hdi_lower),
    upper = ifelse(predictor == "rho_g_mean", rho_g_hdi_upper, rho_l_hdi_upper),
    predictor = recode(predictor, rho_g_mean = "Wealth (GDP)", rho_l_mean = "Health (Life)")
  )

g_corrs <- ggplot(plot_rho, aes(x = continent, y = mean_corr, color = predictor)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  labs(
    y = "Correlation (Pearson r)",
    x = "Continent"
  ) +
  theme_minimal(base_size = 13) +
  scale_color_manual(values = c("steelblue", "tomato")) +
    theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12)
  )
ggsave(
  filename = paste0(dir, "cont_corrs.pdf"),
  plot = g_corrs,
  width = 8,
  height = 6
)



# among the European countries, what is the rank of Slovenia in GDP?
europe_data <- data %>%
  filter(continent == "Europe") %>%
  distinct(country, log_gdp)
europe_data <- europe_data %>%
  arrange(desc(log_gdp))
slovenia_gdp_rank <- which(europe_data$country == "Slovenia")
cat(
  sprintf(
    "Slovenia's GDP rank in Europe: %d out of %d countries\n",
    slovenia_gdp_rank,
    nrow(europe_data)
  )
)

# among the European countries, where does Slovenia stand in GDP
# print out the maximum, mean, Slovenian and minimum GDP
cat(
  sprintf(
    "Europe GDP (log): max = %.2f, mean = %.2f, Slovenia = %.2f, min = %.2f\n",
    max(europe_data$log_gdp),
    mean(europe_data$log_gdp),
    europe_data$log_gdp[slovenia_gdp_rank],
    min(europe_data$log_gdp)
  )
)
