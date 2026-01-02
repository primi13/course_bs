library(cmdstanr) # for interfacing Stan
library(ggplot2) # for visualizations
library(posterior) # for extracting samples
library(bayesplot) # for some quick MCMC visualizations
library(tidyverse) # for data manipulations
library(psych) # for independent variables correlation plot

# compile and load
model <- cmdstan_model("./homeworks/homework1/linear.stan")
df <- read.csv("./homeworks/homework1/gapminder_life_expectancy.csv",
               stringsAsFactors = TRUE)
# class(df$continent) # it is a factor
# head(df)
# str(df)


# log transform right skewed PDFs and center/standardize predictors
df <- df %>%
  mutate(
    log_gdp = log(gdp_per_capita + 1), # +1 to avoid log(0)
    log_pop = log(population + 1),
    # center numeric predictors for stability
    year_s = scale(year, center = TRUE, scale = sd(year)), # standardized
    log_gdp_s = as.numeric(scale(log_gdp)),
    log_pop_s = as.numeric(scale(log_pop)),
    continent = factor(continent)
  )
# head(df)


# check correlations between independent numerical variables
# they are fine (< 0.25)
# population and gdp_per_capita are now approximately
# normally distributed after log-transform (nice)
pairs.panels(df %>% dplyr::select(year_s, log_gdp_s, log_pop_s),
             method = "pearson",
             hist.col = "lightblue",
             density = TRUE,
             ellipses = FALSE)

# categorical variables to dummies (but without intercept)
X_full <- model.matrix(~ year_s + log_gdp_s + log_pop_s + continent, data = df)
X <- X_full[, -1, drop = FALSE]   # remove intercept column, keep dummies
# X <- dplyr::select(df, year_s, log_gdp_s, log_pop_s, continent)

stan_data <- list(
  n = nrow(df),
  m = ncol(X),
  X = X,
  y = df$life_expectancy
)


# fit
fit <- model$sample(
  data = stan_data,
  seed = 13,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  refresh = 500
)


# diagnostics ------------------------------------------------------------------
# traceplot (everything looks OK)
mcmc_trace(fit$draws())

# summary (everything looks OK)
fit$summary()


samples_of_params <- as_draws_df(fit$draws()) %>%
  dplyr::select(-.chain, - .iteration, - .draw)
colnames(samples_of_params)[3:(3+ncol(X)-1)] <- colnames(X)
samples_of_params[1:10, 5:10]


# 1. Question
mcse(samples_of_params$year_s > 0) # mean = 1, se = 0, answer: YES
mcse(samples_of_params$year_s)

# 2. Question
# from the summary output:
    # After controlling for year and continent, a 1 SD increase in log 
    # GDP per capita is associated with ~6.4 years higher life expectancy.

    # A 1 SD increase in log population is associated with ~0.3 years 
    # higher life expectancy.

# or just raw correlation with life expectancy:
mcse(samples_of_params$log_gdp_s)
mcse(samples_of_params$log_pop_s)
cor(df$log_gdp, df$life_expectancy)
cor(df$log_pop, df$life_expectancy)

# 3. Question (Estimate how long you are expected to live given your 
# birth year and the fact that you live in Europe.)
my_birth_year <- 2002
my_year_s <- (my_birth_year - mean(df$year)) / sd(df$year)
my_input <- c(1, # intercept
    my_year_s, # year_s
    0, # log_gdp_s
    0, # log_pop_s
    0, # continentAmericas
    0, # continentAsia
    1, # continentEurope
    0  # continentOceania
)
draws_df <- samples_of_params %>%
  dplyr::select(a, year_s, log_gdp_s, log_pop_s,
         continentAmericas, continentAsia,
         continentEurope, continentOceania)
draws_mat <- as.matrix(draws_df)
my_life_expectancy_samples <- draws_mat %*% my_input
mcse(my_life_expectancy_samples)
quantile(my_life_expectancy_samples, probs = c(0.025, 0.5, 0.975))


# 4. Question (What is the probability that an average European with
# the same birth year as you will live longer than their average
# counterparts on other continents?)

# avg input for each continent with my birth year (2002)
pred_mean_Africa   <- draws_mat %*% c(1, my_year_s, 0, 0, 0, 0, 0, 0)
pred_mean_Americas <- draws_mat %*% c(1, my_year_s, 0, 0, 1, 0, 0, 0)
pred_mean_Asia     <- draws_mat %*% c(1, my_year_s, 0, 0, 0, 1, 0, 0)
pred_mean_Europe <- draws_mat %*% c(1, my_year_s, 0, 0, 0, 0, 1, 0)
pred_mean_Oceania  <- draws_mat %*% c(1, my_year_s, 0, 0, 0, 0, 0, 1)
longer_then_mean_Americas <- pred_mean_Europe > pred_mean_Americas
longer_then_mean_Asia     <- pred_mean_Europe > pred_mean_Asia
longer_then_mean_Oceania  <- pred_mean_Europe > pred_mean_Oceania
longer_then_mean_Africa   <- pred_mean_Europe > pred_mean_Africa
longer_then_mean_else <- mcse(c(
  longer_then_mean_Americas,
  longer_then_mean_Asia,
  longer_then_mean_Oceania,
  longer_then_mean_Africa
))
longer_then_mean_else

# # 5. Question (What is the probability that an individual European
# # with the same birth year as you will live longer than an
# # individual from another continent?)

# introduce uncertainty by adding noise from normal distribution
# with sd = sigma (residual sd from the model)
draws_with_sigma_df <- samples_of_params %>%
  dplyr::select(a, year_s, log_gdp_s, log_pop_s,
         continentAmericas, continentAsia,
         continentEurope, continentOceania, sigma)
draws_with_sigma_mat <- as.matrix(draws_with_sigma_df)
n_draws <- nrow(draws_with_sigma_mat)

sigma_draws <- as.numeric(draws_with_sigma_mat[, "sigma"])
eps_Af <- rnorm(n_draws) * sigma_draws
eps_Am <- rnorm(n_draws) * sigma_draws
eps_As <- rnorm(n_draws) * sigma_draws
eps_E <- rnorm(n_draws) * sigma_draws
eps_Oc <- rnorm(n_draws) * sigma_draws

y_Africa   <- pred_mean_Africa   + eps_Af
y_Americas <- pred_mean_Americas + eps_Am
y_Asia     <- pred_mean_Asia     + eps_As
y_Europe   <- pred_mean_Europe   + eps_E
y_Oceania  <- pred_mean_Oceania  + eps_Oc

longer_then_else <- c(
  y_Europe > y_Africa,
  y_Europe > y_Americas,
  y_Europe > y_Asia,
  y_Europe > y_Oceania
)
mcse(longer_then_else)



# 6. Question (Quantify how much longer (or shorter)
# your life expectancy is, compared to your professor
# (year of birth 1985).)

prof_birth_year <- 1985
prof_year_s <- (prof_birth_year - mean(df$year)) / sd(df$year)
prof_input <- c(1, prof_year_s, 0, 0, 0, 0, 1, 0)
prof_life_expectancy_samples <- draws_mat %*% prof_input

life_expectancy_diff_samples <-
  my_life_expectancy_samples - prof_life_expectancy_samples
mcse(life_expectancy_diff_samples)
quantile(life_expectancy_diff_samples, probs = c(0.025, 0.5, 0.975))
