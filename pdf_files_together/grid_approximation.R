# libraries
library(ggplot2)

# set grid resolution
resolution <- 100

# create the grid
grid <- seq(from = 0, to = 1, length.out = resolution)

# prior
prior <- rep(1, resolution)
prior

# sequence 100100101011011001111111111101
n <- 30
z <- 20

# likelihood
likelihood <- dbinom(z, n, prob = grid)
likelihood

# posterior
posterior <- likelihood * prior

# standardize the posterior so it sums to 1
posterior <- posterior / sum(posterior)

# visualize
posterior
df <- data.frame(x = grid, y = posterior)
ggplot(data = df, aes(x = x, y = y)) +
  geom_bar(stat = "identity",
           color = "#67a9cf",
           fill = "#67a9cf",
           alpha = 0.5) +
  xlim(0, 1) +
  xlab("") +
  ylab("density") +
  theme_minimal()