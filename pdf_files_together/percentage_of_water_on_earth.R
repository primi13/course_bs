# library
# library(tidyverse)
library(ggplot2)
library(cowplot)

# x axis
x <- seq(0, 1, 0.1)

# water
water <- x * 2

# land
land <- 2 - water

# prior
prior <- rep(1, 11) / 11

# plots storage
plots <- list()

# plot our initial belief
df <- data.frame(x, prior)
p <- ggplot(df, aes(x, prior)) +
  geom_bar(stat = "identity") +
  xlab("Water percentage") +
  ylab("Belief") +
  ylim(0, 1)

# store
plots[[1]] <- p

# set posterior
our_belief <- NULL

saw_land <- function(our_belief, plots) {
  # calculate the posterior
  if (is.null(our_belief)) {
    posterior <- prior * land
  } else {
    posterior <- our_belief * land
  }
  posterior <- posterior / sum(posterior)
  
  # plot
  df <- data.frame(x, posterior)
  plot <- ggplot(df, aes(x, posterior)) +
    geom_bar(stat = "identity") +
    xlab("Water percentage") +
    ylab("Belief") +
    ylim(0, 1)
  
  return(list(posterior, plot))
}

saw_water <- function(our_belief, plots) {
  # calculate the posterior
  if (is.null(our_belief)) {
    posterior <- prior * water
  } else {
    posterior <- our_belief * water
  }
  posterior <- posterior / sum(posterior)
  
  # plot
  df <- data.frame(x, posterior)
  plot <- ggplot(df, aes(x, posterior)) +
    geom_bar(stat = "identity") +
    xlab("Water percentage") +
    ylab("Belief") +
    ylim(0, 1)
  
  return(list(posterior, plot))
}

# sample location from https://www.realrandom.net/location.html
# 11 samples gave us (1 - water, 0 - land)
y <- c(1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1)

for (observation in y) {
  if (observation == 1) {
    result <- saw_water(our_belief, plots)
  } else {
    result <- saw_land(our_belief, plots)
  }
  
  # update our belief and store the plot
  our_belief <- result[[1]]
  plots[[length(plots) + 1]] <- result[[2]]
}

plot_grid(plotlist = plots, ncol = 3, nrow = 4, scale = 0.9)