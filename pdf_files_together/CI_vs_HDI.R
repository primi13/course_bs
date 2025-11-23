# libraries
library(ggplot2)

# precision/number of samples
n <- 10000

# arbitrary gamma parameters
alpha <- 5
beta <- 50

# generate some random gamma numbers
x <- rgamma(n, alpha, beta)

# let us calculate 90% HDI
cred_mass <- 0.9

# sort our samples
x <- sort.int(x, method = "quick")
# cat("x[1:10]: ", x[1:10], "\n\n")

# number of total values to exclude (10% of all data)
exclude <- n - floor(n * cred_mass)
# cat("n: ", n, "\n\n")
# cat("n * cred_mass: ", n * cred_mass, "\n\n")
# cat("floor(n *cred_mass): ", floor(n *cred_mass), "\n\n")
# cat("exclude: ", exclude, "\n\n")

# in the most extreme cases we exclude everything (10% of data in our case)
# either at the bottom or in the upper part of the distribution
# these are our bounds
upper_bound <- x[(n - exclude + 1):n]
lower_bound <- x[1:exclude]
# cat("upper_bound[1:10]: ", upper_bound[1:10], "\n\n")
# cat("lower_bound[1:10]: ", lower_bound[1:10], "\n\n")

# in order to discard 10% of data we need to use a pair from the bounds vectors
# find the lower/upper pair with minimum distance between them
# smaller the difference higher the density of that chunk
# this is then our solution
best <- which.min(upper_bound - lower_bound)

# extract the best pair
result <- c(lower_bound[best], upper_bound[best])

# 90% confidence interval
q5 <- quantile(x, 0.05)
q95 <- quantile(x, 0.95)

# report
cat(paste0("90% HDI: [", round(result[1], 3), ", ", round(result[2], 3), "]"), "\n")
cat(paste0("90% CI: [", round(q5, 3), ", ", round(q95, 3), "]"))

# plot
df <- data.frame(x=x)
ggplot(data = df) +
  geom_density(aes(x=x), color=NA, fill="skyblue", alpha=0.75) +
  xlab("difference") +
  geom_vline(xintercept = q5,
             linetype = "dashed",
             color = "grey80",
             linewidth = 1) +
  geom_vline(xintercept = q95,
             linetype = "dashed",
             color = "grey80",
             linewidth = 1) +
  geom_vline(xintercept = result[1], color = "grey40", linewidth = 1) +
  geom_vline(xintercept = result[2], color = "grey40", linewidth = 1)
