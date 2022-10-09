# Give the sample information.

n <- 625
yes <- 75
no <- n - yes

# Calculate the sample proportion (point estimate)
# using the given sample information.

(phat <- yes / n)

# Specify the responses of the students.
# Let 1 indicate yes and let 0 indicate no.

(responses <- c(rep(c(1, 0), c(yes, no))))

# Use the mean function to calculate the
# sample proportion (point estimate).

mean(responses)

# Specify the number of bootstrap samples to simulate.

nsim <- 10000

# Set the seed for simulation.

set.seed(13)

# Create a matrix with n rows and nsim columns.

sim <- matrix(NA, nrow = n, ncol = nsim)

# Simulate nsim bootstrap samples.

for(i in 1:nsim){
  sim[,i] = sample(responses, n, replace = TRUE)
}

# Apply the mean function to the bootstrap samples
# to obtain the bootstrapped proportions.

phat_boot <- apply(sim, 2, mean)

# Determine the frequencies (counts) for the
# bootstrapped proportions.

(freq <- table(phat_boot))

# Give a graphical representation for the
# bootstrapped proportions.

barplot(freq,
        main = "Bootstrapped proportions of Tuks students \n interested in joining the ice skating club",
        xlab = "Bootstrapped proportions",
        ylab = "Count",
        col = 'lawngreen')

# Calculate a 90% bootstrap percentile confidence
# interval for the population proportion.

(lower <- sort(phat_boot)[500])
(upper <- sort(phat_boot)[9500])

# Use the quantile function to calculate the 
# 90% bootstrap percentile confidence interval.

(lower90 <- quantile(phat_boot, 0.05))
(upper90 <- quantile(phat_boot, 0.95))

# Use the quantile function to calculate a 
# 95% bootstrap percentile confidence interval.

(lower95 <- quantile(phat_boot, 0.025))
(upper95 <- quantile(phat_boot, 0.975))