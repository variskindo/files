# Give the totals and frequencies from the
# contingency table.

n1 <- 40
n2 <- 40
n <- n1 + n2

yes1 <- 28
yes2 <- 20

no1 <- n1 - yes1
no2 <- n2 - yes2

# Calculate the point estimate for the
# difference in proportions.

(phat_food <- yes1 / n1)
(phat_love <- yes2 / n2)
(diff_phat <- phat_food - phat_love)

# Specify the two groups of cats based on
# the given totals for the type of reward.

(reward <- c(rep('food', n1), rep('love', n2)))

# Specify the outcomes based on the frequencies
# from the contingency table.
# Let 1 indicate yes and let 0 indicate no.

(outcomes <- c(rep(c(1, 0), c(yes1, no1)),
               rep(c(1, 0), c(yes2, no2))))

# Use the mean function to calculate the point
# estimate for the difference in proportions.

mean(outcomes[1:n1])
mean(outcomes[(n1+1):n])
mean(outcomes[1:n1]) - mean(outcomes[(n1+1):n])

# Specify the number of random samples to simulate.

nsim <- 10

# Set the seed for simulation.

set.seed(7)

# Create a matrix with n rows and nsim columns.

sim <- matrix(NA, nrow = n, ncol = nsim)

# Simulate nsim samples in which the outcomes
# are randomized.

for(i in 1:nsim){
  sim[,i] = sample(outcomes, n, replace = FALSE)
}

# Apply the mean function to the simulated samples to
# obtain the simulated differences in proportions.

sim_dist <- apply(sim[1:n1,], 2, mean) - 
            apply(sim[(n1+1):n,], 2, mean)

# Determine the frequencies (counts) for each
# difference in proportion obtained.

(freq <- table(sim_dist))

# Give a graphical representation for the
# simulated differences in proportions under
# the null hypothesis of independence.

barplot(freq,
        main = "Simulated differences in proportions \n under the null hypothesis of independence",
        xlab = "Simulated differences in proportions",
        ylab = "Count",
        col = 'salmon')

# Calculate a p-value for testing the null
# hypothesis of independence. 

(pvalue <- sum(sim_dist >= diff_phat) / nsim)