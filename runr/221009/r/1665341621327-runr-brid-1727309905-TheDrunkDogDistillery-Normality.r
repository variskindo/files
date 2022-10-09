# Read the data into R from a CSV file.

brandy <- read.csv("brandy.csv", header = T)
x <- brandy$Amount

# Calculate the sample size, sample mean
# and sample standard deviation.

(n <- length(x))
(x_mean <- mean(x))
(x_sd <- sd(x))

# Draw a histogram.

hist(x,
     main = "Histogram for the amount of brandy (ml) in 200 filled bottles",
     xlab = "Amount of brandy (ml)",
     xlim = c(695, 705),
     border = "saddlebrown", 
     col = "sandybrown")

# Draw a density histogram with the
# normal density curve overlaid.

hist(x,
     probability = TRUE,
     main = "Density histogram for the amount of brandy (ml) in 200 filled bottles",
     xlab = "Amount of brandy (ml)",
     xlim = c(695, 705),
     border = "saddlebrown", 
     col = "sandybrown")
curve(dnorm(x, mean = x_mean, sd = x_sd), 
      col = "brown",
      lwd = 1.5,
      add = TRUE,
      yaxt = "n")

# Draw a normal probability plot.

qqnorm(x,
       main = "Normal probability plot for the amount of brandy (ml) in 200 filled bottles",
       col = "sandybrown")
qqline(x,
       col = "brown")

# Test for normality using various hypothesis tests.

library(nortest)
library(normtest)

# Pearson's chi-square goodness-of-fit test:

pearson.test(x)

# Lilliefors (Kolmogorov-Smirnov) test:

lillie.test(x)

# Cramér-von Mises test:

cvm.test(x)

# Anderson-Darling test:

ad.test(x)

# Shapiro-Wilk test:

shapiro.test(x)

# Jarque-Bera test:

jb.norm.test(x)