sqrt(4)

log(10) 
# log(100) with base of 10 

log(2.718282)
2.718282 ^ 2.302585
exp(1)

X <- 10

X <- c(1, 3, 5, 4, 6, 7, 8, 9, 10) # data on X

# length function provides 'n'
nx <- length(X) # stored length(X) in abject called nx

nx

Y <- c(2,3,4,3,5,7,6,7,9)
length(Y)

meanx <- sum(X)/ nx

mean(X)

# calculated SD

sqrt((sum ((X-mean(X))^2)) / (nx-1))

sd(X)

library(psych)

describe(X)
xx <- rnorm(1000)
hist(rnorm(1000), breaks = 20)

plot(density(xx))

plot(X,Y, pch = 16, col = "yellow")


abline(Y~X)


cor.test(X,Y)


install.packages("ctv")

library(ctv)

install.views("Psychometrics")








