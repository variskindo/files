

## how to simulate random variables 

## from normal DISTRIBUTION with mean 2 and sd=0.4
data <- rnorm(10000, 2, .4)
hist(data, main = 'Simulated normal random variable')
data

## from beta distribution with parameters 5,5 
data <- rbeta (10000,5,5)
hist(data, main = 'simulated symmetric Beta random variable')
data


## from beta distribution with parameters 5,50 
data <- rbeta(10000,5,50)
hist(data, main ='simulated Right skewed beta random variable')
data

## from beta distribution with parameters 5,50 
data <- rbeta(10000,5,50)
hist(data, main ='simulated left skewed beta random variable')
data
