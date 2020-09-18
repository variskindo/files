source("R2BootTest2.R")

library(readxl)
Quality <- read_excel("Quality.xls")


################################### ASL estimation from observed data ############################################

X <- Quality$X
Ym <- Quality$Ym
gYl <- log(Quality$Yl)
hYr <- log(Quality$Yr)

n <- length(X)
m <- 10000
R <- 1000

# Testing statistic computation from observed data
Tn_ref <- Tn(cbind(X, Ym, gYl, hYr), 1:n)
print(Tn_ref)


# Regressions
Ym_hat <- lm(Ym ~ X)
gYl_hat <- lm(gYl ~ X)
hYr_hat <- lm(hYr ~ X)  

Zm <- Ym - Ym_hat$coefficients[2]*X
Zl <- gYl - gYl_hat$coefficients[2]*X
Zr <- hYr - hYr_hat$coefficients[2]*X

# Bootstrap initialization
boot.data <- cbind(X, Zm, Zl, Zr)
boot.res <- numeric(R)
p <- numeric(m)


for (j in 1:m) {
  
  # Bootstrap procedure
  for(i in 1:R){
    
    boot.ind <- sample(1:n, n, replace = T)
    boot.res[i] <- Tn(boot.data, boot.ind)
    
    # Printing some testing statistics to check if everything works fine
    # bbb <- seq(1, R, by = R/100)
    # if(i %in% bbb){
    #   print(boot.res[i])
    # }
    
  }
  
  # Evaluating the j-th p value for each iteration
  p[j] <- mean(boot.res > Tn_ref)
  
  # # Printing some pvalues to check if everything works fine
  # aaa <- seq(1, m, by = m/1000)
  # if(j %in% aaa){
  #   print(p[j])
  # }
  
}
mean(p)


###################################### Simulation experiment ############################################################

n <- 50
m <- 10000
R <- 1000
alpha <- 0.05


for (j in 1:m) {
  
  X <- rnorm(n, 0, 1)
  
  Ym <- rnorm(n, 0, 1)
  gYl <- rnorm(n, 0, 1)
  hYr <- rnorm(n, 0, 1)
  
  # Testing statistic computation from simulated data
  Tn_ref <- Tn(cbind(X, Ym, gYl, hYr), 1:n)
  # print(Tn_ref)
  
  # Regressions
  Ym_hat <- lm(Ym ~ X)
  gYl_hat <- lm(gYl ~ X)
  hYr_hat <- lm(hYr ~ X)  
  
  Zm <- Ym - Ym_hat$coefficients[2]*X
  Zl <- gYl - gYl_hat$coefficients[2]*X
  Zr <- hYr - hYr_hat$coefficients[2]*X
  
  # Bootstrap initialization
  boot.data <- cbind(X, Zm, Zl, Zr)
  boot.res <- numeric(R)
  p <- numeric(m)
  
  # Bootstrap procedure
  for(i in 1:R){
    
    boot.ind <- sample(1:n, n, replace = T)
    boot.res[i] <- Tn(boot.data, boot.ind)
    
    # Printing some testing statistics to check if everything works fine
    # bbb <- seq(1, R, by = R/100)
    # if(i %in% bbb){
    #   print(boot.res[i])
    # }
    
  }
  
  # Evaluating the j-th p value for each iteration
  p[j] <- mean(boot.res > Tn_ref)
  
  # # Printing some pvalues to check if everything works fine
  # aaa <- seq(1, m, by = m/1000)
  # if(j %in% aaa){
  #   print(p[j])
  # }
  
}

# The probability of rejection is the proportion of p less than some alpha fixed
prob_rejection_hat <- mean(p < alpha)
prob_rejection_hat
  

#################################### Test's power estimation ###########################################################

n <- 200
m <- 1000
R <- 1000
alpha <- 0.05

# Generating Pitman's hypotesis
nr.hn <- 10 # number of Pitman's hypotesis
delta <- seq(from = 0, to = 0.05, length.out = nr.hn)
an <- (n)^0.5 * delta # an goes from 0 to 0.2 when n = 200

for (h in 1:nr.hn) {
  
  for (j in 1:m) {
    
    X <- rnorm(n, 0, 1)
    
    # Data under Alternative Hypotesis
    Ym <- X*an[h] + rnorm(n, 0, 1)
    gYl <- X*an[h] + rnorm(n, 0, 1)
    hYr <- X*an[h] + rnorm(n, 0, 1)
    
    Tn_ref <- Tn(cbind(X, Ym, gYl, hYr), 1:n)
    # print(Tn_ref)
    
    # Regressions
    Ym_hat <- lm(Ym ~ X)
    gYl_hat <- lm(gYl ~ X)
    hYr_hat <- lm(hYr ~ X)  
    
    Zm <- Ym - Ym_hat$coefficients[2]*X
    Zl <- gYl - gYl_hat$coefficients[2]*X
    Zr <- hYr - hYr_hat$coefficients[2]*X
    
    
    # Bootstrap initialization
    boot.data <- cbind(X, Zm, Zl, Zr)
    boot.res <- numeric(R)
    p <- numeric(m)
    
    # Bootstrap procedure
    for(i in 1:R){
      
      boot.ind <- sample(1:n, n, replace = T)
      boot.res[i] <- Tn(boot.data, boot.ind)
      
      # Printing some testing statistics to check if everything works fine
      # bbb <- seq(1, R, by = R/100)
      # if(i %in% bbb){
      #   print(boot.res[i])
      # }
      
    }
    
    # Evaluating the j-th p value for each iteration
    p[j] <- mean(boot.res > Tn_ref)
    
    # # Printing some pvalues to check if everything works fine
    # aaa <- seq(1, m, by = m/1000)
    # if(j %in% aaa){
    #   print(p[j])
    # }
    
  }
  
  # The probability of rejection is the proportion of p less than some alpha fixed
  prob_rejection_hat <- mean(p < alpha)
  pow[h] <- prob_rejection_hat
}

plot(x = an, y = pow)

