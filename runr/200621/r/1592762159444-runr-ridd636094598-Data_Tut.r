hgm <- c(20,19,29,28,23,14,21,24,22,15,25,13,27,28,18,24,25,25,28,28)
cgm <- c(13,13,22,19,15,8,14,15,13,11,18,8,21,21,11,17,19,18,19,19)
cyl <- c(8,8,4,6,6,16,8,8,10,12,4,128,4,4,12,6,4,6,6,4)

r = 1 # Number of regressors
n = 20 # Number of observations
mydata = data.frame(hgm=hgm,cgm=cgm,cyl=cyl)

######## SCATTER PLOTS ###################
par(mfrow=c(1, 2))
scatter.smooth(cyl,hgm)
scatter.smooth(cyl,cgm)

################ BOX PLOTS ####################
par(mfrow=c(1, 3))
boxplot(hgm,main="Highway Gas Mileage")
boxplot(cgm,main="City Gas Mileage")
boxplot(cyl,main="Number of cylinders")

############### SIMPLE CORRELATION COEFFICIENT #####
r_cyl_hgm = cor(cyl,hgm)
r_cyl_cgm = cor(cyl,cgm)

print("Correlation, r_cyl_hgm = "); print(r_cyl_hgm)
print("Correlation, r_cyl_cgm = "); print(r_cyl_cgm)

cyl_inv <- (1/cyl) 

model1 <- lm(formula=hgm~cyl_inv,data=mydata)
cylv <- seq(min(cyl),max(cyl),0.01)
hgmv <- predict(model1,list(cyl_inv=(1/cylv)))
lines(cylv,hgmv,col="red")

print(model1)
print(summary(model1))

model2 <- lm(formula=cgm~cyl_inv,data=mydata)
cylv <- seq(min(cyl),max(cyl),0.01)
cgmv <- predict(model2,list(cyl_inv=(1/cylv)))
lines(cylv,cgmv,col="blue")

print(model2)
print(summary(model2))

# Lets calculate the 95% simultaneous confidence interval
alpha = 0.05

#Simultaneous Confidence Intervals for model 1
CI_MIN_1 = 0*c(1:r+1)
CI_MAX_1 = 0*c(1:r+1)

#Extract parameters from the fitted object
coeff1 = coefficients(model1) # these are the beta_hats

# Extract the variance/covariance matrix of the model parameters
cov_coeff_1 = vcov(model1) #these are the var(beta_hat)
print("The covariance of the parameters of model 1 is var(beta_hat)")
print(cov_coeff_1)

for (i in c(1:(r+1))) {
  CI_MIN_1[i] = coeff1[i] - sqrt((r+1)*qf(1-alpha,r+1,n-r-1))*sqrt(cov_coeff_1[i,i])
  CI_MAX_1[i] = coeff1[i] + sqrt((r+1)*qf(1-alpha,r+1,n-r-1))*sqrt(cov_coeff_1[i,i])
}

print("Simultaneous 95% Confidence Interval for model 1") 
print(rbind(CI_MIN_1,CI_MAX_1))

#Simultaneous Confidence Intervals for model 2
CI_MIN_2 = 0*c(1:r+1)
CI_MAX_2 = 0*c(1:r+1)

#Extract parameters from the fitted object
coeff2 = coefficients(model2) # these are the beta_hats

# Extract the variance/covariance matrix of the model parameters
cov_coeff_2 = vcov(model2) #these are the var(beta_hat)
print("The covariance of the parameters of model 2 is var(beta_hat)")
print(cov_coeff_2)

for (i in c(1:(r+1))) {
  CI_MIN_2[i] = coeff2[i] - sqrt((r+1)*qf(1-alpha,r+1,n-r-1))*sqrt(cov_coeff_2[i,i])
  CI_MAX_2[i] = coeff2[i] + sqrt((r+1)*qf(1-alpha,r+1,n-r-1))*sqrt(cov_coeff_2[i,i])
}

print("Simultaneous 95% Confidence Interval for model 2") 
print(rbind(CI_MIN_2,CI_MAX_2))

# There is a great command in R to calculate the confidence intervals
print("Univariate 95% Confidence Interval for parameters for model 1")
print(confint(model1, level = 0.95))

# There is a great command in R to calculate the confidence intervals
print("Univariate 95% Confidence Interval for parameters for model 2")
print(confint(model2, level = 0.95))

