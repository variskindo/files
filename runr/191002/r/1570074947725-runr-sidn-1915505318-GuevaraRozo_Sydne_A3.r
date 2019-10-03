####################################################
# Content:Assignment 3 
# Date: October 2, 2019 - change date as needed                          
# Author: Sydne Guevara Rozo  
####################################################


#######################################################################
## Delete any objects left over from former runs using the rm() 
##    command (next) in the script.
#######################################################################
rm(list=ls(all=TRUE))

##########################################################################
## First bring the data into R. A3data.csv can be read into R using
##      read.csv() or read.table
##########################################################################
### 1)
#########################################
A3data <- read.csv("A3data.csv",
         header = TRUE, sep = ",")

##########################################################################
## Get some basic information about the data.
## Graph the data and overlay the fitted simple linear model.
##########################################################################

# basic information about data, e.g. mean, range, etc.
summary(A3data)
dim(A3data)   # how many rows and columns?
names(A3data) # variable names in data set

# Create basic scatterplot matrix
pairs(~ mwat + temp + elev + glacier + lake + wbf + area, 
      data=A3data, main="Scatterplot Matrix for MWAT data")

# create some variable transformations
A3data$ln_area <- log(A3data$area)
A3data$sqrt_glacier <- sqrt(A3data$glacier)
A3data$sqrt_lake <- sqrt(A3data$lake)



# add in other ones you want to try out here:
sq_area <- log(A3data$area)^2 # Due to SD are curved transformation is done using X^2
ln_glacier <- log(A3data$glacier)
sqrt_wbf <- sqrt(A3data$wbf)
sqrt_lake <- sqrt(A3data$lake)



# add in additional scatterplot matrices
pairs(~ mwat + temp + elev + ln_glacier + sqrt_lake + sqrt_wbf +sq_area, 
      data=A3data, main="Scatterplot Matrix for MWAT data")

##########################################################################
## 2) & 3)
##########################################################################

###########################################
# Model 1: example model using area, elev and sqrt_lake as X-variables --> change as you see fit!
model.1 <- lm(mwat ~  area + elev + sqrt_lake, data=A3data) # y first, then x's.

# summary() and anova() to begin with
summary(model.1)  # gives the slope, intercept, R^2, Adjusted-R^2, etc.
anova(model.1)   # gives analysis of variance table

###### Store the yhats and residuals
# Store the predicted values as yhat.1, the raw residuals
# (observed - predicted) as resid.3, and the standardized
# residuals as stdresid (resid divided by root MSE).
A3data$yhat.1 <- fitted(model.1)  
A3data$resid.1 <- resid(model.1) 
A3data$stdresid.1 <- resid(model.1)/summary(model.1)$sigma 


###### Plot yhat versus observed y.
# Fitted line plot
# Plot the predicted y versus the observed y.  How similar are they?
plot(yhat.1 ~ mwat, data=A3data, xlab="Observed MWAT", 
     ylab="Predicted MWAT", main="Predicted vs. observed MWAT",
     pch=19)
abline(a=0,b=1,col="red")   # plot a reference line 
# a is the intercept, b is the slope

# Residual plot
# How big are the errors (residuals) as you go across the yhats?
# 1. Do the + values balance the - values for each range (small,
# medium, large) yhat values? (No lack of fit?)
plot(resid.1 ~ yhat.1, data = A3data, xlab="Predicted MWAT", 
     ylab="Residuals",main="Residual Plot",pch=19)
abline(a=0,b=0,col="red")


###########################################
# Model 2:

###########################################
# Model 3:

###########################################
# Model 4:


# select 2 of the above 4 models and proceed with the following analysis
# only for these two models!

##########################################################################
## 4)
##########################################################################

######################################################################
# steps a) through f) for first model that you picked
######################################################################

# a)
###### Diagnostic plots to check assumptions.
# Residual plot
# 2. Is the spread of the errors the same for all yhats? (Equal variances?)
plot(resid.1 ~ yhat.1, data = A3data, xlab="Predicted MWAT", 
     ylab="Residuals",main="Residual Plot",pch=19)
abline(a=0,b=0,col="red")

# Normality plot,normality histogram and normality test.
# First a normality (QQ) plot to see if the
# standardized residuals follow a normal distribution
qqnorm(A3data$stdresid.1, pch=19)   # normality ("QQ") plot
qqline(A3data$stdresid.1,col=2)

# Check normality again but using a histogram.  A bit difficult 
# since there is so little data. The function hist() is used.
hist(A3data$stdresid.1, freq=T,          
     breaks = 6, density=10, xlab="Residuals", ylab="Frequency",
     main="Histogram of Residuals", col="green", border="black")

# Also do a normality test
shapiro.test(A3data$stdresid.1) # Shapiro-Wilk normality test

# b - e) find information in summary output
summary(model.1)


# f)
###### Get 95% confidence intervals of mean volha given the provided X values
###### for a new X not in the original dataset.

ln_areanew <- log(12)
sqrt_lakenew <- sqrt(5)
sqrt_glaciernew <- sqrt(30)
new <- data.frame(ln_area=ln_areanew, sqrt_lake=sqrt_lakenew, sqrt_glacier=sqrt_glaciernew) 
new

### NOTE: You will need to modify the above four lines for the X variables that you used!


# 95% confidence and prediction interval for mean y given the new x.
# Remember to change to whichever model you used.
predCI.new <- predict(lm(mwat ~ ln_area + sqrt_lake + sqrt_glacier, data=A3data), new, interval="confidence",level=0.95) 
predCI.new


######################################################################
# steps a) through f) for second model that you picked
######################################################################
# copy code from above and modify as needed



##############################################################################
#  Clean up your files, etc.
##############################################################################
ls() 
rm(list=ls(all=TRUE))     



