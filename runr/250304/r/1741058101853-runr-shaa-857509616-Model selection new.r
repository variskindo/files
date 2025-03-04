data<-matrix(c(78.5,7,26,6,60,
74.3,1,29,15,52,
104.3,11,56,8,20,
87.6,11,31,8,47,
95.9,7,52,6,33,
109.2,11,55,9,22,
102.7,3,71,17,6,
72.5,1,31,22,44,
93.1,2,54,18,22,
115.9,21,47,4,26,
83.8,1,40,23,34,
113.3,11,66,9,12,
109.4,10,68,8,12),13,5,byrow=T)
colnames(data)<-c('Y','X1','X2','X3','X4')
data<-data.frame(data)
RegOut<-lm(Y~X1+X2+X3+X4,data=data)
#install.packages('olsrr')
library(olsrr)
# All possible regression and its plots
k<-ols_step_all_possible(RegOut)
k
plot(k)
ols_step_best_subset(RegOut)
p<-ols_step_best_subset(RegOut)
plot(p)



#### FORWARD STEPWISE PROCEDURE #####################


# Correlation coefficient for the data
cor(data)
RegOut1<-lm(Y~X4,data=data)
summary(RegOut1)
# Partial correlation
#install.packages('ppcor')
library(ppcor)
# partial correlation for two variables given a third variable
pcor.test(data$Y,data$X1,data$X4)
pcor.test(data$Y,data$X2,data$X4)
pcor.test(data$Y,data$X3,data$X4)
RegOut2<-lm(Y~X4+X1,data=data)
summary(RegOut2)
# partial correlation for two variables given a set of variables
pcor.test(data$Y,data$X2,data[,c(5,2)])
pcor.test(data$Y,data$X3,data[,c(5,2)])
RegOut3<-lm(Y~X4+X1+X2,data=data)
summary(RegOut3)
RegOut4<-lm(Y~X1+X2+X3,data=data)
summary(RegOut4)
# Final model
RegOut5<-lm(Y~X1+X2,data=data)
summary(RegOut5)





###### END OF FORWARD STEPWISE PROCEDURE ############


#### FORWARD SLECTION PROCEDURE #####################


# Correlation coefficient for the data
cor(data)
RegOut1<-lm(Y~X4,data=data)
summary(RegOut1)
# Partial correlation
library(ppcor)
# partial correlation for two variables given a third variable
pcor.test(data$Y,data$X1,data$X4)
pcor.test(data$Y,data$X2,data$X4)
pcor.test(data$Y,data$X3,data$X4)
RegOut2<-lm(Y~X4+X1,data=data)
summary(RegOut2)
# partial correlation for two variables given a set of variables
pcor.test(data$Y,data$X2,data[,c(5,2)])
pcor.test(data$Y,data$X3,data[,c(5,2)])
RegOut3<-lm(Y~X4+X1+X2,data=data)
summary(RegOut3)
RegOut4<-lm(Y~X4+X1+X2+X3,data=data)
summary(RegOut4)

# As X3 is not significant so the final model is 
RegOut3<-lm(Y~X4+X1+X2,data=data)
summary(RegOut3)



###### END OF FORWARD SELECTION PROCEDURE ############


###### BACKWARD ELEMINATION PROCEDURE #################
 
RegOut1<-lm(Y~X1+X2+X3+X4,data=data)
summary(RegOut1)
RegOut2<-lm(Y~X1+X2+X4,data=data)
summary(RegOut2)
RegOut3<-lm(Y~X1+X2,data=data)
summary(RegOut3)



###### END OF BACKWARD ELEMINATION PROCEDURE ############




# A hospital surgical unit was interested in predecting survival
# in patients undergoing a particular type of liver operation. A 
# random selection of 54 patients was available for analysis. From 
# each patient record, the following information was extracted from 
# the preoperational evaluation:
# X1= Blood clotting score, X2= prognostic index, which includes the
# age of patient, X3= enzyme function test score, X4= liver function
# test score, Y= survival time.
# Since the survival time distribution is substantially skewed to the 
# right, the logarithm of the survival time logy<-log(y) was taken
# as the dependent variable.
# load surgical data in olsrr
library(olsrr)
data(surgical)
da<-surgical
da[,9]<-log(da[,9])
data<-da[,c(1,2,3,4,9)]
colnames(data)<-c('X1','X2','X3','X4','logY')
data<-data.frame(data)

# Correlation coefficient for the data
cor(data)
RegOut1<-lm(logY~X3,data=data)
summary(RegOut1)




# Partial correlation
library(ppcor)
# partial correlations for each pair of variables given others
pcor(data) 
# partial correlation for two variables given a third variable
pcor.test(data$logY,data$X1,data$X3)
pcor.test(data$logY,data$X2,data$X3)
pcor.test(data$logY,data$X4,data$X3)

RegOut2<-lm(logY~X3+X2,data=data)
summary(RegOut2)

# partial correlation for two variables given a set of variables
pcor.test(data$logY,data$X1,data[,c(3,2)])
pcor.test(data$logY,data$X4,data[,c(3,2)])


RegOut3<-lm(logY~X3+X2+X1,data=data)
summary(RegOut3)

RegOut4<-lm(logY~X3+X2+X1+X4,data=data)
summary(RegOut4)

#Therefore the final model by forward stepwise selection procedure 
#is RegOut3


### Backward elemination procedure
RegOut1<-lm(logY~X1+X2+X3+X4,data=data)
summary(RegOut1)
RegOut2<-lm(logY~X1+X2+X3,data=data)
summary(RegOut2)



#checking for normality of reisduals
resi<-residuals(RegOut3)
qqnorm(resi)
qqline(resi)
hist(resi)

#checking for constant variance
yhat<-fitted.values(RegOut3)
plot(yhat, resi)
abline(h=0)


