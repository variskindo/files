setwd("F:/ba")
getwd()

library(car)
library(caret)
library(cluster)
library(forecast)
library(TSA)
library(tseries)
library(gdata)
library(ggplot2)
library(randomForest)
library (rpart.plot)
library (rpart)
library(nortest)
library(car)
library(caret)
library(class)
library(devtools)
library(e1071)
#library(ggord)
library(ggplot2)
library(Hmisc)
library(sos)
library(MASS)
library(nnet)
library(plyr)
library(pROC)
library(psych)
library(scatterplot3d)
library(SDMTools)
library(dplyr)
library(ElemStatLearn)
library(rpart)
library(rpart.plot)
library(randomForest)
library(neuralnet)

library(ElemStatLearn)
library(SDMTools)
library(pROC)
library(Hmisc)

data<- read.csv(file.choose())
data<- na.omit(data)
View(data)
dim(data)
summary(data)
str(data)



levels(data$Gender)
data$Female<- ifelse(data$Gender=="Female",1,0)
data$TwoWheeler<-ifelse(data$Transport=="TwoWheeler",1,0)
data$Car<-ifelse(data$Transport=="Car",1,0)
data$PublicTransport<-ifelse(data$Transport=="PublicTransport",1,0)
str(data)

set.seed(1234)
pd<-sample(2,nrow(data),replace=TRUE, prob=c(0.7,0.3))
train<-data[pd==1,]
dim(train)
View(data)

val<-data[pd==2,]
dim(val)
View(data)

#shapiro.test(checkpoor$MPCE)
train<-train[,c(3,4,8,10:13)]
View(train)
val<-val[,c(3,4,8,10:13)]
str(train)


linear.data<-lm(Age~Gender+Engineer+MBA+WorkExp, data=data)
linear.data
summary(linear.data)


logit<-glm(TwoWheeler~Female+Engineer+MBA+license, data=data)
logit
summary(logit)
vif(logit)


logit_1<-glm(Car~Female+Engineer+MBA+license, data=data)
logit_1
summary(logit_1)
vif(logit_1)

logit_2<-glm(PublicTransport~Female+Engineer+MBA+license, data=data)
logit_2
summary(logit_2)
vif(logit_2)

varImp(logit_2)
pred.logit_2 <- predict.glm(logit_2, newdata=val, type="response")
pred.logit_2


#val.logit$pred<-predict.glm(logit.5.var, newdata=val.logit, type="response")

#Classification
  tab.logit_2<-table(val$PublicTransport,pred.logit_2>0.5)
tab.logit_2
#Logit
accuracy.logit_2<-sum(diag(tab.logit_2))/sum(tab.logit_2)
accuracy.logit_2


