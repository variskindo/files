#Logistic Regression
#PimaIndiansDiabetes dataset available in package mlbench
install.packages("mlbench")
library(mlbench)
data(PimaIndiansDiabetes)
str(PimaIndiansDiabetes)

PID <- PimaIndiansDiabetes
head(PID)

#Ensure no NA values exist in any coloumns, if exists they should be removed

table(complete.cases(PID))

#install caret package(Classification and  Training)
#contains functions to create predictive models like data splitting, preprocessing etc.

install.packages("caret")
library(caret)
# Create two partitions 70% and 30% of the dataset
set.seed(900)
partition <- createDataPartition(y=PID$diabetes, p = 0.7, list = FALSE)

trainingdata<-PID[partition, ]
testdata <- PID[-partition, ]

#Create a logisti model
 pidmodel <- glm(formula = diabetes~., data = trainingdata, family = binomial())
 
 #Display the summary of the model
 summary(pidmodel)
 
 #for determining the accuracy of the model- confusion matrix is implemented
 #its a crosstab of actual vs predicted value
 
 #Use predict function with test data
 anspredict <-predict(pidmodel, newdata= testdata, type = "response")
 
 #display the details
 summary(anspredict)
#Minimum value is zerp and maximum value is 0.97, predicted value lies between 0 and 1
#We have to convert these predicted values(stored in anspredict) into two groups neg and pos
#We will set the threshold level at 50%
 
  # converting the value
 convert <- ifelse(anspredict < 0.5, "neg", "pos")
 table(convert)
 #shows that 168 neg and 68 pos values are there in convert
 
 
#Display the confusion matrix
newPid <- data.frame(predicted = convert,actual = testdata$diabetes)
#newPid has two cols, predicted and actuals

result <- confusionMatrix(factor(newPid$actual), factor(newPid$predicted))
#The above command compares values of actual and predicted of each and every record
#of new PID and stores in result 

#The confusion matrix shows original data had 150 records  in which
##Actual diabetes neg is 150(134+16)  and pos is 80(39 + 41) out of which
#134 neg is predicted correctly and 41pos is predicted correctly
#16 values predicte neg are actually pos, 39 values predicted pos are actually neg

#The output accuracy of 0.7609 shows that the model is 76.09%  accurate
#The kappa value( inter-rater agreement) of 0.4439 shows that the model 
#has moderate agreement

#Kappa Statistics
#0 = agreement equivalent to chance
#<0 No agreement
#0 - .20 Slight
#.21 - .40 Fair
#.41 - .60 Moderate
#.61 - .80 Substantial
#.81-1.0 Perfect

#The  pidmodel shows that age, pressure and triceps are insignificant 
#so make a model with only significant var 

newmodel<- glm(formula = diabetes~pregnant + glucose+mass+pedigree+insulin,
               data = trainingdata, family = binomial())

summary(newmodel)

#Predicting the values from test sample
ansnew <- predict(newmodel, newdata = testdata, type = "response")
summary(ansnew)

#Convertt the predicted values
convertnew <- ifelse(ansnew<0.5, "neg", "pos")
table(convertnew)

#Using Confusion matrix for comparision
newPid2 <- data.frame(predicted = convertnew, actual = testdata$diabetes)
newresult <- confusionMatrix(factor(newPid2$actual), factor(newPid2$predicted))
newresult
#The results show that kappa is 0.4033, and accuracy is 75.22%
#There is no  improvement in the model

#We can use chisq test using anova() function to determine whether
#there is significant difference in the models

#Using anova()
#Ho: There is no significant difference between  the two models
anova(pidmodel, newmodel, test = "Chisq")

#Interpretation
#Check the p value to state whether there is significant difference in the two models
