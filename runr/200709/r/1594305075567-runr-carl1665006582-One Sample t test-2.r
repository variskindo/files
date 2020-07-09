## One Sample t test




Input = ("
Instructor       Student  Sodium
'MSU Teacher'  a        1200
'MSU Teacher'  b        1400
'MSU Teacher'  c        1350
'MSU Teacher'  d         950
'MSU Teacher'  e        1400
'MSU Teacher'  f        1150
'MSU Teacher'  g        1300
'MSU Teacher'  h        1325
'MSU Teacher'  i        1425
'MSU Teacher'  j        1500
'MSU Teacher'  k        1250
'MSU Teacher'  l        1150
'MSU Teacher'  m         950
'MSU Teacher'  n        1150
'MSU Teacher'  o        1600
'MSU Teacher'  p        1300
'MSU Teacher'  q        1050
'MSU Teacher'  r        1300
'MSU Teacher'  s        1700
'MSU Teacher'  t        1300
")

Data = read.table(textConnection(Input),header=TRUE)

###  Check the data frame
library(psych)
headTail(Data)
str(Data)
summary(Data)

# Histogram of data
# A histogram of the data can be examined to determine
# if the data are sufficiently normal.

x = Data$Sodium
library(rcompanion)
plotNormalHistogram(x)
hist(x)
                    
# Normal quantile plot of data
x = Data$Sodium 
qqnorm(x)
qqline(x, col="red")


# One-sample t-test
t.test(Data$Sodium,
       mu       = 1500,
       conf.int = 0.95)


