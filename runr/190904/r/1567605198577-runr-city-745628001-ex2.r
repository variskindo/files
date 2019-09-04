
## From Basic Statistics book

setwd("C:/Users/admin/Desktop/EconStat")   ## load directory containing files

data <- read.csv("ex2.csv")          ## load data file to RStudio 
data                                 ## display elements  
attach(data)                         ## load variable names to memory


## create histogram
hist(y, main = "Histogram of anxiety scores")

## obtain their stem-and-leaf plot
stem(y, scale = .5)

## get median, mean value
sort(y)
length(y)
median(y)

mean(y)
mean(y, trim=5)


## compute the variance and sd
var(y)
sd(y)
Range = max(y)- min(y)


## Compute quartiles Q1, Q2, Q3
quantile(y)
quantile(y)[2]    ## this gives us the first quartile

summary(y)


## create boxplot
boxplot(y, main = "Boxplot of Anxiety Scores", ylab = "Anxiety Score")



boxplot(y~g, main = "Boxplot of College Aspiration Scores by Gender",
        ylab = "College Aspiration Score", xlab = "Gender: Boys = 0, Girls = 1")

















## get relative frequency


