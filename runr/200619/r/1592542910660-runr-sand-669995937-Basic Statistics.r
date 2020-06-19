2 + 2 # control + enter  or control + R
# works as calculator

#Packages
install.packages("readr")
library(readr)

install.packages()

# Read data into R
education <- read_csv("C:/Datasets_BA/360DigiTMG/DS_India/360DigiTMG DS India Module wise PPTs/Module 02 Data Types, SamplingExploratory Data Analytics (EDA), Descriptive Analytics/Data/education.csv")
education <- read.csv(file.choose()) # load csv file into R

# C:\Users\\education.csv - this is windows default file path with a '\'
# C:\\Users\\Desktop\\education.csv - change it to '\\' to make it work in R

View(education)

#Exploratory Data Analysis
#Measures of Central Tendency / First moment business decision

mean(education$workex) # '$' is used to refer to the variables within object
attach(education) # When used we can directly refer to the variable name

mean(gmat)
mean(workex)

rm(xyz) #Remove specific object to free RAM (memory)
rm(list=ls()) # Remove all to free RAM (memory)

median(workex) #Median

mode(workex)

#Mode
y <- c(19, 4, 5, 7, 29, 19, 29, 13, 25, 19)

Mode <- function(x){
     a = table(x) # x is a vector
     return(a[which.max(a)])
}

Mode(y)

# Measures of Dispersion / Second moment business decision
var(workex) # variance
sd(workex) #standard deviation
range <- max(workex) - min(workex) # range

install.packages("moments")
library(moments)

#Third moment business decision
skewness(workex)

#Fourth moment business decision
kurtosis(workex)

#Graphical Representation
barplot(gmat)
dotchart(gmat)

hist(gmat) #histogram

boxplot(gmat) #boxplot
y <- boxplot(gmat)
y$out # to see outliers

#Probability Distribution
install.packages("UsingR")
library("UsingR")
densityplot(gmat)

# Normal Quantile-Quantile Plot
qqnorm(gmat)
qqline(gmat)
qqnorm(workex)
qqline(workex)

#transformation to make workex variable normal
qqnorm(log(workex))
qqline(log(workex))

# z-distribution
pnorm(680,711,29) # given a value, find the probability

qnorm(0.025) # given probability, find the Z value
##

# t-distribution
pt(1.98, 139) # given a value, find the probability
qt(0.975, 139) # given probability, find the t value


