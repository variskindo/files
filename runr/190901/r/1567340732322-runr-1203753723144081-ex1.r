
## From Basic Statistics book

setwd("C:/Users/admin/Desktop/EconStat")   ## load directory containing files

data <- read.csv("ex1.csv")          ## load data file to RStudio 
data                                 ## display elements  
attach(data)                         ## load variable names to memory

## create pie chart
pie(faculty)                         ## create pie chart with default colours
pie(faculty, labels = dept, main = "Piechart of Faculty by Department")

## create bar plot
barplot(faculty)
barplot(faculty, names.arg = dept, main = "Relative Frequency of Faculty by Department")

## get total number faculties
sum(faculty)

## get relative frequency
barplot(faculty/53, names.arg = dept, main = "Barplot of Faculty by Department")


