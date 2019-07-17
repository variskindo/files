####Understanding R - Day1#######

ls()
rm(list = ls())
help(rm)
x <- 1:4
y <- 3:10
rm(x)
getwd()
setwd("D:\\Week1")
history()
save.image()

###Check R version & update R####
version
install.packages("installr")
require(installr)
library(installr)

if(!require(installr)) {
  install.packages("installr");require(installr)
}
updateR()

###Installing and using packages#######
install.packages('reshape2')
library(reshape2)
update.packages()

# The basic arithmetic operator
# Addition
3 + 7
# Substraction
7 - 3
# Multiplication
3 * 7
# Divison
7/3
# Exponentiation
2^3
# Modulo: returns the remainder of the division of 8/3
8 %% 3

log2(x) # logarithms base 2 of x
log10(x) # logaritms base 10 of x
exp(x) # Exponential of x

cos(x) # Cosine of x
sin(x) # Sine of x
tan(x) #Tangent of x

abs(x) # absolute value of x
sqrt(x) # square root of x

#####Data Types######
## Numeric
x <- c(1,2,3,4.5,6,8.0)
class(x)

###Integer###
x <- c(1:10)
class(x)
is.integer(x)

# Logical
is_it <- TRUE
class(is_it)

###character###
x <- c("a","B","g")
is.character(x)

##Factor
x <- factor(c(1,1,1,2,3,3,4,5))
class(x)
is.factor(x)
levels(x)

age <- c(34,35,36)
is.numeric(age)
is.character(age)
is.factor(age)
is.logical(age)
age <- as.factor(age)

####Assignment operator - "<-" & "="
median(x = 1:10)
x
median(x <- 1:10)
x

# Factors
apple_colors <- c('green','green','yellow','red','red','red','green')
factor_apple <- factor(apple_colors)
nlevels(factor_apple)
levels(factor_apple)

# String
x <- "Hello World!"
print(x)
class(x)
length(x)
nchar(x)
substr(x,2,4)
substring(x, 2, 4:6)

###########Data Structures#######

# Vectors 

car_name <- c("Honda","BMW","Ferrari")
car_color = c("Black","Blue","Red")
car_cc = c(2000,3400,4000)

# List
cars <- list(name =c("Honda","BMW","Ferrari"), color =c("Black","Blue","Red"), cc =c(2000,3400,4000))
cars
class(cars)

# Matrix
mdat <- matrix(c(1,2,3, 11,12,13), nrow =2, ncol =3, byrow =TRUE, dimnames =list(c("row1", "row2"), c("C.1", "C.2", "C.3")))
##Transpose
t(mdat)
# Deconstruction
c(mdat)

#Column & Row Sums
rowSums(mdat)
colSums(mdat)

# dataframe
cars <- data.frame(name =c("Honda","BMW","Ferrari"), color =c("Black","Blue","Red"), cc =c(2000,3400,4000))
cars

######Indexing#######
# A sample vector
v <- c(1,4,4,3,2,2,3)
v[c(2,3,4)]
v[2:4]

# Create a sample data frame
data <- read.table(header=T, text='
                   subject Gender size
                   1   M    7
                   2   F    6
                   3   F    9
                   4   M   11
                   ')
names(data)
colnames(data)

#to get the dimensions
dim(data)
dimnames(data)
dimnames(data)[[1]]

# Get the element at row 1, column 3
data[1,3]
data[1,"size"]

# Get rows 1 and 2, and all columns
data[1:2, ] 
data[c(1,2), ]

# Get rows 1 and 2, and only column 2
data[1:2, 2]
data[c(1,2), 2]

# Get rows 1 and 2, and only the columns named "Gender" and "size"
data[1:2, c("Gender","size")]
data[c(1,2), c(2,3)]
data[['size']]

###Indexing with a boolean vector
v > 2
v[v>2]
v[ c(F,T,T,T,F,F,T)]

# A boolean vector   
data$subject < 3
data[data$subject < 3, ]
data[c(TRUE,TRUE,FALSE,FALSE), ]
which(data$subject < 3)
data

##Negative indexing
# Drop the first element
v[-1]
# Drop first three
v[-1:-3]
# Drop just the last element
v[-length(v)]

#####Getting a subset of a data structure
subset(v, v<3)
v[v<3]
# Another vector
t <- c("small", "small", "large", "medium")

# Remove "small" entries
subset(t, t!="small")
t[t!="small"]

# One important difference between the two methods is that you can assign values to
# elements with square bracket indexing, but you cannot with subset().

v[v<3] <- 9
subset(v, v<3) <- 9 ### you cannot assign values 

subset(data, subject < 3)
data[data$subject < 3, ]

# Subset of particular rows and columns
subset(data, subject < 3, select = -subject)
subset(data, subject < 3, select = c(Gender,size))
subset(data, subject < 3, select = Gender:size)
data[data$subject < 3, c("Gender","size")]


# Logical AND of two conditions
subset(data, subject < 3  &  Gender=="M")
data[data$subject < 3  &  data$Gender=="M", ]

# Logical OR of two conditions
subset(data, subject < 3  |  Gender=="M")
data[data$subject < 3  |  data$Gender=="M", ]

# Condition based on transformed data
subset(data, log2(size) > 3 )
data[log2(data$size) > 3, ]
