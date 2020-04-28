
##########################################################
#STA 224: COMPUTATIONAL METHODS AND DATA ANALYSIS II
############################################################

#Download R and R packages  (Most codes in R are stored in packages which can 
#be downloaded from the internet.)
####################################################################

#Go to the website: http://cran.r-project.org/
#To download R, click on the link "Download for Windows" in CRAN (https://cran.r-project.org/)
#Select R Packages; "Available CRAN Packages By Name" and choose the package wanted
#Here chose for instance ISwR
#Go to downloads and select Windows binary: ISwR_2.0-6.zip 
#or the latest version available. 
#Back in R, instal package from local zip folder.


#DATASETS REQUIRED

#"Multiple_Regression_Data.csv"





# Introduction to R
# Operations in R
#########################################################

10+9
2+2
3-2
3*4
8/2
6^2
log(2)
log10(2)
log(exp(3))
?log()


#Vector of numbers (manual data entry)
############################################################

mydata<-c(21, 23, 23,32,42,23,35,44,32,12,13)  
#here, we have assigned an object name "mydata" to the data set.

mean(mydata)
var(mydata)
sqrt(var(mydata))  #my comments
sd(mydata)


plot(mydata)
plot(mydata, type="b", col="blue", 
main="My First Graph in R", xlab="My X axis", ylab="my Y axis")

########################################################################
#Getting Help in R
########################################################################


# To open HTML help
help.start()

#when you are sure of the R-function to use.
?lm 
help(lm)
?mean
?var
?hist

#To search a function whose real name used in 
#R is not clear to you.

help.search("histogram")
help.search("linear models")
help.search("variance")



#############################################################
# DATA SIMULATION
# Generating (Pseudo Random) Numbers
############################################################

ser_no <- 1:100
ser_no

# a random permutation (sampling from the data x without replacing)
sample(x)

# bootstrap resampling -- only if length(x) > 1! Replacement allowed.
sample(x, replace = TRUE)

# 100 Bernoulli trials (Create a coin tossing experiment)
coin_toss<-sample(c(0,1), 100, replace = TRUE)	
coin_toss
sex<-sample(c("Male", "Female"), 100, replace = TRUE)	
sex
sample(c(0,1), 50, replace=T)

#Alternatively, sample "Heads" amd "Tails" instead of 0 and 1.
coin_toss_1<-sample(c("Head","Tail"), 100, replace = TRUE)
coin_toss_1

# Generate a sample of thirty five random numbers between 1 and 100
sample(1:100, size=35, replace=TRUE)
?sample()

# To generate integers WITHOUT replacement:
sample(1:100, 35, replace=FALSE)


# Pseudo Random Numbers from Statistical distributions
############################################################################################

?distributions()    #getting help on the available distributions

#rexp(n, rate = 1)   #syntax for the Exponential distribution
rexp(100, rate = 1)   #generates 100 random numbers of Exponential distribution, rate lambda=1

#rnorm(n,mean,sd)   #Normal distribution
rnorm(50, mean = 0, sd=1)  #By default, mean=0, var=1  (generates standard normal)
dogmeat<-rnorm(70, mean=40, sd=23)   #generate 70 observations.
hist(dogmeat)
marks<-rnorm(198, mean=65, sd=15)
round(marks, 0)
round(marks, 1)

#Now generate 100 observations from a normal distribution with mean = 40 
# and variance = 25. 

#rbinom(n, size, prob)     #Binomial distribution
rbinom(100, size=67, prob=0.5)
sum1<-(dbinom(3, size=10, prob=0.5)+ 
dbinom(2, size=10, prob=0.5)+
dbinom(1, size=10, prob=0.5)+
dbinom(0, size=10, prob=0.5))
sum1
pbinom(3, 10, 0.5)

#Uniform distribution
runif(100, min=0, max=1)
y<-runif(100, min=10, max=100)
plot(dunif(y), xlim = c(-10, 150), type="p", col = "red")

#Other distribution properties. Consider the Binomial distribution.
##########################################################################################

#1. Probability distribution. P(X=x)
#############################################################################

#Binomial pmf
#dbinom(x, size, prob, log = FALSE)
dbinom(5, size=10, prob=0.3)  # returns probabilities that x=5.
dbinom(46:54, prob=0.3, size=100)       
# returns probabilities that x takes value 46, 47, ..., 54.


#Cumulative Distribution function [CDF P(X<=x)]
###############################################################################
#pbinom(q, size, prob, lower.tail = TRUE, log.p = FALSE)

pbinom(2, size=10, prob=0.3)   # returns P(X<=2) when n=10, p=0.3  

#verify using the dbinom that pbinom returns cumulative dist.
xx<-dbinom(0:2, 10, 0.3)
xx
sum(xx)


#Quantile a: If P(X<=a)=p, then "a" is a quantile for the distribution
# that is associated with (p)% on the lower side of the distribution.
########################################################################
#qbinom(p, size, prob, lower.tail = TRUE, log.p = FALSE)
#also useful for computing p-values

qbinom(0.8, size=10, prob=0.3, lower.tail = TRUE, log.p = FALSE)
qnorm(0.975)     #Recall  Z=1.96
qnorm(0.95)      #Recall  Z=1.65
qnorm(0.8577)
qt(0.975, 15)
qt(0.95, 10)


############################################################################
#Importing and exporting data from Excel to R and vice versa.
#############################################################################

#x<-read.table("clipboard", sep=",", header=TRUE)
#x<-read.table("C:\\Users\\Admin\\Desktop\\Book1.csv", sep=",", header=TRUE)
#x

getwd()
setwd("C:\\Users\\Admin\\Documents\\Nelson\\Maths\\Lecturer_notes\\Computational Methods\\STA224")
x_new<-read.table("Multiple_Regression_Data.csv", sep=",", header=TRUE)
#as.numeric(x)
View(x_new)
names(x_new)
colnames(x_new)<-c("Observation","Fertilizer_input","Rainfall","Yield")
head(x_new); 
names(x_new)
hist(x_new$Yield, probability=TRUE)
hist(log10(x_new$Yield), breaks=10)


#Alternative way of improting csv data, Ms Excel
datax<-read.csv("DATAX.csv", sep=",", header=TRUE)
head(datax)
colnames(datax)<-c("Gen", "Ag", "Uni")
head(datax)
attach(datax)
mean(Ag)



#Exporting or Saving data outside R.
#########################################################

#set your own working directory where you want to have the data saved.
setwd("C:\\Users\\Admin\\Documents\\Nelson\\Maths\\Lecturer_notes\\Computational Methods\\STA224")

y<-runif(17)
z<-rnorm(17)
x_new<-read.csv("Multiple_Regression_Data.csv", sep=",", header=TRUE)
dim(x_new)
xx_new <-as.data.frame(cbind(x_new,y,z))
write.table(xx_new, file = "Multiple_Reg_Data_new.csv", append =FALSE, sep=",")
write.table(xx_new, file = "Multiple_Reg_Data_new1.csv", sep=",", row.names = FALSE)
#?write.table


#############################################################################
#   Data Exploration: Graphics
#############################################################################

#generate random numbers, eg from normal distribution with mean 0, sd=2 
z <- rnorm(50,0,2)

x <- rnorm(50,0,2)
y <- rnorm(50,0,1)


#Histogram and boxplot
x_new <- rnorm(50,0,2)
#x_new data above
hist(x_new)
hist(x_new, probability=TRUE)
hist(x_new, breaks=20)   #number of bars = 20.

z <- rnorm(50,0,2) 
hist(z, main="Weight Gain",xlab="Weight (kg)",
     ylab ="Frequency", col="blue", freq=F)

z1 <- rnorm(50,50,3)
min(z1);max(z1)
hist(z1, main="Weight Gain(40+yrs)",xlab="Weight (kg)",
     ylab ="Frequency", col="green", freq=F)


boxplot(z, main = "My box plot")

#Stem and leaf plot
#########################################

scores<-c(2,3,16,23,14,12,4,13,12,4,13,2,0,0,0,6, 28,31,14,4,8,2,5)
length(scores)   #23 observations
stem(scores)

#Multiple plots on the same page

par(mfrow=c(2,2))
z1 <- rnorm(50,0,2)
hist(z1, main="Weight Gain(0-10yrs)",
	xlab="Weight (kg)",
     ylab ="Frequency", col="blue", 
	freq=F)
z2 <- rnorm(50)
hist(z2, main="Weight Gain (11-25yrs)",
	xlab="Weight (kg)",
     ylab ="Frequency", col="yellow", 
	freq=F)
z3 <- rnorm(50,3,2.232)
hist(z3, main="Weight Gain (26-40yrs)",
	xlab="Weight (kg)",
     ylab ="Frequency", col="grey", 
	freq=F)
z4 <- rnorm(50,10,2.5)
hist(z4, main="Weight Gain(40+yrs)",
xlab="Weight (kg)",
     ylab ="Frequency", col="green", 
freq=F)
par(mfrow=c(1,1))


#Line plots

x <- runif(50,0,2)
head(x)
x[1:6]
y <- runif(50,0,2)
head(y)
length(y)
plot(sort(x), y, type="l", main="Main Title", sub="Sub_title here",
     xlab="x-ais label", ylab="y-axis-label")


x <- runif(50,0,2)
x;
y <- runif(50,0,2);
plot(x, y, main="Kenya", sub="Kisumu",
     xlab="weights", ylab="heights")               #add title and subtitle

text(0.6,0.6,"Point xyz")       #add text to a graph
abline(h=.6,v=.6)
abline(b=0.7,a=0.3)                             #add a vertical and horizontal line at 0.6 each
?abline()
#abline(a = intercept, b = grad, h = horizontal, v = vertical)

require(stats)
sale5 <- c(6, 4, 9, 7, 6, 12, 8, 10, 9, 13)
plot(sale5)
abline(lsfit(1:10,sale5))
abline(lsfit(1:10,sale5, intercept = FALSE), col= 4) # less fitting

#Overlaying graphs (histogram and a normal density plot)

x <- rnorm(100)        #100 data from std normal dist.
head(x)
x1<-rnorm(100, mean=10, sd=12)
x1
hist(x,freq=F)
curve(dnorm(x),add=T)

#improvement (to esnure the ylim covers the range of the probabilities).

h <- hist(x, plot=F)
ylim <- range(0, h$density, dnorm(0))
hist(x, freq=F, ylim=ylim)
curve(dnorm(x), add=T)

dnorm(0)
h$density
range(23, 34, 23, 12, 24, 32)

#Histogram and a normal plot
########################################################

x<-rnorm(1000,0,1)
hist(x,ylab="probability", main="Histogram of N(0,1)",
     breaks=seq(from=-5, to=5,length=21), pro=T,ylim=c(0,0.5))
y<-seq(from=-4,to=4,length=300)
lines(y,dnorm(y,0,1))

#Pie chart
#########################################################

DataY<-c(23, 32, 34, 34, 23, 25, 43, 32, 23, 23, 34, 43, 25, 43, 32, 42, 42, 54, 54)
DataY.counts<-table(DataY)
DataY.counts
pie(DataY.counts, main="RAINBOW COLORS")
names(DataY.counts)<-c("red", "orange", "yellow", "green", "blue", "indigo", "violet")
pie(DataY.counts, main="RAINBOW COLORS")
pie(DataY.counts, main="RAINBOW COLORS", col=c("red", "orange", "yellow", "green", "blue", "purple", "violet"))


###################################################################################
#MATRICES AND MATRIX PROPERTIES
##################################################################################


#Vectors and Matrices
#########################################################

n<-c(10,12,32,42,45)
mean(n)
var(n)
seq(from=0,to=100,by=6);
seq(0, 100, by=10)
seq(from=0,to=100,length=11)

rep(3,10)

rep(c(1,2,3,4),time=c(1,2,3,4))
rep(c(1,2,3,4),time=c(4,3,2,1))
rep("B", 6)
gender<-rep(c("F", "M"), time=c(45,55))
gender
ages<-rnorm(100, mean=40, sd=12)
ages
data1<-cbind(gender, ages)
data1<-as.data.frame(data1)
data1
attach(data1)
tapply(ages, gender, mean)

z<-c(3,1,5,8,-4)
sort(z);
sort(c(3,1,5,8,-4),decreasing = TRUE)
?sort

x<-seq(from=1,to=31,by=6)
x
nn<-matrix(x,ncol=3,byrow=F)
nn
nn[2,3]
matrix(3, 2, 3)
matrix(1:6, 2, 3)
A<-matrix(1:9, 3, 3, byrow =TRUE)
A

#indexing (selecting elements of matrix A)
A[1,]   
A[2,3]
A[,1]

#transpose, determinant and inverse of A
B<-matrix(c(16,21,23,12,21,3,21,14,9), byrow=TRUE, nrow=3)
det(B)    #determinant
solve(B)   #Inverse
round(solve(B)%*%B)      #identity matrix
t(B)       #transpose of A
diag(B);   #diagonal of A
#qr(B);     # QR decomposition of a matrix.
#?qr
eigen(B)   #returns eigenvalues and eigenvectors
eigen(B)$vectors
eigen(B)$values
svd(B)    #singular value decomposition


#Operations on a matrix
##########################################

# +, -, *, /
# +, -, %*%, solve(): solve Ax=b

A<-matrix(1:9, 3, 3, byrow =TRUE)
A+B     #plus
A-B     #minus
A%*%B    #multiplication
A[1,]   #indexing, choose first row of A
A[2,3]   #choose element in 2 row 3 column of A
A[,1]   #choose 1st column of A

t(A);   #transpose A
det(A); #determinant of A
diag(A);   #diagonal of A
#qr(A);     # QR decomposition of a matrix.
#?qr
eigen(A)   #returns eigenvalues and eigenvectors
eigen(A)$vectors
svd(A)    #singular value decomposition



x<-c(12, 13, 21)
y<-c(21,32,43)
rbind(x,y)

matrix(A)
rbind(A[1,], A[2,], A[3,]);  #row bind.
cbind(A[,1], A[,2], A[,3])   #column bind



###############################################################
#### Solving own equations in R
##############################################################


#Solve systems of linear equations
##########################################################

#simultaneously solve 2x-3y=6 and 4x-2y=2;

A<-matrix(c(2,-3,4,-2), ncol=2, byrow=T)
A
b<-c(6,2)
qr.solve(A, b);
solve(qr(A), b)

y<--10/4
y

x<-3/4
x

B<-matrix(c(0.29,-0.02,-0.22,-0.02, -0.28, 0, -0.22, 0, 1.89), ncol=3, byrow=T)
B
c<-c(0,0, 0)
qr.solve(B, c);
solve(qr(B), c)


#Eigen values and eigen vectors

eigen(B)

#Further example

runif(12)   #12 observations from the uniform distribution
A <- matrix(runif(12), 4)
A
b <- 1:4
qr.solve(A, b);
solve(qr(A), b)
solve(qr(A, LAPACK=TRUE), b)
?solve()

#######################################################
# LU Decomposition of a matrix A
#####################################################

#install.packages("Matrix")
library(Matrix)
set.seed(1)
mm <- matrix(round(rnorm(9),2), nrow = 3)
mm

lu(mm)
str(lum <- lu(mm))
elu <- expand(lum)
elu 


# three components: "L", "U", and "P", the permutation
elu$L %*% elu$U

(m2 <- with(elu, P %*% L %*% U)) # the same as 'mm'
mm
#stopifnot(all.equal(as(mm, "matrix"), as(m2, "matrix")))

#Examples
#Other systems of linear equations


##Overdetermined system
##In mathematics, a system of equations is considered overdetermined if 
##there are more equations than unknowns.[citation needed] 
##An overdetermined system is almost always inconsistent (it has no solution)
##when constructed with random coefficients.
##############################################################################


A <- matrix(runif(12), 4); A
b <- 1:4; b
qr.solve(A, b) # or 
solve(qr(A), b)
solve(qr(A, LAPACK = TRUE), b)
# this is a least-squares solution, cf. lm(b ~ 0 + A)

## Underdetermined system
##  In contrast, the underdetermined case occurs when the system has been 
## underconstrained. That is, when the number of equations is fewer than 
## the number of unknowns. Such systems usually have an infinite number of 
## solutions
#############################################################################

A <- matrix(runif(12), 3)
A
b <- 1:3
qr.solve(A, b)
solve(qr(A, LAPACK = TRUE), b)
# solutions will have one zero, not necessarily the same one


#Cholesky Decomposition of a Matrix A.
###############################################################


A = as.matrix(data.frame(c(3,4,3),c(4,8,6),c(3,6,9)))
colnames(A) <- NULL
A

A.chol <- chol(A)
A.chol

t(A.chol)
t(A.chol) %*% A.chol


### NUMERICAL DIFFERENTIATION
################################################################

#Richardson extrapolation
#######################################################################

#install.packages("numDeriv")
library(numDeriv)

a<-1:10
f<-function(x){5*x*exp(-2*x)}
b<-grad(f, x<-a, method= "Richardson")
b    #gradient of f at points x=a.

var

f(a)
plot(f(a), type="l", col="blue", ylim=c(-2,2))
lines(b, col="red", ylim=c(-2,2))


## Newton Raphson algorithm
################################################################################

func<-function(x){x^2 - 10}
curve(func, col="blue", lwd=2, from=0, n=100, xlim=c(0,5), ylab="f(x)")
abline(a=1, b=2, lty=3)
abline(a=0, b=0, lty=5)


require(utils)
uniroot(func, c(1,4))



#Newton raphson function


newton.raphson <- function(f, a, b, tol = 1e-5, n = 1000) {
  require(numDeriv) # Package for computing f'(x)
  
  x0 <- a # Set start value to supplied lower bound
  k <- n # Initialize for iteration results
  
  # Check the upper and lower bounds to see if approximations result in 0
  fa <- f(a)
  if (fa == 0.0) {
    return(a)
  }
  
  fb <- f(b)
  if (fb == 0.0) {
    return(b)
  }
  
  for (i in 1:n) {
    dx <- genD(func = f, x = x0)$D[1] # First-order derivative f'(x0)
    x1 <- x0 - (f(x0) / dx) # Calculate next value x1
    k[i] <- x1 # Store x1
    # Once the difference between x0 and x1 becomes sufficiently small, output the results.
    if (abs(x1 - x0) < tol) {
      root.approx <- tail(k, n=1)
      res <- list('root approximation' = root.approx, 'iterations' = k)
      return(res)
    }
    # If Newton-Raphson has not yet reached convergence set x1 as x0 and continue
    x0 <- x1
  }
  print('Too many iterations in method')
}


newton.raphson(func, 3, 4)

eduard<-function(x){x^3 - 2*x -5}
curve(eduard, xlim = c(0, 5))
abline(a=0, b=0)

newton.raphson(eduard, 2,3)



cynthia <-function(x){exp(2*x) - x -6}
curve(cynthia, col="brown", xlim = c(-4,4), ylim = c(-5, 5))
abline(h=0)
newton.raphson(cynthia, 1,2)


#########################################################################
#STATISTICAL INFERENCE
#########################################################################



## Simple Linear Regression
####################################################

#Fertilizer input = x
#Crop yield = y

x<-c(6.43, 5.48, 7.65, 7.01, 7.17, 7.61, 6.09, 6.62, 7.61, 6.66)
x
y<-c(9.86, 12.51, 11.86, 9.15, 8.29, 10.43, 11.28, 5.83, 15.91, 16.62 )
lm1<-lm(y~x)
lm1
summary(lm1)
coef(lm1)[2]

?lm()

#Diagnostics

mean(x)     #returns the mean of x

plot(x,y)                        #scatter plot
lines(x, fitted(lm1))            #add a line of fit
segments(x, fitted(lm1), x, y)   #adds lines depicting the errors
plot(fitted(lm1), resid(lm1))    #no pattern expected around y=0 line.
qqnorm(resid(lm1))               #the points should lie along a straigt diagonal
hist(resid(lm1))


plot(lm1)


##Multiple Linear Regression
#######################################

Yield<-c(1.1,2.3,3.4,4.7, 5.1, 6.2, 6.3)
Rainfall<-c(1.1, 1.15, 2.32, 2.5, 3, 3.5,3.8)
Fertilizer<-c(0.1, 0.2, 0.25, 0.3, 0.38, 0.42, 0.54)
weeding<-c(rep(1:3), rep(2,4))
pesticide<-c(rep(1, 5), rep(2, 2))
region<-1:7
drought<-c(1,1,1,1,1,1,2)

#design matrix
constants<-rep(1,7)
X<-as.matrix(cbind(constants, Rainfall, Fertilizer, weeding, pesticide, region, drought))
X
solve(X)
X%*%solve(X)
round(X%*%solve(X))

#coeficients

solve(t(X)%*%X)%*%t(X)%*%Yield

#model

lm2<-lm(Yield~Rainfall + Fertilizer + weeding + pesticide + region + drought)
lm2
summary(lm2)
coef(lm2)[1]
coef(lm2)[2]
coef(lm2)[3]

lm3<-lm(Yield~Fertilizer + Rainfall*weeding)
lm3
summary(lm3)




#Excercize: Solving other types of equations
#################################################

nelson<-function(x){
  y=x^2+3*x+4
  return(y)
}

nelson(4)
nelson(0.5)
nelson(sqrt(8.7))

#Monthly loan repayment rate#################

MRR<-function(P,R,N){
  MRR=P*(R+R/((1+R)^N-1));
  return(MRR);
}

#Monthly Loan Repayment for principal loan of 200,000/= at a rate of 
#8% p.a. to be paid in 30 years
MRR(200000,8/1200,30*12)

#Monthly Loan Repayment for principal loan of 200,000/= at a rate of 
#7.5% p.a. to be paid in 20 years
MRR(200000,7.5/1200,20*12)

#Quadratic function#######

quad<-function(x){
  y<-x^2;
  return(y);
}

x<--3:3
a<-0.5
quad(x)
plot(quad(x), type="l", col="blue")



#############################################################################
##TESTS ON MEANS
############################################################################

# t-test, one sample
##########################################################################

#daily intake of a certain nutrient in KJ.

install.packages("ISwR")
library(ISwR)
data(intake)
head(intake)
attach(intake)
t.test(pre,post,paired=TRUE)

daily.intake<-c(5260, 5470, 5640, 6180, 6390, 6515, 6805, 7515, 7515, 8230, 8770)
mean(daily.intake)
var.di<-var(daily.intake)
sd.di<-sqrt(var.di)
sd.di
quantile(daily.intake)

# Does the mean Daily Intake differ from an average of 7725 KJ?

t.test(daily.intake, mu=7725)  # default mean =0, default alternative= two sided, 
#default CI=95%.
t.test(daily.intake, mu=7725, alternative="greater", conf.level=0.99)



# t-test, 2 sample
################################################################################

#Example One:
#Consider two samples of data 
#showing managerial success indices of 2 groups of managers 

group1<-c(65, 58, 78, 60, 68, 69, 66, 70, 53, 71, 63, 63)
group2<-c(62, 53, 36, 34, 56, 50, 42, 57, 46, 68, 48, 42, 52, 53, 43)
var.test(group1, group2)
var.test(group2, group1)

#t.test(group1, group2)
t.test(group1, group2, var.equal=TRUE, conf.level=0.99)
#t.test(group1, group2, alternative = c("two.sided", "less", "greater"),
#      mu = 0, paired = FALSE, var.equal = FALSE,
#     conf.level = 0.95, ...)

#data sets
######################################

X<-rnorm(10, 7, 0.5)  #normal distributed sample with mean 7, sd=0.5
Y<-rnorm(10, 12, 0.05)
x <- rnorm(100) # mean =0, sd=1, standard normal
y <- rnorm(100)

#Normality tests
#########################

hist(x,freq=F)
curve(dnorm(x),add=T)
boxplot(x, horizontal=FALSE)
qqnorm(x)
shapiro.test(x)


#################################################
#Chisquare tests
################################################


# 2X2 contigency table
#############################

data1 <- matrix(c(12, 6, 11, 7), nrow=2, ncol=2, byrow=T)
data1
colnames(data1) <- c("Diabetic","Non-diabetic")
rownames(data1) <- c("Men","Women")
data1
chisq.test(data1)

# 3X4 contigency table
#############################



data2 <- matrix(c(157, 65, 181, 10, 126, 82, 142, 46, 58, 45, 60, 28), nrow=3, byrow=TRUE)
data2
colnames(data2) <- c("A","B","C", "D")
rownames(data2) <- c("Small","Medium","Large")
data2
chisq.test(data2)




# ANOVA-One way
#################################################


library(ISwR)
data(red.cell.folate)
red.cell.folate
attach(red.cell.folate)
lm1<-anova(lm(folate~ventilation))
summary(lm1)
lm2<-aov(folate~ventilation)   #ANOVA command aov
summary(lm2)

#stripchart (compare the means graphically): Refer to ISwR

xbar <- tapply(folate, ventilation, mean)
s <- tapply(folate, ventilation, sd)
n <- tapply(folate, ventilation, length)
sem <- s/sqrt(n)
stripchart(folate~ventilation, method="jitter",jit=0.05,pch=16,vert=T)
arrows(1:3,xbar+sem,1:3,xbar-sem,angle=90,code=3,length=.1)
lines(1:3,xbar,pch=4,type="b",cex=2)



#pairwise mean comparison

pairwise.t.test(folate, ventilation, p.adj="bonferroni")

#p.adjust.methods
# c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
#   "fdr", "none")



#Bartlets test for equality of variances in anova

bartlett.test(folate~ventilation)

#anova for unequal variance groups

oneway.test(folate~ventilation)



#Two way ANOVA
#####################################

#Example 1


heart.rate <- data.frame(hr = c(96,110,89,90,128,100,72,79,100,
                                92,106,86,78,124, 98,68,75,106,
                                86,128,85,78,18,100,67,70,104,
                                92,14,83,83,118,94,71,74,82),
                         subj=gl(9,1,36),
                         time=gl(4,9,36,labels=c(0,30,60,120)))


anova(lm(hr~subj+time))


#Example 2

factor1<-as.factor(c(rep("1", 8),rep("2",7)))
factor2<-as.factor(c(rep("1", 5),rep("2",3), rep("1",1), rep("2",6)))
obs<-as.numeric(c(2,4,2,3,4,14,9,10,63,15,10,12,15,14,18))
data1<-cbind(factor1, factor2, obs)
data1<-as.data.frame(data1)
attach(data1)
anova(lm(obs ~ factor1 + factor2))


#Cheking the Normality assumption in Data. 
###################################################


op <- par(mfrow = c(2, 2))
hist(islands)
utils::str(hist(islands, col = "gray", labels = TRUE))

hist(sqrt(islands), breaks = 12, col = "lightblue", border = "pink")
##-- For non-equidistant breaks, counts should NOT be graphed unscaled:
r <- hist(sqrt(islands), breaks = c(4*0:5, 10*3:5, 70, 100, 140),
          col = "blue1")
text(r$mids, r$density, r$counts, adj = c(.5, -.5), col = "blue3")
sapply(r[2:3], sum)
sum(r$density * diff(r$breaks)) # == 1
lines(r, lty = 3, border = "purple") # -> lines.histogram(*)
par(op)

require(utils) # for str
str(hist(islands, breaks = 12, plot =  FALSE)) #-> 10 (~= 12) breaks
str(hist(islands, breaks = c(12,20,36,80,200,1000,17000), plot = FALSE))

hist(islands, breaks = c(12,20,36,80,200,1000,17000), freq = TRUE,
     main = "WRONG histogram") # and warning

require(stats)
set.seed(14)
x <- rchisq(100, df = 4)

## Comparing data with a model distribution should be done with qqplot()!
qqplot(x, qchisq(ppoints(x), df = 4)); abline(0, 1, col = 2, lty = 2)

## if you really insist on using hist() ... :
hist(x, freq = FALSE, ylim = c(0, 0.2))
curve(dchisq(x, df = 4), col = 2, lty = 2, lwd = 2, add = TRUE)








