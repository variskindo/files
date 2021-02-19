library(car)
library(nortest)
library(foreign)
x<-runif(100)
y<-3+2*x+rnorm(100)
par(mfrow=c(1,2))
plot(x,y)
abline(lm(y~x))
plot(lm(y~x),panel=points,which=1)

y<-3+2*x+(x/2)*rnorm(100)
plot(x,y)
abline(lm(y~x))
plot(lm(y~x),panel=points,which=1)

y<-exp(3+2*x)+3*rnorm(100)
plot(x,y)
abline(lm(y~x))
plot(lm(y~x),panel=points,which=1)
par(mfrow=c(1,1))
qqPlot(rnorm(100),main="Normal",envelope=FALSE)
par(mfrow=c(1,2))
qqPlot(runif(100),main="Uniform",envelope=FALSE)
qqPlot(rt(100,3),main=expression(t[3]),envelope=FALSE)
qqPlot(rexp(100),main="Exponential",envelope=FALSE)
qqPlot(10-rexp(100),main="10-Exponential",envelope=FALSE)

res<-array(NA,c(5,6))

y.norm<-rnorm(100)
res[1,1]<-shapiro.test(y.norm)[[1]]
res[1,2]<-shapiro.test(y.norm)[[2]]
res[1,3]<-ks.test(y.norm,pnorm)[[1]]
res[1,4]<-ks.test(y.norm,pnorm)[[2]]
res[1,5]<-lillie.test(y.norm)[[1]]
res[1,6]<-lillie.test(y.norm)[[2]]


y.unif<-runif(100)
res[2,1]<-shapiro.test(y.unif)[[1]]
res[2,2]<-shapiro.test(y.unif)[[2]]
res[2,3]<-ks.test(y.unif,pnorm)[[1]]
res[2,4]<-ks.test(y.unif,pnorm)[[2]]
res[2,5]<-lillie.test(y.unif)[[1]]
res[2,6]<-lillie.test(y.unif)[[2]]

y.t_3<-rt(100,3)
res[3,1]<-shapiro.test(y.t_3)[[1]]
res[3,2]<-shapiro.test(y.t_3)[[2]]
res[3,3]<-ks.test(y.t_3,pnorm)[[1]]
res[3,4]<-ks.test(y.t_3,pnorm)[[2]]
res[3,5]<-lillie.test(y.t_3)[[1]]
res[3,6]<-lillie.test(y.t_3)[[2]]

y.exp<-rexp(100)
res[4,1]<-shapiro.test(y.exp)[[1]]
res[4,2]<-shapiro.test(y.exp)[[2]]
res[4,3]<-ks.test(y.exp,pnorm)[[1]]
res[4,4]<-ks.test(y.exp,pnorm)[[2]]
res[4,5]<-lillie.test(y.exp)[[1]]
res[4,6]<-lillie.test(y.exp)[[2]]

y.10_exp<-10-rexp(100)
res[5,1]<-shapiro.test(y.10_exp)[[1]]
res[5,2]<-shapiro.test(y.10_exp)[[2]]
res[5,3]<-ks.test(y.10_exp,pnorm)[[1]]
res[5,4]<-ks.test(y.10_exp,pnorm)[[2]]
res[5,5]<-lillie.test(y.10_exp)[[1]]
res[5,6]<-lillie.test(y.10_exp)[[2]]


######################################
par(mfrow=c(2,1))
e_p<-rep(NA,101)
e_m<-rep(NA,101)
e0<-rnorm(1)
e_p[1]<-e0
e_m[1]<-e0
r<-.75
s<-4
for(i in 2:101){
e_p[i]<- e_p[i-1]*r+rnorm(1)/s
e_m[i]<- -e_m[i-1]*r+rnorm(1)/s
}
plot(e_p[2:101],type="b",ylab="Residuals")
plot(e_m[2:101],type="b",ylab="Residuals")

  
#################
#library(xlsReadWrite)
#exptrav.dat<-read.xls("C:\\Users\\Tablet\\my cocument_19_5_2011\\courses\\regression\\exptrav.xls",colNames = TRUE,sheet=1, type = "data.frame")
library(foreign)
exptrav.dat <- read.spss("C:\\Users\\Israel\\Documents\\My Document\\courses\\regression\\exptrav.sav",  to.data.frame=TRUE)
model.1<-lm(Exptrav~Income,exptrav.dat)
summary(model.1)
Anova(model.1)
attach(exptrav.dat)
par(mfrow=c(1,1))
plot(Income,Exptrav)
abline(model.1)
plot(model.1,panel=points,which=1)
abline(0,0)
plot(Pop,resid(model.1))
abline(0,0)
identify(Pop,resid(model.1),labels=1:51)
plot(Pop,abs(resid(model.1)))
identify(Pop,abs(resid(model.1)),labels=1:51)

model.2.1<-lm(I(Exptrav/Pop)~I(1/Pop)+I(Income/Pop)-1)
summary(model.2.1)
Anova(model.2.1)
plot(model.2.1,panel=points,which=1)
abline(0,0)

model.2.2<-lm(Exptrav~Income,weight=I(Pop^(-2)))
summary(model.2.2)
Anova(model.2.2)
plot(model.2.2,panel=points,which=1)
model.residuals<-resid(model.1)

###Goldfeld-Quandt Test
exptrav.dat.sort<-exptrav.dat[order(Pop),]
exptrav.dat.begin<-exptrav.dat.sort[1:20,]
exptrav.dat.end<-exptrav.dat.sort[32:51,]
model.begin<-lm(Exptrav~Income,exptrav.dat.begin)
model.end<-lm(Exptrav~Income,exptrav.dat.end)
F_GQ<-sum(resid(model.end)^2)/sum(resid(model.begin)^2)
F_GQ

qf(c(.025,.975),18,18)
1-pf(F_GQ,18,18)
######################################


y<-arima.sim(n=100,list(ar=c(0.75)))
plot(y,type="b")
y<-arima.sim(n=100,list(ar=c(-0.75)))
plot(y,type="b")
####################################################################

dat<-read.spss("C:\\Users\\Israel\\Documents\\My Document\\courses\\doe\\example_one.sav",
               to.data.frame=TRUE)
linear.fit<-lm(strength~cotton,data=dat )
summary(linear.fit)
anova(linear.fit)
with(dat,plot(cotton,strength,xlab="Cotton Percentage",ylab="Strength"))
abline(linear.fit)
mean.fit<-sapply(split(dat$strength,dat$cotton),mean)
x<-(3:7)*5
lines(x,mean.fit,col=2)
linear.resid<-resid(linear.fit)
temp<-cbind(Obs=1:25,linear.resid)
temp<-temp[order(linear.resid),]
temp<-cbind(Obs=temp[,1],i=1:25,q=ppoints(1:25),Zq=qnorm(ppoints(1:25)),Residuals=temp[,2])
print(temp)
plot(linear.fit,which=1,add.smooth=FALSE)
qqnorm(linear.resid,datax = FALSE)
qqline(linear.resid,datax = FALSE)
qqPlot(linear.resid,envelope=FALSE)

SSE<-sum(linear.resid^2)
var.v<-sapply(split(dat$strength,dat$cotton),var)
SSPE<-sum(var.v*4)
SSLF<-SSE-SSPE
F_L<-(SSLF/(5-2))/(SSPE/(25-5))
qf(0.95,5-2,25-5)
1-pf(F_L,5-2,25-5)

Non.linear.fit<-lm(strength~factor(cotton),data=dat )
summary(Non.linear.fit)
anova(Non.linear.fit)
Non.linear.resid<-resid(Non.linear.fit)
plot(Non.linear.fit,which=1,add.smooth=FALSE)
qqnorm(Non.linear.resid,datax = FALSE)
qqline(Non.linear.resid,datax = FALSE)
qqPlot(Non.linear.resid,envelope=FALSE,main="Normal QQ Plot")



