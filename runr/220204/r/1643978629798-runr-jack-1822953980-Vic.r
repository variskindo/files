#comment box for function 1
#################################################
#purpose: function that takes a vector of prices,
# calculates the log-returns, and returns several
# numerical summaries, mean, sd, median, IQR
#   
#input: Prices
# 
#arg1: Prices is a vector of random or (observed)  
# prices
#
#output: Returns the Mean, Sd, Median, and IQR 
#################################################
#code for function 1 and a call to it:

library('data.table')

Prices <- runif(n = 50, min = 0, max = 1)

Descriptive<-function(x)
  {
  dt<-data.table(Prices)  
  dt[,`:=`(lret=c(NA,diff(log(Prices))))]
  dt[,.(Mean=mean(lret,na.rm=TRUE),Sd=sd(lret,na.rm=TRUE),Med=median(lret,na.rm=TRUE),Iqr=IQR(lret,na.rm=TRUE,type = 7))]
}
Descriptive(dt)