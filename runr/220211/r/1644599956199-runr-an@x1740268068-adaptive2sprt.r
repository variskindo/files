source("core_bernoulli.R")
n=975
s=seq(0,n)
th0=0.05
th1=0.15
th=0.1
f1<-function(s,n,th){ifelse(s==0,n*log(1-th),ifelse(s==n,n*log(th),s*log(th/(1-th))+n*log(1-th)-s*log(s)-(n-s)*log(n-s)+n*log(n)))< -5}
# !(f1(s,n,th0) |f1(s,n,th1))
# 
# !(f1(s,n,th))
# rm(cont)
H=n
cont=list()
accept=list()
accept[[H]]=!(f1(seq(0,H),H,th0))
cont[[H]]=rep(FALSE,H+1)
for(i in 1 : (H-1)){
s=seq(0,i)
cont[[i]]=!(f1(s,i,th0) |f1(s,i,th1))
accept[[i]]=!(f1(s,i,th0))
}
test=rbind(cont,accept)
1-operating_characteristic(test,th0)
operating_characteristic(test,th1)
average_sample_number(test,th)
average_sample_number(test,th0)
average_sample_number(test,th1)
average_sample_number(test,th+0.01)
max_number(test)
