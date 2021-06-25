source("simuldata.fun.r")
library(SGL)
library(ipflasso)
library(mvtnorm)

dir.create("results_correlated")
dir.create("results_uncorrelated")

#######################################
#######################################
## Simulation with uncorrelated data ##
#######################################
#######################################

###############
# Main design #
###############


# B
simulation(n=100,p1=100,p2=1000,beta1=0.5,beta2=0.5,prel1=3,prel2=30,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),plot=FALSE,dir="results_uncorrelated")

# C
simulation(n=100,p1=100,p2=1000,beta1=0.5,beta2=0.5,prel1=10,prel2=10,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),plot=FALSE,dir="results_uncorrelated")

# D
simulation(n=100,p1=100,p2=1000,beta1=0.3,beta2=0.3,prel1=20,prel2=0,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),dir="results_uncorrelated")

# E
simulation(n=100,p1=20,p2=1000,beta1=1,beta2=0.3,prel1=3,prel2=10,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),dir="results_uncorrelated")

# F
simulation(n=100,p1=20,p2=1000,beta1=0.5,beta2=0.5,prel1=15,prel2=3,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),dir="results_uncorrelated")

# A
simulation(n=100,p1=1000,p2=1000,beta1=0.5,beta2=0.5,prel1=10,prel2=10,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",plot=FALSE,alpha=1,competitors="all",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),dir="results_uncorrelated")

###################
# Correlated data #
###################

library(mvtnorm)
library(matrixcalc)

sigma1000.1000<-covMatrix(p=c(1000,1000),b=10,independentModalities=FALSE,rho=0.4)
sigma100.1000<-covMatrix(p=c(100,1000),b=10,independentModalities=FALSE,rho=0.4)
sigma20.1000<-covMatrix(p=c(20,1000),b=10,independentModalities=FALSE,rho=0.4)

# A
simulation(n=100,p1=1000,p2=1000,beta1=0.5,beta2=0.5,prel1=10,prel2=10,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",sigma=sigma1000.1000,alpha=1,competitors="notall",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(1,16),c(2,1),c(4,1),c(8,1),c(16,1)),dir="results_correlated")

# B
simulation(n=100,p1=100,p2=1000,beta1=0.5,beta2=0.5,prel1=3,prel2=30,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",,sigma=sigma100.1000,alpha=1,competitors="notall",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(1,16),c(2,1),c(4,1),c(8,1),c(16,1)),dir="results_correlated")

# C
simulation(n=100,p1=100,p2=1000,beta1=0.5,beta2=0.5,prel1=10,prel2=10,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",sigma=sigma100.1000,alpha=1,competitors="notall",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(1,16),c(2,1),c(4,1),c(8,1),c(16,1)),dir="results_correlated")

# D
simulation(n=100,p1=100,p2=1000,beta1=0.3,beta2=0.3,prel1=20,prel2=0,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",sigma=sigma100.1000,alpha=1,competitors="notall",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(1,16),c(2,1),c(4,1),c(8,1),c(16,1)),dir="results_correlated")

# E
simulation(n=100,p1=20,p2=1000,beta1=1,beta2=0.3,prel1=3,prel2=10,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",sigma=sigma20.1000,alpha=1,competitors="notall",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(1,16),c(2,1),c(4,1),c(8,1),c(16,1)),dir="results_correlated")

# F
simulation(n=100,p1=20,p2=1000,beta1=0.5,beta2=0.5,prel1=15,prel2=3,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",sigma=sigma20.1000,alpha=1,competitors="notall",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(1,16),c(2,1),c(4,1),c(8,1),c(16,1)),dir="results_correlated")


#######################
# Additional settings #
#######################

simulation(n=100,p1=500,p2=500,beta1=0.5,beta2=0.5,prel1=10,prel2=10,prop1=0.5,
			family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
			niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),plot=FALSE,dir="results_uncorrelated")

simulation(n=100,p1=500,p2=500,beta1=0.5,beta2=0.5,prel1=20,prel2=0,prop1=0.5,
			family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
			niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),plot=FALSE,dir="results_uncorrelated")

simulation(n=100,p1=500,p2=500,beta1=0.3,beta2=0.8,prel1=10,prel2=10,prop1=0.5,
			family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
			niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),plot=FALSE,dir="results_uncorrelated")

simulation(n=100,p1=100,p2=1000,beta1=0.5,beta2=0.5,prel1=20,prel2=0,prop1=0.5,
			family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
			niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),plot=FALSE,dir="results_uncorrelated")

simulation(n=100,p1=100,p2=1000,beta1=0.5,beta2=0.5,prel1=0,prel2=20,prop1=0.5,
			family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
			niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),plot=FALSE,dir="results_uncorrelated")

simulation(n=100,p1=100,p2=100,beta1=0.5,beta2=0.5,prel1=10,prel2=10,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",plot=FALSE,alpha=1,competitors="all",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),dir="results_uncorrelated")

simulation(n=100,p1=500,p2=500,beta1=1,beta2=1.5,prel1=3,prel2=3,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",plot=FALSE,alpha=1,competitors="all",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),dir="results_uncorrelated")

simulation(n=100,p1=500,p2=500,beta1=1,beta2=0.3,prel1=20,prel2=20,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",plot=FALSE,alpha=1,competitors="all",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),dir="results_uncorrelated")

simulation(n=100,p1=500,p2=500,beta1=1,beta2=1,prel1=20,prel2=0,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),plot=FALSE,dir="results_uncorrelated")

simulation(n=100,p1=500,p2=500,beta1=0.3,beta2=0.3,prel1=40,prel2=0,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),plot=FALSE,dir="results_uncorrelated")

simulation(n=100,p1=500,p2=500,beta1=0.5,beta2=0.5,prel1=40,prel2=0,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),plot=FALSE,dir="results_uncorrelated")

simulation(n=100,p1=500,p2=500,beta1=0.5,beta2=0.5,prel1=20,prel2=10,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),plot=FALSE,dir="results_uncorrelated")

simulation(n=100,p1=500,p2=500,beta1=0.3,beta2=1.5,prel1=10,prel2=10,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),plot=FALSE,dir="results_uncorrelated")

simulation(n=100,p1=500,p2=500,beta1=0.4,beta2=0.7,prel1=10,prel2=10,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),plot=FALSE,dir="results_uncorrelated")

simulation(n=100,p1=20,p2=2000,beta1=0.5,beta2=0.5,prel1=10,prel2=10,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),plot=FALSE,dir="results_uncorrelated")

simulation(n=100,p1=300,p2=800,beta1=0.5,beta2=0.5,prel1=10,prel2=10,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),plot=FALSE,dir="results_uncorrelated")

simulation(n=100,p1=20,p2=2000,beta1=1,beta2=0.3,prel1=10,prel2=10,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),plot=FALSE,dir="results_uncorrelated")

simulation(n=100,p1=20,p2=2000,beta1=0.5,beta2=0.5,prel1=1,prel2=100,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),plot=FALSE,dir="results_uncorrelated")

simulation(n=100,p1=300,p2=800,beta1=0.8,beta2=0.8,prel1=3,prel2=8,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),plot=FALSE,dir="results_uncorrelated")

simulation(n=100,p1=100,p2=1000,beta1=1,beta2=0.3,prel1=3,prel2=30,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),plot=FALSE,dir="results_uncorrelated")

simulation(n=100,p1=20,p2=2000,beta1=0.5,beta2=0.5,prel1=20,prel2=0,prop1=0.5,
                  family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
                  niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),dir="results_uncorrelated")

simulation(n=100,p1=300,p2=800,beta1=0.5,beta2=0.5,prel1=20,prel2=0,prop1=0.5,
                  family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
                  niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),dir="results_uncorrelated")

simulation(n=100,p1=100,p2=1000,beta1=0.5,beta2=0.5,prel1=20,prel2=10,prop1=0.5,
                  family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
                  niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),dir="results_uncorrelated")

simulation(n=100,p1=100,p2=1000,beta1=0.5,beta2=0.5,prel1=40,prel2=0,prop1=0.5,
                  family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
                  niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),dir="results_uncorrelated")

simulation(n=100,p1=100,p2=1000,beta1=1,beta2=1,prel1=20,prel2=0,prop1=0.5,
                  family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
                  niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),dir="results_uncorrelated")

simulation(n=100,p1=20,p2=2000,beta1=0.5,beta2=0.5,prel1=0,prel2=20,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),dir="results_uncorrelated")

simulation(n=100,p1=300,p2=800,beta1=0.5,beta2=0.5,prel1=0,prel2=20,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),dir="results_uncorrelated")

simulation(n=100,p1=100,p2=1000,beta1=1,beta2=1,prel1=0,prel2=20,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),dir="results_uncorrelated")

simulation(n=100,p1=100,p2=1000,beta1=0.5,beta2=0.5,prel1=0,prel2=40,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),dir="results_uncorrelated")

simulation(n=100,p1=100,p2=1000,beta1=0.5,beta2=0.5,prel1=10,prel2=20,prop1=0.5,
           family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
           niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),dir="results_uncorrelated")

simulation(n=100,p1=20,p2=1000,beta1=1,beta2=0.3,prel1=5,prel2=0,prop1=0.5,
                  family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
                  niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),dir="results_uncorrelated")

simulation(n=100,p1=20,p2=1000,beta1=0.5,beta2=0.5,prel1=10,prel2=10,prop1=0.5,
                  family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
                  niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),dir="results_uncorrelated")

simulation(n=100,p1=20,p2=1000,beta1=1,beta2=1,prel1=3,prel2=3,prop1=0.5,
                  family="binomial",ncv=10,nfolds=5,type.measure="class",alpha=1,competitors="all",
                  niter=100, pflist=list(c(1,1),c(1,2),c(1,4),c(1,8),c(2,1),c(4,1),c(8,1)),dir="results_uncorrelated")
