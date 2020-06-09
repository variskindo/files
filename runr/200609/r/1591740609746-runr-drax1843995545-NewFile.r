set.seed(2020)
xy<-expand.grid(x=seq(1,50,1),y=seq(1,20,1))
edad<-gl(10,100,1000,labels=seq(5,50,5))
prod2019<-round(runif(1000,2,7)) # Numero de Racimos al año
x <- 
plot(xy,cex=prod2019/8,col=edad,pch=16,ylab="hilera",xlab="palma")
df=data.frame(x=seq(1,50,1),y=seq(1,20,1),edad,prod2019,Id=c(1:1000))

dim(df)
library(clhs) # Conditioned Latin Hypercube Sampling
muestrase<-clhs(df[,1:4],size=50,iter=2000,progress=F)
muestrase

p3=df[muestrase,]
namep3<-p3$Id;namep3
plot(xy,cex=prod2019/8,col=edad,pch=16,ylab="hilera",xlab="palma")
points(p3$x,p3$y,cex=1.5)