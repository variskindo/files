#Limpa o Workspace
rm(list=ls())

#Carregando a serie temporal:
yt=scan("ipca.txt")
n=length(yt)
yt
str(yt)
head(yt)
summary(yt)
cv=sd(yt)/mean(yt)
cv
library("moments")
skewness(yt)
kurtosis(yt)
#Graficos:
ts.plot(yt,xlab="t")
#Criando um objeto TS():
Yt=ts(yt,start=c(1997,7),frequency=12)
str(Yt)
head(Yt)
Yt
dev.new()
plot(Yt)
title("IPCA")

#Criando sequencias de datas mensais:
at=seq(as.Date("1997/07/01"),as.Date("2009/03/01"),"months")
at
str(at)
head(at)
#Semanas:
at=seq(as.Date("1997/07/01"),as.Date("2009/03/01"),"weeks")
str(at)
head(at)
#Dias:
at=seq(as.Date("1997/07/01"),as.Date("2009/03/01"),"days")
str(at)
head(at)

#Usando o pacote chron para criar a sequencia:
if(!require("chron")) {install.packages("chron"); library("chron")}
Weeks<-seq.dates("12/06/11","04/10/12",by="weeks")
str(Weeks)
head(Weeks)

#Grafico Serie IPCA:
at=seq(as.Date("1997/07/01"),as.Date("2009/03/01"),"months")
dev.new()
plot(at,yt,xlab="t",ylab="Yt",type='l')
title("IPCA")

#Grafico Serie IPCA de uma maneira diferente:
at=seq(as.Date("1997/07/01"),as.Date("2009/03/01"),"months")
dev.new()
plot(at,yt,xlab="t",ylab="Yt",type='l',axes=F)
axis.Date(1,at=at,labels=TRUE,tcl=-0.2,format="%m-%Y",cex.axis=0.8)
axis(2,at=seq(min(yt),max(yt),0.3),las=1,tck=+0.01,cex.axis=0.8)
title("IPCA")

# Autocorrelation function:
yt1=c(1,1.5,1.8,2,4)
ts.plot(yt1)
acf(yt1,plot=FALSE)
acf(yt1)

# Autocorrelation function serie IPCA:
acf(yt,plot=FALSE)
# Autocorrelogram:
acf(yt,lag=36)
dev.new()
pacf(yt,lag=36)
shapiro.test(yt)
dev.new()
qqnorm(yt)


#Teste de Ljung-Box e Box-Pierce para autocorrelacao:
x<-rnorm(50)
library("tseries")
Box.test(x,lag=1,type="Ljung-Box")
#Serie IPCA:
#Default: Box-Pierce
Box.test(yt,lag=1)
Box.test(yt,lag=6)
Box.test(yt,lag=12)
Box.test(yt,lag=24)
Box.test(yt,lag=36)

Box.test(yt,lag=1,type="Ljung-Box")
Box.test(yt,lag=6,type="Ljung-Box")
Box.test(yt,lag=12,type="Ljung-Box")
Box.test(yt,lag=24,type="Ljung-Box")
Box.test(yt,lag=36,type="Ljung-Box")

library("tseries")
# Testes de raiz unitaria serie IPCA:
adf.test(yt)
PP.test(yt)

#Serie ICV:
yt=scan("icv.txt")
summary(yt)
cv=sd(yt)/mean(yt)
cv
library("moments")
skewness(yt)
kurtosis(yt)
ts.plot(yt)
dev.new()
# Autocorrelogram:
acf(yt,lag=36)
Box.test(yt,lag=6,type="Ljung-Box")
adf.test(yt)
PP.test(yt)

# Outro Exemplo:
x<-rnorm(1000)
adf.test(x)
PP.test(x)
y<-cumsum(x) # não estacionaria
adf.test(y)
PP.test(y)



