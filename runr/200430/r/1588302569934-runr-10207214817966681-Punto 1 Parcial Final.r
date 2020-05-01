rm(list=ls())

bd3<-read.table("clipboard",header = T, dec = ",")
attach(bd3)

CDT90

cdt90<-ts(CDT90, frequency = 12, start = c(2015,1))
par(mfrow=c(1,1))
plot(cdt90,main="Tasa de interés del CDT90 desde \n 2015-1 hasta 2019-2")


###TRANSFORMANDO LA VARIANZA
library(MASS)
bc1<-boxcox(cdt90~1)

bc1

lam<-bc1$x[which.max(bc1$y)]
### Según el resultado de Box-Cox, se aplica inversa a la serie

rcdt<-1/cdt90

par(mfrow=c(1,2))
plot(rcdt, main="Inversa del CDT90")
plot(cdt90, main="CDT90")


####SE APLICA LA PRUEBA DE RAÍZ UNITARIA

###DICKEY-FULLER
##H0: pi=0 H1: pi<0

library(urca)


dfcdt.In<-ur.df(rcdt, lags = 3, type = "drift") ##Con intercepto
summary(dfcdt.In)


##No hay suficiente evidencia para rechazar H0. La inversa del CDT es I(1)


drcdt<-diff(rcdt)

par(mfrow=c(1,1))
plot(drcdt, main="Inversa del CDT90 (Primera diferencia)")

###Pruebas de Raíz unitaria para la primera diferenciación

dfdcdt.Tr<-ur.df(drcdt, lags = 3, type = "drift") ##DICKEY-FULLER
summary(dfdcdt.Tr)

##LAS PRUEBAS SUGIEREN QUE LA PRIMERA DIFERENCIACIÓN ES I(0)



####ESTIMANDO EL MODELO


par(mfrow=c(1,3))
plot(drcdt)
acf(drcdt, lag.max = 13, ylim = c(-1,1))
pacf(drcdt, lag.max = 13, ylim = c(-1,1))


##Modelos
library(tseries)
m1<-arma(drcdt, order = c(1,0))
summary(m1)

m1.1<-arima(drcdt, order = c(1,0,0))
summary(m1.1)

m1.2<-arima(rcdt, order = c(1,1,0))
m1.2
summary(m1.2)



##Validación de los supuestos ##

res<-m1.2$resid



par(mfrow=c(1,3))
plot(res)
acf(res, lag.max = 13, ylim = c(-1,1))
pacf(res, lag.max = 13, ylim = c(-1,1))


###PRUEBA LJUNG-BOX
##H0: pk=0 vs H1: pk diferente de 0
Box.test(res, type = "Ljung-Box", lag = 13)

##La Ljung-Box, dice que no hay suficiente evidencia para rechazar H0. Luego,
###Las autocorrelaciones se asumen 0

###PRUEBA DE WHITE PARA HETEROSCEDASTICIDAD
white.test(res, lag = 3)



jarque.bera.test(res)



###PROBEMOS CON UN MA (1)

m2<-arma(drcdt, order = c(0,1))
summary(m2)

m2.1<-arima(drcdt, order = c(0,0,1))
summary(m2.1)

m2.2<-arima(rcdt, order = c(0,1,1))
m2.2
summary(m1.2)


##Validación de los supuestos ##

res1<-m2.2$resid



par(mfrow=c(1,3))
plot(res1)
acf(res1, lag.max = 13, ylim = c(-1,1))
pacf(res1, lag.max = 13, ylim = c(-1,1))


###PRUEBA LJUNG-BOX
##H0: pk=0 vs H1: pk diferente de 0
Box.test(res1, type = "Ljung-Box", lag = 13)

##La Ljung-Box, dice que no hay suficiente evidencia para rechazar H0. Luego,
###Las autocorrelaciones se asumen 0

###PRUEBA DE WHITE PARA HETEROSCEDASTICIDAD
white.test(res1, lag = 3)


jarque.bera.test(res1)



###PROBEMOS CON UN ARMA (1,1)

m3<-arma(drcdt, order = c(1,1))
summary(m3)

m3.1<-arima(drcdt, order = c(1,0,1))
summary(m3.1)

m3.2<-arima(rcdt, order = c(1,1,1))
m3.2
summary(m3.2)


##Validación de los supuestos ##

res2<-m3.2$resid



par(mfrow=c(1,3))
plot(res2)
acf(res2, lag.max = 13, ylim = c(-1,1))
pacf(res2, lag.max = 13, ylim = c(-1,1))


###PRUEBA LJUNG-BOX
##H0: pk=0 vs H1: pk diferente de 0
Box.test(res2, type = "Ljung-Box", lag = 13)

##La Ljung-Box, dice que no hay suficiente evidencia para rechazar H0. Luego,
###Las autocorrelaciones se asumen 0

###PRUEBA DE WHITE PARA HETEROSCEDASTICIDAD
white.test(res2, lag = 3)

##

jarque.bera.test(res2)








##Pronósticos
pron<-predict(m1.2, n.ahead = 5)
pron

x11()
ts.plot(rcdt,pron$pred, col=8:10)
lines(pron$pred+pron$se,lty="dashed", col=1)
lines(pron$pred-pron$se,lty="dashed", col=1)


library(forecast)
par(mfrow=c(1,1))
plot(forecast(m1.2,5,level = 0.95))



