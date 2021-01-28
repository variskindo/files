
#CÓDIGO CURSO DE INTRODUCCIÓN AL ARBITRAJE ESTADÍSTICO

#ricovictor.com


# Usaremos sólo 2 pares de divisas
# Preferentemente con valor del pip parecidos y cotizaciones
   #usdnok eurusd -- no 
# No haremos transformaciones en las series

# al mirar las betas no puede haber mucha diferencia

# Usaremos 2 métodos, con filtro de cointegración y sin él
# Temporalidad Diaria- así podrá usarlo cualquier persona
# métodos de salida y entrada








#install.packages("quantmod")

library(quantmod)



library("tseries")

library("fImport")

#library("urca")
#library("vars")

#???getFX(c("EUR/SEK","EUR/NOK","EUR/USD","GBP/USD","USD/CHF","EUR/AUD","USD/CAD","USD/JPY","AUD/NZD","AUD/USD","EUR/NZD","EUR/GBP","GBP/CAD","GBP/AUD","GBP/JPY","EUR/CAD","AUD/JPY","EUR/JPY","GBP/CHF","EUR/CHF","NZD/USD","NZD/JPY"),from="2012-01-01")
#getFX(c("EUR/SEK","EUR/NOK","EUR/USD","GBP/USD","USD/CHF","EUR/AUD","USD/CAD","USD/JPY","AUD/NZD","AUD/USD","EUR/NZD","EUR/GBP","GBP/CAD","GBP/AUD","GBP/JPY","EUR/CAD","AUD/JPY","EUR/JPY","GBP/CHF","EUR/CHF","NZD/USD","NZD/JPY"))


# 1 Descarga de datos Yahoo 

getSymbols("EURUSD=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("GBPUSD=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("EURGBP=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("USDNOK=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("USDSEK=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("USDCHF=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("EURCHF=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("AUDUSD=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("NZDUSD=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("AUDJPY=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("NZDJPY=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("EURJPY=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("GBPJPY=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("USDCAD=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("GBPCAD=X",quote="Close",from ="2012-01-20",periodicity = "daily")
#getSymbols("^DJI",quote="Close",from ="2012-01-01",periodicity = "daily")
getSymbols("GBPAUD=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("AUDCAD=X",quote="Close",from ="2012-01-20",periodicity = "daily")
#getSymbols("^DJI",quote="Close",from ="2012-01-01",periodicity = "daily")
getSymbols("NZDCAD=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("AUDNZD=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("USDJPY=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("EURCAD=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("EURNZD=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("EURAUD=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("GBPCHF=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("NZDCHF=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("CHFJPY=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("NZDJPY=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("CADJPY=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("CADCHF=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("AUDCHF=X",quote="Close",from ="2012-01-20",periodicity = "daily")
getSymbols("GBPNZD=X",quote="Close",from ="2012-01-20",periodicity = "daily")



##
##SEMANAL
#getSymbols("CADCHF=X",quote="Close",from ="2008-01-10",periodicity = "weekly")
#getSymbols("AUDCHF=X",quote="Close",from ="2008-01-10",periodicity = "weekly")
##
getSymbols("EURCNH=X",quote="Close",from ="2012-01-10",periodicity = "daily")
getSymbols("USDTRY=X",quote="Close",from ="2012-01-10",periodicity = "daily")
getSymbols("USDHKD=X",quote="Close",from ="2012-01-10",periodicity = "daily")
getSymbols("USDZAR=X",quote="Close",from ="2012-01-10",periodicity = "daily")
getSymbols("USDMXN=X",quote="Close",from ="2012-01-10",periodicity = "daily")
getSymbols("EURMXN=X",quote="Close",from ="2012-01-10",periodicity = "daily")
getSymbols("EURZAR=X",quote="Close",from ="2012-01-10",periodicity = "daily")
getSymbols("GBPNZD=X",quote="Close",from ="2012-01-20",periodicity = "daily")

getSymbols("EURTRY=X",quote="Close",from ="2012-01-10",periodicity = "daily")
getSymbols("USDPLN=X",quote="Close",from ="2012-01-10",periodicity = "daily")
getSymbols("USDHUF=X",quote="Close",from ="2012-01-10",periodicity = "daily")

#getSymbols("BTC-USD",quote="Close",from ="2012-01-10",periodicity = "daily")
#getSymbols("ETH-USD",quote="Close",from ="2012-01-10",periodicity = "daily")
#getSymbols("LTC-USD",quote="Close",from ="2012-01-10",periodicity = "daily")

#oanda #fred 

# yahoo investing - excel

# 

#1 eurchf-eurusd
# audcad-audnzd

#2 audcad-nzdchf
# eurgbp-audcad

dep=`EURUSD=X`[,4]    # y o variable dependiente
indep=`GBPUSD=X`[,4]  # x o variable dependiente

tail(dep)[6]
tail(indep)[6]

plot(dep,type="l")
plot(indep,type="l")


# ventana de tiempo


vent<-120
vent2<- vent-1

## Regresion lineal- para sacar la beta

beta=c()
for(i in vent:length(dep))
{f=lm(dep[(i-vent2):i]~indep[(i-vent2):i]-1) # -1 para eliminar el intercepto
beta[i]=coef(f)}

plot(beta,type="l")

BETA<-na.omit(beta)
BETA

#na.omit(adf.test(BETA))

summary(BETA)



#### Spread de Arbitraje -- tal como se explica en el vídeo


y=c()
for(i in vent :length(dep))
{
  y[i]=beta[i]*indep[i]-dep[i]
}
plot(y,type="l")

SPREAD <-na.omit(y)
tail(SPREAD)
plot(SPREAD,type="l")

####

estacionariedad<- adf.test(SPREAD)
estacionariedad

#estacionareidad si es menor 0.05-0.1

summary(SPREAD)


sd(SPREAD)
sd(SPREAD)*2

plot(SPREAD[100:length(SPREAD)],type="l")



#desviacion típica

summary(na.omit(y))
mean(na.omit(y))
meany=mean(na.omit(y))
meany

sd(na.omit(y))
2*sd(na.omit(y))

sd1= mean(na.omit(y))+sd(na.omit(y))
sd1
sd11= mean(na.omit(y))-sd(na.omit(y))
sd11
sd2=mean(na.omit(y))+2*sd(na.omit(y))
sd2
sd22=mean(na.omit(y))-2*sd(na.omit(y))
sd22

#Máxima pérdida

max(na.omit(y))- sd1
min(na.omit(y))-sd11

abline(mean(na.omit(y)),0,col="green")
abline(mean(na.omit(y))+sd(na.omit(y)),0,col="orange")
abline(mean(na.omit(y))-sd(na.omit(y)),0,col="orange")
abline(mean(na.omit(y))+2*sd(na.omit(y)),0,col="blue")
abline(mean(na.omit(y))-2*sd(na.omit(y)),0,col="blue")


#cantidad a meter BETA

tail(beta)[6]
#0.8820873

#comprariamos de x(GBPUSD)0.88 por cada 1 EURUSD -vende

#0.08 - 0.1
#0.01 -0.01




#length(SPREAD)

if (estacionariedad$p.value<0.1)  # si es menor que 0.05-0.1 existe cointegración,por tanto seguimos para crear spread
{
  spread=y[(length(y)-2000):length(y)]
  MEANY<-mean(na.omit(spread))
  plot(spread,type="l")
  abline(mean(na.omit(spread)),0,col="green")
  abline(mean(na.omit(spread))+sd(na.omit(spread)),0,col="orange")
  abline(mean(na.omit(spread))-sd(na.omit(spread)),0,col="orange")
  abline(mean(na.omit(spread))+1.5*sd(na.omit(spread)),0,col="yellow")
  
  abline(mean(na.omit(spread))-1.5*sd(na.omit(spread)),0,col="yellow")
  
  abline(mean(na.omit(spread))+2*sd(na.omit(spread)),0,col="blue")
  abline(mean(na.omit(spread))-2*sd(na.omit(spread)),0,col="blue")
  if(tail(spread,n=1)>(mean(na.omit(spread))+sd(na.omit(spread)))){
    cat("Vender",tail(beta,n=1),"de X  por cada y comprado ")
    cant.y= 1
    cant.x= -(tail(beta,n=1))
    
  }
  if(tail(spread,n=1)<(mean(na.omit(spread))-sd(na.omit(spread)))){
    cat("Comprar",tail(beta,n=1),"de X por cada y vendido ")
    cant.y= -1
    cant.x= tail(beta,n=1)
    
  }
}else
{
  ("No hay cointegracion")
} 


tail(dep)[6] # y
tail(indep)[6] # X


#### Ajuste de posición,seguimiento y expectativa de DD



tail(spread)[6]


tail(spread)[6]- MEANY

tail(beta,n=1)

abs(tail(spread)[6])- abs(MEANY)

summary(SPREAD)

#sd(SPREAD)
#sd(SPREAD)*2


#### reflexiones 

# poco lotaje -- al menos hasta que controleis los flotantes
# cuentas pequeñas- meted la misma - y pocos spread
# pocos spread- 1000 - 3 spread 0.01 o 0.02
# 1.5 o 2---- 1- 0.5



