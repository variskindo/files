library(dplyr)
library(TSA)
library(tseries)
library(forecast)

datos <- read.table (file = "clipboard", dec = ".", sep = "\t", header = T)
datos <- filter (datos, datos$NombreDepartamento == "LA GUAJIRA")
precio <- as.numeric(datos$precio)
precio <- matrix(precio,12028,1)

n <- 11
x <- c()

for (i in 1:1092) {
  x[i+1] <- mean(precio[i*11+1 : (1+i)*11,])
  x
}


precio[12013:12023,]
mean(precio[1092*11+1:12023,])


serie_precio = ts(precio,frequency=1,start=c(2017,01))
fechas = seq(as.Date("2017/01/01"), length.out = length(precio), by = "day")
