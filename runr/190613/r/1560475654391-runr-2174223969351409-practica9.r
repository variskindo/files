
Temperatura<-c(-18,-15,-10,-5,0,5,10,19)
Temperatura
consumo<-c(5.2,4.7,4.5,3.6,3.4,3.1,2.7,1.8)
consumo
aves <- data.frame(Temperatura,consumo)
aves
plot(aves)

m <- lm(consumo~Temperatura, data=aves)
aves
m
plot(consumo~temperatura, data=aves, pch=18,col="gray30",xlab="Temp (C)", 
     ylab="Consumo de oxigeno (ml/g/h)",xlim=c(-20,20), ylim=c(0,6))
abline(m, col="red")
summary(m)

anova (m)
anova(m)
coef(m)
confint(m)
 
librerias datasets
data("airquality")
data
plot(airquality$Temp,airquality$Ozone, xlab="Temperatura", 
     ylab="Ozono", pch=21, col="steelblue")
cor.test(airquality$Temp,airquality$Ozone, data=airquality, method="pearson")

arnew<-na.omit(airquality)
arnew
cor(arnew,method="pearson")
cor

plot(airquality)


g1<-c(86,71,77,68,91,72,77,91,70,71,88,87)
g1
g2<-c(88,77,76,64,96,72,65,90,65,80,81,72)
g2
plot(g1, g2, pch=15, col="seagreen", main=" Valores de agresividad en gemelos identicos ", xlab="gemelo 1", ylab="gemelo 2", cex=1.4)
grid(col="gray80")
grid
plot
cor.test(g1,g2, method ="spearman")
