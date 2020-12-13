library(pdfetch) 
library(ggplot2)
library(zoo)
library(reshape2) # funcion melt
library(psych) #analisis de correlacion con pirotecnia
library(gridExtra) #combinacion de graficos en ggplot

rm(list = ls()) # lipieza de datos
# Datos Banco Mundial -----------------------------------------------------


lista_paises = c("ARG", "BOL", "CHL", "PER") #seleccion de paises
lista_indicadores = c("BX.KLT.DINV.WD.GD.ZS") # seleccion del indicador
fecha_inicio = as.Date("2000-01-01")
fecha_fin = as.Date("2021-12-31")

datos_paises = pdfetch_WB(lista_indicadores, lista_paises) #traida de datos

#seleccion de datos por fecha
datos_paises = datos_paises[as.Date(index(datos_paises)) >= fecha_inicio &
                              as.Date(index(datos_paises)) <= fecha_fin]



#renombramiento de columnas
colnames(datos_paises) = c("Argentina", "Bolivia", "Chile", "Peru")

#formato de fechas por Años

#indicacion de fechas
indice_fechas = as.Date(index(datos_paises))
indice_fechas = format(indice_fechas, format="%Y")
datos_paises = as.data.frame(datos_paises)
datos_paises$fecha = indice_fechas

# Estadisticas descriptivas -----------------------------------------------

lista_color = c("#00aeef", "#153E90", "#EF7215", "#777777")


datos_paises_b = melt(datos_paises, id.vars = c("fecha")) #formato largo
colnames(datos_paises_b) = c("fecha", "Pais", "Valor")

par(mfrow=c(1,1))
plot(datos_paises$fecha, datos_paises$Argentina, type="b", col=lista_color[1], 
     lwd=3, xlab="Año", ylab="% del PIB ", pch=19, 
     ylim=c(min(datos_paises[ ,1:4]), max(datos_paises[ ,1:4])),
     main="Flujo de inversion extranjera por paises (2000-2019)",
     sub="Fuente: Banco Mundial", cex.sub = 0.8)
lines(datos_paises$fecha, datos_paises$Bolivia, type="b", col=lista_color[2], 
      lwd=3, pch=19)
lines(datos_paises$fecha, datos_paises[, 3], type="b", col=lista_color[3], 
      lwd=3, pch=19)
lines(datos_paises$fecha, datos_paises[, 4], type="b", col=lista_color[4], 
      lwd=3, pch=19)
legend("bottomright",lista_paises, lwd=3, col=lista_color, horiz=TRUE, cex=0.6,
       inset=.01)




ggplot(as.data.frame(datos_paises_b), aes(x = Pais, y = Valor))+
  geom_boxplot(fill = lista_color[1:4])+theme_light()+
  labs(y = "Inversion extranjera (% PIB)", x = "Pais", 
        title = "Flujo de inversión extranjera por paises (2000-2019)",
       caption= "Fuente: Banco Mundial")+
  theme(axis.title.x = element_text(size = 10), plot.title = element_text(size=14))

par(mfrow=c(1,1))
boxplot(formula = Valor ~ Pais, data =  datos_paises_b, xlab = "Pais", 
        ylab = "Inversion extranjera (% PIB)", 
        col = lista_color[1:4], main="Flujo de inversión extranjera por paises (2000-2019)")


par(mfrow=c(2,2))
boxplot(x=datos_paises$Argentina, main="Flujo Inv. Ext. Argentina (2000-2019)", 
        col=lista_color[1], ylab= "% del PIB")
boxplot(datos_paises$Bolivia, main="Flujo Inv. Ext. Bolivia (2000-2019)", 
        col=lista_color[2], ylab= "% del PIB ")
boxplot(x=datos_paises$Chile, main="Flujo Inv. Ext. Chile (2000-2019)", 
        col=lista_color[3], ylab= "% del PIB")
boxplot(datos_paises$Peru, main="Flujo Inv. Ext. Perú (2000-2019)", 
        col=lista_color[4], ylab= "% del PIB",
        sub="Fuente: Banco Mundial", cex.sub = 1)



#png(filename = "/Users/josemanuelizquierdo/Dropbox/Clases\ JMI\ Uandes/Econometria\ 1-2021/Clases/R\ Files/Clase\ 1/subplot_boxplot.png",
 #   width = 1600, height = 1200)
#par(mfrow=c(2,2))
#boxplot(x=datos_paises$Argentina, main="Flujo Inv. Ext. Argentina (2000-2019)", 
#        col=lista_color[1], ylab= "% del PIB")
#boxplot(datos_paises$Bolivia, main="Flujo Inv. Ext. Bolivia (2000-2019)", 
#        col=lista_color[2], ylab= "% del PIB ")
#boxplot(x=datos_paises$Chile, main="Flujo Inv. Ext. Chile (2000-2019)", 
#        col=lista_color[3], ylab= "% del PIB")
#boxplot(datos_paises$Peru, main="Flujo Inv. Ext. Peru (2000-2019)", 
#       col=lista_color[4], ylab= "% del PIB")
#dev.off()
#file.show(""/Users/josemanuelizquierdo/Dropbox/Clases\ JMI\ Uandes/Econometria\ 1-2021/Clases/R\ Files/Clase\ 1/subplot_boxplot.png"")




# Analisis de Varianza Covarianza -----------------------------------------


par(mfrow=c(1,1))
plot(datos_paises$fecha, datos_paises$Chile, pch=19, cex=1.25, col=lista_color[1],
     xlab="Año", ylab="% del PIB ",main="Flujo inversion extranjera Chile (2000-2019)",
     sub="Fuente: Banco Mundial", cex.sub = 0.8)
abline(h=mean(datos_paises$Chile), col="black", lwd=2, lty=2)



par(mfrow=c(2,2))
plot(datos_paises$fecha, datos_paises$Argentina, pch=19, cex=1.25, col=lista_color[1],
     xlab="Año", ylab="% del PIB ",main="Flujo Inv. Ext. Argentina \n(2000-2019)")
abline(h=mean(datos_paises$Argentina), col="red", lwd=2, lty=2)
plot(datos_paises$fecha, datos_paises$Bolivia, pch=19,cex=1.25 ,col=lista_color[2],
     xlab="Año", ylab="% del PIB ",main="Flujo Inv. Ext. Bolivia \n(2000-2019)")
abline(h=mean(datos_paises$Bolivia), col="red", lwd=2, lty=2)
plot(datos_paises$fecha, datos_paises$Chile, pch=19, cex=1.25, col=lista_color[3],
     xlab="Año", ylab="% del PIB ",main="Flujo Inv. Ext. Chile \n(2000-2019)")
abline(h=mean(datos_paises$Chile), col="red", lwd=2, lty=2)
plot(datos_paises$fecha, datos_paises$Peru, pch=19, cex=1.25, col=lista_color[4],
     xlab="Año", ylab="% del PIB ",main="Flujo Inv. Ext. Perú \n(2000-2019)",
     sub="Fuente: Banco Mundial", cex.sub = 1)
abline(h=mean(datos_paises$Peru), col="red", lwd=2, lty=2)


## Lolliplots

#Varianza Chile
ggplot(datos_paises, aes(x=fecha, y=Chile)) +
  geom_segment( aes(x=fecha, xend=fecha, y=mean(datos_paises$Chile),
                    yend=Chile), color="grey") +
  geom_point( color=lista_color[1], size=4) +
  labs(y = "Inversion extranjera (% PIB)", x = "Año", 
       title = "Flujo de inversión extranjera Chile (2000-2019)",
       caption= "Fuente: Banco Mundial")+
  theme_bw() +
  geom_hline(yintercept=mean(datos_paises$Chile), linetype="dashed", 
             color = "black", size=1)+
  theme(panel.border = element_rect(color = "black", fill=NA, size=0.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=16, hjust = 0.5)) 

# Covarianza Chile-Peru
ggplot(datos_paises, aes(x=fecha, y=Chile)) +
  geom_segment( aes(x=fecha, xend=fecha, y=mean(datos_paises$Chile),
                    yend=Chile), color="grey") +
  geom_point(color=lista_color[1], size=4) +
  geom_segment(aes(x=fecha, xend=fecha, y=mean(datos_paises$Peru),
                    yend=Peru), color="black") +
  geom_point(data=datos_paises, aes(x=fecha, y=Peru), color=lista_color[2], size=4) +
  labs(y = "Inversion extranjera (% PIB)", x = "Año", 
       title = "Flujo de inversión extranjera Chile v/s Perú \n(2000-2019)",
       caption= "Fuente: Banco Mundial")+
  theme_bw() +
  geom_hline(yintercept=mean(datos_paises$Chile), linetype="dashed", 
             color = lista_color[1], size=1)+
  geom_hline(yintercept=mean(datos_paises$Peru), linetype="dashed", 
             color = lista_color[2], size=1)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.position="bottom", 
        plot.title = element_text(size=16, hjust = 0.5))



#Covarianza Chile-Argentina
ggplot(datos_paises, aes(x=fecha, y=Chile)) +
  geom_segment(aes(x=fecha, xend=fecha, y=mean(datos_paises$Chile),
                    yend=Chile), color="grey") +
  geom_point(color=lista_color[1], size=4) +
  geom_segment(aes(x=fecha, xend=fecha, y=mean(datos_paises$Argentina),
                   yend=Argentina), color="black") +
  geom_point(data=datos_paises, aes(x=fecha, y=Argentina), color=lista_color[2], size=4) +
  labs(y = "Inversion extranjera (% PIB)", x = "Año", 
       title = "Flujo de inversión extranjera Chile v/s Argentina \n(2000-2019)",
       caption= "Fuente: Banco Mundial")+
  theme_bw() +
  geom_hline(yintercept=mean(datos_paises$Chile), linetype="dashed", 
             color = lista_color[1], size=1)+
  geom_hline(yintercept=mean(datos_paises$Argentina), linetype="dashed", 
             color = lista_color[2], size=1)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.position="bottom", 
        plot.title = element_text(size=16, hjust = 0.5))


# Análisis de correlación -------------------------------------------------

#Correlacion Chile-Peru  Chile-Argentina
par(mfrow=c(1,2))
plot(datos_paises$Chile, datos_paises$Peru, pch=19, cex=1.25, col=lista_color[1],
     xlab="Chile", ylab="Perú",
     main="Flujo Inv. Ext. Chile v/s Perú \n(2000-2019)")
abline(h=mean(datos_paises$Peru), col="red", lwd=2, lty=2)
abline(v=mean(datos_paises$Chile), col="black", lwd=1.75, lty=2)
plot(datos_paises$Chile, datos_paises$Argentina, pch=19, cex=1.25, col=lista_color[2],
     xlab="Chile", ylab="Argentina",
     main="Flujo Inv. Ext. Chile v/s Argentina \n(2000-2019)",
     sub="Fuente: Banco Mundial", cex.sub = 0.8)
abline(h=mean(datos_paises$Argentina), col="red", lwd=2, lty=2)
abline(v=mean(datos_paises$Chile), col="black", lwd=1.75, lty=2)

#Correlacion Bolivia-Peru
par(mfrow=c(1,2))
plot(datos_paises$Bolivia, datos_paises$Peru, pch=19, cex=1.25, col=lista_color[1],
     xlab="Bolivia", ylab="Perú",
     main="Flujo Inv. Ext. Bolivia v/s Perú \n(2000-2019)")
plot(datos_paises$Bolivia, datos_paises$Peru, pch=19, cex=1.25, col=lista_color[1],
     xlab="Bolivia", ylab="Perú",
     main="Flujo Inv. Ext. Bolivia v/s Perú \n(2000-2019)",
     sub="Fuente: Banco Mundial", cex.sub = 0.8)
abline(h=mean(datos_paises$Peru), col="red", lwd=2, lty=2)
abline(v=mean(datos_paises$Bolivia), col="black", lwd=1.75, lty=2)




# Pair plot simple
pairs(datos_paises[,1:4], pch = 19, col=lista_color[1],
      main="Analisis comparativo de Flujos de Inv.Ext. (2000-2019)")

# Pair plot parafernalico, requiere libreria psych
pairs.panels(datos_paises[,1:4], 
             method = "pearson", # correlation method
             hist.col = lista_color[1],
             density = TRUE,  # show density plots
             breaks=5,
             ellipses=F,
             lm=T,
             main="Analisis comparativo de Flujos de Inv.Ext. (2000-2019)")




# Numeros aleatorios y relaciones no lineales -----------------------------
#Numeros Fabricados

n_datos = 35
set.seed(123)
alpha = rbinom(n_datos, 100, 0.6)
beta = 0.65 * alpha + runif(n_datos, min=-5, max=5)
gamma = -0.2 * alpha + runif(n_datos, min=-5, max=5)
set.seed(456)
delta = rbinom(n_datos, 100, 0.6)

numeros_griegos = data.frame(Alpha=alpha, Beta=beta, Gamma=gamma, 
                                Delta=delta)

par(mfrow=c(1,2))
plot(numeros_griegos$Alpha, pch=16, col=lista_color[1], xlab="", ylab="",
     main="Datos de variables Alpha, Beta", ylim=c(min(0.9*numeros_griegos[,1:2]), 
                                                   max(1.1*numeros_griegos[,1:2])))
points(numeros_griegos$Beta, pch=16, col=lista_color[2])
abline(h=mean(numeros_griegos$Alpha), col=lista_color[1], lwd=2, lty=2)
abline(h=mean(numeros_griegos$Beta), col=lista_color[2], lwd=2, lty=2)
legend("topleft",c("Alpha", "Beta"), lwd=3, col=lista_color, horiz=TRUE, 
       cex=0.4, inset=.01)
plot(numeros_griegos$Alpha, numeros_griegos$Beta, pch=19, col=lista_color[3],
     xlab="Alpha", ylab="Beta",
     main="Comparasión datos: Alpha Beta")
abline(h=mean(numeros_griegos$Beta), col="black", lwd=1.25, lty=2)
abline(v=mean(numeros_griegos$Alpha), col="black", lwd=1.25, lty=2)



p11 = ggplot(numeros_griegos, aes(x=index(numeros_griegos), y=Alpha)) +
  geom_segment( aes(x=index(numeros_griegos), xend=index(numeros_griegos), 
                    y=mean(numeros_griegos$Alpha),
                    yend=Alpha), color="grey") +
  geom_point( color=lista_color[1], size=2) +
  geom_segment(aes(x=index(numeros_griegos), xend=index(numeros_griegos), 
                   y=mean(numeros_griegos$Beta),
                   yend=Beta), color="black") +
  geom_point(data=numeros_griegos, aes(x=index(numeros_griegos), 
                                       y=numeros_griegos$Beta), 
             color=lista_color[2], size=2)+
  labs(y = "Datos", x = "", title = "Análisis de datos: Alpha,Beta")+
  theme_bw() +
  geom_hline(yintercept=mean(numeros_griegos$Alpha), linetype="dashed", 
             color = lista_color[1], size=1)+
  geom_hline(yintercept=mean(numeros_griegos$Beta), linetype="dashed", 
             color = lista_color[2], size=1)+
  theme(panel.border = element_rect(color = "black", fill=NA, size=0.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=14, hjust = 0.5)) 


p12 = ggplot(data=numeros_griegos, aes(x=Alpha, y=Beta))+
  geom_point(size=3, color=lista_color[3])+
  geom_hline(yintercept=mean(numeros_griegos$Beta), linetype="dashed", 
             color="black", size=0.5)+
  geom_vline(xintercept=mean(numeros_griegos$Alpha), linetype="dashed",
             color="black", size=0.5)+
  labs(y = "Beta", x = "Alpha", title = "Comparación de datos: Alpha,Beta")+
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill=NA, size=0.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=14, hjust = 0.5)) 

grid.arrange(p11, p12, nrow=1)

pairs.panels(numeros_griegos[,1:2], 
             method = "pearson", # correlation method
             hist.col = lista_color[1],
             density = TRUE,  # show density plots
             breaks=5,
             ellipses=F,
             lm=T,
             main="Analisis comparativo: Alpha, Beta")


#Alpha Gamma
p21 = ggplot(numeros_griegos, aes(x=index(numeros_griegos), y=Alpha)) +
  geom_segment( aes(x=index(numeros_griegos), xend=index(numeros_griegos), 
                    y=mean(numeros_griegos$Alpha),
                    yend=Alpha), color="grey") +
  geom_point( color=lista_color[1], size=2) +
  geom_segment(aes(x=index(numeros_griegos), xend=index(numeros_griegos), 
                   y=mean(numeros_griegos$Gamma),
                   yend=Gamma), color="black") +
  geom_point(data=numeros_griegos, aes(x=index(numeros_griegos), 
                                       y=numeros_griegos$Gamma), 
             color=lista_color[2], size=2)+
  labs(y = "Datos", x = "", title = "Análisis de datos: Alpha,Gamma")+
  theme_bw() +
  geom_hline(yintercept=mean(numeros_griegos$Alpha), linetype="dashed", 
             color = lista_color[1], size=1)+
  geom_hline(yintercept=mean(numeros_griegos$Gamma), linetype="dashed", 
             color = lista_color[2], size=1)+
  theme(panel.border = element_rect(color = "black", fill=NA, size=0.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=14, hjust = 0.5)) 


p22 = ggplot(data=numeros_griegos, aes(x=Alpha, y=Gamma))+
  geom_point(size=3, color=lista_color[3])+
  geom_hline(yintercept=mean(numeros_griegos$Gamma), linetype="dashed", 
             color="black", size=0.5)+
  geom_vline(xintercept=mean(numeros_griegos$Alpha), linetype="dashed",
             color="black", size=0.5)+
  labs(y = "Gamma", x = "Alpha", title = "Comparación de datos: Alpha, Gamma")+
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill=NA, size=0.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=14, hjust = 0.5)) 

grid.arrange(p21, p22, nrow=1)

pairs.panels(numeros_griegos[,1:3], 
             method = "pearson", # correlation method
             hist.col = lista_color[1],
             density = TRUE,  # show density plots
             breaks=5,
             ellipses=F,
             lm=T,
             main="Analisis comparativo: Alpha, Beta y Gamma")


# Alpha Delta
p31 = ggplot(numeros_griegos, aes(x=index(numeros_griegos), y=Alpha)) +
  geom_segment( aes(x=index(numeros_griegos), xend=index(numeros_griegos), 
                    y=mean(numeros_griegos$Alpha),
                    yend=Alpha), color="grey") +
  geom_point( color=lista_color[1], size=2) +
  geom_segment(aes(x=index(numeros_griegos), xend=index(numeros_griegos), 
                   y=mean(numeros_griegos$Delta),
                   yend=Delta), color="black") +
  geom_point(data=numeros_griegos, aes(x=index(numeros_griegos), 
                                       y=numeros_griegos$Delta), 
             color=lista_color[2], size=2)+
  labs(y = "Datos", x = "", title = "Análisis de datos: Alpha,Delta")+
  theme_bw() +
  geom_hline(yintercept=mean(numeros_griegos$Alpha), linetype="dashed", 
             color = lista_color[1], size=1)+
  geom_hline(yintercept=mean(numeros_griegos$Delta), linetype="dashed", 
             color = lista_color[2], size=1)+
  theme(panel.border = element_rect(color = "black", fill=NA, size=0.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=14, hjust = 0.5)) 


p32 = ggplot(data=numeros_griegos, aes(x=Alpha, y=Delta))+
  geom_point(size=3, color=lista_color[3])+
  geom_hline(yintercept=mean(numeros_griegos$Delta), linetype="dashed", 
             color="black", size=0.5)+
  geom_vline(xintercept=mean(numeros_griegos$Alpha), linetype="dashed",
             color="black", size=0.5)+
  labs(y = "Delta", x = "Alpha", title = "Comparación de datos: Alpha, Delta")+
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill=NA, size=0.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=14, hjust = 0.5)) 

grid.arrange(p31, p32, nrow=1)

pairs.panels(numeros_griegos[,1:4], 
             method = "pearson", # correlation method
             hist.col = lista_color[1],
             density = TRUE,  # show density plots
             breaks=5,
             ellipses=F,
             lm=T,
             main="Analisis comparativo: Alpha, Beta, Gamma y Delta")




# Relacion Cubica No lineal
variable_x = runif(100, min=-5, max=5)
variable_y = variable_x^2 + rnorm(100, mean=0, sd=2)


par(mfrow=c(1,1))
plot(variable_x, variable_y, pch=19, cex=1.25, col=lista_color[1],
     xlab="Variable X", ylab="Variable Y",
     main="Analisis de Variables X,Y")
abline(h=mean(variable_y), col="red", lwd=2, lty=2)
abline(v=mean(variable_x), col="black", lwd=1.75, lty=2)


pairs.panels(cbind(variable_x, variable_y), 
             method = "pearson", # correlation method
             hist.col = lista_color[1],
             density = TRUE,  # show density plots
             breaks=5,
             ellipses=F,
             lm=T)

# Extras ------------------------------------------------------------------




#https://rpubs.com/bjk127/436726
#https://www.stevejburr.com/post/scatter-plots-and-best-fit-lines/ grafico util regresion
# https://www.youtube.com/watch?v=qrqty2TFLak graficos con pirotecnia de espacio tiempo
# http://www.sthda.com/english/wiki/add-text-to-a-plot-in-r-software texto dentro de grafico

# https://www.r-graph-gallery.com/index.html libreria grafica de r
# https://statisticsglobe.com/add-subscript-and-superscript-to-plot-in-r incorporar elevados o subindices a textos

# Tests -------------------------------------------------------------------

par(mfrow=c(1,1))
plot(datos_paises$fecha, datos_paises$Argentina, type="l", col=lista_color[1], 
     lwd=3, xlab="Año", ylab="% del PIB ", 
     ylim=c(min(datos_paises[ ,1:4]), max(datos_paises[ ,1:4])),
     main="Flujo de inversion extranjera por paises (2000-2019)")
lines(datos_paises$fecha, datos_paises$Bolivia, type="l", col=lista_color[2], 
      lwd=3)
lines(datos_paises$fecha, datos_paises[, 3], type="l", col=lista_color[3], 
      lwd=3)
lines(datos_paises$fecha, datos_paises[, 4], type="l", col=lista_color[4], 
      lwd=3)
legend("bottomright",lista_paises, lwd=3, col=lista_color, horiz=TRUE, cex=0.6,
       inset=c(0, -0.3), xpd=T)


ggplot(numeros_griegos, aes(x=index(numeros_griegos), y=Alpha)) +
  geom_segment( aes(x=index(numeros_griegos), xend=index(numeros_griegos), 
                    y=mean(numeros_griegos$Alpha),
                    yend=Alpha), color="grey") +
  geom_point( color=lista_color[1], size=2) +
  geom_segment(aes(x=index(numeros_griegos), xend=index(numeros_griegos), 
                   y=mean(numeros_griegos$Beta),
                   yend=Beta), color="black") +
  geom_point(data=numeros_griegos, aes(x=index(numeros_griegos), 
                                       y=numeros_griegos$Beta), 
             color=lista_color[2], size=2)+
  labs(y = "Datos", x = "", title = "Analisis de datos Alpha,Beta")+
  theme_bw() +
  geom_hline(yintercept=mean(numeros_griegos$Alpha), linetype="dashed", 
             color = lista_color[1], size=1)+
  geom_hline(yintercept=mean(numeros_griegos$Beta), linetype="dashed", 
             color = lista_color[2], size=1)+
  theme(panel.border = element_rect(color = "black", fill=NA, size=0.8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=16, hjust = 0.5)) 


