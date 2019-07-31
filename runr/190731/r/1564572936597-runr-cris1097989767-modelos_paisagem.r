ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("glm2", "car", "raster", "ggplot2", "doParallel", "foreach", "rgdal",
              "gdalUtils", "rgeos", "sp", "maptools", "MuMIn", "gstat", "nlme",
              "spdep", "ncf", "lme4", "lmerTest","gridExtra")
ipak(packages)
rm(ipak)
rm(packages)

windowsFonts(f =  windowsFont("Times New Roman"))
setwd(dir = choose.dir())
#MODELOS DA PAISAGEM
#abundancia <- read.table("abundancia.txt", header = T)
presencas <- read.table("presenca_porcentagem.txt", header = T)
abundPorc <- read.table("dadosAbundPorcent.txt", header = T)
abundAdeq <- read.table("abundancia_adeq.txt", header = T)
presAdeq <- read.table("presenca_adeq.txt", header = T)

####################################################################################
####################################   M1w   #######################################
############# MODELO DE ABUNDANCIA CONTROLANDO O ESFORCO COMO ARGUMENTO ############
                                                                                   #
modelos <- list()                                                                  #
pseudR2 <- rep(NA, times=59)                                                       #
for(i in 7:65){                                                                    #
  modelo <- glm(abund ~ abundPorc[,i] + adequabilidade,                            #
                weights = esforco, family = poisson, data = abundPorc)             #
  resul <- summary(modelo)                                                         #
  modelos[[i]] <- resul                                                            #
  r2 <- (1 - (resul$deviance/resul$null.deviance))                                 #
  pseudR2[i] <- r2                                                                 #
}                                                                                  #
                                                                                   #
R2s <- na.exclude(pseudR2)                                                         #
escalas <- seq(1, 30, 0.5)
habitat <- abundPorc[,which(R2s==max(R2s))]                                        #
M1w <- glm(abund ~ habitat + adequabilidade,              #
           weights = esforco, family = poisson, data = abundPorc)                  #   
summary(M1w)                                                                       #
                                                                                   #
#plot dos efeitos parciais dos modelos (altitude)                                  #
crPlot(M1w, variable = "altitude", id=F,                                           #
       col = carPalette()[1], col.lines = "red",                                   #
       xlab="Altitude", ylab = "Componente + Residuos (Abundancia)",               #
       pch = 1, lwd = 1, grid=F, ellipse=FALSE, main = NULL, axes = F)             #
axis(side = 1, at=seq(-40, 1000, 200))                                             #
axis(side = 2, at=seq(-2,3,1))                                                     #
#plot dos efeitos parciais dos modelos (habitat)                                   #
crPlot(M1w, variable = abundPorc[,which(R2s == max(R2s))], id=F,                   #
       col = carPalette()[1], col.lines = "red",                                   #
       xlab="Habitat", ylab = "Componente + Residuos (Abundancia)",                #
       pch = 1, lwd = 1, grid = F, main = NULL, axes = F)                          #
axis(side = 1, at=seq(-0.05, 1, 0.21))                                             #
axis(side = 2, at=seq(-2,6,1))                                                     #
#plot dos efeitos parciais dos modelos (adequabilidade)                            #
crPlot(M1w, variable = "adequabilidade", id=F,                                     #
       col = carPalette()[1], col.lines = "red",                                   #
       xlab="Adequabilidade", ylab = "Componente + Residuos (Abundancia)",         #
       pch = 1, lwd = 1,grid=F, ellipse=FALSE, main = NULL, axes = F)              #
axis(side = 1, at=seq(0, 1, 0.2))                                                  #
axis(side = 2, at=seq(-2,5,1))                                                     #
                                                                                   #
#residuos                                                                          #
ResM1w<-ggplot() +
  geom_hline(yintercept = 0, colour="red", linetype="dashed", size=1)+
  geom_point(aes(M1w$fitted.values,M1w$residuals),colour="grey50", size=2)+
  theme_classic()+
  annotate("text", x = 100, y = 6.5, label = "Cenário A", family = "f", size = 4) +
  labs(x = "Valores ajustados", y = "Resíduos") +
  theme(legend.title = element_blank(),
        legend.position = "none",
        axis.text = element_text(colour = "black", family = "f", size = 12),
        axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        text = element_text(colour = "black", family = "f", size = 12))
                                                                                   #
#plot da escala de efeito                                                          #
ggplot() +                                                                         #
  geom_line(aes(x = escalas, y = R2s)) +                                           #
  annotate("segment", x=escalas[which(R2s == max(R2s))],                           #
           xend=escalas[which(R2s == max(R2s))], y=max(R2s), yend=min(R2s),        #
           colour="red", size = 0.1) +                                             #
  annotate("text", x = escalas[which(R2s == max(R2s))],                            #
           y = min(R2s), label = escalas[which(R2s == max(R2s))]) +                #
  theme_classic()                                                                  #
                                                                                   #
####################################################################################


####################################################################################
####################################   M1v   #######################################
############# MODELO DE ABUNDANCIA CONTROLANDO O ESFORCO COMO VARIAVEL #############
                                                                                   #
modelos2 <- list()                                                                 #
pseudR2.2 <- rep(NA, times=59)                                                     #
for(i in 7:65){                                                                    #
  modelo2 <- glm(abund ~ abundPorc[,i] + esforco + adequabilidade,      #
                 family = poisson, data = abundPorc)                               #
  resul2 <- summary(modelo2)                                                       #
  modelos2[[i]] <- resul2                                                          #
  r2.2 <- (1 - (resul2$deviance/resul2$null.deviance))                             #
  pseudR2.2[i] <- r2.2                                                             #
}                                                                                  #
                                                                                   #
R2s.2 <- na.exclude(pseudR2.2)                                                     #
escalas2 <- seq(1, 30, 0.5)                                                        #
habitat <- abundPorc[,which(R2s.2 == max(R2s.2))]
M1v <- glm(abund ~ habitat + adequabilidade + esforco,                             #
           family = poisson, data = abundPorc)                                     #
summary(M1v)                                                                       #
                                                                                   #
#plot dos efeitos parciais dos modelos (altitude)                                  #
crPlot(M1v, variable = "altitude", id=F,                                           #
       col = carPalette()[1], col.lines = "red",                                   #
       xlab="Altitude", ylab = "Componente + Residuos (Abundancia)",               #
       pch = 1, lwd = 1, grid=F, ellipse=FALSE, main = NULL, axes = F)             #
axis(side = 1, at=seq(-100, 900, 200))                                             #
axis(side = 2, at=seq(-2.2,4,1))                                                   #
#plot dos efeitos parciais dos modelos (habitat)                                   #
crPlot(M1v, variable = abundPorc[,which(R2s.2 == max(R2s.2))], id=F,               #
       col = carPalette()[1], col.lines = "red",                                   #
       xlab="Habitat", ylab = "Componente + Residuos (Abundancia)",                #
       pch = 1, lwd = 1, grid=F, ellipse=FALSE, main = NULL, axes = F)             #
axis(side = 1, at=seq(-0.8, 1, 0.2))                                                  #
axis(side = 2, at=seq(-2,6,1))                                                     #
#plot dos efeitos parciais dos modelos (adequabilidade)                            #
crPlot(M1v, variable = "adequabilidade", id=F,                                     #
       col = carPalette()[1], col.lines = "red",                                   #
       xlab="Adequabilidade", ylab = "Componente + Residuos (Abundancia)",         #
       pch = 1, lwd = 1, grid=F, main = NULL, axes = F)                            #
axis(side = 1, at=seq(0, 1, 0.2))                                                  #
axis(side = 2, at=seq(-2,6,1))                                                     #
                                                                                   #
#plot dos efeitos parciais dos modelos (esforco)                                   #
crPlot(M1v, variable = "esforco", id=F,                                            #
       col = carPalette()[1], col.lines = "red",                                   #
       xlab="Esforco amostral", ylab = "Componente + Residuos (Abundancia)",       #
       pch = 1, lwd = 1, grid=F, main = NULL, axes = F)                            #                 #
axis(side = 1, at=seq(-150, 7050, 1028.571))                                       #
axis(side = 2, at=seq(-2,6,1))                                                     #
                                                                                   #
#residuos                                                                          #
ResM1v<-ggplot() +
  geom_hline(yintercept = 0, colour="red", linetype="dashed", size=1)+
  geom_point(aes(M1v$fitted.values,M1v$residuals),colour="grey50", size=2)+
  theme_classic()+
  annotate("text", x = 75, y = 6.5, label = "Cenário B", family = "f", size = 4) +
  labs(x = "Valores ajustados", y = "Resíduos") +
  theme(legend.title = element_blank(),
        legend.position = "none",
        axis.text = element_text(colour = "black", family = "f", size = 12),
        axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        text = element_text(colour = "black", family = "f", size = 12))                                                           #

mapa <- grid.arrange(ResM1w,ResM1v,ncol = 2)
ggsave(plot = mapa, filename = "C:/Users/DELL/Dropbox/Mestrado/residuos.tiff",
       width = 15, height = 7.5, units = "cm", dpi = 300)
#
#plot da escala de efeito                                                          #
ggplot() +                                                                         #
  geom_line(aes(x = escalas2, y = R2s.2)) +                                        #
  annotate("segment", x=escalas2[which(R2s.2 == max(R2s.2))],                      #
           xend=escalas2[which(R2s.2 == max(R2s.2))],                              #
           y=max(R2s.2), yend=min(R2s.2),                                          #
           colour="red", size = 0.1) +                                             #
  annotate("text", x = escalas2[which(R2s.2 == max(R2s.2))],                       #
           y = min(R2s.2), label = escalas2[which(R2s.2 == max(R2s.2))]) +         #
  theme_classic()                                                                  #
                                                                                   #
####################################################################################


####################################################################################
####################################### M2w ########################################
###### MODELOS DE ABUNDANCIA DELIMITADO PARA AREAS COM ADEQUABILIDADE > 0.5 ########
####################### CONTROLANDO O ESFORCO COMO ARGUMENTO #######################
                                                                                   #
modelos6 <- list()                                                                 #
pseudR2.6 <- rep(NA, times=59)                                                     #
for(i in 7:65){                                                                    #
  modelo6 <- glm(abund ~ abundAdeq[,i], weights = esforco,              #
                 family = poisson, data = abundAdeq)                               #
  resul6 <- summary(modelo6)                                                       #
  modelos6[[i]] <- resul6                                                          #
  r2.6 <- (1 - (resul6$deviance/resul6$null.deviance))                             #
  pseudR2.6[i] <- r2.6                                                             #
}                                                                                  #
                                                                                   #
R2s.6 <- na.exclude(pseudR2.6)                                                     #
escalas2 <- seq(1, 30, 0.5)                                                        #
M2w <- glm(abund ~ abundAdeq[,which(R2s.6 == max(R2s.6))],              #
           weights = esforco, family = poisson, data = abundAdeq)                  #
summary(M2w)                                                                       #
                                                                                   #
#plot dos efeitos parciais dos modelos (habitat)                                   #
crPlot(M2w, variable = abundAdeq[,which(R2s.6 == max(R2s.6))], id=F,               #
       col = carPalette()[1], col.lines = "red",                                   #
       xlab="Habitat", ylab = "Componente + Residuos (Abundancia)",                #
       pch = 1, lwd = 1, grid=F, main = NULL, axes = F)                            #
axis(1, at=seq(-0.05,1.5,0.2))                                                     #
axis(2, at=seq(-2,4,1))                                                            #
#plot dos efeitos parciais dos modelos (altitude)                                  #
crPlot(M2w, variable = "altitude", id=F,                                           #
       col = carPalette()[1], col.lines = "red",                                   #
       xlab="Altitude", ylab = "Componente + Residuos (Abundancia)",               #
       pch = 1, lwd = 1, grid=F, main = NULL, axes = F)                            #
axis(1, at=seq(-200,1020,305))                                                     #
axis(2, at=seq(-2,2,1))                                                            #
                                                                                   #
#residuos                                                                          #
plot(M2w$fitted.values, M2w$residuals, xlab = "Valores ajustados",                 #
     ylab = "Residuos", axes=T) #residuos                                          #
plot(M2w$fitted.values, M2w$residuals, xlab = "Valores ajustados",                 #
     ylab = "Residuos", axes=F) #residuos                                          #
axis(1, at=seq(0,250,50))                                                          #
axis(2, at=seq(-1.5,3,1))                                                          #
abline(h=0, col = "red")                                                           #
                                                                                   #
#plot da escala de efeito                                                          #
ggplot() +                                                                         #
  geom_line(aes(x = escalas, y = R2s.6)) +                                         #
  annotate("segment", x=escalas[which(R2s.6 == max(R2s.6))],                       #
           xend=escalas[which(R2s.6 == max(R2s.6))], y=max(R2s.6), yend=min(R2s.6),# 
           colour="red", size = 0.1) +                                             #
  annotate("text", x = escalas[which(R2s.6 == max(R2s.6))], y = min(R2s.6),        #
           label = escalas2[which(R2s.6 == max(R2s.6))]) +                         #
  theme_classic()                                                                  #
                                                                                   #
####################################################################################

####################################################################################
###################################### M2v #########################################
####### MODELOS DE ABUNDANCIA DELIMITADO PARA AREAS COM ADEQUABILIDADE > 0.5 #######
####################### CONTROLANDO O ESFORCO COMO VARIÁVEL ########################
                                                                                   #
modelos7 <- list()                                                                 #
pseudR2.7 <- rep(NA, times=59)                                                     #
for(i in 7:65){                                                                    #
  modelo7 <- glm(abund ~ abundAdeq[,i] + esforco, family = poisson,     #
                 data = abundAdeq)                                                 #
  resul7 <- summary(modelo7)                                                       #
  modelos7[[i]] <- resul7                                                          #
  r2.7 <- (1 - (resul7$deviance/resul7$null.deviance))                             #
  pseudR2.7[i] <- r2.7                                                             #
}                                                                                  #
                                                                                   #
R2s.7 <- na.exclude(pseudR2.7)                                                     #
escalas <- seq(1, 30, 0.5)                                                         #
M2v <- glm(abund ~ abundAdeq[,which(R2s.7 == max(R2s.7))] + esforco,    #
           family = poisson, data = abundAdeq)                                     #
summary(M2v)                                                                       #
                                                                                   #
#plot dos efeitos parciais dos modelos (esforco)                                   #
crPlot(M2v, variable = "esforco", id=F,                                            #
       col = carPalette()[1], col.lines = "red",                                   #
       xlab="Esforco", ylab = "Componente + Residuos (Abundancia)",                #
       pch = 1, lwd = 1, grid=F, main = NULL, axes = F)                            #
axis(1, at=seq(-200,8000,1000))                                                    #
axis(2, at=seq(-2,4,1))                                                            #
                                                                                   #
#plot dos efeitos parciais dos modelos (habitat)                                   #
crPlot(M2v, variable = abundAdeq[,which(R2s.7 == max(R2s.7))], id=F,               #
       col = carPalette()[1], col.lines = "red",                                   #
       xlab="Habitat", ylab = "Componente + Residuos (Abundancia)",                #
       pch = 1, lwd = 1, grid=F, main = NULL, axes = F)                            #
axis(1, at=seq(0,1,0.2))                                                           #
axis(2, at=seq(-2,4,1))                                                            #
                                                                                   #
#plot dos efeitos parciais dos modelos (altitude)                                  #
crPlot(M2v, variable = "altitude", id=F,                                           #
       col = carPalette()[1], col.lines = "red",                                   #
       xlab="Altitude", ylab = "Componente + Residuos (Abundancia)",               #
       pch = 1, lwd = 1, grid=F, main = NULL, axes = F)                            #
axis(1, at=seq(-30,930,100))                                                       #
axis(2, at=seq(-2,3,1))                                                            #
                                                                                   #
#residuos                                                                          #
plot(M2v$fitted.values, M2v$residuals, xlab = "Valores ajustados",                 #
     ylab = "Residuos", axes=T) #residuos                                          #
plot(M2v$fitted.values, M2v$residuals, xlab = "Valores ajustados",                 #
     ylab = "Residuos", axes=F) #residuos                                          #
axis(1, at=seq(0,200,50))                                                          #
axis(2, at=seq(-1.5,3.5,1))                                                        #
abline(h=0, col = "red")                                                           #
                                                                                   #
#plot da escala de efeito                                                          #
ggplot() +                                                                         #
  geom_line(aes(x = escalas, y = R2s.7)) +                                         #
  annotate("segment", x=escalas[which(R2s.7 == max(R2s.7))],                       #
           xend=escalas[which(R2s.7 == max(R2s.7))], y=max(R2s.7), yend=min(R2s.7),#
           colour="red", size = 0.1) +                                             #
  annotate("text", x = escalas[which(R2s.7 == max(R2s.7))], y = min(R2s.7),        #
           label = escalas2[which(R2s.7 == max(R2s.7))]) +                         #
  theme_classic()                                                                  #
                                                                                   #
####################################################################################


############################################
# Modelos de presenca e ausencia controlando o ESFORCO com o argumento WEIGHTS
#com altitude, quantidade de habitat e adequabilidade
# funcao glm da o erro "algoritmo nao convergiu", e a funcao glm2 roda
Modelosp <- list()
PseudR2p <- rep(NA, times=19)
for(i in 7:25){
  Modelop <- glm2(presenca ~ presencas[,i] + adequabilidade, weights = esforco, family = binomial, data = presencas)
  Resulp <- summary(Modelop)
  Modelosp[[i]] <- Resulp
  R2p <- (1 - (Resulp$deviance/Resulp$null.deviance))
  PseudR2p[i] <- R2p
  #print(summary(modelo))
}

r2sp <- na.exclude(PseudR2p)
Escalasp <- seq(1, 10, 0.5)
habitat <- presencas[,which(r2sp == max(r2sp))]
M3w <- glm2(presenca ~ habitat + adequabilidade, weights = esforco, family = binomial, data = presencas)
summary(M3w)
crPlots(M3w)
plot(M3w$fitted.values, M3w$residuals) #residuos

#plot dos efeitos parciais dos modelos
avPlot(M4wp, variable = "altitude", id=F,
       col = carPalette()[1], col.lines = "red",
       xlab="Altitude", ylab = "Probabilidade de ocorrencia", pch = 1, lwd = 1,
       type=c("Wang", "Weisberg"), grid=F,
       ellipse=FALSE, main = NULL, axes = T)
axis(1, at=seq(-3050,4000,1000))
axis(2, at=seq(-20,50,10))


#residuos
plot(M3v$fitted.values, M3v$residuals, xlab = "Valores ajustados", ylab = "Residuos", axes=T) #residuos
plot(M3v$fitted.values, M3v$residuals, xlab = "Valores ajustados", ylab = "Residuos", axes=F) #residuos
axis(1, at=seq(0,200,50))
axis(2, at=seq(-1.5,4,1))
abline(h=0, col = "red")

#plot da escala de efeito
ggplot() +
  geom_line(aes(x = escalas, y = R2s.7)) +
  annotate("segment", x=escalas[which(R2s.7 == max(R2s.7))], xend=escalas[which(R2s.7 == max(R2s.7))], y=max(R2s.7), yend=min(R2s.7), 
           colour="red", size = 0.1) +
  annotate("text", x = escalas[which(R2s.7 == max(R2s.7))], y = min(R2s.7), label = escalas2[which(R2s.7 == max(R2s.7))]) +
  theme_classic()

#plot da escala de efeito
ggplot() +
  geom_line(aes(x = Escalas, y = r2s)) +
  annotate("segment", x=Escalas[which(r2s == max(r2s))], xend=Escalas[which(r2s == max(r2s))], y=max(r2s), yend=min(r2s), 
           colour="red", size = 0.1) +
  annotate("text", x = Escalas[which(r2s == max(r2s))], y = min(r2s), label = Escalas[which(r2s == max(r2s))]) +
  theme_classic()

# Modelos de presenca e ausencia controlando o ESFORCO como VARIAVEL
# funcao glm da o erro "algoritmo nao convergiu", e a funcao glm2 roda
Modelos <- list()
PseudR2 <- rep(NA, times=19)
for(i in 7:25){
  Modelo <- glm2(presenca ~ presencas[,i] + adequabilidade + esforco, family = binomial, data = presencas)
  Resul <- summary(Modelo)
  Modelos[[i]] <- Resul
  R2 <- (1 - (Resul$deviance/Resul$null.deviance))
  PseudR2[i] <- R2
  #print(summary(modelo))
}

r2s.2 <- na.exclude(PseudR2)
Escalas2 <- seq(1, 10, 0.5)
M3v <- glm2(presenca ~ presencas[,which(r2s.2 == max(r2s.2))] + adequabilidade + esforco, family = binomial, data = presencas)
summary(M3v)
avPlots(Modelo3)
plot(Modelo3$fitted.values, Modelo3$residuals) #residuos

#plot da escala de efeito
ggplot() +
  geom_line(aes(x = Escalas2, y = r2s.2)) +
  annotate("segment", x=Escalas2[which(r2s.2 == max(r2s.2))], xend=Escalas2[which(r2s.2 == max(r2s.2))], y=max(r2s.2), yend=min(r2s.2), 
           colour="red", size = 0.1) +
  annotate("text", x = Escalas2[which(r2s.2 == max(r2s.2))], y = min(r2s.2), label = Escalas2[which(r2s.2 == max(r2s.2))]) +
  theme_classic()

# Modelos de presenca e ausencia controlando o ESFORCO com o argumento WEIGHTS
# Com restri??o de ?reas com adequabilidade acima de 0.5
# funcao glm da o erro "algoritmo nao convergiu", e a funcao glm2 roda
Modelos10 <- list()
PseudR10 <- rep(NA, times=19)
for(i in 7:25){
  Modelo10 <- glm2(presenca ~ altitude + presAdeq[,i], weights = esforco, family = binomial, data = presAdeq)
  Resul10 <- summary(Modelo10)
  Modelos10[[i]] <- Resul10
  R2.10 <- (1 - (Resul10$deviance/Resul10$null.deviance))
  PseudR10[i] <- R2.10
  #print(summary(modelo))
}

r2s10 <- na.exclude(PseudR10)
Escalas <- seq(1, 10, 0.5)
M4w <- glm2(presenca ~ altitude + presAdeq[,which(r2s10 == max(r2s10))], weights = esforco, family = binomial, data = presAdeq)
summary(M4w)
avPlots(Modelo10)
plot(Modelo10$fitted.values, Modelo10$residuals) #residuos

#plot da escala de efeito
ggplot() +
  geom_line(aes(x = Escalas, y = r2s10)) +
  annotate("segment", x=Escalas[which(r2s10 == max(r2s10))], xend=Escalas[which(r2s10 == max(r2s10))], y=max(r2s10), yend=min(r2s10), 
           colour="red", size = 0.1) +
  annotate("text", x = Escalas[which(r2s10 == max(r2s10))], y = min(r2s10), label = Escalas[which(r2s10 == max(r2s10))]) +
  theme_classic()

# Modelos de presenca e ausencia controlando o ESFORCO com o VARI?VEL
# Com restri??o de ?reas com adequabilidade acima de 0.5
# funcao glm da o erro "algoritmo nao convergiu", e a funcao glm2 roda
Modelos11 <- list()
PseudR11 <- rep(NA, times=19)
for(i in 7:25){
  Modelo11 <- glm2(presenca ~ altitude + presAdeq[,i] + esforco, family = binomial, data = presAdeq)
  Resul11 <- summary(Modelo11)
  Modelos11[[i]] <- Resul11
  R2.11 <- (1 - (Resul11$deviance/Resul11$null.deviance))
  PseudR11[i] <- R2.11
  #print(summary(modelo))
}

r2s11 <- na.exclude(PseudR11)
Escalas <- seq(1, 10, 0.5)
M4v <- glm2(presenca ~ altitude + presAdeq[,which(r2s11 == max(r2s11))]+ esforco, family = binomial, data = presAdeq)
summary(M4v)
avPlots(Modelo11)
plot(Modelo11$fitted.values, Modelo11$residuals) #residuos

#plot da escala de efeito
ggplot() +
  geom_line(aes(x = Escalas, y = r2s11)) +
  annotate("segment", x=Escalas[which(r2s11 == max(r2s11))], xend=Escalas[which(r2s11 == max(r2s11))], y=max(r2s11), yend=min(r2s11), 
           colour="red", size = 0.1) +
  annotate("text", x = Escalas[which(r2s11 == max(r2s11))], y = min(r2s11), label = Escalas[which(r2s11 == max(r2s11))]) +
  theme_classic()
