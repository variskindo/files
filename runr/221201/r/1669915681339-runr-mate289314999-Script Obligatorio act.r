rm(list=ls())
View(Homeprice)

# Se pide 4 (Variable cualitativa "neighborhood")

FrecAbs_neighborhood     <- table(Homeprice$neighborhood)
FrecAbs_neighborhood

FrecRel_neighborhood      <- prop.table(FrecAbs_neighborhood)
FrecRel_neighborhood

FrecPorc_neighborhood     <- (FrecRel_neighborhood*100)
FrecPorc_neighborhood

Tablas_Frecuencias_neighborhood   <- cbind(FrecAbs_neighborhood, FrecRel_neighborhood, FrecPorc_neighborhood)
Tablas_Frecuencias_neighborhood

barplot(FrecAbs_neighborhood, main ="Gráfico de barras Ranking de barrios", xlab ="Ranking", 
        ylab ="Frecuencia absoluta", col = 7)

pie(FrecAbs_neighborhood, main = "Gráfico de torta Ranking de barrios")


# Se pide 5 (Variables cuantitativas "sale" y "rooms")

# Variable "sale"
# A)

clases        <-seq(0,650,50)
clases

Homeprice$clase_sale   <-cut(Homeprice$sale,breaks = clases)

FrecAbs_sale         <-table(Homeprice$clase_sale)
FrecAbs_sale

FrecRel_sale         <-prop.table(FrecAbs_sale)
FrecRel_sale

FrecPorc_sale        <-(FrecRel_sale*100)
FrecPorc_sale

FrecAcum_sale        <-cumsum(FrecAbs_sale)
FrecAcum_sale

FrecRelAcum_sale     <-cumsum(FrecRel_sale)
FrecRelAcum_sale

FrecPorcAcum_sale    <-cumsum(FrecPorc_sale)
FrecPorcAcum_sale

Tablas_frecuencias_sale   <- cbind(FrecAbs_sale,FrecRel_sale,FrecPorc_sale, FrecAcum_sale,FrecRelAcum_sale, FrecPorcAcum_sale)
Tablas_frecuencias_sale

# B)

hist(Homeprice$sale, main ="Histograma variable sale", xlab ="Precio", 
     ylab ="Frecuencia absoluta", col = 7)

# C) 

media_sale    <-mean(Homeprice$sale)
media_sale

mediana_sale  <-median(Homeprice$sale)
mediana_sale

getmode            <- function(v) {
  uniqv            <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

moda_sale         <- getmode(Homeprice$sale)
moda_sale

cuartiles_sale     <- quantile(Homeprice$sale, c(0.25, 0.50,  0.75), type = 6)
cuartiles_sale

cuartiles_sale         <- as.vector(cuartiles_sale)
cuartiles_sale

rango_sale    <-max(Homeprice$sale)-min(Homeprice$sale)
rango_sale

RIC_sale     <-IQR(Homeprice$sale)
RIC_sale

varianza_sale  <-var(Homeprice$sale)
varianza_sale

desv_est_sale  <-sd(Homeprice$sale)
desv_est_sale

CV_sale   <-desv_est_sale/media_sale*100
CV_sale

# D)

Homeprice $sale_valorz <-(Homeprice$sale-media_sale)/desv_est_sale
Homeprice $sale_valorz

which(Homeprice$sale_valorz >3)
which(Homeprice$sale_valorz <(-3))

resumen_5num_sale  <- quantile(Homeprice$sale, c(0, 0.25, 0.50,  0.75, 1), type = 6)
resumen_5num_sale

RIC_sale     <-IQR(Homeprice$sale)
RIC_sale

LimiteInf_sale  <- resumen_5num_sale[2]-1.5*RIC_sale
LimiteInf_sale

LimiteSup_sale  <- resumen_5num_sale[4]+1.5*RIC_sale
LimiteSup_sale

which(Homeprice$sale  > LimiteSup_sale)
which(Homeprice$sale  < LimiteInf_sale)

boxplot(Homeprice$sale)


# E) 


CoefVar_sale <- (desv_est_sale/media_sale)
CoefVar_sale


# Variable "rooms"

# A)

clases        <-seq(0,12,2)
clases

Homeprice$clase_rooms   <-cut(Homeprice$rooms,breaks = clases)

FrecAbs_rooms         <-table(Homeprice$clase_rooms)
FrecAbs_rooms

FrecRel_rooms         <-prop.table(FrecAbs_rooms)
FrecRel_rooms

FrecPorc_rooms        <-(FrecRel_rooms*100)
FrecPorc_rooms

FrecAcum_rooms        <-cumsum(FrecAbs_rooms)
FrecAcum_rooms

FrecRelAcum_rooms     <-cumsum(FrecRel_rooms)
FrecRelAcum_rooms

FrecPorcAcum_rooms    <-cumsum(FrecPorc_rooms)
FrecPorcAcum_rooms

Tablas_frecuencias_rooms   <- cbind(FrecAbs_rooms,FrecRel_rooms,FrecPorc_rooms, FrecAcum_rooms,FrecRelAcum_rooms, FrecPorcAcum_rooms)
Tablas_frecuencias_rooms

# B) 

hist(Homeprice$rooms, main ="Histograma variable rooms", xlab ="Cantidad de cuartos", 
     ylab ="Frecuencia absoluta", col = 5)

# C)

media_rooms    <-mean(Homeprice$rooms)
media_rooms

mediana_rooms  <-median(Homeprice$rooms)
mediana_rooms

getmode            <- function(v) {
  uniqv            <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

moda_rooms         <- getmode(Homeprice$rooms)
moda_rooms

cuartiles_rooms     <- quantile(Homeprice$rooms, c(0.25, 0.50,  0.75), type = 6)
cuartiles_rooms

cuartiles_rooms         <- as.vector(cuartiles_rooms)
cuartiles_rooms

rango_rooms    <-max(Homeprice$rooms)-min(Homeprice$rooms)
rango_rooms

RIC_rooms     <-IQR(Homeprice$rooms)
RIC_rooms

varianza_rooms  <-var(Homeprice$rooms)
varianza_rooms

desv_est_rooms  <-sd(Homeprice$rooms)
desv_est_rooms

CV_rooms   <-desv_est_rooms/media_rooms*100
CV_rooms

# D)

Homeprice $rooms_valorz <-(Homeprice$rooms-media_rooms)/desv_est_rooms
Homeprice $rooms_valorz

which(Homeprice$rooms_valorz > 3)
which(Homeprice$rooms_valorz < (-3))

resumen_5num_rooms  <- quantile(Homeprice$rooms, c(0, 0.25, 0.50,  0.75, 1), type = 6)
resumen_5num_rooms

RIC_rooms     <-IQR(Homeprice$rooms)
RIC_rooms

LimiteInf_rooms  <- resumen_5num_rooms[2]-1.5*RIC_rooms
LimiteInf_rooms

LimiteSup_rooms  <- resumen_5num_rooms[4]+1.5*RIC_rooms
LimiteSup_rooms

which(Homeprice$rooms  > LimiteSup_rooms)
which(Homeprice$rooms  < LimiteInf_rooms)

boxplot(Homeprice$rooms)

# E)

CoefVar_rooms <- (desv_est_rooms/media_rooms)
CoefVar_rooms

# Se pide 6 (Tabulación cruzada de las variables cuantitativas)

Tab_cruzada_cuanti <- table(Homeprice$sale, Homeprice$rooms)
Tab_cruzada_cuanti

# Se pide 7 (Análisis de asociación entre la variable cualitativa "neighborhood" y la variable cuantitativa "sale")

clases        <-seq(0,650,50)
clases

Homeprice$clase_sale   <-cut(Homeprice$sale,breaks = clases)

Chi.cuad <- chisq.test(Homeprice$neighborhood,Homeprice$sale)
Chi.cuad <- Chi.cuad$statistic 

# Se pide 8 *(Analisis de relacion entre las dos variables cuantitativas)

Correlacion <- cor(Homeprice$sale, Homeprice$rooms)

Dispersion <- plot(Homeprice$sale, Homeprice$rooms, xlab="sale", ylab="rooms", main = "Dispersión entre variable Sales y Rooms" )












