

#Abrir la base de datos pulso
library(readxl)
energalcoh <- read_excel("alex/4to ciclo/Estadistica/grupo/energalcoh.xlsx")
View(energalcoh)

#Estadistica Descriptiva 
summary(energalcoh)

summary(energalcoh$AñoU)
energalcoh[is.na(energalcoh$AñoU),]
sd(energalcoh$AñoU)
length(energalcoh$AñoU)
hist(energalcoh$AñoU)
hist(energalcoh$AñoU, main = "Histograma de la variable Año Universitario")
hist(energalcoh$AñoU, main = "Histograma de la variable Año Universitario", xlab="Año U", ylab="Frequency", col=5)
boxplot(energalcoh$AñoU)

summary(energalcoh$Edad)
energalcoh[is.na(energalcoh$Edad),]
sd(energalcoh$Edad)
length(energalcoh$Edad)
hist(energalcoh$Edad)
hist(energalcoh$Edad, main = "Histograma de la variable Edad")
hist(energalcoh$Edad, main = "Histograma de la variable Edad", xlab="Edad", ylab="Frequency", col=5)
boxplot(energalcoh$Edad)

summary(energalcoh$Tipodebebener)
energalcoh[is.na(energalcoh$Tipodebebener),]
sd(energalcoh$Tipodebebener)
length(energalcoh$Tipodebebener)
hist(energalcoh$Tipodebebener)
hist(energalcoh$Tipodebebener, main = "Histograma de la variable Tipo de Bebida energizante")
hist(energalcoh$Tipodebebener, main = "Histograma de la variable Tipo de Bebida energizante", xlab="tipo bebida energizante", ylab="Frequency", col=5)
boxplot(energalcoh$Tipodebebener)

summary(energalcoh$vecesmes)
energalcoh[is.na(energalcoh$vecesmes),]
sd(energalcoh$vecesmes)
length(energalcoh$vecesmes)
hist(energalcoh$vecesmes)
hist(energalcoh$vecesmes, main = "Histograma de la variable de Veces al mes que consumes bebidas energizantes")
hist(energalcoh$vecesmes, main = "Histograma de la variable de Veces al mes que consumes bebidas energizantes", xlab="Veces al mes", ylab="Frequency", col=2)
boxplot(energalcoh$vecesmes)

summary(energalcoh$ocasionesener)
energalcoh[is.na(energalcoh$ocasionesener),]
sd(energalcoh$ocasionesener)
length(energalcoh$ocasionesener)
hist(energalcoh$ocasionesener)
hist(energalcoh$ocasionesener, main = "Histograma de la variable de ocesiones de consumo de bebidas energizantes")
hist(energalcoh$ocasionesener, main = "Histograma de la variable de ocesiones de consumo de bebidas energizantes", xlab="Ocasiones", ylab="Frequency", col=3)
boxplot(energalcoh$ocasionesener)

summary(energalcoh$motivoconsu)
energalcoh[is.na(energalcoh$motivoconsu),]
sd(energalcoh$motivoconsu)
length(energalcoh$motivoconsu)
hist(energalcoh$motivoconsu)
hist(energalcoh$motivoconsu, main = "Histograma de la variable de motivos de consumo")
hist(energalcoh$motivoconsu, main = "Histograma de la variable de motivos de consumo", xlab="motivos de consumo", ylab="Frequency", col=1)
boxplot(energalcoh$motivoconsu)

summary(energalcoh$tipoalcohol)
energalcoh[is.na(energalcoh$tipoalcohol),]
sd(energalcoh$tipoalcohol)
length(energalcoh$tipoalcohol)
hist(energalcoh$tipoalcohol)
hist(energalcoh$tipoalcohol, main = "Histograma de la variable de tipo de alcohol con se mezcla")
hist(energalcoh$tipoalcohol, main = "Histograma de la variable de tipo de alcohol con se mezcla", xlab="Tipo de alcohol", ylab="Frequency", col=8)
boxplot(energalcoh$tipoalcohol)

summary(energalcoh$dianormalconsu)
energalcoh[is.na(energalcoh$dianormalconsu),]
sd(energalcoh$dianormalconsu)
length(energalcoh$dianormalconsu)
hist(energalcoh$dianormalconsu)
hist(energalcoh$dianormalconsu, main = "Histograma de la variable del consumo de alcohol en un día de consumo normal")
hist(energalcoh$dianormalconsu, main = "Histograma de la variable del consumo de alcohol en un día de consumo normal", xlab="consumo de alcohol en un día normal", ylab="Frequency", col=7)
boxplot(energalcoh$dianormalconsu)


#Convertir la variable sexo en categorica con etiquetas
energalcoh$Sex <- factor(energalcoh$Sex,
                    levels = c(1,2),
                    labels = c("Masculino", "Femenino"))
summary(energalcoh$Sex)


#Convertir la variable Zona de residencia en categorica con etiquetas
energalcoh$Zonaresidencia <- factor(energalcoh$Zonaresidencia,
                      levels = c(1,2),
                      labels = c("Urbana", "Rural"))
summary(energalcoh$Zonaresidencia)


#Convertir la variable Situacion sentiemntal en categorica con etiquetas
energalcoh$Situasent <- factor(energalcoh$Situasent,
                    levels = c(1,2),
                    labels = c("soltero", "en una relacíón"))
summary(energalcoh$Situasent)

#Convertir la variable vives solo en categorica con etiquetas
energalcoh$Vivesolo <- factor(energalcoh$Vivesolo,
                               levels = c(0,1),
                               labels = c("si", "no"))
summary(energalcoh$Vivesolo)

#Convertir la variable has consumido bebidas energeticas en categorica con etiquetas
energalcoh$hasconsumbebenerg <- factor(energalcoh$hasconsumbebenerg,
                               levels = c(0,1),
                               labels = c("si", "no"))
summary(energalcoh$hasconsumbebenerg)


#Convertir la variable conoces su composición en categorica con etiquetas
energalcoh$Conocesucomp <- factor(energalcoh$Conocesucomp,
                                       levels = c(0,1),
                                       labels = c("si", "no"))
summary(energalcoh$Conocesucomp)


#Convertir la variable Tiene sabor agradable en categorica con etiquetas
energalcoh$saborgradable <- factor(energalcoh$saborgradable,
                                       levels = c(0,1,2),
                                       labels = c("si", "no", "tal vez"))
summary(energalcoh$saborgradable)


#Convertir la variable has combinado las bebidas energizantes con alcohol en categorica con etiquetas
energalcoh$Combinado <- factor(energalcoh$Combinado,
                                   levels = c(0,1),
                                   labels = c("si", "no"))
summary(energalcoh$Combinado)


summary(energalcoh$Edad)

boxplot(energalcoh$AñoU~energalcoh$Sex)

boxplot(energalcoh$AñoU~energalcoh$Zonaresidencia)

qqnorm(energalcoh$AñoU)
qqline(energalcoh$AñoU)


#Grafico de Barras de las variables cualitativas
#Crear una matriz 
table(energalcoh$Sex)
summary(energalcoh$Sex)

table(energalcoh$Zonaresidencia)
table(energalcoh$Situasent)
table(energalcoh$Vivesolo)
table(energalcoh$hasconsumbebenerg)
table(energalcoh$Conocesucomp)
table(energalcoh$saborgradable)
table(energalcoh$Combinado)

Sexo <- table (energalcoh$Sex)
Zonaresidencia <- table (energalcoh$Zonaresidencia)
Situacionsentimental <- table (energalcoh$Situasent)
Conquienvives <- table (energalcoh$Vivesolo)
hasconsumidoenergizantes <- table (energalcoh$hasconsumbebenerg)
ConocesComposición <- table (energalcoh$Conocesucomp)
TienesaborAgradable <- table (energalcoh$saborgradable)
Combinadoenergizantesconalcohol <- table (energalcoh$Combinado)

#Grafico de Barras
Sexo
barplot(Sexo, main="Distribucion según el sexo", xlab="Sexo", names.arg=c("Masculino", "Femenino"))

Zonaresidencia
barplot(Zonaresidencia, main = "Distribucion segùn Zona de residencia", xlab = "Zona de residencia", names.arg=c("Urbana", "Rural"))

Situacionsentimental
barplot(Situacionsentimental, main = "Distribucion segùn la Situación Sentimental", xlab = "Situación Sentimental", names.arg=c("soltero", "en una relacíón"))

Conquienvives
barplot(Conquienvives, main = "Distribucion segùn si vive solo", xlab = "Vives solo", names.arg=c("si", "no"))

hasconsumidoenergizantes
barplot(hasconsumidoenergizantes, main = "Distribucion segùn si consume bebidas energizantes", xlab = "Consumen bebidas energizantes", names.arg=c("si", "no"))

ConocesComposición
barplot(ConocesComposición, main = "Distribucion segùn si conoce su composición", xlab = "Conoce su composición", names.arg=c("si", "no"))

TienesaborAgradable
barplot(TienesaborAgradable, main = "Distribucion segùn si tiene sabor agradable", xlab = "Tiene sabor agradable", names.arg=c("si", "no"))

Combinadoenergizantesconalcohol
barplot(Combinadoenergizantesconalcohol, main = "Distribucion segùn si ha combinado energizantes con alcohol", xlab = "ha combinado energizantes con alcohol", names.arg=c("si", "no"))

#Grafico Cirular
pie(Sexo, main="Distribucion según el sexo", xlab="Sexo", names.arg=c("Masculino", "Femenino"))
pie(Zonaresidencia, main = "Distribucion segùn Zona de residencia", xlab = "Zona de residencia", names.arg=c("Urbana", "Rural"))
pie(Situacionsentimental, main = "Distribucion segùn la Situación Sentimental", xlab = "Situación Sentimental", names.arg=c("soltero", "en una relacíón"))
pie(Conquienvives, main = "Distribucion segùn si vive solo", xlab = "Vives solo", names.arg=c("si", "no"))
pie(hasconsumidoenergizantes, main = "Distribucion segùn si consume bebidas energizantes", xlab = "Consumen bebidas energizantes", names.arg=c("si", "no"))
pie(ConocesComposición, main = "Distribucion segùn si conoce su composición", xlab = "Conoce su composición", names.arg=c("si", "no"))
pie(TienesaborAgradable, main = "Distribucion segùn si tiene sabor agradable", xlab = "Tiene sabor agradable", names.arg=c("si", "no"))
pie(Combinadoenergizantesconalcohol, main = "Distribucion segùn si ha combinado energizantes con alcohol", xlab = "ha combinado energizantes con alcohol", names.arg=c("si", "no"))




#Tabla descriptiva
attach(energalcoh)
library(descr)
#Tabla de distribución de frecuencia de la variable sexo
tabla_sexo = freq(Sex, plot=F)
tabla_sexo
freq(Sex, plot=F)

#Tabla de distribución con porcentaje acumulado
tabla_sexo = freq(ordered(Sex), plot = F)
tabla_sexo

#Tabla de distribución de frecuencia de la variable Zona de residencia
tabla_Zonaderesidencia =freq(Zonaresidencia, plot=F)
tabla_Zonaderesidencia
freq(Zonaresidencia, plot=F)

freq(Situasent, plot=F)
freq(Vivesolo, plot=F)
freq(hasconsumbebenerg, plot=F)
freq(Tipodebebener, plot=F)
freq(Situasent, plot=F)
freq(Conocesucomp, plot=F)
freq(saborgradable, plot=F)
freq(Combinado, plot=F)
freq(tipoalcohol, plot=F)



#Tabla de contigencia
Situacionsentimental_sexo = crosstab(Sex, Situasent, prop.r = T, plot = F)
Situacionsentimental_sexo

Situacionsentimental_sexo = crosstab(Sex, Situasent, prop.c = T, plot = F)
Situacionsentimental_sexo

Situacionsentimental_sexo = crosstab(Sex, Situasent, prop.t = T, plot = F)
Situacionsentimental_sexo


#Nueva froma de generar gráficos a traves de ggplot
library(ggplot2)
attach(energalcoh)

boxplot(energalcoh)
ggplot(energalcoh, aes(x=AñoU))+
  geom_dotplot(dotsize = 0.6, binwidth = 4)
median(AñoU)

#ZDiagrama de cajas de la variable AñoU de acuero al sexo
qplot(Sex, AñoU, data = energalcoh, geom = "boxplot", fill=Sex)

#Histogrma 
ggplot(energalcoh, aes(x=AñoU))+
  geom_histogram(binwidth = 10, color="black", fill= "green1")+
  labs(x="AñoUniversitario (En años)", y="Frecuencia absoluta", title = "Histograma del Año Universitario")

ggplot(energalcoh, aes(x=Tipodebebener))+
  geom_histogram(binwidth = 3, color="black", fill= "violet")+
  labs(x="Tipo de bebidas energéticas (Algunos)", y="Frecuencia absoluta", title = "Histograma de tipo de bebidas energeticas")

a= ggplot(Pulse, aes(Sex))+
  geom_bar(fill="skyblue3", width = 0.3)+
  labs(x="Sexo", y="Frecuencia absoluta", title = "Distribución según el sexo")+
  theme_minimal(base_size = 14)
a

a= ggplot(Pulse, aes(Sex))+
  geom_bar(fill="skyblue3", width = 0.3)+
  labs(x="Sexo", y="Frecuencia absoluta", title = "Distribución según el sexo")+
  theme_minimal(base_size = 14)
a

b= ggplot(Pulse, aes(Sex, fill= Smoke))+
  geom_bar(position = "dodge")+
  scale_fill_brewer(palette = "Dark2")+
  labs(x="Sexo", y="Frecuencia absoluta", title= "Habito de Fumar según el sexo")+
  theme_minimal(base_size = 14)+
  theme(plot.title = element_text(hjust = 0.5))
b


#categorizar una variable numérica
summary(energalcoh$`16.¿Con que frecuencia consume alguna bebida alcoholica?`)
#Consumo normal esta entre 3 y 5, sano, consumo menor a 3, entre 3 y 5 consumo normal. 
# mayor a 5 Borracho
energalcoh$catenergalcoh[energalcoh$`16.¿Con que frecuencia consume alguna bebida alcoholica?`<3]<-"Sano"
energalcoh$catenergalcoh[energalcoh$`16.¿Con que frecuencia consume alguna bebida alcoholica?`>=3 & energalcoh$`16.¿Con que frecuencia consume alguna bebida alcoholica?`<5]<-"Lo normal"
energalcoh$catenergalcoh[energalcoh$`16.¿Con que frecuencia consume alguna bebida alcoholica?`>=5]<-"Borracho"
energalcoh$catenergalcoh <- factor(energalcoh$catenergalcoh)



library(descr)
attach(energalcoh)
freq(catenergalcoh, plot=F)
boxplot(`16.¿Con que frecuencia consume alguna bebida alcoholica?`)

subset = subset(Pulse, select = c("Active","Rest","Sex"))
#Borrar las personas fumadoras
nofumadores <- subset(Pulse, Smoke=="No Fumadores")

#Borrar a las personas con bradiesfigmia
pulson_alto = subset(Pulse, catpulso=="Pulso normal" | catpulso=="Taquiesfitmia")
freq(pulson_alto$catpulso, plot=F)

#Borrar a las personas con Taquiesfitmia
pulson_bajo = subset(Pulse, catpulso=="Pulso normal" |  catpulso=="Bradiesfigmia")
freq(prueba$catpulso, plot=F)

#Borrar personas con bradiesfitgmia
summary(Pulse$Rest)
pulson_alto = subset(Pulse, catpulso=="Taquiesfitmia") 
summary(pulson_alto$catpulso)

versionversion

