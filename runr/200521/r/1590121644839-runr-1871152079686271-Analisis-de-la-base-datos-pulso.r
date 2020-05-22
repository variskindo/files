
#Abrir la base de datos pulso
library(readxl)
Pulse <- read_excel("C:/Users/ALEX/Desktop/4to ciclo/Pulse.xlsx")

#Estadistica Descriptiva 
summary(Pulse)

summary(Pulse$Active)
Pulse[is.na(Pulse$Active),]
sd(Pulse$Active)
length(Pulse$Active)
hist(Pulse$Active)
hist(Pulse$Active, main = "Histograma de la variable active")
hist(Pulse$Active, main = "Histograma de la variable active", xlab="Frequency", ylab="Activo", col=5)
boxplot(Pulse$Active)

summary(Pulse$Rest)
sd(Pulse$Rest)
length(Pulse$Rest)
hist(Pulse$Rest)
hist(Pulse$Rest, main = "Histograma del pulso en descanso")
hist(Pulse$Rest, main = "Histograma del pulso en descanso", xlab="Frequency", ylab="pulso en descanso", col=2)
boxplot(Pulse$Rest)

summary(Pulse$Hgt)
sd(Pulse$Hgt)
length(Pulse$Hgt)
hist(Pulse$Hgt)
hist(Pulse$Hgt, main = "Histograma de la altura")
hist(Pulse$Hgt, main = "Histograma de la altura", xlab="Frequency", ylab="Altura", col=8)
boxplot(Pulse$Hgt)

summary(Pulse$Wgt)
sd(Pulse$Wgt)
length(Pulse$Wgt)
hist(Pulse$Wgt)
hist(Pulse$Wgt, main = "Histograma del peso")
hist(Pulse$Hgt, main = "Histograma del peso", xlab="Frequency", ylab="Peso", col=3)

boxplot(Pulse$Wgt)

#Convertir la variable smoke en categorica con etiquetas
Pulse$Smoke <- factor(Pulse$Smoke,
                       levels = c(0,1),
                       labels = c("No Fumadores", "Fumadores"))

summary(Pulse$Smoke)

#Convertir la variable sexo en categorica con etiquetas
Pulse$Sex <- factor(Pulse$Sex,
                      levels = c(0,1),
                      labels = c("Masculino", "Femenino"))

summary(Pulse$Sex)

#Convertir la variable ejercicio en categorica con etiquetas
Pulse$Exercise <- ordered(Pulse$Exercise,
                    levels = c(1,2,3),
                    labels = c("1 dia", "2 dias", "3dias"))

summary(Pulse$Exercise)

boxplot(Pulse$Active~Pulse$Sex)

boxplot(Pulse$Active~Pulse$Smoke)

qqnorm(Pulse$Active)
qqline(Pulse$Active)

table(Pulse$Sex)
summary(Pulse$Sex)

table(Pulse$Smoke)
table(Pulse$Active)

Sexo <-table(Pulse$Sex)
Sexo
barplot(Sexo, main = "Distribucion segùn sexo", xlab = "Sexo", names.arg=c("Masculino", "Femanino"))

#Tabla descriptiva
attach(Pulse)
library(descr)
#Tabla de distribución de frecuencia de la variable sexo
tabla_sexo = freq(Sex, plot=F)
tabla_sexo
freq(Sex, plot=F)
freq(Sex, plot=T)
#Tabla de distribución con porcentaje acumulado
tabla_sexo = freq(ordered(Sex), plot = F)
tabla_sexo

freq(Smoke, plot=F)

#Tabla de contigencia
fumar_sexo = crosstab(Sex, Smoke, prop.r = T, plot = F)
fumar_sexo

fumar_sexo = crosstab(Sex, Smoke, prop.c = T, plot = F)
fumar_sexo

fumar_sexo = crosstab(Sex, Smoke, prop.t = T, plot = F)
fumar_sexo

#Nueva froma de generar gráficos a traves de ggplot
library(ggplot2)
attach(Pulse)

boxplot(Active)
ggplot(Pulse, aes(x=Active))+
  geom_dotplot(dotsize = 0.6, binwidth = 4)
median(Active)

#ZDiagrama de cajas de la variable Active de acuero al sexo
qplot(Sex, Active, data = Pulse, geom = "boxplot", fill=Sex)

#Histogrma 
ggplot(Pulse, aes(x=Active))+
  geom_histogram(binwidth = 10, color="black", fill= "green1")+
  labs(x="Pulso(Latidos por minuto)", y="Frecuencia absoluta", title = "Histograma del pulso luego de relizar actividad fisica")
  
ggplot(Pulse, aes(x=Rest))+
  geom_histogram(binwidth = 5, color="black", fill= "violet")+
  labs(x="Pulso(Latidos por minuto)", y="Frecuencia absoluta", title = "Histograma del pulso en reposo")

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
summary(Pulse$Rest)
#El pulso normal esta entre 60 y 100, bradiesfigmia, pulso menor a 60, entre 60 y 99 pulso normal. 
# mayor a 100 taquiesfigmia
Pulse$catpulso[Pulse$Rest<60]<-"Bradiesfigmia"
Pulse$catpulso[Pulse$Rest>=60 & Pulse$Rest<100]<-"Pulso normal"
Pulse$catpulso[Pulse$Rest>=100]<-"Taquiesfitmia"
Pulse$catpulso <- factor(Pulse$catpulso)

library(descr)
attach(Pulse)
freq(catpulso, plot=F)
boxplot(Rest)

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
