
#Permite leer los datos
#====================
banco<-read.delim("clipboard")
#Permite utilizar las etiquetas del conjunto de datos
attach(banco)

#====================
#GRÁFICOS UNIVARIADOS
#====================

#====================
#Diagrama de puntos
#====================
dotchart(table(NIF), cex=0.7, main="Distribución del Número de Integrantes de la Familia ", xlab="# de Personas")
dotchart(table(Genero), cex=0.7, main="Distribución del Género", xlab="# de Clientes", ylab="Género")
stripchart(NIF)
stripchart(NIF~Sucursal)

#######################
#Diagrama de Intervalos
#######################
library(psych)
error.bars(Ingreso)
error.bars(Ingreso,main="Gráfico de intervalo",ylab="Ingreso",col="green",alpha=0.02)

###########################
#Diagrama de tallos y hojas
###########################
stem(Ingreso)
stem(Ingreso[Sucursal=="San Borja"])
by(Ingreso,Sucursal,stem)

#Otra forma creando un subconjunto
SB<-subset(banco,Sucursal=="San Borja")
SI<-subset(banco,Sucursal=="San Isidro")
SL<-subset(banco,Sucursal=="San Luis")
stem(SB$Ingreso)

##################
#Diagrama de cajas
##################
boxplot(Ingreso)
boxplot(Ingreso~Sucursal)
boxplot(Ingreso~Sucursal,horizontal=TRUE,col=3)
summary(Ingreso)
tapply(Ingreso,Sucursal, summary)


###############################
#Diagrama de densidad de Kernel
###############################
plot(density(Ingreso,kernel="gaussian"))

#Permite dividir la pantalla gráfica en tres filas y una columna
library(agricolae)
par(mfrow = c(3, 1))
boxplot(Ingreso)
histo<-hist(Ingreso,breaks = "Sturges", xlab="Monto", ylab="Número de clientes",border=F)
polygon.freq(histo,frequency=1,col="red")
plot(density(Ingreso,kernel="gaussian"),col="blue",main="Gráfico de Densidad",xlab="Ingreso de clientes",ylab="Cantidad")

plot(density(SB$Ingreso,kernel="gaussian"),col="blue",main="San Borja",xlab="Ingreso de clientes",ylab="Cantidad")
plot(density(SI$Ingreso,kernel="gaussian"),col="red",main="San Isidro",xlab="Ingreso de clientes",ylab="Cantidad")
plot(density(SL$Ingreso,kernel="gaussian"),col="green",main="San Luis",xlab="Ingreso de clientes",ylab="Cantidad")


################
#Diagrama Violin
################
par(mfrow = c(1,1))
library(vioplot)
#Crea subconjuntos
vioplot(SB[,3],SI[,3])
vioplot(SB$Ingreso,SI$Ingreso)

library(violinmplot)
violinmplot(Sucursal~Ingreso,data=banco)
violinmplot(Ingreso~Sucursal,data=banco)

#Divide la pantalla gráfica en dos filas y una columna
par(mfrow=c(2,1))
mu<-2
si<-0.6
bimodal<-c(rnorm(1000,-mu,si),rnorm(1000,mu,si)) 
uniform<-runif(2000,-4,4)
normal<-rnorm(2000,0,3)
vioplot(bimodal,uniform,normal)
boxplot(bimodal,uniform,normal)

###############################
#Gráfico de probabilidad Normal
###############################
par(mfrow=c(1,1))
qqnorm(Ingreso)
qqline(Ingreso)

#####################
#Piramide Poblacional
#####################
library(plotrix)
m.pop<-c(3.2,3.5,3.6,3.6,3.5,3.5,3.9,3.7,3.9,3.5,3.2,2.8,2.2,1.8,1.5,1.3,0.7,0.4)
f.pop<-c(3.2,3.4,3.5,3.5,3.5,3.7,4,3.8,3.9,3.6,3.2,2.5,2,1.7,1.5,1.3,1,0.9)
edades<-c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-44","85+")
mcol<-color.gradient(c(0,0,0.5,1),c(0,0,0.5,1),c(1,1,0.5,1),18)
fcol<-color.gradient(c(1,1,0.5,1),c(0.5,0.5,0.5,1),c(0.5,0.5,0.5,1),18)
par(mar=pyramid.plot(m.pop,f.pop,labels=edades,top.labels=c("Masculino","Edades","Femenino"),main="Piramide Poblacional de Australia 2002",gap=0.5,show.values=TRUE))
par(mar=pyramid.plot(m.pop,f.pop,labels=edades,top.labels=c("Masculino","Edades","Femenino"),main="Piramide Poblacional de Australia 2002",lxcol=mcol,rxcol=fcol,gap=0.5,show.values=TRUE))

#######################
#GRÁFICOS MULTIVARIADOS
#######################

###################
#Gráfico de Mosaico
###################
library(vcd)
mosaicplot(~Sucursal+Genero, data = banco, color = 1:2, main="Distribución del Género según sucursal")

######################
#Gráfico de Dispersión
######################
plot(Ingreso,Crédito,col=3)

########################
#Matriz de Dispersión 2D
########################
pairs(banco[,3:6])
library(car)
scatterplotMatrix(banco[,3:6])

########################
#Matriz de Dispersión 3D
########################
library(scatterplot3d)
scatterplot3d(banco[,3:5],main="Gráfico de Dispersión 3D")

#########################
#Gráfico de correlaciones
#########################
library(corrplot)
M<-cor(banco[,3:6],method="spearman")
corrplot(M, method="circle")
corrplot(M, method="number")
corrplot(M, method="color")
corrplot(M, type="upper")
corrplot.mixed(M)
corrplot.mixed(M, lower="ellipse", upper="circle")

library(corrgram)
corrgram(banco[,3:6],lower.panel=panel.cor,upper.panel=panel.conf,cor.method="pearson")

library(psych)
pairs.panels(banco[,3:6])

library(PerformanceAnalytics)
chart.Correlation(banco[,3:6], histogram=TRUE, pch=20)

library(GGally)
ggpairs(banco[,3:6])


##################
#Caras de Chernoff
##################
library(aplpack)
faces(banco[1:20,3:6])
faces(banco[1:20,3:6], face.type=2)
faces(mtcars[1:10,-c(8,9)])
faces(mtcars[1:20,c(-8,-9)])
faces(abalone[1:10,-1])

library(TeachingDemos)
faces2(banco[1:20,3:6])

library(DescTools)
PlotFaces(banco[1:20,3:6])

#####################
#Gráfico de Estrellas
#####################
library(graphics)
stars(banco[1:10,3:6])

#################################
#Gráfico de Coordenadas Paralelas
#################################
library(MASS)
parcoord(banco[1:20,3:6],col=c(1,2,3,4),var.label = T)
#Se debe usar siempre y cuando los grupos se encuentre ordenados
parcoord(banco[1:21,3:6],col=1 + (0:20)%/%7)

#################
#Nube de palabras
#################
#Instalar paquetes
install.packages("tm")  # para text mining
install.packages("SnowballC") # para el texto derivado
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes

#Cargar paquetes
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#Lectura de datos
filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
text <- readLines(filePath)

#Carga los datos como un corpus
docs <- Corpus(VectorSource(text))

#Permite inspeccionar el documento
inspect(docs)

#Cambia caracteres especiales por espacio en blanco
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convierte el texto a minúsculas
docs <- tm_map(docs, content_transformer(tolower))
# Elimina números
docs <- tm_map(docs, removeNumbers)
# Elimina comunes palabras "stopwords" en english  
docs <- tm_map(docs, removeWords, stopwords("english"))
# Elimina propios stopword
# Se debe especificar stopwords como un vector de caracteres
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Elimina puntuaciones
docs <- tm_map(docs, removePunctuation)
# Elimina espacios en blanco extras
docs <- tm_map(docs, stripWhitespace)

#Construye el texto como una matriz
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#Genera el Word Cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

