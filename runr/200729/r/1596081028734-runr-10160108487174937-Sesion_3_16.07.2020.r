#---------------------------------------------------------------------------------------------
#CREAR DATA-FRAMES
#---------------------------------------------------------------------------------------------
id <- c(1,2,3,4,5)
nombre <- c("Juan", "Margarita", "Rubén", "Daniel", "Diana")
apellido <- c("Sánchez", "García", "Rosales", "Mora", "Méndez")
sexo <- c("Masculino", "Femenino", "Masculino", "Masculino", "Femenino")
#
base1 <- data.frame(id,nombre, apellido, sexo)
class(base1)
View(base1)
cnames<-c("Id","Nombre","Apellido","Sexo")
colnames(base1) <- cnames
View(base1)
#
hijos<-c(0,1,5,3,2)
edad <-c(30,35,40,29,50)
id<-c(2,4,6,7,1)
base2<-cbind(id,hijos,edad)
cnames<-c("Id","Hijos","Edad")
colnames(base2) <- cnames
View(base2)
#
base3<-merge(x=base1, y=base2, by="Id")
View(base3)
#
base4<-merge(x=base1, y=base2, by="Id", all.x = TRUE)
View(base4)
#
base5<-merge(x=base1, y=base2, by="Id", all.y = TRUE)
View(base5)
#
merge(x=base1, y=base2, by = "Id", all = TRUE)
#
id <- c(1,2,3,4,5)
educ <- c("Licenciatura", "Licenciatura", "Posgrado", "Licenciatura", "Preparatoria")
base6<-data.frame(id,educ)
cnames<-c("Id","Educación")
colnames(base6)<- cnames
View(base6)
#
base7 <- cbind(base1,base6$Educación)
View(base7)
base8<-merge(x=base1, y=base6, by="Id")
View(base8)
#
linea<-data.frame(6,"Rosa","Martínez","Femenino","Posgrado")
cnames<-c("Id","Nombre","Apellido","Sexo","Educación")
colnames(linea)<- cnames
View(linea)
base9<-rbind(base8,linea)
View(base9)

#------------------------------------------------------------------------------------------------
#CARGAR DATOS
#------------------------------------------------------------------------------------------------
install.packages("haven")
#
library(haven)
nsw <- read_dta("C:/Users/HP PROBOOK/Dropbox/Econometrics/Eco3/nsw.dta")
View(nsw)
class(nsw)
nsw<-as.data.frame(nsw)
attach(nsw)


#--------------------------------------------------------------------------------------------
#Tablas de frecuencia simples
#--------------------------------------------------------------------------------------------
table(treat)
table(treat, useNA='always')
table(treat, exclude=c(0))
tabla1<-table(treat)
tabla1
#
prop.table(tabla1, margin=NULL)
#
tabla2<-table(nsw$treat,nsw$black)
cnamesf <- c("Control", "Tratamiento")
cnamesc <- c("Blanco", "No blanco")
rownames(tabla2) <- cnamesf
colnames(tabla2) <- cnamesc
tabla2
tabla3=prop.table(tabla2, margin=NULL)
#
addmargins(tabla2)
addmargins(tabla3)
addmargins(tabla3,margin=1)
addmargins(tabla3,margin=2)

#--------------------------------------------------------------------------------------------
#Gráficos clásicos
#--------------------------------------------------------------------------------------------
counts <- table(age)
barplot(counts, xlab="Edades", ylab="Frecuencia", main="Gráfica de barras simple",col="blue")
#
barplot(counts, main="Gráfica de barras simple",col="gray", horiz=TRUE,
        xlab="Frecuencia", ylab="Edades")
#
counts <- table(nsw$treat,nsw$age)
cnames <- c("Control", "Tratamiento")
rownames(counts)
rownames(counts) <- cnames
barplot(counts, main="Distribución de edades por tratamiento",
        xlab="Edad", col=c("darkblue","red"), ylab="Frecuencia",
        legend = rownames(counts))
#
counts <- table(nsw$treat,nsw$age)
rownames(counts) <- cnames
barplot(counts, main="Distribución de edades por tratamiento",
        xlab="Edad", col=c("darkblue","red"), ylab="Frecuencia",
        legend = rownames(counts),beside=TRUE)

#--------------------------------------------------------------------------------------
#Histogramas
#--------------------------------------------------------------------------------------
hist(age)
hist(age,freq = FALSE, nclass=50, main = "Histograma",col="gray",xlab="Edad",ylab="Densidad")
curve(dnorm(x, mean=mean(age), sd=sd(age)), to=max(age), col ="red",lwd = 1, add = TRUE)
hist(age,freq = TRUE, nclass=50, main = "Histograma",col="blue",xlab="Edad",ylab="Frecuencia")
#
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
hist(nsw$age, col = "green",xlab="Edad",ylab="Frecuencia",main="Histograma: edad")
hist(nsw$education, col = "red",xlab="Educación",ylab="Frecuencia",main="Histograma: educación")


#--------------------------------------------------------------------------------------
#Box-plot
#--------------------------------------------------------------------------------------
boxplot(age,age)
boxplot(age[treat==0],age[treat==1], col="gray",xlab="Treatments",cex.lab=0.75,cex.axis=0.7,ylab="Edad")
boxplot(age[treat==0],age[treat==1], col="gray",xlab="Treatments",cex.lab=0.75,cex.axis=0.7,ylab="Edad")
boxplot(age ~ treat, data=nsw, col="deepskyblue", cex.axis=0.7,las = 2, ylab="Edad", xlab="Tratamiento", cex.lab=0.75)


#---------------------------------------------------------------------------------------
#Dispersión
#---------------------------------------------------------------------------------------
plot(age,education,col="blue",xlab="Edad",ylab="Educación",main="Diagrama de dispersión")
grid()
#
par(mfrow = c(1, 2), mar = c(5, 4, 2, 1))
with(subset(nsw, treat == "0"), plot(age,education, main = "Control",col="blue",xlab="Edad",ylab="Educación"))
grid()
with(subset(nsw, treat == "1"), plot(age,education, main = "Tratamiento",col="blue",xlab="Edad",ylab="Educación"))
grid()



#-------------------------------------------------------------------------------------------------
#Gráficos de densidad-kernel
#-------------------------------------------------------------------------------------------------
density1 <- density(nsw$age) 
plot(density1,xlab="Edad",ylab="Densidad",main="Función de densidad",col="red")
grid()
polygon(density1, col="blue", border="black")
#
#
dim(nsw)
dim(nsw)[1]
dim(nsw)[2]
rend=rnorm(dim(nsw)[1],mean=0.03,sd=0.1)
plot(rend)
plot(rend,type="l",col="blue",xlab="Periodos",ylab="Rendimentos",main="Serie de tiempo")
grid()
#
density2 <- density(rend)
hist(rend,freq = FALSE, nclass=100, main = "Histograma",col="gray",xlab="Rendimiento",ylab="Densidad")
lines(density2,col="red")
curve(dnorm(x, mean=mean(rend), sd=sd(rend)), to=max(rend), col ="blue",lwd = 1, add = TRUE)


#-------------------------------------------------------------------------------------------------
#Regresión simple
#-------------------------------------------------------------------------------------------------
plot(re75~education,data=nsw,pch=16,xlab="Educación",ylab="Ingreso",main="Regresión simple")
abline(lm(re75~education,data=nsw),col="red")
grid()
#
model<-lm(re75~education,data=nsw)
summary(model)


#---------------------------------------------------------------------------------------------------
#Gráficos extendidos: introducción
#---------------------------------------------------------------------------------------------------
library(ggplot2)
qplot(nsw$education,nsw$re75,xlab="Educación",ylab="Ingreso")
#
qplot(nsw$education,nsw$re75,geom=c("point", "line"),xlab="Educación",ylab="Ingreso")
#
qplot(nsw$education,nsw$re75,geom = c("point", "smooth"),xlab="Educación",ylab="Ingreso")







