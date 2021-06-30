#Examen 3# 
#29-6-2021#
#Cecilio Chacón Soto B31890#

#Pregunta 1, ¿cuál columna contiene datos que no se distribuyen normalmente"#

#Pregunta 2, Ordenar los promedios de cada columna de menor a mayor"#

#Pregunta 3: #Falso o verdadero# 
#El promedio de la columna 1 es estadísticamente diferente del promedio de la columna 2 (use un alfa=0.05)#

#Preguna 4: #Falso o verdadero# 
#El promedio de la columna 1 es estadísticamente diferente del promedio de la columna 3 (use un alfa=0.05)#

#Script#

setwd("C:/Users/Control/Desktop/R/Quiz 3")

quiz3 <- read.csv(file="C:/Users/Control/Desktop/R/Quiz 3/Quiz3.csv", header=T, dec=".", sep=",")

# Pruebas 

shapiro.test(quiz3$columna1)
shapiro.test(quiz3$columna2)
shapiro.test(quiz3$columna3)
shapiro.test(quiz3$columna4)


summary(quiz3)

# Comparaciones
t.test(quiz3$columna1,quiz3$columna2 )
t.test(quiz3$columna1,quiz3$columna4 )



                  