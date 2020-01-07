#Preparamos la base para trabajar con ella

install.packages("readxl")
install.packages("foreign")
library(readxl)
library(foreign)

download.file(url = "https://raw.githubusercontent.com/PazCastroCL/metodosdeinvestigacion/master/base_droga_mod.xlsx",
              destfile = "base_droga.xlsx", method = "curl")

base_droga <- read_excel("base_droga.xlsx")

summary(base_droga)

#Notamos que hay columnas con datos numericos pero que su clase es "character". Hay que cambiarlo

install.packages("dplyr")
library(dplyr)

base_droga <- mutate_at(base_droga, vars(urbancenso,
                                         indigena,
                                         alcant,
                                         electr,
                                         gini,
                                         IPC,
                                         escolaridad,
                                         IDH), as.numeric)
#ya estan cambiadas las clases
#función para que los NA se reconoscan como valores reales

base_droga[] <- lapply(base_droga, function(x) {
  is.na(levels(x)) <- levels(x) == "NA"
  x
})

# Paz: agregueé table aqui, para que se vea claramente cuaántos Nas tienes.
table(is.na(base_droga))

#Ahora omitimos los NA

# Paz: aqui solo estas diciendo "muestrame"...pero no estas asignando. Yo acabo de asignar ese "muestrame la base sin NAs a un nuevo objeto 'base_droga.no.na'
base_droga.no.na = na.omit(base_droga)

#Ahora tenemos una base limpia, por lo tanto comenzamos el analisis de la base para crear 
#nuestra pregunta de investigación.

#CODEBOOK
#provincia
#region	
#pob2017:	poblacion 2017
#urbancenso: porcentaje de gente urbana	
#indigena: porcentaje de indigenas	
#superficie:  superficie de la comuna	
#alcant:	 porcentaje de gente con alcantarillado
#electr: porcentaje de gente con electricidad
#pobreza:	 porcentaje de pobres
#escuelas:	numero de escuelas publicas
#salud:	 numero de recintos de salud publicos
#crimen:	crimenes de mayor connotacion social (asesinatos, robos, etc)
#gini:	 coeficiente de gini mide desigualdad
#IPC:  ingreso per capita de la comuna
#escolaridad:	años de escolaridad
#IDH:	indice de desarrollo humano
#pdtotal:  personas detenidas producto de infringir la ley de drogas

#PREGUNTA DE INVESTIGACIÓN:

# ¿ Cuales son los factores que pueden provocar que haya un mayor número de personas detenidas 
# por infringir la ley de drogas?
# Paz: esta pregunta esta interesante. 

#HIPOTESIS DE INVESTIGACIÓN

#Hipotesis 0: 
# Una Hipotesis plantea que los factores que pueden afectar que haya un mayor número de
#personas detenidas por inflingir la ley de drogas son: la pobreza, coeficiente de gini
# y el número de recintos de salud pública.



#Hipotesis 1(alternativa)
#Una Hipotesis alternativa es que los factores que pueden afectar que haya un mayor número 
#de personas detenidas por inflingir la ley de drogas son: la pobreza, coeficiente de gini,
#el número de recintos de salud pública, la escolaridad y el número de escuelas públicas.

# Paz: mmm. No sé. Leo ambas hipotesis, y las encuentro muy parecidas Por que mejor no pensar en las siguientes hipotesis? Es una propuesta. La primera hipotesis (la 0) puede ser la que dicen. Es decir, mas pobreza causa mas droga. La teoria podria ser una idea de falta de oportunidades te lleva a evadir la realidad, y consumir drogas. La alternativa, puede que, en verdad, el consumo de droga tiene que ver con un fenomeno urbano. A mas poblacion urbana, mas consumo de droga. Es decir, cuando urbancenso sube, pdtotal sube tambien. La teoria podria ser que en verdad, la gente al sentirse desafectado y sin un lugar de pertenencia (independiente de si eres rico o pobre), consumes mas drogas. La H0 es economica, la H1 es sociologica. Piensen en los controles. Tanto H0 como H1 se confirman/rechazan viendo el coeficiente y significancia de gini (para H0) y urbancenso (en H1). Pueden incluso tener el mismo set de controles para ambas hipotesis, solo que en un modelo (el de la H0), la varianle importante es gini, mientras que en el otro modelo (H1) la variable importante es urbancenso.


#CREACIÓN DE MODELOS

#apagamos la notación cientifica
options(scipen = 999)

hipotesis.0 = lm(pdtotal ~ pobreza + gini + salud , base_droga) #juega con las variables a ver si pillas algo interesante
summary(hipotesis.0)


hipotesis.1 = lm(pdtotal ~ pobreza + gini + salud + escuelas + escolaridad , base_droga)
summary(hipotesis.1)

#problema con como se ven las variables

#TABLA DE COMPARACIÓN ENTRE AMBOS MODELOS
install.packages("pacman")
library(pacman)

p_load(texreg)
screenreg(list(hipotesis.0,
               hipotesis.1))

