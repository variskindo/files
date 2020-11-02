library(tidyverse)
library(readr)
peliculas <- read_csv("C:/Users/jwile/Dropbox/Data Science/R/peliculas.csv")
View(peliculas)
head(peliculas)
peliculas
## Parte I
## a) Con cuantas películas cuenta el catálogo? 

nrow(peliculas)

## b) Cuántas películas no tienen información de presupuesto?

Presupuesto0 = filter(peliculas, budget == 0)
nrow(Presupuesto0)

## c)Cuántas películas hay de Walt Disney?

WaltDisney2 = str_which(peliculas$company, "Walt")
WaltDisney = peliculas[WaltDisney2,]

nrow(WaltDisney)

## d) Directores con nombre Stuart

Stuart = str_which(peliculas$director, "^Stuart")
Stuart = peliculas[Stuart,]
nrow(Stuart)


## e) Cuál es el género que más aparece en el sitio? 
Gen = as.data.frame(table(peliculas$genre))
Ord = order(Gen$Freq, decreasing = TRUE)
Gen= Gen[Ord,] 
head(Gen, n = 1L)
## Parte II 

# a) Porcentaje UK y score igual o mayor 7 

UK7 = filter(peliculas, country == "UK" & score >= 7)
nrow(UK7)
(nrow(UK7)/nrow(peliculas))*100

# b) Películas no de acción con Tom Cruise
Tom = filter(peliculas, star == "Tom Cruise" & genre != "Action")
nrow(Tom)


# c) Entre películas de drama, cuál posee mayor presupuesto?

Drama = filter(peliculas, genre == "Drama") 
Ord = order(Drama$budget, decreasing = TRUE) 
Drama = Drama[Ord, ]  
head(Drama, n = 1L)

# d) ¿Cuáles son las tres películas realizadas en 1990 con mayor score? 

Año = filter(peliculas, year == 1990)
Ord = order(Año$score, decreasing = TRUE)
Año = Año[Ord,]
head(Año, n = 3L)

# e) ¿Quién ha dirigido más películas de comedia? ¿Cuántas fueron? ¿Cuál es la más larga?
#Primero director y número de películas
Comedia = filter(peliculas, genre == "Comedy")
DirComedia = as.data.frame(count(Comedia, director))
DirComedia
Ord = order(DirComedia$n, decreasing = TRUE)
DirComedia = DirComedia[Ord,]
head(DirComedia, n = 1L)
#Ahora película más larga
Woody= filter(peliculas, director  == "Woody Allen")
ord = order(Woody$runtime, decreasing = TRUE)
Woody = Woody[ord,]
head(Woody, n = 1L)

 ## Parte III 
Warner = filter(peliculas, company == "Warner Bros.")
Año = group_by(Warner, year) 
Tabla = summarize(Año, maxi = max(budget), peliAño = n())
Tabla         

Gene = group_by(Warner, genre)
Tabla2=summarize(Gene, promGen = mean(budget), sumVotos = sum(votes))
Tabla2

## Parte IV

IV = group_by(Warner, year,genre)
IV = summarize(IV, Valores = mean(score))
IV = spread(IV, key = genre, value = Valores)


