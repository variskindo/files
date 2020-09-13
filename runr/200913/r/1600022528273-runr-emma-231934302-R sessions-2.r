# SESSION #1 2020/09/10
# OPERATIONS
## Diff�rentes op�rations:
1+1
5-4
2*3
8/(3-2)

## Faire une s�quence de nombres entiers de 10 � 20:
10:20

# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------

# OBJETS
a <- 1
a + 2

### Exercice 1. : 
### 1.  Cr�er un objet appel� "n" et donner lui une s�quence de nombre de
###     10 � 20 et faire l'op�ration "+" de cet objet avec "a". (observer le r�sultat)
### 2.  Cr�er un second objet de la m�me longueur que n, appeler le m.
###     Faire les op�rations +, - et * entre n et m.

## Voir la liste des objets cr��s
ls()

## Autres op�rations avec les s�quences de nombres entiers
n = 1:12
n%*%n
n%o%n

# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------

# FONCTIONS
## Arrondir
round(1.55466, 3)
## Calcul de factoriel
factorial(5)
## Calcul de la moyenne d'une s�quence de nombre 
mean(1:12)
mean(n)
## Combinaison de fonctions
round(mean(n))
## Prendre un �chantillon d'une s�quence de nombre
sample(x=1:100, size = 100*.05)
## Arguments d'une fonction
args(round)
?sample
## Faire un �chantillon avec remise en faisant une simulation avec une paire de d�s de 
## six (6) faces.
die <- 1:6
dice <- sample(die, size = 2, replace = TRUE)
dice
## Construire sa propre fonction
## Format :
ma_fonction <- function() {}
## Exemple d'une fonction qui calcul la superficie d'un rectangle
superficie_rectangle <-  function(L, l) {
  superficie <- L*l
  superficie
}
superficie_rectangle(9,5)

# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------
# BIBLIOTHEQUES
# Installer une biblioth�que
install.packages("ggplot2")
# Charger une biblioth�que
library("ggplot2")
# Faire un nuage de points 
x <- c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
x
y <- x^3
qplot(x, y)
# Faire un histogramme
x <- c(5, 5, 5, 8, 7, 5, 7, 6, 6, 6)
qplot(x, binwidth = 1)
?ggplot()
# Cr�er un �chantillon de donn�es al�atoire
# Initialiser une graine
set.seed(8)
# Cr�er une distribution de poisson avec un certain nombre de valeurs al�toires
df <- data.frame(taille_m = rpois(75, 1.5))
# Regarder ce que fait la fonction rpois
?rpois()
# V�rifier la taille de l'�chantillon
sum(df$taille_m)/length(df$taille_m)
min(df$taille_m)
max(df$taille_m)
# En faire un histogramme tout en affichant les valeurs
ggplot(df, aes(x=taille_m)) + stat_bin(binwidth = 1) +
  ylim(c(0, 30)) + stat_bin(binwidth = 1, geom = "text", aes(label=..count..),
                           vjust =-1.5) 
### Exercice 2. :
### Cr�er un dataframe plus de 50 observations avec des variables qui 
### suivent une distribution normale, khi-deux, poisson et en faire les histogrammes. 

# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------

# SESSION 2 20200911
## Cr�er un dataframe avec plusieurs colonnes 
# Pour la reproduction
set.seed(80)
df <- data.frame(taille_m = rpois(50, 1.3), poids_norm = rnorm(50))
df
# Changer son r�pertoire de travail 
setwd("E:\\")
# Charger un fichier excel csv
avocado <- read.csv("avocado.csv",
               header = TRUE,
               quote = "\"",
               stringsAsFactors = TRUE,
               strip.white = TRUE, na.strings = "")

## Charger un workbook excel "xlsx"
install.packages("file_name.xlsx")
library("xlsx")
read.xlsx()

## Charger un workbook excel "xls"
install.packages("gdata")
library(gdata)
# Read first sheet
data <- read.xls("file_name.xls")

## Charger un fichier SPSS
install.packages("foreign")
library(foreign)
data <- read.spss("file_name.sav")

## Identifier le nom des colonnes dans un df (dataframe)
names(avocado)
## Acc�der � une colonne d'un dataframe
avocado$Total.Volume
## Lister les valeurs uniques d'une colonnes d'un df
unique(avocado$year)

## Faire la somme de plusieurs colonnes 
avocado$AveragePrice + avocado$AveragePrice^2
## Cr�er une nouvelle colonne dans un df
avocado$PrixMoyenCarre <- avocado$AveragePrice^2
avocado$PrixMoyenCarre
names(avocado)

## Fonction "si", "si, sinon"
## Syntaxes:
# if(TRUE|FALSE)
# ifelse(test, TRUE_CASE, FALSE_CASE)
set.seed(5)
ifelse(runif(50, 0, 1) < 0.5, rnorm(50, 20, 4), rpois(50, 2))

# Boucles : "Pour", "Tant que"
## Syntaxes:
for (valeur in �l�ment) {
  action
}
## Exemple
for (value in c("My", "first", "for", "loop")) {
  message(value)
}
##
while (condition) {
  code
}