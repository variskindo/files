#---------------------------------------------------------------#
## R une super calculette                                      ##
rm(list = ls())


10*(1+1+1.5)  # = 35         # calculs

10**2         # = 100        # carre 
10^2          # idem

100**(1/2)    # = 10         # puissance
100^(1/2)     # idem

sqrt(100)     # = 10         # racine   
pi            # = 3.1416     # pi  
cos(pi)       # = -1         # cos
sin(pi/2)     # = 1          # sin
exp(1)        # = 2.71882    # exponentielle
log(1)        # = 0          # log neperien

round(2.566)     # arrondi a un entier
round(pi,0)      # idem 
round(pi,2)      # arrondi 2 chiffres apres ,

a <- 100
a
print(a)

v <- c(10,20,30)  # un vector
v
length(v)         # longueur du vector

length(a)
is.vector(a)

2*v+1             # sur chaque composante du vector
v**2              # carre de chaque composante
log(v)            # log de chaque composante

w <- c(1,2,3)     # un autre vector
v-w               # soustraction membre a membre
v*w               # multiplication membre a membre
v/w               # division membre a membre
v%*%w             # produit scalaire

sum(v)         # = 60  somme
mean(v)        # = 20  moyenne
min(v)         # = 10  minimum
max(v)         # = 30  maximum  
sd(v)          # = 10  ecart type
median(v)      # = 20  medianne

u <- c(1,2,3,4,5,6,7,8) # un autre vector
u[2]                    # deuxieme composante

u[3:5]                  # nouveau vector
                        # issu des composantes 3 a 5

u[8] <- 80              # affectation une composante
u

u[1:5] <- 1             # affectation 5 composantes           
u

#---------------------------------------------------------------#
# fonctions interessantes sur les vectors                       #

v <- c(10,20,30,30,60,50)      # un vector
w <- c(20,10,31,31,61,51)      # un autre vector
u <- c(5 ,5 ,5 ,32,62,49)      # un autre vector

str(v)                    # jeter un oeil sur les data
sum(is.na(v))             # nb de valeurs manquantes
v_ <- c(NA,v,NA,NA)       # un vecteur avec 3 valeur manquantes
v_
sum(is.na(v_))            # nb valeurs manquantes


range(v)                  # min et max du vector
range(v_)                 # min et max du vector ECHEC !
range(v_ , na.rm = TRUE)  # sans tenir compte des NA

quantile(v)                        # quartiles de v
quantile(v, probs =c(0,0.1,0.9,1)) # 80/20

summary(v)                     # resume
sd(v, na.rm = TRUE)            # ecart type

cor(v,w)                       # coeff correlation entre vectors

sort(v)                        # vector tri ordre croissant
sort(v, decreasing = TRUE)     # vector tri ordre decroissant
order(w)                       # donne pointer tri sur elements

rank(w, ties.method="min")     # vecteur des rangs
                               # de valeurs base plus petit=1
                               # "min" : style compet. sport

rank(w, ties.method="max")     # vecteur des rangs
                               # de valeurs base plus petit=1
                               # "min" : style data-sciences



pmax(v,w,u)                    # valeurs max membre a membre
pmin(v,w,u)                    # valeurs min membre a membre

cumsum(v)                      # sommes cumulees
cumprod(v)                     # produits successifs

cummax(w)                      # maximum entre membre 
                               # considere et membres precedents
cummin(w)                      # idem avec min



#---------------------------------------------------------------#
# booleens - logical                                            #

a <- 1
b <- 2

(a == 1)                       # TRUE
(a == b)                       # FALSE
(a <= b)                       # TRUE

A <- c(TRUE,TRUE,FALSE,FALSE)
B <- c(TRUE,FALSE,TRUE,FALSE)
A & B                          # table de verite de "et"
A | B                          # table de verite de "ou"
! A                            # non-A
xor(A,B)                       # table verite ou exclusif
!A|B                           # table de l'implication A==>B

str(A)                         # vector compose de logical

c <- (a > b)                   # stocker le resultat d'un test
c

v <- c(10,20,30,30,60,50)      # un vector
t <- (v > 30)                  # vecteur resultant du test 
t                              # membre a membre

w <- v[(v>30)]                 # on ne garde que les membres
                               # avec expression TRUE
w    

which(v == 30)                 # trouve les indices ou
                               # membre egal a 30

which(v == max(v))             # trouve les indices ou
                               # membre egal val max membre

which(v == min(v))             # idem mais recherche min

s <- 1*t                       # transformation en vecteur 1,0
s

v <- c(10,20,70,30,60,50)      # un vector
all(v > 5)                     # ?toutes les val sont sup a 5
any(v < 5)                     # ?une valeur inf a 5

#---------------------------------------------------------------#
# ensemble                                                      #

H <- unique(c("e","g","g","h","h","h"))
H

P <- c("e","f","g","h")
Q <- c("g","h","i","j")
union(P,Q)
intersect(P,Q)
setdiff(P,Q)
setdiff(Q,P)
union(setdiff(P,Q),setdiff(Q,P))

H
P
H %in% P
P %in% H
which(P %in% H)

#---------------------------------------------------------------#
# listes                                                        #

l <- list (nom  = "Dupond",  # liste cle-valeur
           age  = 25,     
           vect = v)
l$nom                        # valeurs
l$age
l$vect

l[[1]]                       # par indice
l[[2]]
l[[3]]

is.vector(l[[3]])            # c'est bien un vector
l[[3]][2]                    # dont voici le deuxime element

#---------------------------------------------------------------#
# factors                                                       #

coul <- factor(c("bleu","vert",
                 "rouge","bleu","vert")) # factor pas d'ordre
coul
str(coul)         # montre les levels et leur indice
coul[2]           # vert
levels(coul)      # les levels

t <- (coul[2] == "vert") # test si vert
t

which(coul == "vert")    # indice si vert
which(coul != "vert")    # indice si pas vert

# decoupage d'une serie en factors                  #
set.seed(0)           # initialisation alea
v <- c(rnorm(100))    # creation vector d'essai
str(v)                # 
range(v)              # min et max de v
summary(v)            # resume
sd(v, na.rm = TRUE)   # ecart type

hist(v)               # distribution obtenue

                      # decoupage sur les quartiles
w <- cut(v, breaks = quantile(v), labels = FALSE)
w

table(w)                   # repartition effectifs
sum(table(w)) == length(w) # pas egaux

which(is.na(w))       # oups une valeur inconnue
w[59]
v[59]                 # c'est une des bornes !

Position(is.na,w)     # une alternative donne premier pos

                      # refondre les bornes 
bornes <- c(quantile(v)[1]- 0.0001*abs(min(v)),
            quantile(v)[2],
            quantile(v)[3],
            quantile(v)[4],
            quantile(v)[5]+ 0.0001*abs(max(v))
            )
bornes
w <- cut(v, breaks = bornes, labels =c("Q1","Q2","Q3","Q4"))
w           
which(is.na(w))
w[59]
v[59] 
                            # verification des classes
which(w == "Q1")            # indices des Q1
v[which(w == "Q1")]         # valeur origine des Q1

quantile(v)                 # pour memoire
range(v[which(w == "Q1")])  # Q1 
range(v[which(w == "Q2")])
range(v[which(w == "Q3")])
range(v[which(w == "Q4")])


#---------------------------------------------------------------#
#  avec ordre                                                   #

ordre <- factor(c("petit","moyen","grand"),
                ordered=TRUE,         # creation ordre
                ) 
ordre


ordre <- factor(c("0_petit","1_moyen","2_grand"),
                ordered=TRUE,         # creation ordre
                )  
ordre

ordre[1] < ordre[2]               # TRUE


#---------------------------------------------------------------#
# tableaux                                                      #

o <- c( 1,1,1,111,   # un vector d'observations
        1,1,2,112,
        1,1,3,113,
        1,2,1,121,
        1,2,2,122,
        1,2,3,123,
        2,1,1,211,
        2,1,2,212,
        2,1,3,213,
        2,2,1,221,
        2,2,2,222,
        2,2,3,223
       )

m <- matrix(byrow = TRUE,        # la matrice correspondante
            nrow  = length(o)/4,
            o)

m

m[2,4] # cellule ligne 2, col 4
m[2,]  # ligne 2
m[,4]  # valeurs transposee de la col 4
matrix(m[,4])  # colonne 4

                                 # cube olap
cube <- xtabs( V4 ~ V1 + V2 + V3, data = m)
cube
dim(cube)
cube[1,2,3]  # ligne 1, col 2, profondeur 3
cube[,,1]    # tranche sur profondeur = 1
cube[,,2]    # tranche sur profondeur = 2
cube[,,3]    # tranche sur profondeur = 3

cube[,1,]    # tranche sur col = 1
cube[,2,]    # tranche sur col = 2

cube[1,,]    # tranche sur ln = 1
cube[2,,]    # tranche sur ln = 2

cube[1,1,]   # intersection ln=1,col=1

is.matrix(cube[1,,]) # coherence matrix array
dim(cube[1,,])       # respect dimensions

sum(cube[,,])        # somme toutes les valeurs

apply(cube,1,sum)  # somme deuxieme et troisieme dim
sum(cube[1,,])     # somme tout ln = 1
sum(cube[2,,])     # somme tout ln = 2

apply(cube,2,sum)  # somme premiere et troisieme dim
sum(cube[,1,])     # somme tout col = 1
sum(cube[,2,])     # somme tout col = 2

apply(cube,3,sum)  # somme premiere et deuxieme dim
sum(cube[,,1])     # somme tout profondeur = 1
sum(cube[,,2])     # somme tout profondeur = 2
sum(cube[,,3])     # somme tout profondeur = 3

apply(cube,c(1,2),sum) # somme sur la troisieme dim
sum(cube[1,1,])
sum(cube[1,2,])
sum(cube[2,1,])
sum(cube[2,2,])

apply(cube,c(1,3),sum) # somme sur la deuxieme dim
apply(cube,c(2,3),sum) # somme sur la premiere dim


apply(cube,c(1,2,3),sum) == cube  # somme sur rien

apply(cube,c(1,2), max) # max sur la troisieme dim
apply(cube,1, min) # min sur deuxime et trois dim

array(1:8, c(2,4)) # creer un tableau 

#---------------------------------------------------------------#
# tables de vérité                                              #


x <- c(NA, FALSE, TRUE)                 # nos cas de figure
x 
names(x) <- c("Inconnu","Faux","Vrai")  # ajout de noms de col
x
outer(x, x, "&")                        # table de verite

#---------------------------------------------------------------#
# probas                                                        #

table_v <- expand.grid(0:1,0:1,0:1,0:1)
table_v
nrow(table_v)

nb<- apply(table_v,1,sum)
nb

(nb ==2)

sum(nb==2)

sum(nb==2)/nrow(table_v)

f <- function(x){sum(nb==x)/nrow(table_v)}
res <- sapply(0:4,f)
names(res) <- as.character(0:4)
res
sum(res)

#---------------------------------------------------------------#
# fonctions les bases                                           #

f <- function(x){x*1.1}          # fonction simple
f(100)
f(x = 10)                        # on peut stipuler les noms de
                                 # variables

f <- function(x = 1){x*1.1}      # f  avec x par defaut
f()                              # f(1)                  

#---------------------------------------------------------------#
v <- c(1,2,3,4,5)
sapply(v,f)                      # appliquer f sur membres de
                                 # v 

#---------------------------------------------------------------#
# demonstration efficace, compter sur place le nb de doublons   #
x <- c(10,25,4,10,9,9)
y <- sapply(x,function(z){sum((x==z))})
y

#---------------------------------------------------------------#
g <- function(x,y){ a <- 10^x    # fonction de 2 variables
                    b <- 10^y
                    (b-a)/(b+a)}
g(1,1)
g(1,1.5)
g(1.5,1.5)


#---------------------------------------------------------------#
# operateurs                                                    #

`%g_op%`    <- g                  # un operateur 
1 %g_op% 1                        # idem g(1,1)
1 %g_op% 1.5                      # idem g(1,1.5)

v <- c(1, 1  ,1 ,1.5, 1.5, 1.5)
w <- c(1, 1.5,1 ,1.5, 1,   1.5)
v %g_op% w                        # operateur vectorisable!


#---------------------------------------------------------------#
## fonction et portee des variables                            ##

rm(list =ls()) # cette instruction vide l'environnement
# c'est souvent utile !

#---------------------------------------------------------------#
f <- function(){
  100
  }
f()            # retourne la derniere valeur


f <- function(){
  return(200) # la fonction est finie ici
  return(300) # 
  100         # 
  }
f()              # retourne 200

#---------------------------------------------------------------#
f <- function(x = 100){
  y <- x + 1
  y
  }
f()            # utilise la valeur par defaut
f(200)         # additionne 1

y <- 300       # 
f(y)           # additionne 1
y              # y est intact


#---------------------------------------------------------------#
f <- function(x = 100){
  x <- x + 1
  x
  }
f()            # utilise la valeur par defaut
f(200)         # additionne 1

y <- 300       # 
f(y)           # additionne 1
y              # y est intact

#---------------------------------------------------------------#
f <- function(x = 100){ # ne retourne pas de valeur !!
  x <<- x + 1        # remarquez <<-
  }
f()            # ne fait rien ?
f(200)         # ne fait rien ?

y <- 300       # 
f(y)           # ne fait rien ?
y              # y est intact

x    # une variable globale x existe maintenant
# et vaut 301

#---------------------------------------------------------------#
rm(list =ls()) # cette instruction vide l'environnement

f <- function(x = 100){ 
  y <<- x - 1        # remarquez <<-
  z <-  x + 1
  z                  # ce z est local
  }

y <- 300
z <- 400
f(z)           # additionne 1
y              # y vaut 399 : attention !
z              # z est intact

z <- f(z)      # pour appliquer sur z
z              # z vaut 401

z <- c(10,100,1000)
z <- f(z)      # pour appliquer sur z
z

cube <- f(cube)        # sur notre cube 
cube

cube <- 2 * cube + 1  # il suffisait de faire cela
cube

cube <- xtabs( V4 ~ V1 + V2 + V3, data = m)
isImpair <- function(n){as.logical(n%%2)}
isImpair(cube)            # pas resultat voulu !!!
cube_imp <- apply(cube,c(1,2,3),isImpair)
cube_imp
#---------------------------------------------------------------#
# plus loin avec les fonctions                                  #


# acces explicite a une variable globale                        #
rm(x)                        # destruction de x

f <- function() {get( "x" )  # acces explicite
                 x+1
                 }

f()                          # erreur
x <- 10
f()                          # fonctionne

#---------------------------------------------------------------#
# fonctions anonymes                                            #

(function(x) 2*x+1)(100)
(function(x,y) {c(x+y,x*y,x-y,x/y)})(1,2)

v <- c(10,21,30)
sapply(v,(function(x){as.logical(x%%2)}))

f <- function(x,y){x+y}
z <- 100
sapply(v,(function(x) f(x,z))) # sapply fonction 2 variables
sapply(v,f(x,z))               # car ceci ne fonctionne pas !


#---------------------------------------------------------------#
# acceder au code de la fonction                                #

f <- function(x=0) {x^2}
body(f)

f(2)
body(f) <- quote(x^3)
body(f)
f(2)
              # etudier une fonction
args(sapply)  # arguments
body(sapply)  # corps

#---------------------------------------------------------------#
# fontions qui retournent des fonctions                         #

f <- function(n){ 
                g <- function(z) {summary(z)}
                h <- function(z) {sd(z)}
                k <- function(z) {range(z)}
                l <- list(k,g,h)
                l[[(n%%3)+1]]
                }

v <- c(1,2,3,4)
f(1)(v)         # resume
f(2)(v)         # ecart type
f(3)(v)         # min et max

#---------------------------------------------------------------#
# passage de fonction en argument                               #

v_ <- function(f) {f(v)} #dualiser 
v_(function(x) {2*x})
v_(mean)

second_degre <- function(f,a,b,c,x){a*f(x)^2+b*f(x)+c}
second_degre(cos,2,0,-1,v)          # 2 cos(x)**2 - 1  =
cos(2*v)                            # cos(2x)

#---------------------------------------------------------------#
## structures de controle                                      ##

x <- 20                              # variable test
if (x < 10) {print("x < 10")}   else 
            {print ("x >= 10")}


for (x in c(1,10,20)) {print(2*x)}   # boucle for sur sequence

s <- seq(from= 1,to= 20,by = 4)      # fabrique une sequence
s

for (x in s) {print (x)}             # boucle sur la sequence

l <- list("trois","deux","un")       # liste 
while (length(l) > 0) {              # tant que faire :
  print(l[[length(l)]])
  l[[length(l)]] <- NULL             # vide une position
  }                                  # fin de depil
length(l)

l <- list()
repeat {                             # 999 premier carres
  i <- max(length(l),0)
  l[[length(l)+1]] <- i^2 
  if (length(l) >= 1000) {break}     # repeat until break 
  }
head(l)                              # les 6 premiers                       
tail(l)                              # les 6 derniers

                                     # idem mais plus R
l <- as.list(seq(from  =1, to = 999)^2)
tail(l)


#---------------------------------------------------------------#
# boucle sur une matrice et retrouve positions qui matchent     #

check_matrice <- function(mat,valeur = TRUE){  
  k <- 0
  x <- vector()
  y <- vector()
  for (i in 1:nrow(mat)){
    for (j in 1:ncol(mat)){
      if (mat[i,j] == valeur){
        k <- k+1
        x[k] <- i
        y[k] <- j
        }
      }
    }
  xy <- matrix(rbind(x,y),nrow = 2)
  }

m <- matrix(1:12,nrow = 3)
m

(m > 5)

c <- check_matrice((m>5))
c

for ( k in 1:ncol(c)) {
  cat(k,"ieme valeur > 5 : ",m[c[1,k],c[2,k]],"\n")
  }

(function(k)(m[c[1,k],c[2,k]]))(3)

sapply(1:ncol(c),(function(k)(m[c[1,k],c[2,k]])))


#---------------------------------------------------------------#
# bases de la gestion des chaines de caracteres                 #

paste("il","est","beau")          # colle avec espace
paste0("c","h","a","t")           # colle sans espace

r <- list("un","petit","chat")    # une liste de mots
str(r)
paste(r,collapse ="-")       # colle elements liste avec tiret

r <- c("c","h","i","e","n")  
str(r)
paste(r,collapse ="")        # colle elements vector sans espace  
 
substr("un petit chat",4,8)   # extraction sous-chaine de car

strsplit("un petit chat"," ") # decoupe chaine via les blancs
strsplit("un petit chat"," ")[[1]][2]

t <- c("un petit chat","un petit chien")
strsplit(t," ")

strsplit("un petit chat","t") # decoupe chaine via les "t"

sub("petit","gros","un petit chat") # substitution chaine 

chartr("H.","Mr...","H. Laude") #substitution meme nb de car
chartr("H.","           ","H. Laude")

grep("petit",t)
grep("chat" ,t)


tolower("UN grand JOUR")        # mise en minuscule
toupper("UN grand JOUR")        # mise en majuscule

toupper("& # $ £ éçèêùµ+} \132 \n \" ") 

gsub("[a/e/i]","_","un petit chat") # substitution via expression
                                    # reguliere

#---------------------------------------------------------------#
# formatage des nombres avant edition par exemple               #

w <- v*pi                         # pour avoir des virgules !
w
w1 <- format(w)                   # transforme en caractere
w1

w2 <- format(w, digits = 2)       # idem avec 2 digits 
w2

as.numeric(w1)-as.numeric(w2)     # ce ne sont plus les memes val 

format(w, nsmall = 10)            # 10 apres virgule 
format(w, digits = 1)             # round (et pas trunc)




#---------------------------------------------------------------#

d <- as.Date("01/01-2016","%d/%m-%y") # decode une date
d
str(d)                                # c'est bien une date

d+366                                 # additionne 366 jours

Sys.Date()                      # date courante de notre machine

format(Sys.Date(), "%a %d %b")  # formattage date

? strptime                      # regardons les options !

Sys.time()                      # heure systeme de la machine

#---------------------------------------------------------------#
# Bench comparatif sapply et boucle for                         #

f <- function(x){x^3+x^2+1/(x+10)} # une fonction

x_   <- vector()                   # init axe des x
big  <- vector()                   # init gros vecteur a traiter
big2 <- vector()                   # init gros vecteur a traiter
y1_  <- vector()                   # init axe y pour courbe 1
y2_  <- vector()                   # init axe y pour courbe 2              

for (j in 1:10) {
  n <- 50000 *(j^2 * round(log(j+1))+1) # ecart croissant sur n
  x_[j] <- n                            # cette iteration sur j
                                        # se fait sur n lignes
                                        
  set.seed(1789)                     # init generateur alea
  big   <- runif(n)                  # big : n valeurs alea
  t_    <- Sys.time()                # t au debut
  
  big2   <- sapply(big,f)             # application de f sur big
  
  d_     <- Sys.time() - t_          # t fin
  y1_[j] <- d_                       # memo t fin pour iter j

  set.seed(1789)
  big   <- runif(n)
  t_    <- Sys.time()
  
  for (i in 1:n ) {big2[i] <- f(big[i])}
  
  d_    <- Sys.time() - t_
  y2_[j] <- d_
  }

#---------------------------------------------------------------#
                                   # socke le resultat
 
bench_sapply <-  cbind(x_,y1_)     # j lignes n, delta temps
bench_for    <-  cbind(x_,y2_)     # j lignes n, delta temps


#---------------------------------------------------------------#

                             # plot le resultat

op <- par()                  # stocke anciene valeur disposition
                             # des plots et parametres generaux
par(mfrow = c(1,2))          # dispose 2 plots sur 1 ligne
                                   # plots simples pas tres beaux
plot(bench_sapply,
     xlab ="nombre de lignes",
     ylab ="temps exec pour sapply")                 
plot(bench_for   ,
     xlab ="nombre de lignes",
     ylab ="temps exec pour boucle for")   
par(op)                            # important ! reinit par() !

#---------------------------------------------------------------#
# amelioration des plots                                        #

? par                              # parametres de plot
? points                           # parametres de plot

plot(rbind(bench_sapply,bench_for),   # cadre vide a l'echelle
     type = "n",                      # on ne dessine rien
     xlab = "nombre de lignes (0 - 10 millions)",
     ylab = "temps en secondes",
     cex.axis = 0.8,               # caract axes plus petits
     las  = 2)                     # ecrire perpendic  aux axes

                                   # courbe sapply, puis points
lines(bench_sapply, type ="l",lwd=1,col = "blue")
lines(bench_sapply, type ="b",lwd=1,col = "blue"
                             ,pch = 1,cex =0.5)
                                   # courbe for, puis points
lines(bench_for, type ="l",lwd=1,col = "red")
lines(bench_for, type ="b",lwd=1,col = "red"
                          ,pch = 4,cex =0.5)

legend("topleft",               # une legende en haut a gauche
       legend=c("sapply","for"),# legende
       lty=1,                   # ligne continue
       lwd=1,                   # ligne peu epaisse
       pch=c(1,4),              # type de symbole
       col=c("blue","red"),
       ncol=2,                  # legende sur 2 colonnes
       bty="n",                 # pas de cadre
       cex=0.8,                 # ecrire plus petit
       text.col=c("blue","red"),
       inset=0.01)              # positionnement legende


#---------------------------------------------------------------#
# nombres complexes                                             #

z <- 4 + 3i   # un nombre complexe
Mod(z)        # longueur de z
Re(z)         # la partie reelle
Im(z)         # la partie imaginaire

z1 <- 1 + 1i
Arg(z1)*180/pi  # angle en degre
Arg(z1)         # angle en radian (argument)



#---------------------------------------------------------------#
# visualiser les nombres complexes                              #

                              # une fonction de visualisation
plot_complex <- function(v){
  mr <- max(abs(Re(v)))       # max des abs axe horiz (Re) 
  mi <- max(abs(Im(v)))       # max des abs axe vert  (Im)

  w1 <- c(-mr,-mi)            # calcul 2 pts extreme du plot
  w2 <- c( mr, mi)
  w  <- rbind(w1,w2)          # on en fait un tab avec 2 lignes
  
  t <- paste(as.character(v),
             "/longueur",
             as.character(round(Mod(v),3)),
             "/angle",
             as.character( round(Arg(v)*180/pi)),
             "°"
             )

  plot(w,                     # plot cadre invible points extrm
       type ="n", 
       bty ="n",
       xlab="1 -> Re(z)",
       ylab="i -> Im(z)"
       )
  abline(v=0, lty = 4)        # axes centraux pointille
  abline(h=0, lty = 4)

  for (i in 1:length(v)) {    # ligne+point pour chaque complex
    lines(c(0,v[i]),type ="l", asp = 1)
    lines(c(v[i])  ,type ="p", asp = 1)
    text(c(v[i]),labels =t[i], pos = 2,offset=0.5,cex=0.7)
    }
  }

z2 <- -1 - 1i
z3 <- -1 + 2i

plot_complex(z)               # premier  essai sur z
plot_complex(z2)              # deuxieme essai sur z2

v <- c(z,z1,z2,z3)            # plusieurs complexes
plot_complex(v)


#---------------------------------------------------------------#
## BONUS                                                       ##
#---------------------------------------------------------------#
# trouver de l'aide                                             #

? mean                # aide sur la fonction mean()

?? base::mean         # cherche les aides avec le mot
# mean dans  le package base

help.search("mean")   # mean dans tous les packages
RSiteSearch("mean")   # cherche dans http://search.r-project.org 
RSiteSearch("mean covariance") # mean AND covariance

#---------------------------------------------------------------#
# gestion de la memoire                                         #

length(l)
ls()                                 # liste variables
rm(l)                                # supprime variable l
l
rm(list = ls())                      # supprime toutes les var
ls()                                 # effectivement

#---------------------------------------------------------------#
# test unitaires integres                                       # 

a <- 1
b <- 2
stopifnot(a == 1, b==3 )



#---------------------------------------------------------------#
# passage par valeur et pas par reference                       #

a <- 1                             
b <- a                        # copie de a dans b
a <- 2                      
b                             # comportement different de py

#---------------------------------------------------------------#

