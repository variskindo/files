####################################################################################################################################
################################ +++++++++++++++++++++ CATANE ++++++++++++++++ #####################################################
####################################################################################################################################

# calcule les probablités de chaque combinaison de dés

# définit le nombre de faces des dés
f <- 6


# la somme n des 2 dés est comprise entre d*1 et f*d
n <- seq(d,f*2)

############################################################################################################
# pour chaque somme des 2 dés, il existe un certain nombre de n1 et n2 qui répondent à n1 + n2 = n
# la fonction suivante calcule la probablité d'obtenir n

prob_lancer <- function(x,f=6) {
  if (x>2*f|x==1) {
    stop("la somme des 2 dés est supérieure à 1 et ne peut pas dépasser ",2*f)
  } else if (x>f) {
    y <- seq ((x-f),f)
  } else {
    y <- seq(1,(x-1))
  }
  return(cat("la probabilité d'obtenir",x,"est de",length(y),"/",f^2,"=",round(length(y)/f^2,4)))
}

# cette fonction calcule la probabilité d'avoir au moins une ressource à partir d'une colonie ou d'une ville située à l'intersection de 1 à 3 tuiles
# attention, si 2 tuiles portent le même numéro, ne spécifier qu'un des 2 numéros
prob_ressource <- function(h1,h2=0,h3=0,f=6) {
  nb_jet <- function(x,f=6) {
    if (x==0) {
      return(0)
    } else if (x>2*f|x==1) {
      stop("la somme des 2 dés est supérieure à 1 et ne peut pas dépasser ",2*f)
    } else if (x>f) {
      y <- seq ((x-f),f)
    } else {
      y <- seq(1,(x-1))
    }
    return(length(y))
  }
  p <- (nb_jet(h1)+nb_jet(h2)+nb_jet(h3))
  
  return(cat("la probabilité d'obtenir au moins une ressource à cette position est de ",p,"/",f^2,"=",round(p/f^2,4)))
}








