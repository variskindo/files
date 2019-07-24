####################################################################################################################################
################################ +++++++++++++++++++++ CATANE ++++++++++++++++ #####################################################
####################################################################################################################################

# calcule les probablit�s de chaque combinaison de d�s

# d�finit le nombre de faces des d�s
f <- 6


# la somme n des 2 d�s est comprise entre d*1 et f*d
n <- seq(d,f*2)

############################################################################################################
# pour chaque somme des 2 d�s, il existe un certain nombre de n1 et n2 qui r�pondent � n1 + n2 = n
# la fonction suivante calcule la probablit� d'obtenir n

prob_lancer <- function(x,f=6) {
  if (x>2*f|x==1) {
    stop("la somme des 2 d�s est sup�rieure � 1 et ne peut pas d�passer ",2*f)
  } else if (x>f) {
    y <- seq ((x-f),f)
  } else {
    y <- seq(1,(x-1))
  }
  return(cat("la probabilit� d'obtenir",x,"est de",length(y),"/",f^2,"=",round(length(y)/f^2,4)))
}

# cette fonction calcule la probabilit� d'avoir au moins une ressource � partir d'une colonie ou d'une ville situ�e � l'intersection de 1 � 3 tuiles
# attention, si 2 tuiles portent le m�me num�ro, ne sp�cifier qu'un des 2 num�ros
prob_ressource <- function(h1,h2=0,h3=0,f=6) {
  nb_jet <- function(x,f=6) {
    if (x==0) {
      return(0)
    } else if (x>2*f|x==1) {
      stop("la somme des 2 d�s est sup�rieure � 1 et ne peut pas d�passer ",2*f)
    } else if (x>f) {
      y <- seq ((x-f),f)
    } else {
      y <- seq(1,(x-1))
    }
    return(length(y))
  }
  p <- (nb_jet(h1)+nb_jet(h2)+nb_jet(h3))
  
  return(cat("la probabilit� d'obtenir au moins une ressource � cette position est de ",p,"/",f^2,"=",round(p/f^2,4)))
}








