options(digits=5)
################################################################################################
#           Certificat Sciences des Donn�es et Big Data - Toulouse Tech
#                             ann�e 2019-2020
#                         MODULE SENSIBILISATION
#         Section 1: Introduction � R et au calcul statistique
################################################################################################
# Auteur: Florent Bourgeois, INP-ENSIACET, florent.bourgeois@toulouse-inp.fr
# M�J: 01/10/2019
################################################################################################
{# Soit X1 et X2 deux variables al�atoires ind�pendantes qui suivent une loi normale
# ou une loi uniforme, au choix. 
# �crivez en langage R les 6 fonctions (p, d, q, r, e et v)CertificatBigData, 
# dans un m�me script R, qui permettent de calculer/g�n�rer:
# - la fonction de r�partition CDF (Cumulative Distribution Function): pCertificatBigData()
# - la densit� de probabilit� PDF (Probability Density Function): dCertificatBigData()
# - un quartile: qCertificatBigData()
# - des nombres al�atoires: rCertificatBigData()
# - la moyenne: eCertificatBigData()
# - la variance: vCertificatBigData()
# des variables Y = X1+X2, Z = X1*X2 et Z = X1�X2. 
# Ces fonctions devront produire, au choix de l'utilisateur, 
# des graphes pertinents qui permettent de visualiser les r�sultats. 
#
# Au terme de cet exercice, vous r�pondrez gr�ce � votre code de calcul 
# � la question suivante : quelle est la distribution statistique de la 
# somme de 2 variables al�atoires uniformes ? 
# La r�ponse � cette question sera discut�e durant la s�ance en pr�sentiel 
# associ�e � ce module du certificat.
################################################################################################
} # Enonc� du probl�me

# fonction rCertificatBigData()
rCertificatBigData <- function(n,paramX1_1=0,paramX1_2=1,
               paramX2_1=0,paramX2_2=1,
               loi='uniforme',
               operation='somme',
               graphics = FALSE){
{
# fonction qui �chantillonne la somme, produit et ratio de 2 v.a. uniformes ou normales
# entr�es:
#   n : nombre de valeurs renvoy�es
#   paramX1_1: moyenne (loi normale) ou min (loi uniforme) de X1
#   paramX1_2: �cart-type (loi normale) ou max (loi uniforme) de X1
#   paramX2_1: moyenne (loi normale) ou min (loi uniforme) de X2
#   paramX2_2: �cart-type (loi normale) ou max (loi uniforme) de X2
#   loi: 'uniforme' ou 'normale'
#   operation: 'somme', 'produit' ou 'ratio'
#   graphics: TRUE ou FALSE
# sorties: data.frame qui contient:
#   sortie$X1 : vecteur de n valeurs de X1
#   sortie$X2 : vecteur de n valeurs de X2
#   sortie$Z : vecteur de n valeurs de Z  
#
# exemple d'appel: rCertificatBigData(n=10000, paramX1_1=0,paramX1_2=4,paramX2_1=5,paramX2_2=6, loi='uniforme', operation='produit', graphics=TRUE)
}# SIGNATURE
  print(paste(operation," de X1 et X2, v.a. ",loi,"s",sep=""))
  
  # # G�n�ration de X1 et X2
  # if (loi == 'uniforme')
  #   {
  #   if(paramX1_2 <= paramX1_1) paramX1_2 =1.1*paramX1_1 # borne max de la loi uniforme doit �tre sup�rieure � la borne min    
  #   if(paramX2_2 <= paramX2_1) paramX2_2 =1.1*paramX2_1 # borne max de la loi uniforme doit �tre sup�rieure � la borne min        
  #   X1 <- runif(n,min=paramX1_1, max=paramX1_2)
  #   X2 <- runif(n,min=paramX2_1, max=paramX2_2)
  # } 
  # else if (loi == 'normale')
  #   {
  #           X1 <- rnorm(n,mean=paramX1_1, sd=paramX1_2)
  #           X2 <- rnorm(n,mean=paramX2_1, sd=paramX2_2)
  # } 
  # else #on a tap� une loi inconnue
  #   {
  #     loi='normale'
  #     X1 <- rnorm(n,mean=paramX1_1, sd=paramX1_2)
  #     X2 <- rnorm(n,mean=paramX2_1, sd=paramX2_2)
  # }
  # 
  # if (graphics==TRUE) 
  # {
  #   plot(NULL, xlim=c(min(density(X1)$x, density(X2)$x), max(density(X1)$x, density(X2)$x)),
  #        ylim=c(0,1),
  #        xlab="X1, X2",
  #        ylab="Densit� de probabilit� (normalis�e)")
  #   lines(density(X1)$x,density(X1)$y/max(density(X1)$y), lty="solid", col="blue")
  #   lines(density(X2)$x,density(X2)$y/max(density(X2)$y), lty="solid", col="red")
  # }
  # 
  # # G�n�ration de Z
  # if (operation == 'somme') 
  #   {
  #   Z <- X1+X2
  #   label_Z <- "Z = X1+X2"
  # } 
  # else if (operation == 'produit')
  #   {
  #   Z <- X1*X2
  #   label_Z <- "Z = X1*X2"
  # }
  # else if (operation == 'ratio')
  #   {
  #   Z <- X1/X2
  #   label_Z <- "Z = X1/X2"
  # }
  # else
  #   {
  #   #print(" Calcul du ratio de X1 et X2")
  #   operation='somme'
  #   Z <- X1+X2
  #   label_Z <- "Z = X1+X2"
  #   }

  ## Autre syntaxe avec Switch
  switch (loi,
          'uniforme' =
            {
              print("loi uniforme s�lectionn�e")
              X1 <- runif(n,min=paramX1_1, max=paramX1_2)
              X2 <- runif(n,min=paramX2_1, max=paramX2_2)
            },
          'normale' =
            {
              print("loi normale s�lectionn�e")
              X1 <- rnorm(n,mean=paramX1_1, sd=paramX1_2)
              X2 <- rnorm(n,mean=paramX2_1, sd=paramX2_2)
            },
            # autre 
            {
              print("loi normale par d�faut")
              X1 <- rnorm(n,mean=paramX1_1, sd=paramX1_2)
              X2 <- rnorm(n,mean=paramX2_1, sd=paramX2_2)
            }
      )
  switch (operation,
          'somme' =
          {
            Z <- X1 + X2
            label_Z <- "Z = X1+X2"
          },
          'produit' =
          {
            Z <- X1 * X2
            label_Z <- "Z = X1*X2"
          },
          'ratio' =
          {
            Z <- X1 / X2
            label_Z <- "Z = X1/X2"
          },
          # autre 
          {
            operation = 'somme'  
            Z <- X1 + X2
            label_Z <- "Z = X1+X2"
          }
  )   
    
  if (graphics == TRUE) {
    plot(Z, ylab=label_Z)
    plot(density(X1)$x,density(X1)$y/max(density(X1)$y), 
         xlim=c(min(density(X1)$x, density(X2)$x), max(density(X1)$x, density(X2)$x)),
         ylim=c(0,1), type="l", col="blue", xlab="X1, X2", ylab="Densit� de probabilit�")
    lines(density(X2)$x,density(X2)$y/max(density(X2)$y), lty="solid", col="red")
    
  }
  
  # variable de sortie
  sortie <- list(X1,X2,Z,label_Z)
  names(sortie) <- c("X1","X2","Z","label_Z")
  return(sortie) # variable de sortie
}

# fonction eCertificatBigData()
eCertificatBigData <- function(n,paramX1_1=0,paramX1_2=1,
               paramX2_1=0,paramX2_2=1,
               loi='uniforme',
               operation='somme',
               graphics = FALSE){
  {
    # fonction qui calcule la moyenne de la somme, produit et ratio de 2 v.a. uniformes ou normales
    # entr�es:
    #   n : nombre de valeurs renvoy�es
    #   paramX1_1: moyenne (loi normale) ou min (loi uniforme) de X1
    #   paramX1_2: �cart-type (loi normale) ou max (loi uniforme) de X1
    #   paramX2_1: moyenne (loi normale) ou min (loi uniforme) de X2
    #   paramX2_2: �cart-type (loi normale) ou max (loi uniforme) de X2
    #   loi: 'uniforme' ou 'normale'
    #   operation: 'somme', 'produit' ou 'ratio'
    #   graphics: TRUE ou FALSE
    # sorties:
    #   e : vmoyenne des n valeurs
    # exemple d'appel: eCertificatBigData(paramX1_1=0,paramX1_2=4,paramX2_1=5,paramX2_2=6, n=100000, loi='uniforme', operation='somme', graphics=TRUE)
  }# SIGNATURE
  # �chantillonne n valeurs de z
  z <- rCertificatBigData(n,paramX1_1,paramX1_2,paramX2_1,paramX2_2,loi,operation,graphics=FALSE)$Z
  # calcul de la moyenne
  e <- mean(z)
  
  if (graphics == TRUE) {
    pCertificatBigData(e,n,paramX1_1,paramX1_2,paramX2_1,paramX2_2,loi,operation,graphics)
  } # Trac� de la densit� de probabilit� de Z

  print(paste("la moyenne de", operation,"vaut: ", round(e,3)))
  return(invisible(e)) # variable de sortie
}

# fonction vCertificatBigData()
vCertificatBigData <- function(n,paramX1_1=0,paramX1_2=1,
               paramX2_1=0,paramX2_2=1,
               loi='uniforme',
               operation='somme',
               graphics=FALSE){
  {
    # fonction qui calcule la variance de la somme, produit et ratio de 2 v.a. uniformes ou normales
    # entr�es:
    #   n : nombre de valeurs renvoy�es
    #   paramX1_1: moyenne (loi normale) ou min (loi uniforme) de X1
    #   paramX1_2: �cart-type (loi normale) ou max (loi uniforme) de X1
    #   paramX2_1: moyenne (loi normale) ou min (loi uniforme) de X2
    #   paramX2_2: �cart-type (loi normale) ou max (loi uniforme) de X2
    #   loi: 'uniforme' ou 'normale'
    #   operation: 'somme', 'produit' ou 'ratio'
    #   graphics: TRUE ou FALSE    
    # sorties:
    #   v : vmoyenne des n valeurs
    # exemple d'appel: vCertificatBigData(paramX1_1=0,paramX1_2=4,paramX2_1=5,paramX2_2=6, n=100000, loi='uniforme', operation='somme')
  }# SIGNATURE
  # �chantillonne n valeurs de z
  z <- rCertificatBigData(n,paramX1_1,paramX1_2,paramX2_1,paramX2_2,loi,operation,graphics)$Z
  # calcul de la variance
  v <- var(z)
  print(paste("la variance de", operation,"vaut: ", round(v,3)))
  return(v) # variable de sortie
  
}

# fonction pCertificatBigData()
pCertificatBigData <- function(q,n,paramX1_1=0,paramX1_2=1,
               paramX2_1=0,paramX2_2=1,
               loi='uniforme',
               operation='somme',
               graphics=FALSE){
  {
    # fonction qui renvoie la probabilit� P(Z<=q) d'un quartile q de la somme, produit et ratio de 2 v.a. uniformes ou normales
    # entr�es:
    #   q : valeur du quartile
    #   n : nombre de valeurs renvoy�es de la variable Z (taille de l'�chantillon)
    #   paramX1_1: moyenne (loi normale) ou min (loi uniforme) de X1
    #   paramX1_2: �cart-type (loi normale) ou max (loi uniforme) de X1
    #   paramX2_1: moyenne (loi normale) ou min (loi uniforme) de X2
    #   paramX2_2: �cart-type (loi normale) ou max (loi uniforme) de X2
    #   loi: 'uniforme' ou 'normale'
    #   operation: 'somme', 'produit' ou 'ratio'
    #   graphics: TRUE ou FALSE
    # sorties:
    #   p : valeur de la la probabilit� p telle que P(Z >= q)=p
    # exemple d'appel: 
  }# SIGNATURE
  # �chantillonne n valeurs de z
  z <- rCertificatBigData(n,paramX1_1,paramX1_2,paramX2_1,paramX2_2,loi,operation,graphics=FALSE)$Z
  # calcul de la probabilit�
  p <- ecdf(z)(q)
  
  if (graphics == TRUE) {
    vecteur_q <- seq(min(z),max(z),0.01)
    vecteur_p <- ecdf(z)(vecteur_q)
    plot(vecteur_q,vecteur_p,ylim=c(0,1),xlab="z",ylab="Proba(Z <= z)", main="Cumulative Distribution function (CDF)", type="l")
    abline(v=q,lty="dashed", col="red")
    abline(h=p,lty="dashed", col="red")
  } # Trac� de la fonction de r�partition de Z
  
  print(paste("la probabilit� P(Z <= ", q,") vaut: ", round(p,5),sep=""))
  return(p) # variable de sortie
}

# fonction qCertificatBigData()
qCertificatBigData <- function(p,n,paramX1_1=0,paramX1_2=1,
               paramX2_1=0,paramX2_2=1,
               loi='uniforme',
               operation='somme',
               graphics=FALSE){
  {
    # fonction qui renvoie le quartile q de la somme, produit et ratio de 2 v.a. uniformes ou normales
    # entr�es:
    #   p : valeur de la probabilit�
    #   n : nombre de valeurs renvoy�es
    #   paramX1_1: moyenne (loi normale) ou min (loi uniforme) de X1
    #   paramX1_2: �cart-type (loi normale) ou max (loi uniforme) de X1
    #   paramX2_1: moyenne (loi normale) ou min (loi uniforme) de X2
    #   paramX2_2: �cart-type (loi normale) ou max (loi uniforme) de X2
    #   loi: 'uniforme' ou 'normale'
    #   operation: 'somme', 'produit' ou 'ratio'
    #   graphics: TRUE ou FALSE
    # sorties:
    #   q : valeur du quartile q tel que P(Z <= q)= p
    # exemple d'appel: qCertificatBigData(p=0.5, paramX1_1=0,paramX1_2=4,paramX2_1=5,paramX2_2=6, n=100000, loi='normale', operation='somme', graphics=TRUE)
  }# SIGNATURE
  # �chantillonne n valeurs de z
  z <- rCertificatBigData(n,paramX1_1,paramX1_2,paramX2_1,paramX2_2,loi,operation,graphics=FALSE)$Z
  # calcul du quartile
  q <- quantile(ecdf(z),p)
  
  if (graphics == TRUE) {
    pCertificatBigData(q,n,paramX1_1,paramX1_2,paramX2_1,paramX2_2,loi,operation,graphics=FALSE)
  } # Trac� de la densit� de probabilit� de Z
  
  print(paste("le quartile q tel que P(Z <= q)=", p," vaut: ",round(q,5),sep=""))
  return(q) # variable de sortie
}

# fonction dCertificatBigData()
dCertificatBigData <- function(q,n,paramX1_1=0,paramX1_2=1,
               paramX2_1=0,paramX2_2=1,
               loi='uniforme',
               operation='somme',
               graphics=FALSE){
  {
    # fonction qui renvoie la valeur de la densit� de probabilit� d'un quartile q de la somme, produit et ratio de 2 v.a. uniformes ou normales
    # entr�es:
    #   q : valeur du quartile
    #   n : nombre de valeurs renvoy�es
    #   paramX1_1: moyenne (loi normale) ou min (loi uniforme) de X1
    #   paramX1_2: �cart-type (loi normale) ou max (loi uniforme) de X1
    #   paramX2_1: moyenne (loi normale) ou min (loi uniforme) de X2
    #   paramX2_2: �cart-type (loi normale) ou max (loi uniforme) de X2
    #   loi: 'uniforme' ou 'normale'
    #   operation: 'somme', 'produit' ou 'ratio'
    #   graphics: TRUE ou FALSE
    # sorties:
    #   d : valeur de la densit� de probabilit� d telle que dP(Z = q)=d
    # exemple d'appel: dCertificatBigData(q=6, paramX1_1=0,paramX1_2=4,paramX2_1=5,paramX2_2=6, n=100000, loi='normale', operation='somme', graphics=TRUE)
  }# SIGNATURE
  # �chantillonne n valeurs de z
  z <- rCertificatBigData(n,paramX1_1,paramX1_2,paramX2_1,paramX2_2,loi,operation,graphics=FALSE)$Z
  # calcul de la densit� de probabilit�
  d <- density(z)
  #print(paste("d est une variable de type",typeof(d)))
  # interpolation de la densit� de probabilit� � la valeur du quartile q
  res <- approx(d$x,d$y, xout=q)
  #print(paste("res est une variable de type",typeof(res)))
  print(paste("la densit� de probabilit� dP(Z = ", res$x,") vaut: ", round(res$y,5),sep=""))
  
  if (graphics == TRUE) {
    plot(d, xlab="z",ylab="PDF", main="Densit� de probabilit� (PDF)", type="n")
    abline(v=q,lty="dashed", col="red")
    abline(h=res,lty="dashed", col="red")
    polygon(d, col=rgb(red=0.5,blue=0.5,green=0.5,alpha=0.25)) #grey with 75% transparency
  } # Trac� de la fonction de r�partition de Z
  
  return(res) # variable de sortie
} 

# Quelques exemples d'appel
###########################
rCertificatBigData(n=10000, paramX1_1=0,paramX1_2=4,paramX2_1=5,paramX2_2=6, loi='uniforme', operation='produit', graphics=TRUE)
#Z <- dCertificatBigData(q=-10, paramX1_1=0,paramX1_2=4,paramX2_1=5,paramX2_2=6, n=100000, loi='normale', operation='somme', graphics=TRUE)
Z <- pCertificatBigData(q=6, paramX1_1=0,paramX1_2=4,paramX2_1=5,paramX2_2=6, n=100000, loi='uniforme', operation='somme', graphics=TRUE)
#Z <- qCertificatBigData(p=0.025, paramX1_1=0,paramX1_2=4,paramX2_1=5,paramX2_2=6, n=100000, loi='normale', operation='somme', graphics=TRUE)
#mean_Z <- eCertificatBigData(paramX1_1=0,paramX1_2=4,paramX2_1=5,paramX2_2=6, n=100000, loi='uniforme', operation='somme', graphics=TRUE)
varZ <- vCertificatBigData(paramX1_1=0,paramX1_2=4,paramX2_1=5,paramX2_2=6, n=100000, loi='uniforme', operation='somme')