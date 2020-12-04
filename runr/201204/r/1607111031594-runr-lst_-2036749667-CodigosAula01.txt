n    <- 20
x1   <- runif(n,-1,7)
x2   <- rpois(n,5)
erro <- rnorm(n,0,0.5)
y    <- 3 + 1.5*x1 + 4*x2 + erro
dt   <- list(y,x1,x2)
mdl  <- lm(y ~ x1 + x2, data = dt)
summary(mdl)

n       <- 20
vtbeta0 <- c()
vtbeta1 <- c()
vtbeta2 <- c()
for(cont in 1:1000){
  x1   <- runif(n,-1,7)
  x2   <- rpois(n,5)
  erro <- rnorm(n,0,0.5)
  y    <- 3 + 1.5*x1 + 4*x2 + erro
  dt   <- list(y,x1,x2)
  mdl  <- lm(y ~ x1 + x2, data = dt)
  vtbeta0 <- c(vtbeta0, mdl$coef[1])
  vtbeta1 <- c(vtbeta1, mdl$coef[2])
  vtbeta2 <- c(vtbeta2, mdl$coef[3])
}
par(mfrow=c(3,1))
hist(vtbeta0)
hist(vtbeta1)
hist(vtbeta2)
summary(vtbeta0)
cat("\n Desvio:", sd(vtbeta0), "\n")

#Limpa o Workspace
rm(list=ls())

#Habilita o pacote quantmod
library(quantmod)

#Início do período de interesse
inicio = as.Date("2001-01-01")

#Fim do período de interesse
fim = as.Date("2012-12-31")

#Obtêm os dados da PETR4
getSymbols("PETR4.SA", src="YHOO",from=inicio,to=fim)



