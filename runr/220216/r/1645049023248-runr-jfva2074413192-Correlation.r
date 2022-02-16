##CHAPTER 9: CORRELATION##
library("foreign")
uza<-read.csv("UZA.csv")

cor(uza$lnlm,uza$lnvmt) #Pearson correlation coefficient
cor.test(uza$lnlm,uza$lnvmt) #Pearson correlation coefficient and p-value

library(ggm)
names(uza)
uza_par<-uza[,c("lnlm","lnvmt","lnfuel")]
pcor<-pcor(c(1,2,3),cov(uza_par)) #partial correlation coefficient
pcor
pcor.test(pcor,1,157)

cor.test(uza$vmt,uza$pop000,method="spearman") #Spearman correlation coefficient

#ICC
library(irr)
icc(cbind(uza$popden, uza$urbden),model="twoway") #Intraclass correlation coefficient

library(psych)
alpha(cbind(uza$popden, uza$urbden))$total$std.alpha #Cronbach's Alpha