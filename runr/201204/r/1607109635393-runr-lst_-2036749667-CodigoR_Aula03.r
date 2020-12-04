#Limpa o Workspace
rm(list=ls())

#Carregando a serie temporal:
yt=c(1,1.5,1.8,2,4)
#Time series plot:
ts.plot(yt)
# Autocorrelation function:
acf(yt,plot=FALSE)
# Autocorrelogram:
acf(yt)



