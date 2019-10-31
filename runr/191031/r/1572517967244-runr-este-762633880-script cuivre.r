dataptut=dataptutor�_

#Enlever les colones inutiles
dataptut<-dataptut[,c(-1,-2,-4:-6,-11,-14,-17,-19:-24)]
names(dataptut)[1]="code"

#S�lectionner les 306 stations
station_bryo=merge(dataptut,stations,by="code")
station_bryo<-station_bryo[,-c(5,11:15,18:31)]

#============Fraction bryophytes

#S�lection des bryophytes et du cuivre
databryo<- subset(station_bryo, libsupport=="Bryophytes")
databryo_cu<- subset(databryo, libparam=="Cuivre")

#on separe les codes 1 et les codes 10
datbryo_cu10<-subset(databryo_cu,remarque=="10")
datbryo_cu1<-subset(databryo_cu,remarque="1")#pas de valeurs aberantes dans les codes 10 (on a une valeur de 13 pour tous les codes 10 alors que certaines valeur montes a 150 dans les codes 1)

#Supprimer les unit�s inutiles
databryo_cu$unite!="371"#371=milligrammes de cuivre par killogrames
#presence de code 158 (milligrammes par killogrames) et de NA
databryo_cu371=subset(databryo_cu,unite=="371")
databryo_cu158=subset(databryo_cu,unite=="158")
databryo_cutrue=rbind(databryo_cu371,databryo_cu158)
#on a bien elimin� les NA

#Concentration en Cuivre dans les 306 stations en mg de Cr/kg fraction bryo enti�re
databryo_cu_moy=aggregate(resultat~code+dateprel_1+x+y, databryo_cutrue, mean)
write.csv(databryo_cu_moy,file="databryo_cu_moy.csv")


#Boxplot concentration de chrome par anneee
boxplot(databryo_cutrue$resultat~databryo_cutrue$dateprel_1,xlab="Ann�es",ylab="Concentration en mg de Cu/kg", main = "Boxplot de la concentration en Cuivre dans les bryophytes enti�re en fonction des ann�es",col=rainbow(16))
boxplot(databryo_cutrue$resultat~databryo_cutrue$dateprel_1,ylim=c(0,150),xlab="Ann�es",ylab="Concentration en mg de Cu/kg", main = "Boxplot de la concentration en Cuivre dans les bryophytes enti�re en fonction des ann�es",col=rainbow(16))

#separation des codes 1 et 10
datbryo_cu10true<-subset(databryo_cutrue,remarque=="10")
datbryo_cu1true<-subset(databryo_cutrue,remarque="1")

boxplot(datbryo_cu10true$resultat~datbryo_cu10true$dateprel_1,xlab="Ann�es",ylab="Concentration en mg de Cu/kg(codes 10)", main = "Boxplot de la concentration en Cuivre dans les bryophytes enti�re en fonction des ann�es(codes 10)",col=rainbow(16))
boxplot(datbryo_cu1true$resultat~datbryo_cu1true$dateprel_1,ylim=c(0,150),xlab="Ann�es",ylab="Concentration en mg de Cu/kg(codes 1)", main = "Boxplot de la concentration en Cuivre dans les bryophytes enti�re en fonction des ann�es(codes 1)",col=rainbow(16))

#===========Fraction eau=========

#S�lection de l'eau et du cuivre
dataeau<- subset(station_bryo, libsupport=="Eau")
dataeau<-subset(dataeau,codefraction=="3")
dataeau_cu<- subset(dataeau, libparam=="Cuivre")

#Extraction des remarques pour voir les ab�rances
dataeau_cu10<-subset(dataeau_cu,remarque=="10")
dataeau_cu1<-subset(dataeau_cu,remarque="1")#code 10 compris dans la game des code 1, code 1= 0.001:61, code 10= 0.001:10

#Supprimer les unit�s inutiles
dataeau_cu$unite!="304"#tout est �gal � 304 et 133 (304 �g de cu par litres et 133 �g par litres)

#Concentration en Cuivre dans les 306 stations en �g de Cu/L fraction eau filtr�e
dataeau_cu_moy=aggregate(resultat~code+dateprel_1+x+y, dataeau_cu, mean)
write.csv(dataeau_cu_moy,file="dataeau_cu_moy.csv")

#Boxplot concentration de cuivre par annee
boxplot(dataeau_cu$resultat~dataeau_cu$dateprel_1,xlab="Ann�es",ylab="Concentration en �g de Cu/L", main = "Boxplot de la concentration en Cuivre dans l'eau filtr�e en fonction des ann�es",col=rainbow(16))
boxplot(dataeau_cu$resultat~dataeau_cu$dateprel_1,ylim=c(0,35),xlab="Ann�es",ylab="Concentration en �g de Cu/L", main = "Boxplot de la concentration en Cuivre dans l'eau filtr�e en fonction des ann�es",col=rainbow(16))
boxplot(dataeau_cu$resultat~dataeau_cu$dateprel_1,ylim=c(0,2),xlab="Ann�es",ylab="Concentration en �g de Cu/L", main = "Boxplot de la concentration en Cuivre dans l'eau filtr�e en fonction des ann�es",col=rainbow(16))
boxplot(dataeau_cu$resultat~dataeau_cu$dateprel_1,ylim=c(0,5),xlab="Ann�es",ylab="Concentration en �g de Cu/L", main = "Boxplot de la concentration en Cuivre dans l'eau filtr�e en fonction des ann�es",col=rainbow(16))

#separation code 10 et 1
boxplot(dataeau_cu10$resultat~dataeau_cu10$dateprel_1,ylim=c(0,11),xlab="Ann�es",ylab="Concentration en �g de Cu/L", main = "Boxplot de la concentration en Cuivre dans l'eau filtr�e en fonction des ann�es(code 10)",col=rainbow(16))
boxplot(dataeau_cu1$resultat~dataeau_cu1$dateprel_1,ylim=c(0,35),xlab="Ann�es",ylab="Concentration en �g de Cu/L", main = "Boxplot de la concentration en Cuivre dans l'eau filtr�e en fonction des ann�es(code 1)",col=rainbow(16))
boxplot(dataeau_cu1$resultat~dataeau_cu1$dateprel_1,ylim=c(0,5),xlab="Ann�es",ylab="Concentration en �g de Cu/L", main = "Boxplot de la concentration en Cuivre dans l'eau filtr�e en fonction des ann�es(code 1)",col=rainbow(16))
boxplot(dataeau_cu1$resultat~dataeau_cu1$dateprel_1,ylim=c(0,2),xlab="Ann�es",ylab="Concentration en �g de Cu/L", main = "Boxplot de la concentration en Cuivre dans l'eau filtr�e en fonction des ann�es(code 1)",col=rainbow(16))

#===========Nb de prelevement==========
install.packages("reshape2")
library(reshape2)
testcu<-dcast(databryo_cu,libparam+libsupport~ dateprel.1,value.var="resultat", fun.aggregate=length)#donne le nombre de pr�l�vement par ann�e
write.csv(testcu, file="nb_prelevan_Cu.csv")
#des modification ont �t� effectu� sur le fichier via excel
plot(nb_prelevan_Cu$Nb_de_prelevements~nb_prelevan_Cu$Ann�e,ylab="Nombre de pr�levements",xlab="Ann�es",main="Nombre de prelevements de cuivre dans les bryophytes en fonction des ann�es",las=1,lab=c(16,5,0))





