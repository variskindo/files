# This script will produce some graphics of the Hocker (1956) loblolly pine distribution data
#  Remember to set your working directory using the pull-down menus or setwd command
#  For example, if I want data stored on my desktop, I would use the command setwd('C:/Users/bbibles/desktop')
#  Now we need to load the data, which are in a .txt (comma-delimited) file called 'Pitapt.csv'.
#  These data can be loaded with the command below, or in RStudio with the File:Import Dataset:From CSV pull-down menu
pita<-read.csv(file.choose(),header=T)
#
# Run the next block of commands all as one unit - these manipulate the data to create the needed values to graph
names(pita) <- c('Longitude','Latitude','Temp','Precip')
pt.type<-c(rep(1,100), rep(2,43), rep(3,57))
range<-c(rep('Within',100),rep('Outside',100)) 
pita<-cbind(pt.type,range,pita)
pita$pt.type <- factor(pita$pt.type, labels=c("Pine","West","North"))
pita[,5]<-pita[,5]/10
pine<-pita[pita$range=='Within',]
out<-pita[pita$range=='Outside',]
west<-pita[pita$pt.type=='West',]
north<-pita[pita$pt.type=='North',]
type<-c('Pine','Out','West','North')
ave.temp<-c(mean(pine[,5]),mean(out[,5]),mean(west[,5]),mean(north[,5]))
sd.temp<-c(sd(pine[,5]),sd(out[,5]),sd(west[,5]),sd(north[,5]))
nval<-c(length(pine[,5]),length(out[,5]),length(west[,5]),length(north[,5]))
tval<-qt(.975,nval-1)
lcl.temp<-ave.temp-(tval*(sd.temp/(sqrt(nval))))
ucl.temp<-ave.temp+(tval*(sd.temp/(sqrt(nval))))
temps<-data.frame(type,ave.temp,sd.temp,nval,tval,lcl.temp,ucl.temp)
ave.prec<-c(mean(pine[,6]),mean(out[,6]),mean(west[,6]),mean(north[,6]))
sd.prec<-c(sd(pine[,6]),sd(out[,6]),sd(west[,6]),sd(north[,6]))
lcl.prec<-ave.prec-(tval*(sd.prec/(sqrt(nval))))
ucl.prec<-ave.prec+(tval*(sd.prec/(sqrt(nval))))
precips<-data.frame(type,ave.prec,sd.prec,nval,tval,lcl.prec,ucl.prec)
#
#  Now we'll create some graphs.  
#  First we'll create a scatterplot of winter temperature and winter precipitation of points within the loblolly pine distribution,
#  as well as to the north and west of the loblolly pine distribution
plot(pita[,5],pita[,6],type='n',xlab='Winter Temperature (deg C)', ylab='Winter Precipitation (mm)')
points(pine[,5],pine[,6],col=3,pch=3); points(west[,5],west[,6],col=6,pch=2); points(north[,5],north[,6],col=4,pch=4)
legend('topleft',c('Pine','West','North'),col=c(3,6,4),pch=c(3,2,4))
#
#  Now let's look at the 95% Confidence Intervals on winter precipitation at points within, outside of, west, and north of the pine's distribution
p.plot<-barplot(precips[,2],ylim=c(0,350),names.arg=precips[,1],ylab='Winter Precipitation (mm)',main='Precipitation at points within and outside of loblolly pine distribution')
arrows(p.plot,precips[,6],p.plot,precips[,7],col=2,lwd=3,angle=90,length=.25,code=3)
# 
#  Lastly, let's look at boxplots of the winter temperature within the distribution of the pine, and to the west and north
boxplot(pita[,5]~pita[,1],col=c(2,3,4),horizontal=T, notch=T, ylab='Winter Temperature (deg C)')
#
# Remember how to save these graphics as files?
#

