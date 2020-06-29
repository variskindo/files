####update.packages(checkBuilt=TRUE, ask=FALSE)

#Data Undertanding: 1.How many records,2.how many variables are available & Formats& Data dictionary
#3.Target variables,4.iNVALID/MISSIng values,5.outliers,6.Distribution of each variable
#7.visualise data for insights, 8.summary statistics


# dplyr pacakage-it is a grammar of data manipulation,helps to solve data manipulation challenges#

install.packages("dplyr")
library(dplyr)
data(mtcars)
View(mtcars)
?mtcars
##Check missing values
anyNA(mtcars)

##select() - picks variables based on their names.
select()
select(mtcars,mpg,disp)  ##Error as package is not proper
dplyr::select(mtcars,mpg,disp)
A=dplyr::select(mtcars,drat:carb)
View(A)

# filter() picks cases based on their values.
B=dplyr::filter(mtcars, cyl==8 )
View(B)
C=dplyr::filter( mtcars , cyl <6 & vs==1)
View(C)

# arrange() changes the ordering of the rows.- sorting #Default : ascending
dplyr::arrange(mtcars,disp)
dplyr::arrange(mtcars,cyl,disp)
D=dplyr::arrange(mtcars,desc(disp))
View(D)

# mutate() adds new variables that are functions of existing variables
mtcars1=dplyr::mutate(mtcars,custom_disp=disp/vs)
View(mtcars1)

# summarise() reduces multiple values down to a single summary.
dplyr::summarise(mtcars,m=mean(disp),v=mean(vs),sd=sd(vs),first(disp),median(disp))


#Assignment:Practise all dplyr command on below dataset and also other summary functions
data(iris)
View(iris)


##### Apply functions in R ############# 1- rows, 2-columns
?apply 

mat <- matrix(1:10,nrow=5, ncol=6)
mat
a_m1 <- apply(mat, 2, sum)
a_m1
a_m2 <- apply(mat, 1, sd)
a_m2

?lapply   # returns a list of the same length as X
movies <- c("SPYDERMAN","BATMAN","VERTIGO","CHINATOWN")
str(movies)
movies_lower <-lapply(movies, tolower)
str(movies_lower)
#to convert into a vector :Ass
#movies_lower <-unlist(lapply(movies,tolower))
#str(movies_lower)

#sapply
dt = cars
?cars
View(cars)
lmn_cars = lapply(dt,min)
smn_cars = sapply(dt, min)
lmn_cars
smn_cars  
avg <- function(x) {( min(x) + max(x) ) / 2}
fcars <- sapply(dt, avg)
fcars

list1 <- list(a = c(1,2,7,8), b=c(3,4,9,10), c=c(5,6,11,12)) 
print(list1)
sapply (list1,sum )
sapply(list1,range)    

#tapply : function tapply() computes a measure (mean, median, min, max, etc..)
data(iris)
View(iris)
tapply(iris$Sepal.Width, iris$Species, median)

#mapply : mapply applies a Function to Multiple List or multiple Vector Arguments
Q=matrix(c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4)),4,4)
Q

Q=mapply(rep,1:4,4)
Q


####Visulization########
####################Barplot#####################
data("mtcars")
View(mtcars)
barplot(mtcars$disp)

?table
#it is a group by command
counts = table(mtcars$gear)
print(counts)
barplot(counts)
#Horizontal Bar plot
dev.off() ##Error with margins
par(mar = rep(2, 4)) ##Error with margins
barplot(counts,horiz = FALSE)

barplot(counts,main = "Bar Plot" , xlab = "Disp",
        ylab = "Frequency", names=c("High","Medium","Low") ,
        legend=rownames(counts),col=c("red","yellow","green"))

counts1=table(mtcars$vs,mtcars$gear)
View(counts1)
#Stacked Barplot : Data has 2 categorys
barplot(counts1,main="Car Distribution by Gears and VS", 
        xlab="Number of Gears",
        col=c("grey","cornflowerblue"),
        legend = rownames(counts1))

#Bars besides
barplot(counts1,main="Car Distribution by Gears and VS", 
        xlab="Number of Gears",
        col=c("grey","cornflowerblue"),
        legend = rownames(counts1),beside = TRUE)


#################Piechart######################################
slices= c(10,8,7,6,5)
Lbls=c("India","Australia","US","UK","France")
pie(slices,labels=Lbls,main="Pie Chart")

#Pie with  %
pct=(slices/sum(slices)*100)
print(pct)
?paste
Lbls1=paste(c("India","Australia","US","UK","France")," ",round(pct,2),"%",sep="")
Lbls1

pie(slices,labels=Lbls1 , col=rainbow(5),main = "Pie with %")  

# 3 dimensional pie
install.packages("plotrix")
library(plotrix)
slices <- c(10, 12,4, 16, 8)
pct=(slices/sum(slices)*100)
pct

lbls<- paste(c("US", "UK", "Australia", "Germany", "France"), " ", round(pct,2) , 
             "%", sep ="")
pie3D(slices, labels=lbls,explode=0.2,main="3D Pie Chart",labelcex=1) 


##########Histogram#######
data("mtcars")
View(mtcars)
?hist
hist(mtcars$mpg)
hist(mtcars$mpg,breaks = 5,col = "blue",border = "red")


######Kernel Density Plot - This chart is a variation of a Histogram that uses kernel smoothing 
# to plot values,allowing for smoother distributions by smoothing out the noise.###
?density
density_data <-density(mtcars$mpg)
density_data
plot( density_data)
#add color to the plot
polygon(density_data,col="skyblue",border="red")



#####Line Chart###
##Baby MOM weight & Height after birth
Weight<-c(2.5, 2.8, 3.2, 4.8, 5.1,5.9,6.8,7.1,7.8,8.1)
height<-c(1.6,1.8,2.0,2.1,2.2,2.4,2.5,2.7,2.8,2.9)
month<- c(0,1,2,3,4,5,6,7,8,9)
#3pch- 1-22
plot(Weight,month,type = "b", main = "Baby Weight Chart", pch=10, col=34)
lines(height,month,type="c",pch=5, col=564)


colors()           
?plot


############BOXPLOT#########

data("airquality")  
View(airquality)
?airquality

sort(unique(airquality$Ozone))

boxplot(airquality$Ozone, main="Ozone at New York",
        xlab="parts per billion",ylab="ozone",
        horizontal = FALSE,col=53)

summary(airquality$Ozone)
str(airquality$Ozone)
?boxplot

#Assignment 1 : Legends, change values of X-axis & y-axis, how to add labels
#assignment : how can you have multiple boxlot in the same chart

##########HeatMap##########        
?scale
df= scale(mtcars)
View(df)
heatmap(df)
heatmap(df,scale = "column", col = heat.colors(256), main="Cars",Rowv = NA,Colv = NA)
#native palettes of R:terrain.color,rainbow,heat.colors,topo.colors or cm.colors OR library(Rcolorbrewer)
par(mar=c(1,1,1,1))

#####Word Cloud
install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer")
library(tm)
library(wordcloud)
data= file.choose()
setwd("C:/Users/Asus/Desktop/Priyanka/Simpli learn/April-2020/Datasets")
getwd()
data=read.csv("Word_cloud.csv",header = T,stringsAsFactors = F)

 ######## Practise : Map of India - library(ggmaps)#############
 
 install.packages("ggmap")
 library(ggmap)
 India <- c(left = 65, bottom = 5, right = 98, top = 36)
 get_stamenmap(India, zoom = 5, maptype = "toner-lite") %>% ggmap()
 
 #######Practise :Corelatio plot#########
 install.packages("MASS")
 library(MASS)
 data("Boston")
 ?Boston
 View(Boston)
 
 #Check corealtion
 data(boston)
 install.packages(
   
   
 )
 library(corrplot)
 corr=cor(Boston)
 corrplot(corr, type = "lower")
 
 
 

 
atio plot#########
 install.packages("MASS")
 library(MASS)
 data("Boston")
 ?Boston
 View(Boston)
 
 #Check corealtion
 data(boston)
 install.packages(
   
   
 )
 library(corrplot)
 corr=cor(Boston)
 corrplot(corr, type = "lower")
 
 
 
"lower")
 
 
 

 
 

 