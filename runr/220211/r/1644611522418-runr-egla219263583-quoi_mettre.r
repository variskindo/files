setwd("C:/Users/Eglantine/kDrive/CASE/Documents/Personnel/Quoi_mettre")
library(readxl)
habits <- read_excel("habits.xlsx")


Summer=subset(habits,Subset=="Summer")
Winter=subset(habits,Subset=="Winter")
Fall_spring=subset(habits,Subset=="Fall_Spring")

#Summer
Tenue_Summer=NULL
for(i in levels(as.factor(Summer$Type))){
  Sample=(subset(Summer, Type==i))
  Tenue_Summer[i]=sample(Sample$cloth,1)
}
Tenue_Summer=Tenue_Summer[c(1, sample(2:3,1))]
print(Tenue_Summer)

## Fall Spring
Tenue_FS=NULL
for(i in levels(as.factor(Fall_spring$Type))){
  Sample=(subset(Fall_spring, Type==i))
  Tenue_FS[i]=sample(Sample$cloth,1)
}
print(Tenue_FS)

## winter
Tenue_FS=NULL
for(i in levels(as.factor(Winter$Type))){
  Sample=(subset(Winter, Type==i))
  Tenue_FS[i]=sample(Sample$cloth,1)
}
print(Tenue_FS)
