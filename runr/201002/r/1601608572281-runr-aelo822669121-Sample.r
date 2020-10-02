my_var<- 4
my_var
x <- 42
x
class(x)
Numeric_vector<-c(1,2,3,4,5)
Numeric_vector
character_vector<-c("a","b","c","d","e")
character_vector
x<-1:7
x
k<-c(1:50,60:70,80)
k
y<-seq(1,3,by=0.2)
y
z<-seq(1,3,length.out=5)
z
a<-c(1:15)
b<-rep(a, times=4)
b
j<-rep(a, each=4)
j
poker_vector<-c(14000,-5000,2000,-12000,24000)
roulette_vector<-c(-2400,-5000,10000,-35000,1000)
names(poker_vector)<-c("monday","tuesday","wednesday","thursday","friday")
names(roulette_vector)<-c("monday","tuesday","wednesday","thursday","friday")
names(poker_vector)
poker_vector
roulette_vector
total_daily<-poker_vector+roulette_vector
total_daily
total_poker<-sum(poker_vector)
total_poker
total_roulette<-sum(roulette_vector)
total_roulette
overall<-total_poker+total_roulette
overall
poker_wednesday<-poker_vector[3]
poker_wednesday
roulette_threedays<-roulette_vector[c(1,3,5)]
roulette_threedays
sex_vector<-c("Male","Female","Female","Male","Male")
factor_sex_vector<-factor(sex_vector)
factor_sex_vector
temperature<-c("High","Low","High","Low","Medium")
temperature_factor<-factor(temperature,ordered=TRUE,levels=c("Low","Medium","High"))
temperature_factor                          
civil_status_vector<-c("Single","Single","Single","Single","Married","Married","Married","Married","Separated","Separated")
factor_civil_status_vector<-factor(civil_status_vector)
factor_civil_status_vector
frequency_vector<-c("Most of the Time","Most of the Time","Most of the Time","Most of the Time","Most of the Time","Most of the Time","Most of the Time","Sometimes","Sometimes","Hardly Ever")
factor_frequency_vector<-factor(frequency_vector,ordered=TRUE,levels=c("Hardly Ever","Sometimes","Most of the Time"))
factor_frequency_vector
