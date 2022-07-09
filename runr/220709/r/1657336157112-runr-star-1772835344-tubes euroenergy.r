data <-  read.csv(file.choose(), header=TRUE) #untuk membaca file csv, 
#setelah dijalankan kalian akan memilih file csv yang sudah ada.



summary(data)


qqnorm(data$gdp,main="GDP")
qqline(data$gdp)

qqnorm(data$energy,main="Energy")
qqline(data$energy)


library("ggpubr")

ggdensity(data$gdp, 
          main = "GDP",
          xlab = "jumlah")

ggdensity(data$energy, 
          main = "Energy",
          xlab = "jumlah")

ggqqplot(data$gdp)

ggqqplot(data$energy)

shapiro.test(data$gdp)
shapiro.test(data$energy)
