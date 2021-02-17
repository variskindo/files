######## Simulating Stationary Time Series

rm(list = ls()) # this clears already existing variables for the Global
                #Environment


########### AR (p) process
##### AR(1) process
ar1 = arima.sim(list(ar = 0.5), n=200)
head(ar1) # this gives you only the first 6 elements 
plot(ar1) # this plots the time series data simulated

#Obtaining the ACF and PACF plots for the series
acf(ar1) #this gives you the ACF plot of the data
pacf(ar1) #this gives you the PACF plot of the data

##### AR(2) process
ar2 = arima.sim(list(ar = c(0.1, 0.6)), n=500)
plot(ar2)
acf(ar2)
pacf(ar2)

##### AR(3) process
ar3 = arima.sim(list(ar= c(-0.1, -0.5, 0.21)), n= 550)
plot(ar3)
acf(ar3)
pacf(ar3)


########### MA (q) process
##### MA(1)
ma1 = arima.sim(list(ma = 0.1), n= 400)
plot(ma1)
acf(ma1)
pacf(ma1)

###### MA(2) process
ma2 = arima.sim(list(ma = c(0.2, -0.4)), n= 450)
plot(ma2)
acf(ma2)
pacf(ma2)

###### MA(3) process
ma3 = arima.sim(list(ma = c(0.062, -0.47, 0.15)), n= 750)
plot(ma3)
acf(ma3)
pacf(ma3)


################## ARMA(p,q)
#### ARMA(1,1)
arma11 = arima.sim(list(ar= 0.5, ma= 0.2), n= 600)
plot(arma11)
acf(arma11)
pacf(arma11)

##### ARMA(2,1)
arma21 = arima.sim(list(ar= c(0.5, -0.2), ma= 0.2), n= 500)
plot(arma21)
acf(arma21)
pacf(arma21)

#### ARMA(1,2)
arma12 = arima.sim(list(ar = 0.4, ma= c(-0.3, 0.32)), n= 800)
plot(arma12)
acf(arma12)
pacf(arma12)

#### ARMA(2,2)
arma22 = arima.sim(list(ar = c(-0.53, 0.1), ma= c(-0.39, 0.3)), n= 1000)
plot(arma22)
acf(arma22)
pacf(arma22)


##Adding specifications




