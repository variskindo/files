#load the relevant library 
library(forecast)
library(tseries)
library(ggplot2)
library(summarytools)


#Load The data
Air <- Airpassengers

#Exploratory data analysis 
print(Air)

cycle(Air)

#Returns the index of the last observation 
end(Air)

#Returns the index of the first observation 
start(Air)

#Returns the time
time(Air)

#Returns the interval between observations
deltat(Air)

#Checks if it is time series
is.ts(Air)

#Checks the class of the data
class(Air)

#Checks if there is a NA value
sum(is.na(Air))


#Visualuzation of the time series
#plot the data
ts.plot(Air, main = "AirPassengers", ylab = "Air")
#The plot shows there is changing mean over time or trend
#The plot shows there is changing variance
#The plot shows autocorrelation is not constant over time

#Fit a line
abline(reg = lm(Air~time(Air)))

#Check for seasonality 
boxplot(Air~cycle(Air))
#The plot shows there is seasonality 


#Decompose the data into its constituents
decompAir <- decompose(Air)
plot(decompAir)

#plot the seasonal component
ts.plot(decompAir$seasonal, main = "Air(seasonal)", col = "black", lwd = 2)

#plot the trend component
ts.plot(decompAir$trend, main = "Air(trend)", col = "black", lwd = 2)

#plot the random component
ts.plot(decompAir$random, main = "Air(random)", col = "black", lwd = 2)


#first test for stationarity
acf.test(Air);pp.test(Air);kpss.test(Air)
#The data is not stationary from the test

#Stationalize the data
#log transformation 
ts.plot(log(Air), main = "Air(log))
#Shows the autocorrelation function and partial autocorrelation function of the data 
tsdisplay(log(Air))
#The changing variance has been removed but there is still seasonality and trend

#diff transformation and detrendind
#Check the number of differencing needed to stationalize the data
ndiff(Air) 
diff_Air <- diff(Air)
#plot the diff transformed Air
ts.plot(diff_Air, main = "Air(diff))
tsdisplay(diffAir)

#second test for stationarity 
acf.test(Air);pp.test(Air);kpss.test(Air)
#The plot shows trend has been removed
#But there is still seasonality 

#Plot the log transformed residue of the decomposed data
plot(decompose(log(Air))$random)
#The plot appears to be stationary

#Determining the order of AR(p) and MA(q) terms
tsdisplay(diff(log(Air)))
#SCENERIO1
#ACF plot shows a gradually decreasing trend
#PACF cuts immediately after lag 1 AR(1)

#SCENERIO2
#ACF cuts after lag 1 MA(2)
#PACF shows a gradually decreasing trend


#Estimate the model parameters or model fitting 
arimaModels <- auto.arima(Air, max.p = 5, max.q = 10)
arimaModel$residue 

#Predict the future
preds <- predict(Air, n.head = 12)
print(preds)

plot(Air, main = "Actual vs Fit Model", ylab = "AirPassengers", lwd = 3)
lines(arimaModel$fitted, col = "black",  lwd = 3)
legend("topleft", legend = c(Actual , ARIMA fit), col = c("green", "black"), lwd = 3)


plot(arimaModel$pred, main = "Actual vs Predicted model", ylab = "AirPassengers")



