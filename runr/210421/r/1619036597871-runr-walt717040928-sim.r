#simulating ARIMA model
#simulating autoregressive model
Ar = arima.sim(model = list(order = c(1, 0, 0)), ar = 0.9, n = 100)

#plot the sim Ar
plot(Ar)

#Shows the acf and pacf plot
tsdisplay(Ar)

#Shows the acf and pacf plot
acf2(Ar)
#acf tails off and pacf cuts out at lag p(AR)
#pacf tails off and acf cuts out at lag p

#simulating a moving average model
Ma = arima.sim(model = list(order = c(0, 0, 1)), n = 100)

#plot the Ma model
plot(Ma)

#shows the acf and pacf plot
tsdisplay(Ma)

#shows the acf and pacf plots
acf2(Ma)