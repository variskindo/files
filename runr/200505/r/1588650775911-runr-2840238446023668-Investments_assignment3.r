rm(list=ls())




# libraries 
library("readxl")

# reads in data and subset to our group
All.data <- read_excel("Data_Assignment3_2020.xlsx", sheet = "Q1, 2, and 3 -- Group1")
All.data

# Calculate the daily returns
All.data$Price
Returns = c()
for(i in 1:(length(All.data$Price)- 1)){
  Returns[i] = log(All.data$Price[i+1] / All.data$Price[i])
  
}
#mReturns = data.frame(All.data$Date[-1], Returns)
#mReturns

# Annualized mean en stdev
Ann.mean = mean(Returns) * 250
Ann.SD = sd(Returns) * sqrt(250)

#length(mReturns$Returns)

# S0 stays the same all the times
s0 = All.data$Price[length(All.data$Price)] # Initial starting price





####### TEST


# Paramters binominal tree
u = exp(Ann.SD * sqrt(1/50))
d = 1/u

# Get starting price
# get D and U values
# The maximum period is 50 so this array works fine for all periods
# Next we will test it with this number of periods and after adjust it to working for all periods
s0 = All.data$Price[length(All.data$Price)]
s0
D.values = c()
U.values = c()
for(i in 1:50){
  D.values[i] = s0 * d**i
  U.values[i] = s0 * u**i
}

rf.period = 0.005 / 50
s0
Q <- (s0*(1 + rf.period) - D.values[1]) / (U.values[1] - D.values[1])
length(D.values)
length(U.values)
last_period = c()

for(i in 1:25){
  last_period[i] = U.values[(52 -   2*i)]
  
}
U.values
last_period
last_period[26] = s0

j = 27
for(i in 1:25){
  last_period[j] = D.values[(2*i)]
  j = j  + 1
}
last_period
D.values
length(last_period)
D.values
last_period


C.start <- function(array){
  C.init = c()
  for (i in 1:length(array)){
    if (array[i] > s0){
      C.init[i] = (array[i] - s0)
    }
    else{
      C.init[i] = 0
    }
  }
  return(C.init)
}
test <- C.start(last_period)

D.values
U.values
last_period

######## END TEST





# Previous calculation
# val 1 = Q * s0*u**50 / (1 + rf.period) + (1 - Q ) * s0**48
# val 2 = Q * s0*u**48 / (1 + rf.period) + (1 - Q ) * s0**46
# ... 
# val 25 = Q * s0*u**2 / (1 + rf.period) + (1 - Q ) * s0
# val 26 = Q * s0*u / (1 + rf.period) + (1 - Q ) * s0*d
# ... 
# val 50 = Q * s0*d**48 / (1 + rf.period) + (1 - Q ) * s0*d**50



# Previous calculation
# new.val 1 = Q * val1 / (1 + rf.period) + (1 - Q ) * val2
# new.val 2 = Q * val2 / (1 + rf.period) + (1 - Q ) * val3
# ... 
# new.val 25 = Q * val25 / (1 + rf.period) + (1 - Q ) * val26
# new.val 26 = Q * val26 / (1 + rf.period) + (1 - Q ) * val27
# ... 
# new.val 49 = Q * val49 / (1 + rf.period) + (1 - Q ) * val50



### TRUE CODE


##### Binominal  tree calculation

# function that calculates the last elements of the tree for a given numberof periods N
# W.R.T.  the stock prices
# After this we will define the prices - strike price and zeros

# THIS ONLY WORKS FOR for even number N
Last_period_even <- function(N){
  # Adjusted parameters for a given N
  u = exp(Ann.SD * sqrt(1/N))
  d = 1/u
  
  # We have to adjust the D and U values
  # Since our number of periods relies on N 
  D.values = c()
  U.values = c()
  for(i in 1:N){
    D.values[i] = s0 * d**i
    U.values[i] = s0 * u**i
  }
  
  # Calculate the last values of the tree w.r.t. stock prices
  last = c()
  for(i in 1:(N / 2)){
    last[i] = U.values[(N -   2*i + 2)]
  }
  last[(N/ 2 + 1)] = s0
  j = (N/ 2 + 1) + 1
  for(i in 1:(N / 2)){
    last[j] = D.values[(2*i)]
    j = j  + 1
  }
  return(last)
}

start <- Last_period_even(50) # checking
start

### USE THIS FUNCTION ONLY IF YOU HAVE AN ODD N!!
# function that calculates the last elements of the tree for a given numberof periods N
# Same as previous but adjustetd it a bit
Last_period_odd <- function(N){
  # Parameters tree
  u = exp(Ann.SD * sqrt(1/N))
  d = 1/u
  
  # We have to adjust the D and U values
  # Since our number of periods relies on the N 
  D.values = c()
  U.values = c()
  for(i in 1:N){
    D.values[i] = s0 * d**i
    U.values[i] = s0 * u**i
  }
  
  # Calculate the last values of the tree w.r.t. stock prices
  last = c()
  for(i in 1:((N+1) / 2)){
    last[i] = U.values[(N -   2*i + 2)]
  }
  j = (N+1)/ 2 + 1
  for(i in 1:((N+1) / 2)){
    last[j] = D.values[(2*i - 1)]
    j = j  + 1
  }
  return(last)
}


#start2 <- Last_period_odd(5)
#start2

# Calculates the last values of the binominal option tree w.r.t. OPTION PRICES VALUES
# So this defines the zeros or (strike price - stock price)
# Input: array of last values of the tree w.r.t. the stock price
C.start <- function(array){
  C.init = c()
  for (i in 1:length(array)){
    if (array[i] > s0){
      C.init[i] = (array[i] - s0)
    }
    else{
      C.init[i] = 0
    }
  }
  return(C.init)
}
init <- C.start(start) # checking
init

# Calculate the option price for a given N period
# Input: the last values of the three now adjusted to option pricing 
# Zo array with zeros and or (strike price - stock price)
calculate_C <- function(C.init2, N){
  # REPEAT IT!!
  u = exp(Ann.SD * sqrt(1/N))
  d = 1/u
  
  # Store the last 2 values
  Last_two_values = c()
  
  # We have to adjust the D and U values
  # Since our number of periods relies on the N 
  D.values = c()
  U.values = c()
  for(i in 1:N){
    D.values[i] = s0 * d**i
    U.values[i] = s0 * u**i
  }
  
  # Adjusted risk free rate
  rf.period = 0.005 / N
  
  # Calculate new Q
  Q <- (s0*(1 + rf.period) - D.values[1]) / (U.values[1] - D.values[1])
  
  # Initial array(last values of tree w.r.t. option prices) and final calculate the option price
  temp = C.init2
  for(i in 1:N){
    for(j in 1:(length(temp) - 1)){
      temp[j] = Q * temp[j] / (1 + rf.period) + (1 - Q ) * temp[(j+1)] / (1 + rf.period)
    }
    
    # Saving the last two values of the three for question 3
    if(i == (N-1)){
      Last_two_values = temp[1:2]
    }
    temp = temp[-(length(temp))]
    
  }
  return(list(temp, Last_two_values, Q))
}

calculate_C(init, 50)

#0.480423 * 5.100609  / (1 + 0.005/50) + (1-0.480423) *3.270696 / (1 + 0.005/50) 


### Black scholes 

# Define parameters
r = 0.005 # Risk free rate
T = 1 # Time period
d1 = ((r + Ann.SD**2/2 ) * T) / (Ann.SD * sqrt(T))
d2 = d1 - Ann.SD * sqrt(T)

# Calculate the option price
C0 = s0 * pnorm(d1, mean = 0, sd = 1) - s0*exp(-r*T) * pnorm(d2, mean = 0, sd = 1) # price option
C0

# Check with inbuilt function for black scholes
# Seems just right
# This automatically gives us our delta
library(ragtop)
blackscholes(
  1,
  s0,
  s0,
  0.005,
  1,
  Ann.SD,
  default_intensity = 0,
  divrate = 0,
  borrow_cost = 0,
  dividends = NULL
)

####### Question 3

# Black scholes output from previous question
Black_scholes.output <- blackscholes(
  1,
  s0,
  s0,
  0.005,
  1,
  Ann.SD,
  default_intensity = 0,
  divrate = 0,
  borrow_cost = 0,
  dividends = NULL
)


Black_scholes.output$Delta # Delta
Black_scholes.output$Price # Price
pnorm(d1, mean = 0, sd = 1) # Check manually, Seems just right

## Calculate hedge rate
N50.binominal.result <- calculate_C(init, 50) # Result of binominal three with n = 50
Option.start <- N50.binominal.result[[2]] # Last two values of three

# Calculate the first two values of three w.r.t. prices
# First calculate parameters
u.temp = exp(Ann.SD * sqrt(1/50))
d.temp = 1/u.temp
first_prices = c()
first_prices[1] = s0*u.temp
first_prices[2] = s0*d.temp
Hedging_ratio_bin = (Option.start[1] - Option.start[2]) / (first_prices[1] - first_prices[2])

# Results question 3
Hedging_ratio_bin # Hedging ratio
Black_scholes.output$Delta # Compare it with the black scholes 
##### LOOK YOUTUBE WHY DIFFERENCES AND STUFF

##### Question 4
rm(list=ls())

# reads in data and subset to our group
# FIRST ADJUST THE DATA BECAUSE IT IS REALLY BAD DATA DO COPY AND STUFF!!
All.data <- read_excel("Data_Assignment3_2020.xlsx", sheet = "EURIBOR")
All.data


























