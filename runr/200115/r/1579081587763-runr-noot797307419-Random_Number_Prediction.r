## Importing libraries

# install.packages("openxlsx")
# install.packages('plotly')
# install.packages('dplyr')
library(dplyr)
library(plotly)
library(openxlsx)

## Importing Data
historic_data <- openxlsx::read.xlsx("Random Number prediction/Datapoints.xlsx", startRow = 0)
names(historic_data) <- "Number"

head(historic_data)
validation_set <- openxlsx::read.xlsx("Random Number prediction/Test_Phase_1.xlsx", startRow = 0)
names(validation_set) <- "Number"
head(validation_set)

## Feature engineering

identify_Even_or_Odd <- function(x) {
  if(x == 0) {
    return("Zero")
  } else if((x %% 2) == 0){
    return("Even")
  } else {
    return("Odd")
  }
}

# identify_Even_or_Odd(1234)

identify_Black_or_Red <- function(x) {
  black <- c(15, 4, 2, 17, 6, 13,11,8, 10,24,33,20,31,22,29, 28, 35,26)
  red <- c(32,19,21,25,34,27,36,30,23,5,16,1,14,9,18,7,12,3)
  if(x == 0) {
    return('green')
  } else if(x %in% red) {
    return('Red')
  } else {
    return("Black")
  }
}

identify_single_digit_or_duble_digit <- function(x) {
  if(x >= 10) {
    return("Double")
  } else {
    return("Single")
  }
}

identify_dozen <- function(x) {
  if(x <= 12) {
    return('First')
  } else if(x >12 && x <= 24) {
    return("Second")
  } else {
    return("Third")
  }
}
# identify_dozen(30)
identify_half_dozen <- function(x) {
  if(x <= 6) {
    return("1st")
  }
  if(x >=6 && x<12) {
    return("2nd")
  }
  if(x>=12 && x<18) {
    return("3rd")
  }
  if(x >= 18 && x<24) {
    return("4th")
  }
  if(x<=24 && x<=30) {
    return("5th")
  }
  if(x>30 && x<=36) {
    return("6th")
  }
}
eo <- NULL
rb <- NULL
sd <- NULL
dz <- NULL
hdz <- NULL
for(i in 1:nrow(historic_data)) {
  historic_data$eo[i] <- identify_Even_or_Odd(historic_data$Number[i])
  historic_data$rb[i] <- identify_Black_or_Red(historic_data$Number[i])
  historic_data$sd[i] <- identify_single_digit_or_duble_digit(historic_data$Number[i])
  historic_data$dz[i] <- identify_dozen(historic_data$Number[i])
  # historic_data$hdz[i] <- identify_half_dozen(historic_data$Number[i])
}

head(historic_data)               

plot_this_distribution <- function(vec) {
  # vec <- iris$Species
  tab <- table(vec) %>% as.data.frame()
  names(tab) <- c("item", "Freq")
  plot_ly(data = tab, x = tab$item, y = tab$Freq, type = 'bar')
}
 
plot_this_distribution(historic_data$eo)
plot_this_distribution(historic_data$rb)
plot_this_distribution(historic_data$sd)
plot_this_distribution(historic_data$dz)
head(historic_data)

smp_size <- floor(0.80 * nrow(historic_data))
set.seed(980)
train_ind <- sample(seq_len(nrow(historic_data)), size = smp_size)

train <- historic_data[train_ind, ]
test <- historic_data[-train_ind, ]

train
model <- glm(data = train, formula = Number  ~ rb + eo + sd + dz , family = 'gaussian')
summary(model)
new_data <-  test[,-1]
predicted <- predict(model, new_data)
actuals <- test$Number
test$predicted <- predicted 
View(test)
getProbableNumbers <- function(x) {
  digit <- floor(x)
  decimal <- x%%1
  if(digit>36) {
    set <- c(1:8)
  } 
  if(digit >= 10 && digit <= 36) {
    set <- c(digit, digit +1,  digit -4,  digit+3, digit + 4, digit-1,digit +2, digit -2, digit -3)
  }
  if (digit<10) {
    y <- round(x, digits = 1) %% 1 * 10
    digit <- as.numeric(paste0(digit, y))
    if(digit>36) {
      set <- c(1,7)
    } else {
      set <- c(digit, digit +1,  digit -4,  digit+3, digit + 4, digit-1,digit +2, digit -2, digit -3)
    }
  }
  return(paste0(set, collapse = ','))
}
probable_numbers <- NULL
validation <- NULL

validate_prediction <- function(item, items) {
  return(ifelse(item %in% items, TRUE, FALSE))
}

for(i in 1:nrow(test)) {
  test$probable_numbers[i] <- getProbableNumbers(test$predicted[i])
  items <- strsplit(x = test$probable_numbers[i], split = ",") %>% unlist %>% as.numeric()
  test$validation[i] <- validate_prediction(test$Number[i], items)
}
View(test)

res <- (test[,c(1,7,8)])
View(res)
plot_this_distribution(test$validation)
