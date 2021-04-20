#vectors
companies <- c("A", "A", "A", "B", "B")
cash_flow <- c(1000, 4000, 3000, 2000, 5000)
year <- c(2, 1, 3, 2, 4)

#create a data.frame from the vectors 
cash <- data.frame(companies, cash_flow, year)
cash


#User defined function
i = 5
percent_to_decimal <- function(i, digits = 2) {
    decimal = i/100
    round(decimal, digits)
}

#call the function and assign a variable
int_pct <- percent_to_decimal(i)
int_pct

#present value user defined function
pv <- function(cash_flow, i, year) {
    mult <- 1 + int_pct
    cash_flow * mult^ -year 
}



#call the function and assign a variable
cash$present_value <- pv(cash$cash_flow, 5, cash$year)
cash 

debt <- 5000
cash <- 4000

while (debt > 0) {
    debt = debt - 500
    cash = cash - 500
    print(paste("Debt is remaining:", debt, "Cash is remaining:", cash))
    if (cash == 0) {
        print("You ran out of cash!")
        break
    }
}


debt <- 5000
i = 0
x_axis = i
y_axis = debt 

plot(x_axis, y_axis, xlim = c(0, 10), ylim = c(0, 5000))


while (debt > 0) {
    debt = debt - 500
    i = i + 1
    x_axis = c(i, x_axis)
    y_axis = c(debt, y_axis)
    plot(x_axis, y_axis, xlim = c(0, 10), ylim = c(0, 5000))
}
