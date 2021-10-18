
setwd("C:/Users/wmarshall/OneDrive - Brock University/Teaching/3P82 - Regression/Data/")

mydata = read.csv('house-price-data.csv', header=T)

# Part (a)
y = mydata$y
n = length(y)
X = model.matrix(~ x1 + x2, data=mydata)
solve(t(X)%*%X)


# Part (b)
b = solve(t(X) %*% X) %*% t(X) %*% y
b

# Part (c)
SSres = t(y) %*% y - t(b) %*% t(X) %*% y
k = 2
MSres = SSres / (n - k - 1)
MSres

# Part (d)
SST = sum( (y - mean(y))^2)
R2 = 1 - SSres / SST
R2

