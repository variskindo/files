data <- c(0.873, 2.817, 0.121, -0.945, -0.055, -1.436, 0.360, -1.478, -1.637, -1.869)

test <- shapiro.test(data)
str(test)

## Shapiro-Wilk normality test
if (test$p.value > 0.05){
  print('Data is normally distributed')
} else {
  print('Distribution of the data is not normal')
}


data1 <- c(0.873, 2.817, 0.121, -0.945, -0.055, -1.436, 0.360, -1.478, -1.637, -1.869)

data2 <- c(0.353, 3.517, 0.125, -7.545, -0.555, -1.536, 3.350, -1.578, -3.537, -1.579)

## R command with options
cor(data1, data2, method = "pearson")
cor.test(data1, data2, method = "pearson")

cor.test(data1, data2, method = "spearman")

## Load the library
library(MASS)

## Create a table with the needed variables.
car.data = table(Cars93$AirBags, Cars93$Type)
print(car.data)


## Perform the Chi-Square test
print(chisq.test(car.data))


## Check for normality
pval <- shapiro.test(data1)
pval
pval <- shapiro.test(data2)
pval

pval1 <- pval1$p.value
pval2 <- pval2$p.value
pval1; pval2
