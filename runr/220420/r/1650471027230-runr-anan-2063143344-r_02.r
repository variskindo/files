
source(paste(path, "/r_01.r", sep=""))
library(ggplot2)

x <- c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
x

y <- c(x ^ 3)
y

qplot(x, y)

x <- c(1,2,2,3,3,3,4)
qplot(x, binwidth = 1)

roll <- function(die) {
    dice <- sample(die, size=2, replace=TRUE)
    sum(dice)
}

die = 1:6

rolls = replicate(10000, roll(die))  # rolls 10 times and stores the return sum
qplot(rolls, binwidth=1)
