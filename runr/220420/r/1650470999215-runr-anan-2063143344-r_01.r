die <- 1:6

print(die)

# element-wise operations

print(die - 1)

sq_die = die * die

print(sq_die)

die <- die + 1:6

print(die)

# uneven length vector ops, repeats with 1 first, then multiplies with 2, alternaively
die <- die * c(1,2)
print(die)

v1 = c(1,2,3)
v2 = c(3,4,5)

print(v1)
print(v2)

# inner multiplication of matrix
in_prod = v1 %*% v2
print(in_prod)

# outer multiplication
out_prod = v1 %o% v2
print(out_prod)

# using functions
print(round(3.1412))

print(factorial(3))

print(mean(1:6))

print(round(mean(1:6)))

# sample function returns the size specified, aka random sample
rand_v = sample(x=v1, size=1)
print(rand_v)

die = 1:6
rand = sample(die, size=1)
print(rand)
print(sample(die, size=3))

args(sample)

dice = sample(die, replace=TRUE, size=2)    # default replace is false, so prevents same number for size=2
dice

sum(dice)    # totals dice values

# write a function
roll <- function() {
    die <- 1:6
    dice <- sample(die, size=2, replace=TRUE)
    sum(dice)
}

roll()

roll2 <- function(die) {
    dice <- sample(die, size=2, replace=TRUE)
    sum(dice)
}

roll2(1:6)

