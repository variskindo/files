

## how to add elements of an array
Set <- c(1,5,9,11,32,12,3,5,7)

# Solution 1: by using the built in sum function
Result_sum <- sum(Set)
Result_sum
# Nice result. Only 2 lines are needed.
# Problem is you cannt intervene if there are other process that you want to
# get. For example you may want to add only the even numbers.



## Solution 2: Using a variable and a single for loop

numterms <- length(Set)
sum = 0
for ( i in 1:numterms){
     sum = sum + Set[i]      # this line accumulate the sum for each value of i
}
sum



## Solution 3: Binisaya (Enumerate all terms? What if you have 10,000 items.)
## This kind of solution is good for ignorants only

sum = Set[1] + Set[2] + Set[3] + Set[4] + Set[5] + Set[6] + Set[7] + Set[8] + Set[9]
sum

#3 Final remark: Use for single for loop to accumulate the values



