

# install.packages('data.table')
library(data.table)

# Program spuRs/resources/scripts/change.r

change <- function(x, y.vec = c()) {

  if (x == 0) {
    cat(y.vec, "\n")
  } else {
    coins <- c(200, 100, 50, 20, 10, 5)
    new.x <- x - coins
    new.x <- new.x[new.x >= 0]
    for (z in new.x) {
      y.tmp <- c(y.vec, x - z)
      if (identical(y.tmp, sort(y.tmp))) {
        result <- change(z, y.tmp)
        list <- lapply(result, function(x){
          as.list.data.frame(table(c(row(result)), c(result)))
        })
        rbindlist(list, use.names = TRUE, fill = TRUE)
      }
    }
  }
  return(invisible(NULL))
}

# output 1
change(30,5)

## or alternative method

# install.packages('RcppAlgos')
library(RcppAlgos)


x2 <- 30
coins <- c(200, 100, 50, 20, 10, 5)

list2 <- lapply((1:x2 / min(coins))*5 , function(x) {
  
  y.tmp2 <- comboGeneral(coins, 
                        m = x, 
                        repetition = TRUE, 
                        constraintFun = "sum",
                        comparisonFun = "==", 
                        limitConstraints = x2)

  as.data.frame.matrix(table(c( row(y.tmp2)), c(y.tmp2)))
})

result2 <- rbindlist(list2, use.names = TRUE, fill = TRUE )

result2[is.na(result2) ] <- 0

#output 2
result2

