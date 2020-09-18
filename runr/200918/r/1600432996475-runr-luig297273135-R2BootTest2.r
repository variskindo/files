distn <- function(A, B, lambda = 0.5, ro = 0.5){
    Am <- A[,1]
    Al <- A[,2]
    Ar <- A[,3]
    Bm <- B[,1]
    Bl <- B[,2]
    Br <- B[,3]
    
    distance <- (Am-Bm)^2 + ((Am-(lambda*Al))-(Bm-(lambda*Bl)))^2 + ((Am+(ro*Ar))-(Bm+(ro*Br)))^2
    return(distance) 
  }

Tn <- function(dat, ind){
    # Takes in input {X, Ym, gYl, hYr} or {X, Zm, gZl, hZr}
    X <- dat[ind, 1]
    A <- dat[ind, 2]
    B <- dat[ind, 3]
    C <- dat[ind, 4]
    
    n <- length(X)
    
    # Regressions
    Ym_hat <- lm(A ~ X)
    gYl_hat <- lm(B ~ X)
    hYr_hat <- lm(C ~ X)  
    
    # Saving coefficients
    am_hat <- Ym_hat$coefficients[2]
    al_hat <- gYl_hat$coefficients[2]
    ar_hat <- hYr_hat$coefficients[2]
    
    bm_hat <- Ym_hat$coefficients[1]
    bl_hat <- gYl_hat$coefficients[1]
    br_hat <- hYr_hat$coefficients[1]
    
    # Arguments computation
    Y_tilde <- cbind(A, B, C)
    Y_tilde_hat <- cbind(am_hat*X+bm_hat, al_hat*X+bl_hat, ar_hat*X+bl_hat)
    Y_tilde_avg <- matrix(rep(c(mean(A), mean(B), mean(C)), times = n), nrow = n, byrow = T)
    
    # Testing statistic computation
    return(n * (sum(distn(Y_tilde_hat,Y_tilde_avg)) / sum(distn(Y_tilde,Y_tilde_avg))))
  }


############################################ VECCHIO CODICE #################################################
# 
# R2BootTest <-
#   function (X, Ym, gYl, hYr, m, R, alpha = 0.05, type = c("real", "simul", "power"))
#   {
#     # X: Independent Variable
#     # Ym: Dependent variable, center values
#     # gYl: Dependent variable, left spread values (tranformed by a g(x))
#     # hYr: Dependent variable, right spread values (tranformed by a h(x))
#     # m: Number of iterations
#     # R: Number of Bootstrap Replicates
#     # size: Test size
#     # Type: Depending on the function's purpose:
#             # "real" takes in input observed values and returns the Acceptance Significative Level
#             # "simul" takes in input data generated under null hypotesis and returns the probability of rejection
#             # "power" takes in input data generated under Pitman's alternatives and returns the probability of rejection
#     
#     n <- length(X)
#     
#     # Testing statistic computation from observed data
#     
#     Tn_ref <- Tn(cbind(X, Ym, gYl, hYr), 1:n)
#     print(Tn_ref)
#     
#     # Regressions
#     Ym_hat <- lm(A ~ X)
#     gYl_hat <- lm(B ~ X)
#     hYr_hat <- lm(C ~ X)  
#     
#     
#     Zm <- Ym_hat$residuals
#     Zl <- gYl_hat$residuals
#     Zr <- hYr_hat$residuals
#     
#     
#     # Bootstrap initialization
#     boot.data <- cbind(X, Zm, Zl, Zr)
#     boot.res <- numeric(R)
#     p <- numeric(m)
#     
#     #Bootstrap procedure
#       for (j in 1:m) {
#         
#         for(i in 1:R){
#           
#           boot.ind <- sample(1:n, n, replace = T)
#           boot.res[i] <- Tn(boot.data, boot.ind)
#           
#           # Printing some testing statistics to check if everything works fine
#           # bbb <- seq(1, R, by = R/100)
#           # if(i %in% bbb){
#           #   print(boot.res[i])
#           # }
#           
#         }
#         
#         p[j] <- mean(boot.res > Tn_ref)
#         
#         # # Printing some pvalues to check if everything works fine
#         # aaa <- seq(1, m, by = m/1000)
#         # if(j %in% aaa){
#         #   print(p[j])
#         # }
#         
#       }
#     
#     if(type == "real"){
#       
#       return(mean(p))
#       
#     }
#     
#     else{
#       
#       prob_rejection_hat <- mean(p < alpha)
#       return(prob_rejection_hat)
#       
#     }
# 
#   }