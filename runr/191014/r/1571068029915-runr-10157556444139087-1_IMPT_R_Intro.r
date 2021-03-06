##  =================================
##  Introduction to R
##  =================================

##  1. Print Function
##  -----------------
    print("Hello World") #[1] "Hello World"



##  2. Simple Math
##  ----------------
      3+9-2
      (2+3)*3-(5/2)
                
          # [1] 12.5
    
##  3. Four basic data types
##  --------------------------
    
    ##  (a) Vectors
    ##  -----------
        ## all elements have same type
    
          name_1 <- c("Albert", "Betty", "Candy")
          name_1    #[1] "Albert" "Betty"  "Candy" 

          
          name_2 = c("Xena", "Yoda", "Zebra")
          name_2 #[1] "Xena"  "Yoda"  "Zebra"
          
          age <- c(21, 22, 23)
          age #[1] 21 22 23
          
          class(name_1) #[1] "character"
          class(age) #[1] "numeric"
          is.vector(name_1) #[1] TRUE
          
          
    ##  (b) Matrix
    ##  -------------
        ##   rows and columns
        
          X <- matrix(c(1,2,3,4), nrow=2, ncol=2)
          X
              #       [,1] [,2]
              # [1,]    1    3
              # [2,]    2    4
          
    ##  (c) List
    ##  -----------
          # contains elements of different types
          
          y <- list(name="Mike", gender="M", age=20, company="ABC Coy")
          y
                    # $name
                    # [1] "Mike"
                    # 
                    # $gender
                    # [1] "M"
                    # 
                    # $age
                    # [1] 20
                    # 
                    # $company
                    # [1] "ABC Coy"
          
    ##  (d) Dataframe
    ##  ---------------
          ## used for storing data tables. It is a list of vectors of equal length
          
          name <- c("Mike", "Lucy","John")
          age <- c(20, 25, 30)          
          student <- c(TRUE, FALSE, TRUE)          

          profile1 <- data.frame(name, age, student)          
          profile1          
                  # name age student
                  # 1 Mike  20    TRUE
                  # 2 Lucy  25   FALSE
                  # 3 John  30    TRUE