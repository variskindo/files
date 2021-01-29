library(catstats) #To use test and CI simulations

one_proportion_test(probability_success = 0.5, 
                    sample_size = xx, 
                    number_repetitions = 1000,
                    as_extreme_as = xx, 
                    direction = "greater", 
                    report_value = "number")

