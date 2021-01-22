# Fisher's Exact Test Power Calculations
library(statmod)

# Parameters:
cost_per_subject <- 3
budget <- 2000
number_of_treatments <- 4

x <- matrix(0, 3, 3)

power_levels <- c(0.6, 0.7, 0.8)
expected_differences <- c(0.05, 0.07, 0.1)
alternative = "two.sided"

# Get worst-case power scenario based on values of p1 and p2 that satisfy
# the true_dif_assumption
worst_case_power <- function(n, true_diff_assumption, alternative){
  x <- seq(from = true_diff_assumption*100+1, to = 100)
  
  for (i in (true_diff_assumption*100+1):100){
    p1 <- i/100
    p2 <- p1 - true_diff_assumption

    x[i-true_diff_assumption*100] <- power.fisher.test(p1, p2, n[1], n[2], 
                                                       alternative = alternative)
  }
  return(min(x))
}

# Get n necessary to have sufficiently high worst_case power level
get_n_for_power_level <- function(power_level, starting_n, true_diff_assumption, alternative){
  n <- starting_n
  power <- 0
  while(power < power_level){
    n[1] <- n[1] + 10
    n[2] <- n[2] + 10
    print(n)
    power <- worst_case_power(n, true_diff_assumption, alternative)
  }
  return(n[1])
}



for (i in 1:length(power_levels)){
  for (j in 1:length(power_levels)){
    x[i, j] <- get_n_for_power_level(power_levels[i], c(100,100), expected_differences[j], alternative)
    print(x[i, j])
    }
}
print(x)

# Get best power level for the budget, given the number of treatments


