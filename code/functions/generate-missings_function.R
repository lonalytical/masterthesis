# MISSING DATA GENERATION FUNCTION --------------------
# This function generates missings in one variable based on 
# the relationship with a second variable.
# It assigns a propensity score to each X-value.
# Then, if this value > 0, the value will be erased.

simulate_missings <- function(w, p, beta) {
  # w: variable as a predictor for missingness in MAR
  # p: missingness rate
  # beta: strength of MAR mechanism 
    # (w explains beta^2 * 100 % of variance in missing values)
  
  # z-standardize w
  z <- (w-mean(w)) / sd(w)
  
  e <- rnorm(n = length(z), mean = 0, sd = sqrt(1-beta^2))
  a = qnorm(p)
  
  r <- a + beta * z + e
  
  return(r>0)
}

