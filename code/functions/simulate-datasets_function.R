# DATA SIMULATION FUNCTION --------------------------------------
# This function simulates data from a two-level random intercept model
# with a group-mean centered variable X and the group-mean of X as predictors
# as well as a group-level W as predictor.
# The function accepts the number of groups, the effect of the group mean of X and the
# ICC of X, which is also the residual ICC of Y, as arguments.

simulate_data <- function(N2, gamma01, ICC) {

# N1: sample size on individual level (total number of individuals)
# N2: sample size on group level (total number of groups)
# Nj: number of individuals per group
# gamma10: effect of X on individual level
# gamma01: effect of X on group level
# gamma02: effect of W (group-level variable)
# u: group-level random effect of Y
# psiyqu: residual group-level variance of Y
# e: individual-level random effect of Y
# sigmayqu: residual individual level variance of Y
# u_x: group-level random effect of X
# e_x: individual-level random effect of X
# e_w: random effect of W
# x_c: group-mean centered individual X value
# X_a: Group-mean of X
#
  
  # set constants
  Nj = 10
  gamma10 = 0.3
  gamma02 = 0.0
  corrX_aW = 0.3
  tauxqu = ICC
  sigmaxqu = 1-ICC
  
  
  # simulate X from between- and within-components
  u_x <- rnorm(N2, mean = 0, sd = sqrt(tauxqu))
  u_x <- rep(u_x, each = Nj)
  e_x <- rnorm(N2*Nj, mean = 0, sd = sqrt(sigmaxqu))
  x <- u_x + e_x
  # apply group-mean centering
  ## create group numbers and averages of X
  group <- rep(1:N2, each = Nj)
  x_a <- ave(x, group, FUN = mean)
  ## center X values
  x_c <- x - x_a
  
  # simulate W values correlated with X_a
  e_w <- rnorm(N2, mean = 0, sd = sqrt(1-(corrX_aW^2)))
  e_w <- rep(e_w, each = Nj)
  a <- corrX_aW/(sqrt(tauxqu + sigmaxqu/Nj))
  w <- a * x_a + e_w
  
  # simulate residual variances of Y with Var(fixed) and ICC
  Var_fix = gamma10^2*(sigmaxqu-(sigmaxqu/Nj))+gamma01^2*(tauxqu+(sigmaxqu/Nj))
  psiyqu = ICC * (1-Var_fix)
  sigmayqu = (1-ICC) * (1-Var_fix)
  
  u <- rnorm(N2, mean = 0, sd = sqrt(psiyqu))
  u <- rep(u, each = Nj)
  e <- rnorm(N2*Nj, mean = 0, sd = sqrt(sigmayqu))
  
  # simulate Y
  Y <- gamma10 * x_c + gamma01 * x_a + gamma02 * w + u + e
  
  dat <- data.frame(x = x, 
                    x_a = x_a, 
                    u_x = u_x, 
                    e_x = e_x, 
                    x_c = x_c, 
                    group = group,
                    e_w = e_w,
                    a = a,
                    w = w,
                    Var_fix = Var_fix,
                    psiyqu = psiyqu,
                    sigmayqu = sigmayqu,
                    u = u,
                    e = e,
                    Y = Y)
  return(dat)
}
