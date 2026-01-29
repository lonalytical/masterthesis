# # # # # # # # # # # # # # # # # # # # # #
#                                         #
#         Changing priors in jomo         #
#                                         #
# # # # # # # # # # # # # # # # # # # # # #

here::i_am("code/jomopriors_script.R")

# packages
library(lme4)
library(jomo)
library(mitml)
library(parallel) # for parallel processing
library(rlecuyer) # for random number generator
library(here)
library(miceadds)

# load functions
source(file = here("code", "functions", "simulate-datasets_function.R"))
source(file = here("code", "functions", "generate-missings_function.R"))

# * Simulated Conditions

# NOTE: Only the most critical conditions were used here.

design <- list(
  
  # effect size of gamma01
  gamma01 = .40,
  
  # level-2 sample size
  N2 = list(15, 30, 60),
  
  # ICC
  ICC = .10,
  
  # relationship of missings to other variable (strength of MAR)
  beta = list(0, .30)
  
)

# create design matrix
design.matrix <- expand.grid(lapply(design, seq_along))

# * Parallel processing

# create a cluster
.cl <- parallel::makeCluster(8, type = "PSOCK")  
# NOTE: Number of instances is specified here.

# initiate parallel random number generator across instances
RNGkind("L'Ecuyer-CMRG")
parallel::clusterSetRNGStream(.cl, iseed = 6174) 
# setting a seed for reproducibility

# load required packages on all instances
parallel::clusterEvalQ(.cl, {
  library(here)
  library(lme4)
  library(mitml)
  library(jomo)
  library(miceadds)
})

# export objects to instances
parallel::clusterExport(.cl, varlist = ls())


# *** .....................................
# Simulation
#

# set number of replications
R <- 1000

# make a vector of "runs" (row indices of the design matrix)
runs <- rep(1:nrow(design.matrix), times = R)

# *** START SIMULATION *** #

.result <- parallel::clusterApplyLB(.cl, x = runs, fun = function(r) {
  
  # * Conditions
  
  # read current condition from the design matrix
  condition <- design.matrix[r, ]
  
  # set simulation parameter in accordance with the current condition
  gamma01 <- design$gamma01
  N2 <- design$N2[[condition$N2]]
  ICC <- design$ICC
  beta <- design$beta[[condition$beta]]
  
  # set number of people per group
  Nj = 10
  
  # * Data generation 
  
  # simulate data
  dat0 <- simulate_data(
    N2 = N2, 
    gamma01 = gamma01, 
    ICC = ICC)[,c("x", "Y", "w", "group")]
  
  # simulate missing values
  dat1 <- dat0
  mis <- simulate_missings(w = dat1$w, p = 0.30, beta = beta)
  dat1$x[mis] <- NA
  
  
  # fit multilevel model using MI -------------------------
  # specify imputation model
  fml <- list(Y + x ~ 1 + (1|group),
              w ~ 1)
  
  # impute data with standard priors
  imp1 <- mitml::jomoImpute(data=dat1, formula=fml, n.burn=2000, n.iter=300, m=10)
  
  # define weakly informative prior for the level-2 covariance matrix based on a "prior guess"
  Tau.guess <- diag(c(0.1, 0.1, 1))   # prior guess for 
  Tau.scale <- Tau.guess * 3          # scale matrix of the inverse-Wishart prior (the "3" is the size of the matrix)
  
  # NOTE: Prior guess is for two standardized level-1 variables with ICC of 0.1 (x) and residual 
  # ICC of 1(y) and one standardized level-2 variable (z).
  
  # save priors in a list
  prior <- list(
    Binv = diag(1, 2),  # scale matrix of prior for "Sigma" (default)
    Dinv = Tau.scale    # scale matrix of prior for "Tau" (based on prior guess)
  )
  
  # run MI with weakly informative priors
  imp2 <- mitml::jomoImpute(formula = fml, data = dat1, n.burn=2000, n.iter=300, m=10, prior = prior)

  # fit multilevel model to each dataset
  impList1 <- mitmlComplete(imp1)
  fit.mi1 <- with(impList1, lmer(Y ~ 1 + cwc(x, group) + gm(x, group) + (1|group), REML=TRUE))

  impList2 <- mitmlComplete(imp2)
  fit.mi2 <- with(impList2, lmer(Y ~ 1 + cwc(x, group) + gm(x, group) + (1|group), REML=TRUE))
  
  # * Prepare results
  
  # set parameter names
  par.names <- c("gamma10", "gamma01") # gamma10 = cwc, gamma01 = gm

  # summarize results of MI - Rubins rules for dfs
  pool.miR1 <- testEstimates(fit.mi1) # pool with Rubin´s rules
  res.miR1 <- data.frame(
    method = "MI-R1",
    parameter = par.names,
    est = coef(pool.miR1)[c("cwc(x, group)", "gm(x, group)")],
    se = sqrt(diag(vcov(pool.miR1)))[c("cwc(x, group)", "gm(x, group)")],
    ci_l = confint(pool.miR1)[c("cwc(x, group)", "gm(x, group)"), "2.5 %"],
    ci_u = confint(pool.miR1)[c("cwc(x, group)", "gm(x, group)"), "97.5 %"]
  )
  pool.miR2 <- testEstimates(fit.mi2) # pool with Rubin´s rules
  res.miR2 <- data.frame(
    method = "MI-R2",
    parameter = par.names,
    est = coef(pool.miR2)[c("cwc(x, group)", "gm(x, group)")],
    se = sqrt(diag(vcov(pool.miR2)))[c("cwc(x, group)", "gm(x, group)")],
    ci_l = confint(pool.miR2)[c("cwc(x, group)", "gm(x, group)"), "2.5 %"],
    ci_u = confint(pool.miR2)[c("cwc(x, group)", "gm(x, group)"), "97.5 %"]
  )


  # summarize conditions
  res.condition <- data.frame(
    ID = r,               # condition identifier
    N2 = N2, ICC = ICC, beta = beta # conditions
  )
  
  # bind conditions and results together
  res <- cbind(
    res.condition,                           # conditions
    rbind(res.miR1, res.miR2)  # results
  )
  rownames(res) <- NULL
  
  # create file name for output
  res.filename <- paste("jomopriors_", Sys.getpid(), ".csv", sep = "")
  
  # check if it already exists
  new.file <- !file.exists(here("results", res.filename))
  
  # write output to file
  write.table(
    res, file = here("results", res.filename),
    row.names = FALSE, col.names = new.file, append = !new.file
  )
  
})

# stop cluster after simulation
parallel::stopCluster(.cl)

# *** END SIMULATION *** #