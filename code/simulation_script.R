# # # # # # # # # # # # # # # # # # # # # #
#                                         #
#               Simulation                #
#                                         #
# # # # # # # # # # # # # # # # # # # # # #
options(warn = 2) # turn warnings into errors for debugging

# set path
here::i_am("code/simulation_script.R")

# load packages
library(parallel) # for parallel processing
library(rlecuyer) # for random number generator
library(here)
library(lme4)
library(mitml)
library(jomo)
library(mdmb)
library(miceadds)


# load functions
source(file = here("code", "functions", "simulate-datasets_function.R"))
source(file = here("code", "functions", "generate-missings_function.R"))

# * Simulated Conditions

# NOTE: The simulation design is specified here.


design <- list(
  
  # effect size of gamma01
  gamma01 = list(0, .30),

  # level-2 sample size
  N2 = list(15, 30, 60),
  
  # ICC
  ICC = list(.10, .30),
  
  # relationship of missings to other variable (strength of MAR)
  beta = list(0, .30)

)

# NOTE: Here, we create a "design matrix" that lists all possible conditions by
# combining all levels of the factors above (via expand.grid). The matrix has
# six rows, one for each condition, and consists of numeric values that match the
# levels of each factor in the "design".

# create design matrix
design.matrix <- expand.grid(lapply(design, seq_along))

# *** .....................................
# Simulation
#


# set number of replications
R <- 2

# make a vector of "runs" (row indices of the design matrix)
runs <- rep(1:nrow(design.matrix), times = R)

# *** START SIMULATION *** #

set.seed(6174)

for (r in runs) {

# * Conditions

# read current condition from the design matrix
condition <- design.matrix[r, ]

# set simulation parameter in accordance with the current condition
gamma01 <- design$gamma01[[condition$gamma01]]
N2 <- design$N2[[condition$N2]]
ICC <- design$ICC[[condition$ICC]]
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


# * Analyses 

# fit multilevel model (complete data) --------------------
fit.cd <- lmer(Y ~ cwc(x, group) + gm(x, group) + (1|group), data = dat0, REML = TRUE)

# fit multilevel model (listwise deletion) ----------------
fit.ld <- lmer(Y ~ cwc(x, group) + gm(x, group) + (1|group), data = dat1, REML = TRUE)

# fit multilevel model using MI -------------------------
# specify imputation model
fml <- list(Y + x ~ 1 + (1|group),
            w ~ 1)

# impute data 
imp <- mitml::jomoImpute(data=dat1, formula=fml, n.burn=5000, n.iter=200, m=10)

# fit multilevel model to each dataset
impList <- mitmlComplete(imp)
fit.mi <- with(impList, lmer(Y ~ 1 + cwc(x, group) + gm(x, group) + (1|group), REML=TRUE))

# Fully Bayesian approach -------------------
# mdmb hack: add empty group
dummy <- dat1[1,]
dummy[] <- NA
dummy$group <- N2 + 1
dummy <- dummy[rep(1, each = Nj),]
dat2 <- rbind(dat1[1:nrow(dat1), ], dummy)

# define models (factored regression specification) ---------------
# w conditioned on Y and X
mod_w <- list(model = "linreg", 
              formula = w ~ 1 + gm(Y, group) + gm(x, group),
              variable_level = "group")

# outcome (focal analysis model)
mod_y <- list(model = "mlreg", 
              formula = Y ~ 1 + cwc(x, group) + gm(x, group) + (1|group),
              sampling_level = "group")

# x marginal model
mod_x <- list(model = "mlreg", 
              formula = x ~ 1 + (1|group),
              sampling_level = "group")

# sampling level is group because x means are involved in the outcome model

# specifying which variables will be imputed (combine smaller models in a list)
mod_ind <- list(y = mod_y, x = mod_x)

# imputation
fit.bayes <- mdmb::frm_fb(dat2, 
                     dep = mod_w,
                     ind = mod_ind,
                     burnin = 1000,
                     iter = 3000,
                     aggregation = TRUE)


# * Prepare results

# set parameter names
par.names <- c("gamma10", "gamma01") # gamma10 = cwc, gamma01 = gm

# prepare function for manually calculating CIs (used for small sample CIs in lmer with Snijders&Bosker-dfs)
ci_lmer <- function(x, df, level = 0.95) {
  est <- fixef(x)
  se <- sqrt(diag(vcov(x)))
  tval <- qt(1 - (1-level) / 2, df = df)
  cbind(lower = est - tval * se, upper = est + tval * se)
}

# summarize results of complete data analysis
res.cd <- data.frame(
  method = "CD",
  parameter = par.names,
  est = fixef(fit.cd)[c(2, 3)],
  se = sqrt(diag(vcov(fit.cd)))[c(2, 3)],
  # small sample CIs with Snijders&Bosker-dfs
  ci_l = ci_lmer(fit.cd, df = c(rep(nobs(fit.cd) - 3, 2), ngrps(fit.cd) - 2))[c(2, 3), 1], 
  ci_u = ci_lmer(fit.cd, df = c(rep(nobs(fit.cd) - 3, 2), ngrps(fit.cd) - 2))[c(2, 3), 2]
)

# summarize results of listwise deletion
res.ld <- data.frame(
  method = "LD",
  parameter = par.names,
  est = fixef(fit.ld)[c(2,3)],
  se = sqrt(diag(vcov(fit.ld)))[c(2,3)],
  ci_l = ci_lmer(fit.ld, df = c(rep(nobs(fit.ld) - 3, 2), ngrps(fit.ld) - 2))[c(2,3), 1],
  ci_u = ci_lmer(fit.ld, df = c(rep(nobs(fit.ld) - 3, 2), ngrps(fit.ld) - 2))[c(2,3), 2]
)

# summarize results of MI - Rubins rules for dfs
pool.miR <- testEstimates(fit.mi) # pool with Rubin´s rules
res.miR <- data.frame(
  method = "MI-R",
  parameter = par.names,
  est = coef(pool.miR)[c(2,3)], # same for Rubin and adjusted df
  se = sqrt(diag(vcov(pool.miR)))[c(2, 3)], # same for Rubin and adjusted df
  ci_l = confint(pool.miR)[c(2,3),1],
  ci_u = confint(pool.miR)[c(2,3),2]
)
# summarize results of MI - adjusted dfs
pool.mia <- testEstimates(fit.mi, df = c(rep(nobs(fit.mi[[1]]), 2), N2-2)) # pool with adjusted dfs
res.mia <- data.frame(
  method = "MI-a",
  parameter = par.names,
  est = coef(pool.mia)[c(2,3)], # same for Rubin and adjusted df
  se = sqrt(diag(vcov(pool.mia)))[c(2, 3)], # same for Rubin and adjusted df
  ci_l = confint(pool.mia)[c(2,3),1],
  ci_u = confint(pool.mia)[c(2,3),2]
)

# summarize results of Bayes
res.bayes <- data.frame(
  method = "bayes",
  parameter = par.names,
  est = coef(fit.bayes)[c(9,10)],
  se = sqrt(diag(vcov(fit.bayes))[c(9,10)]),
  ci_l = confint(fit.bayes)[c(9,10),1],
  ci_u = confint(fit.bayes)[c(9,10),2]
)

# summarize conditions
res.condition <- data.frame(
  ID = r,               # condition identifier
  gamma01 = gamma01, N2 = N2, ICC = ICC, beta = beta   # conditions
)

# bind conditions and results together
res <- cbind(
  res.condition,                           # conditions
  rbind(res.cd, res.ld, res.miR, res.mia, res.bayes)  # results
)
rownames(res) <- NULL

# create file name for output
res.filename <- paste("simulation_results_", as.Date(Sys.time()), Sys.getpid(), ".csv", sep = "")

# check if it already exists
new.file <- !file.exists(here("results", res.filename))

# write output to file
write.table(
  res, file = here("results", res.filename),
  row.names = FALSE, col.names = new.file, append = !new.file
)

}

# *** END SIMULATION *** #

