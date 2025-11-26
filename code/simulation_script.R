# # # # # # # # # # # # # # # # # # # # # #
#                                         #
#               Simulation                #
#                                         #
# # # # # # # # # # # # # # # # # # # # # #

# *** .....................................
# Setup
#

# * Packages and Files

# NOTE: Here, packages and file paths are set up. In addition, all user-defined
# functions are loaded here. In this example, we use the packages "lavaan" and
# "mice" and load two additional functions for simulating the data and inducing
# missing values in the simulated data.

# set path and file name
here::i_am("code/simulation_script.R")

# load packages
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

# NOTE: Here, we define how many times each condition will be replicated (R).
# Then, we start the simulation. There are many ways to do this, but one way
# is to (a) create a vector ("runs") of indices that correspond to the rows in
# the design matrix being replicated and then (b) loop over this vector.
# In this example, we set the number of replications to R=10, resulting in a
# vector of length 60 that includes each condition number (1-6) ten times.

# set number of replications
R <- 10

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
dat0 <- simulate_data(N2 = N2, gamma01 = gamma01, ICC = ICC)

# simulate missing values
dat1 <- dat0
mis <- simulate_missings(w = dat1$w, p = 0.30, beta = beta)
dat1$x[mis] <- NA
dat1$x_c <- NA
dat1$x_a[mis] <- NA


# * Analyses 

# fit multilevel model (complete data) --------------------
fit.cd <- lmer(Y ~ x_c + x_a + (1|group), data = dat0, REML = TRUE)

# fit multilevel model (listwise deletion) ----------------
# NOTE: First, centered x values and group means are calculated with only complete rows.
x_a <- tapply(dat1$x, dat1$group, mean, na.rm = TRUE)
dat1$x_a <- rep(x_a, each = Nj)
dat1$x_a[mis] <- NA

dat1$x_c <- dat1$x - dat1$x_a

fit.ld <- lmer(Y ~ x_c + x_a + (1|group), data = dat1, REML = TRUE)

# fit multilevel model using MI -------------------------
# specify imputation model
fml <- list(Y + x ~ 1 + (1|group),
            w ~ 1)

# impute data 
imp <- mitml::jomoImpute(data=dat1, formula=fml, n.burn=5000, n.iter=200, m=10)
# calculate X_a again for each dataset and append to datasets
impList <- mitmlComplete(imp)
cm <- with(impList, clusterMeans(x, group))

for (i in seq_along(impList)) {
  impList[[i]]$x_a <- cm[[i]]
}

# group-mean-center x
for (i in seq_along(impList)) {
  impList[[i]]$x_c <- impList[[i]]$x - impList[[i]]$x_a
}

# fit analysis model to each imputed dataset
fit.mi <- with(impList, lmer(Y ~ 1 + x_c + x_a + (1|group), REML=TRUE))


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
                     burnin = 5000,
                     iter = 2000,
                     aggregation = TRUE)


# * Prepare results

# set parameter names
par.names <- c("gamma10", "gamma01") # gamma10 = cwc, gamma01 = gm

# summarize results of complete data analysis
res.cd <- data.frame(
  method = "CD",
  parameter = par.names,
  est = fixef(fit.cd)[c(2, 3)],
  se = sqrt(diag(vcov(fit.cd)))[c(2, 3)],
  ci_l = confint(fit.cd)[c(4, 5), 1],
  ci_u = confint(fit.cd)[c(4, 5), 2]
)

# summarize results of listwise deletion
res.ld <- data.frame(
  method = "LD",
  parameter = par.names,
  est = fixef(fit.ld)[c(2,3)],
  se = sqrt(diag(vcov(fit.ld)))[c(2,3)],
  ci_l = confint(fit.ld)[c(4,5), 1],
  ci_u = confint(fit.ld)[c(4,5), 2]
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
pool.mia <- testEstimates(fit.mi, df = nlevels(impList[[1]]$group)-2) # pool with adjusted df
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

