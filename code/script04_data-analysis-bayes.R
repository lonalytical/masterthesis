# Fully Bayesian missing data handling ---------

here::i_am("code/script04_data-analysis-bayes.R")

# load packages
library(here)
library(mdmb) # for the Bayes methods
library(miceadds)
library(mitml)


# read in dataset
load(here("data", "MARtest.Rdata"))
load(here("data", "MARtest60.Rdata"))
load(here("data", "MCARtest.Rdata"))

# subset with only variables Y, X, W and group
data <- MARtest[,c("Y", "x", "w", "group")]
data60 <- MARtest[,c("Y", "x", "w", "group")]
dataMCAR <- MCARtest[,c("Y", "x", "w", "group")]

source(file = here("code", "functions", "simulate-datasets_function.R"))
source(file = here("code", "functions", "generate-missings_function.R"))

dat1 <- simulate_data(15, 0.3, 0.3)
mis <- simulate_missings(w = dat1$w, p = 0.30, beta = 0.30)
dat1$x[mis] <- NA
dat1$x_c <- NA

# hack: add empty group
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
mod1 <- mdmb::frm_fb(dat2, 
                     dep = mod_w,
                     ind = mod_ind,
                     burnin = 5000,
                     iter = 2000,
                     aggregation = TRUE)
summary(mod1)
plot(mod1, idparm = 7)
coef(mod1)[c(9,10)]
sqrt(diag(vcov(mod1))[c(9,10)])
confint(mod1)[c(9,10),1]
