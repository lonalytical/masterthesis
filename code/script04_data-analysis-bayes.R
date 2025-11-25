# Fully Bayesian missing data handling ---------

here::i_am("code/script04_data-analysis-bayes.R")

# load packages
library(here)
library(mdmb) # for the Bayes methods

# read in dataset
load(here("data", "MARtest.Rdata"))
load(here("data", "MARtest60.Rdata"))
load(here("data", "MCARtest.Rdata"))

# subset with only variables Y, X, W and group
data <- MARtest[,c("Y", "x", "w", "group")]
data60 <- MARtest[,c("Y", "x", "w", "group")]
dataMCAR <- MCARtest[,c("Y", "x", "w", "group")]



# define models (factored regression specification) ---------------
# w conditioned on Y and X
cov_w <- list("model"="linreg", 
              "formula"= w ~ gm(Y, group) + gm(x, group),
              "variable_level" = "group")
              
# outcome (focal analysis model)
dep <- list("model"="mlreg", 
            "formula"= Y ~ cwc(x, group) + gm(x, group) + (1|group),
            sampling_level = "group")

# x marginal model
ind_x <- list("model"="mlreg", 
              "formula" = x ~ (1|group),
              sampling_level = "group")

# sampling level is group because x means are involved in the outcome model

# specifying which variables will be imputed
ind <- list("x" = ind_x,
            "y" = dep)

# imputation
mod1 <- mdmb::frm_fb(data, dep = cov_w, ind = ind, aggregation=TRUE)
mod2 <- mdmb::frm_fb(data60, dep = cov_w, ind = ind, aggregation=TRUE)
mod3 <- mdmb::frm_fb(dataMCAR, dep = cov_w, ind = ind, aggregation=TRUE)
