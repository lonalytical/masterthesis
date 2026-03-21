###### Convergence diagnostic plots#######
library(here)
library(mitml)
library(coda)
#####MI
imp<-readRDS(here("results", "imp_convergence.rds"))
#summary(imp)
miplot<-plot(imp, print = "psi", pos=c(1,1),trace="imputation")

#### BAYES
fit.bayes<-readRDS(here("results", "fit_bayes_convergence.rds"))
#summary(fit.bayes)
#colnames(fit.bayes$values_coda)

traceplot(fit.bayes$values_coda[, "Y_vcov_group_(Intercept)-(Intercept)"])
acfplot(fit.bayes$values_coda[, "Y_vcov_group_(Intercept)-(Intercept)"])

traceplot(fit.bayes$values_coda[, "x_vcov_group_(Intercept)-(Intercept)"])
acfplot(fit.bayes$values_coda[, "x_vcov_group_(Intercept)-(Intercept)"])

x <- fit.bayes$values_coda[, "Y_beta_gm(x, group)"]

