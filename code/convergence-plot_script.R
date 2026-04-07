###### Convergence diagnostic plots#######
library(here)
library(mitml)
library(coda)
#####MI
imp<-readRDS(here("results", "imp_convergence.rds"))
summary(imp)
plot(imp)
plot(imp, print = "psi", pos=c(1,1),trace="imputation")
plot(imp, print = "psi", pos=c(2,2),trace="imputation")
plot(imp, print = "beta", pos=c(1,2),trace="imputation")
#NOTE: This plots are not rendered in Quarto, so they had to be saved here


#### BAYES
fit.bayes<-readRDS(here("results", "fit_bayes_convergence.rds"))
summary(fit.bayes)
#colnames(fit.bayes$values_coda)

traceplot(fit.bayes$values_coda[, "Y_vcov_group_(Intercept)-(Intercept)"])
acfplot(fit.bayes$values_coda[, "Y_vcov_group_(Intercept)-(Intercept)"])

traceplot(fit.bayes$values_coda[, "x_vcov_group_(Intercept)-(Intercept)"])
acfplot(fit.bayes$values_coda[, "x_vcov_group_(Intercept)-(Intercept)"])



