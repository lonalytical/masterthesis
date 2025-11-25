# analyse complete datasets ----------------------
# declare location of script
here::i_am("code/script02_data-analysis-complete.R")

# load packages
library(here) # for here() function
library(lme4) # for fitting multilevel model to data

# load data
load(here("data", "test.Rdata"))
data <- test

load(here("data", "test30.Rdata"))
data30 <- test30

load(here("data", "test60.Rdata"))
data60 <- test60

# analyse data

fit1 <- lmer(Y ~ x_c + x_a + (1|group), data = data, REML = TRUE)
# brauche ich hier globalen Intercept?
summary(fit1)

fit1 <- lmer(Y ~ x_c + x_a + (1|group), data = test2, REML = TRUE)
# brauche ich hier globalen Intercept?
summary(fit1)

fit2 <- lmer(Y ~ x_c + x_a + (1|group), data = data30, REML = TRUE)
summary(fit2)

fit3 <- lmer(Y ~ x_c + x_a + (1|group), data = data60, REML = TRUE)
summary(fit3)

             

