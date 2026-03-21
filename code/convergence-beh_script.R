# # # # # # # # # # # # # # # # # # # # # #
#                                         #
#         Convergence diagnostics         #
#                                         #
# # # # # # # # # # # # # # # # # # # # # #

here::i_am("code/convergence-beh_script.R")

library(here)
library(lme4)
library(mitml)
library(jomo)
library(mdmb)
library(miceadds)

source(file = here("code", "functions", "simulate-datasets_function.R"))
source(file = here("code", "functions", "generate-missings_function.R"))

set.seed(1234)

# most challenging condition
gamma01 <- 0.40
N2 <- 15
ICC <- 0.10
beta <- 0.30
Nj <- 10

# simulate complete data
dat0 <- simulate_data(
  N2 = N2,
  gamma01 = gamma01,
  ICC = ICC
)[, c("x", "Y", "w", "group")]

# impose missingness
dat1 <- dat0
mis <- simulate_missings(w = dat1$w, p = 0.30, beta = beta)
dat1$x[mis] <- NA

# multilevel MI with jomo
fml <- list(
  Y + x ~ 1 + (1 | group),
  w ~ 1
)

imp <- mitml::jomoImpute(
  data = dat1,
  formula = fml,
  n.burn = 2000,
  n.iter = 300,
  m = 10
)


# fully Bayesian approach with mdmb
dummy <- dat1[1, ]
dummy[] <- NA
dummy$group <- N2 + 1
dummy <- dummy[rep(1, each = Nj), ]
dat2 <- rbind(dat1, dummy)

mod_w <- list(
  model = "linreg",
  formula = w ~ 1 + gm(Y, group) + gm(x, group),
  variable_level = "group"
)

mod_y <- list(
  model = "mlreg",
  formula = Y ~ 1 + cwc(x, group) + gm(x, group) + (1 | group),
  sampling_level = "group"
)

mod_x <- list(
  model = "mlreg",
  formula = x ~ 1 + (1 | group),
  sampling_level = "group"
)

mod_ind <- list(y = mod_y, x = mod_x)

fit.bayes <- mdmb::frm_fb(
  dat2,
  dep = mod_w,
  ind = mod_ind,
  burnin = 1000,
  iter = 3000,
  aggregation = TRUE
)

saveRDS(imp, here("results", "imp_convergence.rds"))
saveRDS(fit.bayes, here("results", "fit_bayes_convergence.rds"))



