# SCRIPT FOR DATA GENERATION ---------------

here::i_am("code/script01_data-generation.R")

# load packages
library(here)

# load functions
source(here("code", "functions", "simulate-datasets_function.R"))
source(here("code", "functions", "generate-missings_function.R"))

# simulate complete datasets ----------------
test <- simulate_data(15, 0.3, 0.3)
View(test)
var(test$x)
var(test$Y)
var(test$w)

test30 <- simulate_data(30, 0.3, 0.3)
save(test30, file = here("data", "test30.Rdata"))

test60 <- simulate_data(60, 0.3, 0.3)
save(test60, file = here("data", "test60.Rdata"))

# testing function with large sample sizes
test2 <- simulate_data(15000, 0.3, 0.3)
var(test2$x)
var(test2$Y)
var(test2$w)
cor(test2$x_a, test2$w)
mean(test2$x)
mean(test2$Y)
mean(test2$w)

# save complete datasets
save(test, file = here("data", "test.Rdata"))

# simulate missings with MCAR mechanism ------------

MCARtest <- test

na <- simulate_missings(MCARtest$w, 0.3, 0)
na
MCARtest[na, "x"] <- NA
View(MCARtest)

# save MCAR datasets 
save(MCARtest, file = here("data", "MCARtest.Rdata"))

MCARtest30 <- test30
na <- simulate_missings(MCARtest30$w, 0.3, 0)
na
MCARtest30[na, "x"] <- NA
View(MCARtest)

# save MCAR datasets 
save(MCARtest30, file = here("data", "MCARtest30.Rdata"))

# simulate missings with MAR mechanism ------------

MARtest <- test
na <- simulate_missings(MARtest$w, 0.3, 0.4)
na
MARtest[na, "x"] <- NA
View(MARtest)
# save MAR datasets ------------------------------
save(MARtest, file = here("data", "MARtest.Rdata"))

MARtest60 <- test60
na <- simulate_missings(MARtest60$w, 0.3, 0.3)
na
MARtest60[na, "x"] <- NA
View(MARtest60)
# save MAR datasets ------------------------------
save(MARtest60, file = here("data", "MARtest60.Rdata"))
     