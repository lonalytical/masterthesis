########### Data preparation for results table ###########


here::i_am("code/table_script.R")

# packages
library(here)
library(dplyr) # for data grouping
library(tidyr)
library(knitr)
library(kableExtra) # for arranging the tables

# read in function for wide tables
source(file = here("code", "functions", "make-wide-table_function.R"))
source(file = here("code", "functions", "make-MCSE-table_function.R"))

# read in results
results <- read.table(file = here("results", "summarized_simulation-results.csv"))


# greek letters for gammas
results$parameter <- ifelse(
  results$parameter == "gamma01", "$\\gamma_{01}$",
  "$\\gamma_{10}$"
)

# create tables for tables
results15_1 <- results %>% filter(N2 == 15 & gamma01 == 0.0)
results15_00 <- make_wide_table(results15_1)
results15_00_m <- make_MCSE_table(results15_1)


results15_2 <- results %>% filter(N2 == 15 & gamma01 == 0.4)
results15_04 <- make_wide_table(results15_2)
results15_04_m <- make_MCSE_table(results15_2)

results30_1<- results %>% filter(N2 == 30 & gamma01 == 0.0 )
results30_00 <- make_wide_table(results30_1)
results30_00_m <- make_MCSE_table(results30_1)

results30_2 <- results %>% filter(N2 == 30 & gamma01 == 0.4)
results30_04 <- make_wide_table(results30_2)
results30_04_m <- make_MCSE_table(results30_2)

results60_1 <- results %>% filter(N2 == 60 & gamma01 == 0.0)
results60_00 <- make_wide_table(results60_1)
results60_00_m <- make_MCSE_table(results60_1)

results60_2 <- results %>% filter(N2 == 60 & gamma01 == 0.4)
results60_04 <- make_wide_table(results60_2)
results60_04_m <- make_MCSE_table(results60_2)



