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

# read in results
results <- read.table(file = here("results", "summarized_simulation-results.csv"))


# greek letters for gammas
results$parameter <- ifelse(
  results$parameter == "gamma01", "$\\gamma_{01}$",
  "$\\gamma_{10}$"
)

# create tables for tables
results15_00 <- results %>% filter(N2 == 15 & gamma01 == 0.0)
results15_00 <- make_wide_table(results15_00)[,-1]

results15_04 <- results %>% filter(N2 == 15 & gamma01 == 0.4)
results15_04 <- make_wide_table(results15_04)[,-1]

results30_00<- results %>% filter(N2 == 30 & gamma01 == 0.0 )
results30_00 <- make_wide_table(results30_00)[,-1]

results30_04 <- results %>% filter(N2 == 30 & gamma01 == 0.4)
results30_04 <- make_wide_table(results30_04)[,-1]

results60_00 <- results %>% filter(N2 == 60 & gamma01 == 0.0)
results60_00 <- make_wide_table(results60_00)[,-1]

results60_04 <- results %>% filter(N2 == 60 & gamma01 == 0.4)
results60_04 <- make_wide_table(results60_04)[,-1]

