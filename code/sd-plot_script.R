########### Coverage plots for MI-R and MI-a ###########

here::i_am("code/sd-plot_script.R")

# packages
library(here)
library(dplyr) # for data grouping
library(tidyr)
library(knitr)
library(ggplot2)


# read in results and functions
results <- read.table(file = here("results", "summarized_simulation-results.csv"))
info15l <- make_condition_table(results, N2_filter = 15)
info30l <- make_condition_table(results, N2_filter = 30)
info60l <- make_condition_table(results, N2_filter = 60)

source(file = here("code", "functions", "plot-empSE_function.R"))

# prepare row information of conditions for looking up
block_info <- results %>%
  distinct(ID, parameter, gamma01, ICC, beta)

# make plots

plot_se15 <- plot_sd(results, info15l, N2_filter = 15,
                           methods = c("CD", "LD", "MI-R", "MI-a", "bayes"))

plot_se30 <- plot_sd(results, info30l, N2_filter = 30,
                           methods = c("CD", "LD", "MI-R", "MI-a", "bayes"))

plot_se60 <- plot_sd(results, info60l, N2_filter = 60,
                           methods = c("CD", "LD", "MI-R", "MI-a", "bayes"))


