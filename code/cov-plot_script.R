########### Coverage plots for MI-R and MI-a ###########

here::i_am("code/cov-plot_script.R")

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

source(file = here("code", "functions", "coverage-plot_function.R"))

# prepare row information of conditions for looking up
block_info <- results %>%
  distinct(ID, parameter, gamma01, ICC, beta)

# make plots with only MI-R and MI-a
plot_cov15_MI <- plot_cov(results, info15l, N2_filter = 15,
                          methods = c("MI-R", "MI-a"))

plot_cov30_MI <- plot_cov(results, info30l, N2_filter = 30,
                          methods = c("MI-R", "MI-a"))


plot_cov60_MI <- plot_cov(results, info60l, N2_filter = 60,
                          methods = c("MI-R", "MI-a"))


plot_cov15_all <- plot_cov(results, info15l, N2_filter = 15,
                           methods = c("CD", "LD", "MI-R", "MI-a", "bayes"))

plot_cov30_all <- plot_cov(results, info30l, N2_filter = 30,
                           methods = c("CD", "LD", "MI-R", "MI-a", "bayes"))

plot_cov60_all <- plot_cov(results, info60l, N2_filter = 60,
                           methods = c("CD", "LD", "MI-R", "MI-a", "bayes"))

###########################FÜR GAMMA10######################


plot_cov15_all <- plot_cov(results, info15l, param = "gamma10", N2_filter = 15,
                           methods = c("CD", "LD", "MI-R", "MI-a", "bayes"))

plot_cov30_all <- plot_cov(results, info30l, param = "gamma10", N2_filter = 30,
                           methods = c("CD", "LD", "MI-R", "MI-a", "bayes"))

plot_cov60_all <- plot_cov(results, info60l, param = "gamma10", N2_filter = 60,
                           methods = c("CD", "LD", "MI-R", "MI-a", "bayes"))
