########### CIW PLOTS ###########

here::i_am("code/ciw-plot_script.R")

# packages
library(here)
library(dplyr) # for data grouping
library(tidyr)
library(knitr)
library(ggplot2)


# read in results and functions
results <- read.table(file = here("results", "summarized_simulation-results.csv"))
source(file = here("code", "functions", "make-ciw-plot_function.R"))

ciw_plot <- plot_ci_width(results = results)