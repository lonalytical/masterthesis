########### Coverage plots for MI-R and MI-a ###########

here::i_am("code/cov-plot_script.R")

# packages
library(here)
library(dplyr) # for data grouping
library(tidyr)
library(knitr)
library(ggplot2)


# read in function for wide tables
source(file = here("code", "functions", "make-wide-table_function.R"))

# read in results
results <- read.table(file = here("results", "summarized_simulation-results.csv"))

# prepare row information of conditions for looking up
block_info <- results %>%
  distinct(ID, parameter, gamma01, ICC, beta)

# filter cases with MI-R and MI-a
results_cov <- results %>% filter(parameter == "gamma01", (method == "MI-a" | method == "MI-R"))
results_cov <- results_cov[,c("ID", "method", "ICC", "beta", "gamma01", "coverage", "mcse_cov")]

results_cov <- results_cov %>% # add MCSE-intervals
  mutate(
    lower = - 2 * mcse_cov,
    upper = 2 * mcse_cov,
    covdev = coverage - 0.95,
    method = factor(method, 
                    levels = c("MI-R", "MI-a")),
    ID = factor(ID)
  )


# Plot
ggplot(results_cov, aes(x = ID, y = covdev, fill = method)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  labs(
    x = "Bedingung",
    y = "Abweichung der Coverage von 95% (mit 4*MCSE -Balken)",
    fill = "Methode"
  ) +
  coord_cartesian(ylim = c(-0.05, 0.05)) +
  theme_minimal(base_size = 14)

