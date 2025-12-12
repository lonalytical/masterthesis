########### Data preparation for results table ###########

here::i_am("code/bias-plot_script.R")

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

#create tables for bias plots N2= 15
results_bias15 <- results %>% filter(N2 == 15, parameter == "gamma01")
results_bias15 <- results_bias15[,c("ID", "method", "ICC", "beta", "gamma01", "bias", "mcse_bias")]

results_bias15 <- results_bias15 %>% # add MCSE-intervals
  mutate(
    lower = bias - 2 * mcse_bias,
    upper = bias + 2 * mcse_bias,
    method = factor(method, 
                    levels = c("CD", "LD", "MI-R", "MI-a", "bayes")),
    ID = factor(ID)
  )

#create tables for bias plots N2= 30
results_bias30 <- results %>% filter(N2 == 30, parameter == "gamma01")
results_bias30 <- results_bias30[,c("ID", "method", "ICC", "beta", "gamma01", "bias", "mcse_bias")]

results_bias30 <- results_bias30 %>% # add MCSE-intervals
  mutate(
    lower = bias - 2 * mcse_bias,
    upper = bias + 2 * mcse_bias,
    method = factor(method, 
                    levels = c("CD", "LD", "MI-R", "MI-a", "bayes")),
    ID = factor(ID)
  )

#create tables for bias plots N2= 60
results_bias60 <- results %>% filter(N2 == 60, parameter == "gamma01")
results_bias60 <- results_bias60[,c("ID", "method", "ICC", "beta", "gamma01", "bias", "mcse_bias")]

results_bias60 <- results_bias60 %>% # add MCSE-intervals
  mutate(
    lower = bias - 2 * mcse_bias,
    upper = bias + 2 * mcse_bias,
    method = factor(method, 
                    levels = c("CD", "LD", "MI-R", "MI-a", "bayes")),
    ID = factor(ID)
  )

# Plot für N2 = 15
ggplot(results_bias15, aes(x = ID, y = bias, fill = method)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  labs(
    x = "Bedingung",
    y = "Bias (mit MCSE ±2)",
    fill = "Methode"
  ) +
  theme_minimal(base_size = 14)


# Plot für N2 = 30
ggplot(results_bias30, aes(x = ID, y = bias, fill = method)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  labs(
    x = "Bedingung",
    y = "Bias (mit MCSE ±2)",
    fill = "Methode"
  ) +
  theme_minimal(base_size = 14)

# Plot für N2 = 60
ggplot(results_bias60, aes(x = ID, y = bias, fill = method)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  labs(
    x = "Bedingung",
    y = "Bias (mit MCSE ±2)",
    fill = "Methode"
  ) +
  theme_minimal(base_size = 14)
