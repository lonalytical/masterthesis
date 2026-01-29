########### Data preparation for results table ###########

here::i_am("code/bias-plot_script.R")

# packages
library(here)
library(dplyr) # for data grouping
library(tidyr)
library(knitr)
library(ggplot2)
library(ggtext)

# read in functions
source(file = here("code", "functions", "condition-table_function.R"))
source(file = here("code", "functions", "make-bias-data_function.R"))

# read in results and create tables
results <- read.table(file = here("results", "summarized_simulation-results.csv"))
info15l <- make_condition_table(results, N2_filter = 15)
info30l <- make_condition_table(results, N2_filter = 30)
info60l <- make_condition_table(results, N2_filter = 60)

results_bias15 <- make_bias_data(results, N2_filter = 15)
results_bias30 <- make_bias_data(results, N2_filter = 30)
results_bias60 <- make_bias_data(results, N2_filter = 60)

# prepare row information of conditions for looking up
block_info <- results %>%
  distinct(ID, gamma01, ICC, beta)

# Plot für N2 = 15
plotbias15 <- ggplot(results_bias15, aes(x = ID, y = bias, fill = method)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  labs(
    title = "$N_2 = 15$",
    x = "",
    y = "Bias (with ±2*MCSE)",
    fill = "Method"
  ) +
  scale_x_discrete(labels="")+
  scale_y_continuous(limits = c(-0.10, 0.05)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        title = element_markdown())


# Plot für N2 = 30
plotbias30 <- ggplot(results_bias30, aes(x = ID, y = bias, fill = method)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  labs(
    title = "$$N_2 = 30$$",
    x = "Bedingung",
    y = "Bias (mit ±2*MCSE)"
  ) +
  scale_x_discrete(labels="")+
  scale_y_continuous(limits = c(-0.10, 0.05)) +
  theme(legend.position = "none", 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_markdown())

# Plot für N2 = 60
plotbias60 <- ggplot(results_bias60, aes(x = ID, y = bias, fill = method)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  labs(
    title = "$$N_2 = 60$$",
    x = "Condition",
    y = "Bias (mit ±2*MCSE)"
  ) +
  scale_x_discrete(labels = apply(info60l, 2, function(x) paste(x, collapse = "\n"))) +
  scale_y_continuous(limits = c(-0.10, 0.05)) +
  theme(legend.position = "none",
        plot.title = element_markdown())
# 
# ##############################BIAS GAMMA10#######################
# results_bias15g10 <- make_bias_data(results, param = "gamma10", N2_filter = 15)
# results_bias30g10 <- make_bias_data(results, param = "gamma10", N2_filter = 30)
# results_bias60g10 <- make_bias_data(results, param = "gamma10", N2_filter = 60)
# 
# 
# # Plot für N2 = 15
# plotbias15 <- ggplot(results_bias15g10, aes(x = ID, y = bias, fill = method)) +
#   geom_col(position = position_dodge(width = 0.8)) +
#   geom_errorbar(
#     aes(ymin = lower, ymax = upper),
#     position = position_dodge(width = 0.8),
#     width = 0.2
#   ) +
#   labs(
#     title = "Bias bei N2 = 15",
#     x = "Bedingung",
#     y = "Bias (mit ±2*MCSE)",
#     fill = "Methode"
#   ) +
#   scale_x_discrete(labels = apply(info15l, 2, function(x) paste(x, collapse = "\n"))) +
#   scale_y_continuous(limits = c(-0.10, 0.05)) +
#   theme_minimal(base_size = 10)
# 
# 
# # Plot für N2 = 30
# plotbias30 <- ggplot(results_bias30g10, aes(x = ID, y = bias, fill = method)) +
#   geom_col(position = position_dodge(width = 0.8)) +
#   geom_errorbar(
#     aes(ymin = lower, ymax = upper),
#     position = position_dodge(width = 0.8),
#     width = 0.2
#   ) +
#   labs(
#     title = "Bias with N2 = 30",
#     x = "Condition",
#     y = "Bias (with ±2*MCSE)"
#   ) +
#   scale_x_discrete(labels = apply(info30l, 2, function(x) paste(x, collapse = "\n"))) +
#   scale_y_continuous(limits = c(-0.10, 0.05)) +
#   theme_minimal(base_size = 10)
# 
# # Plot für N2 = 60
# plotbias60 <- ggplot(results_bias60g10, aes(x = ID, y = bias, fill = method)) +
#   geom_col(position = position_dodge(width = 0.8)) +
#   geom_errorbar(
#     aes(ymin = lower, ymax = upper),
#     position = position_dodge(width = 0.8),
#     width = 0.2
#   ) +
#   labs(
#     title = "Bias bei N2 = 60",
#     x = "Bedingung",
#     y = "Bias (mit ±2*MCSE)",
#   ) +
#   scale_x_discrete(labels = apply(info60l, 2, function(x) paste(x, collapse = "\n"))) +
#   scale_y_continuous(limits = c(-0.10, 0.05)) +
#   theme_minimal(base_size = 10)
# 
