########### Coverage plots for MI-R and MI-a ###########

here::i_am("code/sd-plot_script.R")

# packages
library(here)
library(dplyr) # for data grouping
library(tidyr)
library(knitr)
library(ggplot2)


# read in results
results <- read.table(file = here("results", "summarized_simulation-results.csv"))

methods = c("CD", "LD", "MI-R", "bayes")
df <- results %>%
  filter(parameter == "gamma01", beta == 0.3, method %in% methods) %>%
  mutate(gamma01 = factor(gamma01),
         method  = factor(method, levels = methods),
         lower = empSE - 2 * mcse_empSE,
         upper = empSE + 2 * mcse_empSE)


sd_plot <- ggplot(df, aes(x = gamma01, y = empSE, group = method,
               fill = method)) +
  geom_col(width=0.7, position = position_dodge(width = 0.8))+
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                width = 0.2, 
                position = position_dodge(width = 0.8)) +
  facet_grid(ICC ~ N2, labeller = labeller(
               ICC = function(x) paste0("ICC = ", x),
                N2  = function(x) paste0("N2 = ", x))) +
  theme_minimal() +
  scale_x_discrete() +
  labs(
    title = bquote("Empirical standard deviation of estimates for" ~ gamma["01"] ~ "and MAR"),
    x = expression(gamma["01"]),
    y = "Empirical SD",
    fill = "Method"
  ) +
  theme_grey()
sd_plot
