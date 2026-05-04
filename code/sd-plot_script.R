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
  facet_grid(
    ICC ~ N2,
    labeller = label_bquote(
      rows = ICC == .(ICC),
      cols = N[2] == .(N2)
    )
  ) +
  theme_minimal() +
  scale_x_discrete() +
  labs(
    x = "Effect size",
    y = "Empirical SD (± 2×MCSE)",
    fill = "Method"
  ) +
  theme_grey()+
  scale_fill_discrete(
    name = "Estimation method",
    labels = c("Complete data", "Listwise deletion", "Multiple imputation", "Bayesian")
  )
sd_plot
