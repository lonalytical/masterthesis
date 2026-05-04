########### Coverage plots for MI-R and MI-a ###########

here::i_am("code/cov-plot_script.R")
# packages
library(here)
library(dplyr) 
library(tidyr)
library(knitr)
library(ggplot2)

# read in results and functions
results <- read.table(file = here("results", "summarized_simulation-results.csv"))

#filter data and calculate deviation from zero
datf <- results %>%
  filter(N2 == 15,
         parameter == "gamma01") %>%
  select(ID, method, ICC, beta, gamma01, coverage, mcse_cov) %>%
  mutate(
    MM = ifelse(beta == 0, "MCAR", "MAR"),
    covdev = coverage - 0.95,
    lower = covdev - 2 * mcse_cov,
    upper =  covdev + 2 * mcse_cov,
    ID = factor(ID),
    method = factor(method, levels = c("CD", "LD", "MI-R", "MI-a", "bayes"))
  ) %>%
  arrange(ID, method)

cov_plot<- ggplot(datf, aes(x = method, y = covdev, fill = method)) +
  geom_col(width = 0.7)+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  facet_grid(ICC ~ MM + gamma01,
             labeller = labeller(
               ICC = \(x) paste0("ICC = ", x),
               gamma01 = \(x) paste0("ES = ", x),
               MM = label_value
             )) +
  geom_hline(yintercept = 0) +
  labs(
    x = "",
    y = "Deviation from 95% (± 2×MCSE)",
    fill = "Method"
  ) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 9)
        )+
  scale_fill_discrete(
    name = "Estimation method",
    labels = c("Complete data", "Listwise deletion", "MI-Rubin", "MI-adjusted", "Bayesian")
  )
