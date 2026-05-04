########### Bias plot MAR ###########

here::i_am("code/bias-plot_script.R")

# packages
library(here)
library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)
library(ggh4x)

results <- read.table(file = here("results", "summarized_simulation-results.csv"))

biasd <- results %>%
  filter(parameter == "gamma01",
         method %in% c("CD", "LD", "MI-R", "bayes"),
         beta == 0.3) %>%
  mutate(
    lower = bias - 2 * mcse_bias,
    upper = bias + 2 * mcse_bias,
    method = factor(method, levels = c("CD", "LD", "MI-R", "bayes")),
    ID = factor(ID),
    biasr = round(bias, 3)
  )
biasd <- biasd %>%
  mutate(
    N2_lab = paste0("N[2] == ", N2),
    gamma01_lab = paste0("plain(ES) == ", gamma01),
    ICC_lab = paste0("plain(ICC) == ", ICC)
  )

bias_plot <- ggplot(biasd, aes(x = method, y = bias, fill = method)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  facet_nested(
    rows = vars(N2_lab, gamma01_lab),
    cols = vars(ICC_lab),
    labeller = label_parsed
  ) +
  geom_hline(yintercept = 0) +
  labs(
    x = "",
    y = "Bias (± 2×MCSE)",
    fill = "Method"
  ) +
  theme_grey() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.8, "lines"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  ) +
  geom_text(
    aes(label = biasr, y = 0.1),
    size = 2,
    vjust = 1.5
  ) +
  scale_fill_discrete(
    name = "Estimation method",
    labels = c("Complete data", "Listwise deletion", "Multiple imputation", "Bayesian")
  )
########### Bias plot MCAR ###########

biasMCAR <- results %>%
  filter(parameter == "gamma01",
         method %in% c("CD", "LD", "MI-R", "bayes"),
         beta == 0) %>%
  mutate(
    lower = bias - 2 * mcse_bias,
    upper = bias + 2 * mcse_bias,
    method = factor(method, levels = c("CD", "LD", "MI-R", "bayes")),
    ID = factor(ID),
    biasr = round(bias, 3)
  )

bias_plotMCAR <- ggplot(biasMCAR, aes(x = method, y = bias, fill = method)) +
  geom_col(width = 0.7)+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  facet_nested(
    rows = vars(N2, gamma01),
    cols = vars(ICC),
    labeller = labeller(
      N2 = \(x) paste0("N2 = ", x),
      gamma01 = \(x) paste0("ES = ", x),
      ICC = \(x) paste0("ICC = ", x)
    )
  ) +
  geom_hline(yintercept = 0) +
  labs(
    x = "",
    y = "Bias (± 2×MCSE)",
    fill = "Method"
  )+
  theme_grey()+
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.8, "lines"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )+
  geom_text(aes(label = biasr, 
                y = 0.1),
            size = 2, vjust = 1.5,)+
  scale_fill_discrete(
    name = "Estimation method",
    labels = c("Complete data", "Listwise deletion", "Multiple Imputation", "Bayesian")
  )
