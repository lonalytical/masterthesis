### function to plot coverage deviation ##

plot_cov <- function(results, info_table, N2_filter, 
                                   methods = c("CD", "LD", "MI-R", "MI-a", "bayes")) {
  library(dplyr)
  library(ggplot2)
  
  # Filter Daten
  df <- results %>%
    filter(parameter == "gamma01", N2 == N2_filter, method %in% methods) %>%
    select(ID, method, ICC, beta, gamma01, coverage, mcse_cov) %>%
    mutate(
      covdev = coverage - 0.95,
      lower = - 2 * mcse_cov,
      upper = 2 * mcse_cov,
      method = factor(method, levels = methods),
      ID = factor(ID)
    ) %>%
    arrange(ID, method)
  
  # x-Achsen-Labels aus Info-Tabelle
  labels_vec <- apply(info_table, 2, function(x) paste(x, collapse = "\n"))
  
  # Plot
  p <- ggplot(df, aes(x = ID, y = covdev, fill = method)) +
    geom_col(position = position_dodge(width = 0.8)) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  position = position_dodge(width = 0.8),
                  width = 0.2) +
    scale_x_discrete(labels = labels_vec) +
    labs(
      title = paste0("Coverage der Konfidenzintervalle, N = ", N2_filter),
      x = "Bedingung",
      y = "Abweichung von 95% (mit 4*MCSE)",
      fill = "Methode"
    ) +
    coord_cartesian(ylim = c(-0.025, 0.05)) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(vjust = 0.5, hjust = 0.5))
  
  return(p)
}
