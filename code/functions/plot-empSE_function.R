### function to plot coverage deviation ##

plot_sd <- function(results, info_table, N2_filter, 
                     methods = c("CD", "LD", "MI-R", "MI-a", "bayes")) {
  library(dplyr)
  library(ggplot2)
  
  # Filter Daten
  df <- results %>%
    filter(parameter == "gamma01", N2 == N2_filter, method %in% methods) %>%
    select(ID, method, ICC, beta, gamma01, empSE, mcse_empSE) %>%
    mutate(
      lower = empSE - 2 * mcse_empSE,
      upper = empSE + 2 * mcse_empSE,
      method = factor(method, levels = methods),
      ID = factor(ID)
    ) %>%
    arrange(ID, method)
  
  # x-Achsen-Labels aus Info-Tabelle
  labels_vec <- apply(info_table, 2, function(x) paste(x, collapse = "\n"))
  
  # Plot
  p <- ggplot(df, aes(x = ID, y = empSE, color = method, group = method)) +
    geom_line(linewidth = 1) +                        # Linien verbinden gleiche Methode
    geom_point(size = 2) +                        # Punkte für jede Bedingung
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  width = 0.1) +                  # Errorbars
    scale_x_discrete(labels = labels_vec) +      # mehrzeilige x-Achsenlabels
    labs(
      title = paste0("Empirische Standardabweichung der Schätzwerte, N = ", N2_filter),
      x = "Bedingung",
      y = "empSE (mit ±2*MCSE)",
      color = "Methode"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(vjust = 0.5, hjust = 0.5))
  
  
  
  return(p)
}
