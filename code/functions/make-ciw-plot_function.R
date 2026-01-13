### function to plot CI width ###

plot_ci_width <- function(results, info_table, N2_filter,
                          param = "gamma01",
                          methods = c("CD", "LD", "MI-R", "MI-a", "bayes")) {
  
  library(dplyr)
  library(ggplot2)
  
  # Filter Daten
  df <- results %>%
    filter(parameter == param,
           N2 == N2_filter,
           method %in% methods) %>%
    select(ID, method, ICC, beta, gamma01, ciw) %>%
    mutate(
      method = factor(method, levels = methods),
      ID = factor(ID)
    ) %>%
    arrange(ID, method)
  
  # x-Achsen-Labels aus Info-Tabelle
  labels_vec <- apply(info_table, 2, function(x) paste(x, collapse = "\n"))
  
  # Plot
  p <- ggplot(df, aes(x = ID, y = ciw, color = method, group = method)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_x_discrete(labels = labels_vec) +
    scale_y_continuous(limits = c(0, 1.6)) +
    labs(
      title = paste0(
        "Breite der Konfidenzintervalle für ",
        param,
        ", N = ",
        N2_filter
      ),
      x = "Bedingung",
      y = "Breite",
      color = "Methode"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(vjust = 0.5, hjust = 0.5))
  
  return(p)
}
