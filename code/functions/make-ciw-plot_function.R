### function to plot CI width ###

plot_ci_width <- function(results) {
  
  # Filter Daten
  df <- results %>%
    filter(parameter == "gamma01",
           gamma01 == 0.4,
           ICC == 0.1,
           beta == 0.3) %>%
    select(ID, N2, method, ICC, beta, gamma01, ciw, mcse_ciw) %>%
    mutate(
      MM = ifelse(beta == 0, "MCAR", "MAR"),
      lower = ciw - 2 * mcse_ciw,
      upper =  ciw + 2 * mcse_ciw,
      method = factor(method, levels = c("CD", "LD", "MI-R", "MI-a", "bayes")),
      ID = factor(ID)
    ) %>%
    arrange(ID, method)
  
  
  ciw_plot <- ggplot(df, aes(x = method, y = ciw, fill = method)) +
    geom_col(width = 0.7)+
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
    labs(
      x = "",
      y = "CI Width (± 2×MCSE)",
      fill = "Method"
    ) +
    facet_grid(~N2, labeller = labeller(
      N2 = \(x) paste0("N2 = ", x))) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank())+
    scale_fill_discrete(
      name = "Estimation method",
      labels = c("Complete data", "Listwise deletion", "MI-Rubin", "MI-adjusted", "Bayesian")
    )

  return(ciw_plot)
}
