plot_sd <- function(results, info_table,
                    param   = "gamma01",
                    methods = c("CD", "LD", "MI-R", "bayes"),
                    y_limits = c(0.1, 0.32),
                    sort_by = c("N2", "ICC", "beta", "gamma01")) {
  
  library(dplyr)
  library(ggplot2)
  
  # --- 1) Reproduzierbare Reihenfolge der Conditions (x-Achse) ---
  # `sort_by` definiert die Sortierlogik; wir mappen nur die Spalten, die existieren.
  sort_by <- intersect(sort_by, names(results))
  
  id_levels <- results %>%
    filter(parameter == param) %>%
    distinct(ID, across(all_of(sort_by))) %>%
    arrange(across(all_of(sort_by))) %>%
    pull(ID) %>%
    as.character()
  
  # --- 2) Labels aus info_table robust bauen (named mapping) ---
  labels_vec <- apply(info_table, 2, function(x) paste(x, collapse = "\n"))
  names(labels_vec) <- colnames(info_table)
  
  # sicherstellen, dass Reihenfolge + Länge passt (nur IDs, die wir wirklich plotten)
  labels_vec <- labels_vec[id_levels]
  
  # --- 3) Plot-Dataframe ---
  df <- results %>%
    filter(parameter == param, method %in% methods) %>%
    mutate(
      ID = factor(as.character(ID), levels = id_levels),
      method = factor(method, levels = methods),
      lower = empSE - 2 * mcse_empSE,
      upper = empSE + 2 * mcse_empSE
    )
  
  # --- 4) Plot ---
  ggplot(df, aes(x = ID, y = empSE, color = method, group = method)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
    scale_x_discrete(drop = FALSE, labels = labels_vec) +
    scale_y_continuous(limits = y_limits) +
    labs(
      title = bquote("Empirical standard deviation of estimates for" ~ gamma["01"]),
      x = "Condition",
      y = "EmpSE (± 2×MCSE)",
      color = "Method"
    ) +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
          legend.position = "none")
}