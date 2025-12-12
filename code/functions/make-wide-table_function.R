# transform results to wide format
make_wide_table <- function(data, methods_order = c("CD","LD","MI-R","MI-a","bayes")) {
  
  stats <- c("bias","coverage","empSE")
  new_order <- unlist(lapply(methods_order, function(m) paste0(m, "_", stats)))
  
  wide_table <- data %>%
    pivot_wider(
      id_cols = c(ID, parameter),
      names_from = method,
      values_from = c(bias, coverage, empSE),
      names_glue = "{method}_{.value}"
    ) %>%
    select(ID, parameter, all_of(new_order))
  
  return(wide_table)
}