# transform results to wide format
make_MCSE_table <- function(data, methods_order = c("CD","LD","MI-R","MI-a","bayes")) {
  
  stats <- c("mcse_bias","mcse_cov","mcse_empSE","mcse_ciw")
  new_order <- unlist(lapply(methods_order, function(m) paste0(m, "_", stats)))
  
  wide_table <- data %>%
    pivot_wider(
      id_cols = c(ID, parameter),
      names_from = method,
      values_from = c(mcse_bias, mcse_cov, mcse_empSE, mcse_ciw),
      names_glue = "{method}_{.value}"
    ) %>%
    select(parameter, all_of(new_order))
  
  return(wide_table)
}