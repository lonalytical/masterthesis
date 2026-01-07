### function for filtering bias data and add MCSE ranges ###

make_bias_data <- function(results, N2_filter, param = "gamma01",
                           method_levels = c("CD", "LD", "MI-R", "MI-a", "bayes")) {
  library(dplyr)
  
  results_filtered <- results %>%
    filter(N2 == N2_filter, parameter == param) %>%
    select(ID, method, ICC, beta, gamma01, bias, mcse_bias) %>%
    mutate(
      lower = bias - 2 * mcse_bias,
      upper = bias + 2 * mcse_bias,
      method = factor(method, levels = method_levels),
      ID = factor(ID)
    ) %>%
    arrange(ID, method)
  
  return(results_filtered)
}