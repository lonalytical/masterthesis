## make table to paste into x-axis for graphs ##
condition_table_all <- function(results, param = "gamma01") {
  library(dplyr)
  
  info <- results %>%
    filter(parameter == param) %>%
    arrange(N2, ICC, beta, !!rlang::sym(param)) %>%  # stabil
    distinct(ID, gamma01, ICC, beta, .keep_all = FALSE)
  
  # transpose
  info_long <- info %>%
    select(gamma01, ICC, beta) %>%   # explizit, damit Reihenfolge stimmt
    t() %>%
    as.data.frame()
  
  colnames(info_long) <- as.character(info$ID)
  rownames(info_long) <- c("ES", "ICC", "MM")
  
  # MCAR / MAR ersetzen (falls beta 0/1 kodiert)
  info_long["MM", ] <- ifelse(info_long["MM", ] == 0, "MCAR", "MAR")
  
  # Erste Spalte beschriften 
  info_long["ES",1] <- paste0("ES = ", info_long["ES",1]) 
  info_long["ICC",1] <- paste0("ICC = ", info_long["ICC",1])
  
  info_long
}
