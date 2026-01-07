## make table to paste into x-axis for graphs ##

make_condition_table <- function(results, N2_filter, param = "gamma01") {
  library(dplyr)
  
  # Filter für N2 
  info <- results %>%
    filter(N2 == N2_filter, parameter == param) %>%
    distinct(ID, gamma01, ICC, beta)
  
  # Tabelle transponieren
  info_long <- info %>%
    select(-ID) %>%
    t() %>%
    as.data.frame()
  
  # Spaltennamen = ID
  colnames(info_long) <- info$ID
  
  # Zeilenbeschriftungen ES / ICC / MM
  rownames(info_long) <- c("ES", "ICC", "MM")
  
  # MCAR / MAR ersetzen
  info_long["MM", ] <- ifelse(info_long["MM", ] == 0, "MCAR", "MAR")
  
  # Erste Spalte beschriften
  info_long["ES",1] <- paste0("ES = ", info_long["ES",1])
  info_long["ICC",1] <- paste0("ICC = ", info_long["ICC",1])
  
  return(info_long)
}
