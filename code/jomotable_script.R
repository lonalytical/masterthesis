library(here)
jomo<-read.csv2(here("results", "jomo-results.csv"), sep = ";", dec = ".")
library(dplyr)

## first table: only bias reduction
jomoui<-jomo %>%
  filter(method == "MI-R1") %>%
  select(c(N2, beta, bias, rel_bias))

jomowi<-jomo %>%
  filter(method == "MI-R2") %>%
  select(c(N2, beta, bias, rel_bias))

jomotable <- 
  left_join(jomoui, jomowi, by = c("N2", "beta"), suffix = c("ui", "wi")) %>%
  mutate(
    red = (biasui - biaswi) / biasui * 100,
    red = paste0(round(red, digits = 1), "%"),
    across(c(biasui, rel_biasui, biaswi, rel_biaswi), ~ round(.x,3)),
    across(c(rel_biasui, rel_biaswi), ~ paste0((abs(.x) * 100), "%"))
    )

write.table(jomotable, here("results", "jomotable.csv"))

## second table: uncertainty measures
jomoui2<-jomo %>%
  filter(method == "MI-R1") %>%
  select(c(N2, beta, coverage, stand_err, empSE))

jomowi2<-jomo %>%
  filter(method == "MI-R2") %>%
  select(c(N2, beta, coverage, stand_err, empSE))

jomose_table <- left_join(jomoui2, jomowi2, by = c("N2", "beta"), suffix = c("ui", "wi")) %>%
  transmute(
    N2, beta,
    cov_ui = round(coverageui, 3),
    cov_wi = round(coveragewi, 3),
    se_ui = round(stand_errui, 3),
    se_wi = round(stand_errwi, 3),
    empse_ui = round(empSEui, 3),
    empse_wi = round(empSEwi, 3)
  )

write.table(jomose_table, here("results", "jomose_table.csv"))

