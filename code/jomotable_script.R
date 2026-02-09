library(here)
jomo<-read.csv2(here("results", "jomo-results.csv"), sep = ";", dec = ".")
library(dplyr)

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
