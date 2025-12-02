# prepare data for TD viz

#here::i_am("code/summary_script.R")

# packages
library(here)
library(dplyr) # for data grouping
# read in results file
filename <- "simulation_results_2025-11-267544.csv"
res <- read.csv(file = here("results", filename), 
                sep = "", 
                header = TRUE,
                stringsAsFactors = TRUE)
#View(res)

# calculate coverage (yes/no) per run
## setting true value per run
res$true <- NA
res$true[res$parameter == "gamma10"] <- 0.3
res$true[res$parameter == "gamma01"] <- res$gamma01[res$parameter == "gamma01"]
## look up if CI contains true value
res$cov <- (res$ci_l <= res$true) & (res$ci_u >= res$true)
res$bias <- res$true - res$est

res <- res[res$ID == 1 & res$parameter == "gamma01",]
res <- res %>%
  mutate(
    R = recode(method,
               "CD"   = 0.2,
               "LD"   = 1.0,
               "MI-R" = 0.0,
               "MI-a" = 0.0,
               "bayes"= 0.2,
               .default = 0.5),
    G = recode(method,
               "CD"   = 0.2,
               "LD"   = 0.0,
               "MI-R" = 1.0,
               "MI-a" = 1.0,
               "bayes"= 0.5,
               .default = 0.5),
    B = recode(method,
               "CD"   = 1.0,
               "LD"   = 0.0,
               "MI-R" = 0.5,
               "MI-a" = 1.0,
               "bayes"= 0.5,
               .default = 0.5)
  )
res$method <- as.numeric(factor(res$method, levels = c("CD", "LD", "MI-R", "MI-a", "bayes")))


write.csv(res, 
            file = here("results", "TD_test.csv"))
