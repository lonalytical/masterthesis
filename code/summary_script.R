# # # # # # # # # # # # # # # # # # # # # #
#                                         #
#                Analysis                 #
#                                         #
# # # # # # # # # # # # # # # # # # # # # #

here::i_am("code/summary_script.R")

# packages
library(here)

# read in results file
filename <- "simulation_results_2025-11-267544.csv"
res <- read.csv(file = here("results", filename), 
                sep = "", 
                header = TRUE,
                stringsAsFactors = TRUE)
View(res)

# calculate coverage (yes/no) per run
## setting true value per run
res$true <- NA
res$true[res$parameter == "gamma10"] <- 0.3
res$true[res$parameter == "gamma01"] <- res$gamma01[res$parameter == "gamma01"]
## look up if CI contains true value
res$cov <- (res$ci_l <= res$true) & (res$ci_u >= res$true)

# calculate bias per run
res$bias <- (res$true - res$est)

# average results across repetitions
res.agg <- aggregate(
  cbind(est, se, cov, bias) ~ ID + gamma01 + N2 + ICC + beta + method + parameter, 
  # results of repetitions of the same condition/method/parameter will be put together
  data = res,
  FUN = mean # by calculating the mean
)

View(res.agg)

# calculate relative bias for non-zero effect
res.agg$rbias <- NA
res.agg$rbias[res.agg$parameter == "gamma10"] <- res.agg$bias[res.agg$parameter == "gamma10"] / 0.3
res.agg$rbias[(res.agg$parameter == "gamma01") & (res.agg$gamma01 == 0.3)] <- res.agg$bias[(res.agg$parameter == "gamma01") & (res.agg$gamma01 == 0.3)] / 0.3

# save results file
filename_agg <- paste0("aggregated_", filename)
write.table(res.agg, 
            file = here("results", filename_agg))

