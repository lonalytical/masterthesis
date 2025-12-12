# # # # # # # # # # # # # # # # # # # # # #
#                                         #
#                Analysis                 #
#                                         #
# # # # # # # # # # # # # # # # # # # # # #

here::i_am("code/summary_script.R")

# packages
library(here)
library(dplyr) # for data grouping
 
# find results filenames by naming pattern ("^" = "starts with")
# NOTE: if you have generated data more than once, put the older data sets in a different folder
fn <- list.files(path = here("results"), pattern = "^simulation-results")

# read results from files
res.list <- lapply(here("results", fn), read.table, header = TRUE, stringsAsFactors = TRUE)
res <- do.call(rbind, res.list)

#View(res)

# calculate coverage (yes/no) per run
## setting true value per run
res$true <- NA
res$true[res$parameter == "gamma10"] <- 0.2
res$true[res$parameter == "gamma01"] <- res$gamma01[res$parameter == "gamma01"]
## look up if CI contains true value
res$cov <- (res$ci_l <= res$true) & (res$ci_u >= res$true)

# calculate aggregated measures over replications
res_grouped <- res %>%
  group_by(ID, method, parameter, gamma01, ICC, beta, N2) %>% # group replications of same conditions together
  summarise(
    R = n(), # number of rows per group (= number of replications)
    
    # estimates
    estimate = mean(est), # mean of estimates as global estimate for condition
    stand_err = mean(se), # mean of se as global estimate for se
    
    # performance measures
    bias = mean(est) - unique(true), # bias for condition
    rel_bias = ifelse(unique(true) != 0, bias / unique(true), NA), # relative bias for conditions with non-zero effect
    coverage = mean(cov), # mean coverage across repetitions
    empSE = sd(est), # empirical standard deviation of estimations
    #rms_se = sqrt(mean(se^2)), # root mean square of model SEs
    
    # MCSEs
    mcse_bias = sd(est) / sqrt(R), # empirical sd of estimates divided by sqrt of replication number
    mcse_cov = sqrt(coverage*(1-coverage)/R),
    mcse_empSE = empSE / sqrt(2*(R-1)),
    #mcse_rms_se = sqrt(var(se^2)/(4 * R * mean(se^2))), # Morris formula for MCSE of average modSE
    
    .groups = "drop" # erase grouping at the end
    )


# save results file
write.table(res_grouped, 
            file = here("results", "summarized_simulation-results.csv"))

