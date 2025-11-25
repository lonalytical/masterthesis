# MULTIPLE IMPUTATION JOINT MODELING -------------------------------
# declare location of script
here::i_am("code/impute_data_script.R")

# load packages
library(here) # for here() function
library(jomo) # multilevel JM
library(mitml) # tools for multiple imputation and interface for jomo
library(lme4) # for fitting multilevel model to data

# read in dataset
load(here("data", "MARtest.Rdata"))

# subset with only variables Y, X, W and group
data <- MARtest[,c("Y", "x", "w", "group")]


# specify imputation model
fml <- list(Y + x ~ 1 + (1|group),
            w ~ 1)

# impute data ---------------------------
imp <- mitml::jomoImpute(data=data, formula=fml, n.burn=5000, n.iter=200, m=10)

# inspect diagnostics
summary(imp, autocorrelation = TRUE)
plot.mitml(imp)


# prepare imputations for data analysis -----------------

# calculate X_a again for each dataset and append to datasets
impList <- mitmlComplete(imp)
cm <- with(impList, clusterMeans(x, group))

for (i in seq_along(impList)) {
  impList[[i]]$x_a <- cm[[i]]
}

# group-mean-center x
for (i in seq_along(impList)) {
  impList[[i]]$x_c <- impList[[i]]$x - impList[[i]]$x_a
}
  
  
# data analysis ------------------------
# fit focal analysis model in each dataset
fit <- with(impList, lmer(Y ~ 1 + x_c + x_a + (1|group), REML=TRUE))



# pooling data with rubins rules
testEstimates(fit)

# pooling data with conservative dfs
testEstimates(fit, df = nlevels(impList[[1]]$group)-2)
