# codes to get the mse of wine quality dataset based on 100 replications
rm(list = ls())
source("functions.R")
library(parallel)
library(doSNOW)
library(optparse)
library(dplyr)
library(keras)
option_list <- list(
  make_option("--nrep", type = "integer", default = 2, help = "Number of repetition"),
  make_option("--ratio", type = "double", default = .5, help = "Splitting ratios for training and test, here we choose .5, .75, and .9."),
  make_option("--cores", type = "integer", default = 20, help = "Cores used for parallel computing"),
  make_option("--procedure", type = "character", default = "fnn", help = "Types of regressing procedures, including lasso, scad, svr, xgboost, rf, and fnn.")
)

whiteWine <- read.csv("real_data_wine/winequality-white.csv", sep = ";")
n <- dim(whiteWine)[1]
augment <- matrix(rnorm(500*n), ncol = 500)
whiteWine <- cbind(whiteWine, augment)
whiteWine <- data.frame(whiteWine[c("quality", setdiff(names(whiteWine), "quality"))])

formula <- quality ~ .

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)
# Print the arguments
nrep = opt$nrep
ratio = opt$ratio
cores = opt$cores
procedure = opt$procedure

if(procedure == "lasso"){
  testModel <- TEST_LASSO(formula = formula)
}else if (procedure == "scad"){
  testModel <- TEST_SCAD(formula = formula)
}else if (procedure == "svr"){
  testModel <- TEST_SVM(formula = formula)
}else if (procedure == "rf"){
  testModel <- TEST_RF(formula = formula)
}else if (procedure == "xgboost"){
  testModel <- TEST_XGBOOST(formula = formula)
}else if (procedure == "fnn"){
  testModel <- TEST_FNN(formula = formula, units = c(128, 64, 16))
}

trainIn <- sample(c(1:n), n * ratio)
datT <- data.frame(whiteWine)[trainIn, ]
datE <- data.frame(whiteWine)[-trainIn, ]
# Fit the models
testMod <- testModel(Train.data = datT, Validation.data = datE, calCov = FALSE)
# Calculate mean squared errors
mse_e <- c(mean((datE$quality - testMod$predE)^2), mean((datE$quality - mean(datE$quality))^2))
mse_r <- c(mean((datT$quality - testMod$predT)^2), mean((datT$quality - mean(datT$quality))^2))

numCores = detectCores()
cl <- makeCluster(min(numCores, cores))
registerDoSNOW(cl)

results <- foreach(ii = 1:nrep, .packages = c("dplyr", "keras")) %dopar% 
  {
      set.seed(ii)
      trainIn <- sample(c(1:n), n * ratio)
      datT <- data.frame(whiteWine)[trainIn, ]
      datE <- data.frame(whiteWine)[-trainIn, ]
      # Fit the models
      testMod <- testModel(Train.data = datT, Validation.data = datE, calCov = FALSE)
      # Calculate mean squared errors
      mse_e <- c(mean((datE$quality - testMod$predE)^2), mean((datE$quality - mean(datE$quality))^2))
      mse_r <- c(mean((datT$quality - testMod$predT)^2), mean((datT$quality - mean(datT$quality))^2))
      return(list(mse_e = mse_e, mse_r = mse_r))
  }

MSEe1 <- matrix(ncol = 2, nrow = nrep)
MSEr1 <- matrix(ncol = 2, nrow = nrep)
for (i in 1:nrep) {
  MSEe1[i, ] <- results[[i]]$mse_e
  MSEr1[i, ] <- results[[i]]$mse_r
}

stopCluster(cl)
save.image(file = paste0("results/real_data_wine_",ratio,"_", procedure, ".Rdata"))