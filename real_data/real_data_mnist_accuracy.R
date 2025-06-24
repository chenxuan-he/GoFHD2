# codes to get the prediction accuracy of mnist dataset prediction based on 100 replications
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
  make_option("--procedure", type = "integer", default = 1, help = "Types of regressing procedures, including lasso, scad, svr, xgboost, rf, and fnn.")
)

set.seed(0)
load("real_data_mnist.RData")

n <- dim(data)[1]
formula <- y ~ .

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)
# Print the arguments
nrep = opt$nrep
ratio = opt$ratio
cores = opt$cores
procedure = opt$procedure

if(procedure == 1){
  testModel <- TEST_XGBOOST(formula = formula, binary = TRUE)
}else if (procedure == 2){
  testModel <- TEST_FNN(formula = formula, units = c(1), binary = TRUE, activation = "relu")
}else if (procedure == 3){
  testModel <- TEST_FNN(formula = formula, units = c(1), binary = TRUE, activation = "sigmoid")
}else if (procedure == 4){
  testModel <- TEST_FNN(formula = formula, units = c(64, 16), binary = TRUE, activation = "relu")
}else if (procedure == 5){
  testModel <- TEST_FNN(formula = formula, units = c(128, 64, 16), binary = TRUE, activation = "relu")
}else if (procedure == 6){
  testModel <- TEST_FNN(formula = formula, units = c(128, 64, 16), binary = TRUE, activation = "sigmoid")
}

trainIn <- sample(c(1:n), n * ratio)
datT <- data.frame(data)[trainIn, ]
datE <- data.frame(data)[-trainIn, ]
# Fit the models
testMod <- testModel(Train.data = datT, Validation.data = datE, calCov = FALSE)
accuracy_e <- mean(datE$y == round(testMod$predE))
accuracy_r <- mean(datT$y == round(testMod$predT))

numCores = detectCores()
cl <- makeCluster(min(numCores, cores))
registerDoSNOW(cl)

results <- foreach(ii = 1:nrep, .packages = c("dplyr", "keras")) %dopar% 
  {
    set.seed(ii)
    trainIn <- sample(c(1:n), n * ratio)
    datT <- data.frame(data)[trainIn, ]
    datE <- data.frame(data)[-trainIn, ]
    # Fit the models
    testMod <- testModel(Train.data = datT, Validation.data = datE, calCov = FALSE)
    # Calculate mean squared errors
    accuracy_e <- mean(datE$y == round(testMod$predE))
    accuracy_r <- mean(datT$y == round(testMod$predT))
    return(list(accuracy_e = accuracy_e, accuracy_r = accuracy_r))
  }

Ae1 <- matrix(ncol = 2, nrow = nrep)
Ar1 <- matrix(ncol = 2, nrow = nrep)
for (i in 1:nrep) {
  Ae1[i, ] <- results[[i]]$accuracy_e
  Ar1[i, ] <- results[[i]]$accuracy_r
}

stopCluster(cl)

save(results, file = paste0("results/real_data_mnist_accuracy_",ratio,"_", procedure, ".Rdata"))
