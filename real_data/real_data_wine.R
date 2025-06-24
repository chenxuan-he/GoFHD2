# test the goodness-of-fit for regression task
rm(list = ls())
source("functions.R")
library(parallel)
library(doSNOW)
library(randomForest)
library(xgboost)
library(e1071)
library(dplyr)
library(keras)

set.seed(0)
whiteWine <- read.csv("real_data_wine/winequality-white.csv", sep = ";")

n <- dim(whiteWine)[1]
augment <- matrix(rnorm(500*n), ncol = 500)
whiteWine <- cbind(whiteWine, augment)
whiteWine <- data.frame(whiteWine[c("quality", setdiff(names(whiteWine), "quality"))])

nsplits <- 10
formula <- quality ~ .

testModel1 <- TEST_LASSO(formula = formula)
testModel2 <- TEST_SCAD(formula = formula)
testModel3 <- TEST_SVM(formula = formula)
testModel4 <- TEST_RF(formula = formula)
testModel5 <- TEST_XGBOOST(formula = formula)
testModel6 <- TEST_FNN(formula = formula, units = c(128, 64, 16))

prop <- 0.5
testRes1a <- ccovTest(testModel = testModel1, nsplits = nsplits, ratio = prop, data = whiteWine)
testRes2a <- ccovTest(testModel = testModel2, nsplits = nsplits, ratio = prop, data = whiteWine)
testRes3a <- ccovTest(testModel = testModel3, nsplits = nsplits, ratio = prop, data = whiteWine)
testRes4a <- ccovTest(testModel = testModel4, nsplits = nsplits, ratio = prop, data = whiteWine)
testRes5a <- ccovTest(testModel = testModel5, nsplits = nsplits, ratio = prop, data = whiteWine)
testRes6a <- ccovTest(testModel = testModel6, nsplits = nsplits, ratio = prop, data = whiteWine)

prop <- 0.75
testRes1b <- ccovTest(testModel = testModel1, nsplits = nsplits, ratio = prop, data = whiteWine)
testRes2b <- ccovTest(testModel = testModel2, nsplits = nsplits, ratio = prop, data = whiteWine)
testRes3b <- ccovTest(testModel = testModel3, nsplits = nsplits, ratio = prop, data = whiteWine)
testRes4b <- ccovTest(testModel = testModel4, nsplits = nsplits, ratio = prop, data = whiteWine)
testRes5b <- ccovTest(testModel = testModel5, nsplits = nsplits, ratio = prop, data = whiteWine)
testRes6b <- ccovTest(testModel = testModel6, nsplits = nsplits, ratio = prop, data = whiteWine)

prop <- 0.9
testRes1c <- ccovTest(testModel = testModel1, nsplits = nsplits, ratio = prop, data = whiteWine)
testRes2c <- ccovTest(testModel = testModel2, nsplits = nsplits, ratio = prop, data = whiteWine)
testRes3c <- ccovTest(testModel = testModel3, nsplits = nsplits, ratio = prop, data = whiteWine)
testRes4c <- ccovTest(testModel = testModel4, nsplits = nsplits, ratio = prop, data = whiteWine)
testRes5c <- ccovTest(testModel = testModel5, nsplits = nsplits, ratio = prop, data = whiteWine)
testRes6c <- ccovTest(testModel = testModel6, nsplits = nsplits, ratio = prop, data = whiteWine)

testRes1a$CauchyP
testRes2a$CauchyP
testRes3a$CauchyP
testRes4a$CauchyP
testRes5a$CauchyP
testRes6a$CauchyP

testRes1b$CauchyP
testRes2b$CauchyP
testRes3b$CauchyP
testRes4b$CauchyP
testRes5b$CauchyP
testRes6b$CauchyP

testRes1c$CauchyP
testRes2c$CauchyP
testRes3c$CauchyP
testRes4c$CauchyP
testRes5c$CauchyP
testRes6c$CauchyP
