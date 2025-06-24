# test the goodness-of-fit for binary classification task and comparison of three tests
rm(list = ls())
source("functions.R")
load("real_data_mnist.RData")
library(parallel)
library(doSNOW)
library(randomForest)
library(xgboost)
library(e1071)
library(dplyr)
library(keras)
library(BAGofT)

nsplits <- 1
n <- dim(data)[1]
formula <- y ~ .

testModel1 <- TEST_XGBOOST(formula = formula, binary = TRUE)
testModel2 <- TEST_FNN(formula = formula, units = c(1), binary = TRUE, activation = "relu")
testModel3 <- TEST_FNN(formula = formula, units = c(1), binary = TRUE, activation = "sigmoid")
testModel4 <- TEST_FNN(formula = formula, units = c(64, 16), binary = TRUE, activation = "relu")
testModel5 <- TEST_FNN(formula = formula, units = c(128, 64, 16), binary = TRUE, activation = "relu")
testModel6 <- TEST_FNN(formula = formula, units = c(128, 64, 16), binary = TRUE, activation = "sigmoid")

prop <- 0.5
testRes1a <- ccovTest(testModel = testModel1, nsplits = nsplits, ratio = prop, data = data)
testRes2a <- ccovTest(testModel = testModel2, nsplits = nsplits, ratio = prop, data = data)
testRes3a <- ccovTest(testModel = testModel3, nsplits = nsplits, ratio = prop, data = data)
testRes4a <- ccovTest(testModel = testModel4, nsplits = nsplits, ratio = prop, data = data)
testRes5a <- ccovTest(testModel = testModel5, nsplits = nsplits, ratio = prop, data = data)
testRes6a <- ccovTest(testModel = testModel6, nsplits = nsplits, ratio = prop, data = data)
testBAG1a <- BAGofT::BAGofT(testModel1, data=data, nsplits = nsplits, nsim=0, ne=n*(1-prop))
testBAG2a <- BAGofT::BAGofT(testModel2, data=data, nsplits = nsplits, nsim=0, ne=n*(1-prop))
testBAG3a <- BAGofT::BAGofT(testModel3, data=data, nsplits = nsplits, nsim=0, ne=n*(1-prop))
testBAG4a <- BAGofT::BAGofT(testModel4, data=data, nsplits = nsplits, nsim=0, ne=n*(1-prop))
testBAG5a <- BAGofT::BAGofT(testModel5, data=data, nsplits = nsplits, nsim=0, ne=n*(1-prop))
testBAG6a <- BAGofT::BAGofT(testModel6, data=data, nsplits = nsplits, nsim=0, ne=n*(1-prop))
testGRASP1a <- GRASP_test(testModel=testModel1, data=data, prop=prop, formula=formula)
testGRASP2a <- GRASP_test(testModel=testModel2, data=data, prop=prop, formula=formula)
testGRASP3a <- GRASP_test(testModel=testModel3, data=data, prop=prop, formula=formula)
testGRASP4a <- GRASP_test(testModel=testModel4, data=data, prop=prop, formula=formula)
testGRASP5a <- GRASP_test(testModel=testModel5, data=data, prop=prop, formula=formula)
testGRASP6a <- GRASP_test(testModel=testModel6, data=data, prop=prop, formula=formula)

testRes1a$CauchyP
testRes2a$CauchyP
testRes3a$CauchyP
testRes4a$CauchyP
testRes5a$CauchyP
testRes6a$CauchyP
testBAG1a$pmean
testBAG2a$pmean
testBAG3a$pmean
testBAG4a$pmean
testBAG5a$pmean
testBAG6a$pmean
testGRASP1a
testGRASP2a
testGRASP3a
testGRASP4a
testGRASP5a
testGRASP6a

prop <- 0.75
testRes1b <- ccovTest(testModel = testModel1, nsplits = nsplits, ratio = prop, data = data)
testRes2b <- ccovTest(testModel = testModel2, nsplits = nsplits, ratio = prop, data = data)
testRes3b <- ccovTest(testModel = testModel3, nsplits = nsplits, ratio = prop, data = data)
testRes4b <- ccovTest(testModel = testModel4, nsplits = nsplits, ratio = prop, data = data)
testRes5b <- ccovTest(testModel = testModel5, nsplits = nsplits, ratio = prop, data = data)
testRes6b <- ccovTest(testModel = testModel6, nsplits = nsplits, ratio = prop, data = data)
testBAG1b <- BAGofT::BAGofT(testModel1, data=data, nsplits = nsplits, nsim=0, ne=n*(1-prop))
testBAG2b <- BAGofT::BAGofT(testModel2, data=data, nsplits = nsplits, nsim=0, ne=n*(1-prop))
testBAG3b <- BAGofT::BAGofT(testModel3, data=data, nsplits = nsplits, nsim=0, ne=n*(1-prop))
testBAG4b <- BAGofT::BAGofT(testModel4, data=data, nsplits = nsplits, nsim=0, ne=n*(1-prop))
testBAG5b <- BAGofT::BAGofT(testModel5, data=data, nsplits = nsplits, nsim=0, ne=n*(1-prop))
testBAG6b <- BAGofT::BAGofT(testModel6, data=data, nsplits = nsplits, nsim=0, ne=n*(1-prop))
testGRASP1b <- GRASP_test(testModel=testModel1, data=data, prop=prop, formula=formula)
testGRASP2b <- GRASP_test(testModel=testModel2, data=data, prop=prop, formula=formula)
testGRASP3b <- GRASP_test(testModel=testModel3, data=data, prop=prop, formula=formula)
testGRASP4b <- GRASP_test(testModel=testModel4, data=data, prop=prop, formula=formula)
testGRASP5b <- GRASP_test(testModel=testModel5, data=data, prop=prop, formula=formula)
testGRASP6b <- GRASP_test(testModel=testModel6, data=data, prop=prop, formula=formula)

testRes1b$CauchyP
testRes2b$CauchyP
testRes3b$CauchyP
testRes4b$CauchyP
testRes5b$CauchyP
testRes6b$CauchyP
testBAG1b$pmean
testBAG2b$pmean
testBAG3b$pmean
testBAG4b$pmean
testBAG5b$pmean
testBAG6b$pmean
testGRASP1b
testGRASP2b
testGRASP3b
testGRASP4b
testGRASP5b
testGRASP6b

prop <- 0.9
testRes1c <- ccovTest(testModel = testModel1, nsplits = nsplits, ratio = prop, data = data)
testRes2c <- ccovTest(testModel = testModel2, nsplits = nsplits, ratio = prop, data = data)
testRes3c <- ccovTest(testModel = testModel3, nsplits = nsplits, ratio = prop, data = data)
testRes4c <- ccovTest(testModel = testModel4, nsplits = nsplits, ratio = prop, data = data)
testRes5c <- ccovTest(testModel = testModel5, nsplits = nsplits, ratio = prop, data = data)
testRes6c <- ccovTest(testModel = testModel6, nsplits = nsplits, ratio = prop, data = data)
testBAG1c <- BAGofT::BAGofT(testModel1, data=data, nsplits = nsplits, nsim=0, ne=n*(1-prop))
testBAG2c <- BAGofT::BAGofT(testModel2, data=data, nsplits = nsplits, nsim=0, ne=n*(1-prop))
testBAG3c <- BAGofT::BAGofT(testModel3, data=data, nsplits = nsplits, nsim=0, ne=n*(1-prop))
testBAG4c <- BAGofT::BAGofT(testModel4, data=data, nsplits = nsplits, nsim=0, ne=n*(1-prop))
testBAG5c <- BAGofT::BAGofT(testModel5, data=data, nsplits = nsplits, nsim=0, ne=n*(1-prop))
testBAG6c <- BAGofT::BAGofT(testModel6, data=data, nsplits = nsplits, nsim=0, ne=n*(1-prop))
testGRASP1c <- GRASP_test(testModel=testModel1, data=data, prop=prop, formula=formula)
testGRASP2c <- GRASP_test(testModel=testModel2, data=data, prop=prop, formula=formula)
testGRASP3c <- GRASP_test(testModel=testModel3, data=data, prop=prop, formula=formula)
testGRASP4c <- GRASP_test(testModel=testModel4, data=data, prop=prop, formula=formula)
testGRASP5c <- GRASP_test(testModel=testModel5, data=data, prop=prop, formula=formula)
testGRASP6c <- GRASP_test(testModel=testModel6, data=data, prop=prop, formula=formula)

testRes1c$CauchyP
testRes2c$CauchyP
testRes3c$CauchyP
testRes4c$CauchyP
testRes5c$CauchyP
testRes6c$CauchyP
testBAG1c$pmean
testBAG2c$pmean
testBAG3c$pmean
testBAG4c$pmean
testBAG5c$pmean
testBAG6c$pmean
testGRASP1c
testGRASP2c
testGRASP3c
testGRASP4c
testGRASP5c
testGRASP6c

