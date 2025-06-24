# This is the code for the real data analysis for cortisol stress reactivity dataset following the paper
# Guo, X., Li, R., Liu, J. & Zeng, M. High-dimensional mediation analysis for selecting DNA methylation loci mediating childhood trauma and cortisol stress reactivity. Journal of the American Statistical Association 117, 1110–1121 (2022).
# First, we perform data preprocessing following Guo et al. (2022)
# Download data `E-GEOD-77445.processed.1.zip` (<https://www.ebi.ac.uk/arrayexpress/files/E-GEOD-77445/E-GEOD-77445.processed.1.zip>) as well as `E-GEOD-77445.processed.2.zip` (<https://www.ebi.ac.uk/arrayexpress/files/E-GEOD-77445/E-GEOD-77445.processed.2.zip>). Unzip these two zip files in the `real_data_cortisol/data` folder.
rm(list = ls())
library(dplyr)
library(keras)
source("functions.R")
source("real_data_cortisol/utils_mediation.R")
RawDataPath <- LoadRawData(InputPath = "real_data_cortisol/data/")
topPotential <- 1000 
cleanedData <- MarginalScreening(RawDataPath, topPotential = topPotential)
load(cleanedData)

# Now, we start our testing procedure
dat <- cbind(y,X,S[,-1],M)
# normalize all the cell proportion
prin1 <- dat[,7]
dat2 <- cbind(prin1,dat[,c(2:4,11:(topPotential+18))])
dat2X <- dat[,c(2:4,11:(topPotential+18))]
colnames(dat2)[1] <- "Immune"
set.seed(0)
n <- 85
ne <- 15
nsplits <- 10

formula <- Immune ~ .
testModel <- TEST_LASSO(formula = formula)
testRes1 <- ccovTest(testModel = testModel, nsplits = nsplits, ne = ne, data = data.frame(dat2))

testModel <- TEST_SCAD(formula = formula)
testRes2 <- ccovTest(testModel = testModel, nsplits = nsplits, ne = ne, data = data.frame(dat2))

testModel <- TEST_SVM(formula = formula)
testRes3 <- ccovTest(testModel = testModel, nsplits = nsplits, ne = ne, data = data.frame(dat2))

testModel <- TEST_RF(formula = formula)
testRes4 <- ccovTest(testModel = testModel, nsplits = nsplits, ne = ne, data = data.frame(dat2))

testModel <- TEST_XGBOOST(formula = formula)
testRes5 <- ccovTest(testModel = testModel, nsplits = nsplits, ne = ne, data = data.frame(dat2))

testModel <- TEST_FNN(formula = formula)
testRes6 <- ccovTest(testModel = testModel, nsplits = nsplits, ne = ne, data = data.frame(dat2))

testRes1$CauchyP
testRes2$CauchyP
testRes3$CauchyP
testRes4$CauchyP
testRes5$CauchyP
testRes6$CauchyP

# Now, we obtain the MSE of the regression task based on 100 replications
set.seed(1)
nrep <- 500
formula <- Immune ~ .
testModel1 <- TEST_LASSO(formula = formula)
testModel2 <- TEST_SCAD(formula = formula)
testModel3 <- TEST_SVM(formula = formula)
testModel4 <- TEST_RF(formula = formula)
testModel5 <- TEST_XGBOOST(formula = formula)
testModel6 <- TEST_FNN(formula = formula)

MSEe <- matrix(ncol = 7, nrow = nrep)
MSEr <- matrix(ncol = 7, nrow = nrep)

for (i in c(1:nrep)){
  # randomly generate the indices for training set observations
  trainIn <- sample(c(1 : 85), 70)
  #split the data
  datT <- data.frame(dat2)[trainIn, ]
  datE <- data.frame(dat2)[-trainIn, ]
  
  # fit the model to test by training data
  testMod1 <- testModel1(Train.data = datT, Validation.data = datE)
  testMod2 <- testModel2(Train.data = datT, Validation.data = datE)
  testMod3 <- testModel3(Train.data = datT, Validation.data = datE)
  testMod4 <- testModel4(Train.data = datT, Validation.data = datE)
  testMod5 <- testModel5(Train.data = datT, Validation.data = datE)
  testMod6 <- testModel6(Train.data = datT, Validation.data = datE)
  # store mean squared error
  MSEe[i,1] <- mean((datE$Immune -testMod1$predE)^2)
  MSEr[i,1] <- mean((datT$Immune -testMod1$predT)^2)
  MSEe[i,2] <- mean((datE$Immune -testMod2$predE)^2)
  MSEr[i,2] <- mean((datT$Immune -testMod2$predT)^2)
  MSEe[i,3] <- mean((datE$Immune -testMod3$predE)^2)
  MSEr[i,3] <- mean((datT$Immune -testMod3$predT)^2)
  MSEe[i,4] <- mean((datE$Immune -testMod4$predE)^2)
  MSEr[i,4] <- mean((datT$Immune -testMod4$predT)^2)
  MSEe[i,5] <- mean((datE$Immune -testMod5$predE)^2)
  MSEr[i,5] <- mean((datT$Immune -testMod5$predT)^2)
  MSEe[i,6] <- mean((datE$Immune -testMod6$predE)^2)
  MSEr[i,6] <- mean((datT$Immune -testMod6$predT)^2)
  
  MSEe[i,7] <- mean((datE$Immune - mean(datE$Immune))^2)
  MSEr[i,7] <- mean((datT$Immune - mean(datT$Immune))^2)
}

colMeans(MSEe)*1000
colMeans(MSEr)*1000