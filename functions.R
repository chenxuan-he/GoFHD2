# Function to perform the cumulative covariance test
ccovTest <- function (testModel, data, nsplits = 10, selFun = dcPre(), ne = floor(5 * nrow(data)^(1/2)), ratio=1-ne/nrow(data)) 
{
  # Check for conflicting 'ne' and 'ratio' arguments
  if(!missing(ne) && !missing(ratio)) {
    if(abs(ratio+ne/nrow(data)-1)<=1e-2) warning("specify 'ne' or 'ratio' but not both")
    else stop("specify 'ne' or 'ratio' but not both")
  }else if(missing(ne)){
    ne = floor((1-ratio)*nrow(data))
  } 
  # Perform the test using multiple splits
  testRes <- ccov_multi(testModel = testModel, data = data, nsplits = nsplits, ne = ne, selFun = selFun)
  # Return the test results
  return(invisible(list(pmean = testRes$meanPv, pmedian = testRes$medianPv, CauchyP = testRes$CauchyP,
                        pmin = testRes$minPv, singleSplit.results = testRes$spliDat)))
}

# Function to perform a single split test
ccov_sin <- function(testModel, datset, ne, selFun){
  nr <- nrow(datset)
  # obtain the training set size
  nt <- nr - ne
  # the indices for training set observations
  trainIn <- sample(c(1 : nr), nt)
  #split the data
  datT <- datset[trainIn, ]
  datE <- datset[-trainIn, ]
  # fit the model to test by training data
  testMod <- testModel(Train.data = datT, Validation.data = datE, selFun = selFun)
  # ccov test for the result
  P <- 1-2*abs(0.5 - pnorm(testMod$testn))
  return(list(norm = testMod$testn, p.value = P))
}

# Function to perform the test using multiple splits
ccov_multi <- function(testModel, data,  nsplits, ne, selFun){
  spliDat <- list()
  for (j in c(1:nsplits)){
    spliDat[[j]] <- ccov_sin(testModel = testModel, selFun = selFun, datset = data, ne = ne)
  }
  # Calculate p-values from multiple splits
  pvdat <- unlist(lapply(c(1:nsplits), function(x) spliDat[[x]]$p.value))
  return(list( meanPv = mean(pvdat),
               medianPv = stats::median(pvdat),
               minPv = min(pvdat),
               CauchyP = 0.5-atan(mean(tan((0.5-pvdat)*pi)))/pi,
               spliDat = spliDat )) 
}

# Function to calculate the cumulative covariance E(eps | X)
calCcov <- function(formula, predT, predE, Train.data, Validation.data, selFun = selFun) {
  # retrieve the response and exposure
  Rsp <- as.character(formula)[2]
  Exp <- as.character(formula)[3]
  if (Exp == ".") {
    Xs <- setdiff(colnames(Train.data), Rsp)
  }else if (!stringr::str_detect(Exp, "\\+")){
    Xs <- Exp
  } else {
    Xs <- c(stringr::str_split(Exp, " \\+ ", simplify = TRUE))
  }
  # estimated epsilon on the test set
  epsE <- Validation.data[,Rsp] - predE
  # exposure on the test set
  ExpX <- Validation.data[, Xs]
  # reformulate the dataset on the test set
  dat <- data.frame(e = epsE, ExpX)
  colnames(dat)[1] <- "res"
  # estimated epsilon on the test set
  epsT <- Train.data[,Rsp] - predT
  # exposure on the test set
  ExpT <- Train.data[, Xs]
  # reformulate the dataset on the training set
  datT <- data.frame(e = epsT, ExpT)
  colnames(datT)[1] <- "res"
  # we optionally screen some feature in X out to enhance the power
  selRes <- selFun(datT, parVar = ExpX)
  if (selRes$preSelected) {
    selDat <- dat[, c("res", selRes$parVarNew)]
  }else selDat <- dat
  # calculate the test statistic for testing
  Tnp <- getTnp(selDat, "res")
  Snp <- getSnp(selDat, "res")
  ne <- length(epsE)
  return(sqrt(ne*(ne-1)/2)*Tnp/Snp)
}

# Function to test using glmnet
TEST_LASSO <- function(formula, alpha = 1, binary=FALSE){
  # return a function of train data and test data
  return(function(Train.data, Validation.data, selFun = dcPre(), calCov=TRUE){
    # obtain the response name
    Rsp <- as.character(formula)[2]
    # regressor data 
    XmatT <- model.matrix(formula,  Train.data)[,-1]
    XmatE <- model.matrix(formula,  Validation.data)[,-1]
    # fit lasso regression
    if(!binary){
      lasso_cvlamT <- glmnet :: cv.glmnet(XmatT, Train.data[, Rsp], alpha = alpha)$lambda.min
      lassoModT <- glmnet :: glmnet(XmatT, Train.data[, Rsp], alpha = alpha, lambda = lasso_cvlamT)
    }else{
      lasso_cvlamT <- glmnet :: cv.glmnet(XmatT, Train.data[, Rsp], family = "binomial", alpha = alpha)$lambda.min
      lassoModT <- glmnet :: glmnet(XmatT, Train.data[, Rsp], family = "binomial", alpha=alpha, lambda = lasso_cvlamT)
    }
    # predict on the test set
    predE <- stats :: predict(lassoModT, XmatE, type="response")
    # predict on the training set
    predT <- stats :: predict(lassoModT, XmatT, type="response")
    # calculate the residual
    res <-   (Train.data[, Rsp] -  predT)
    if(calCov){
      testn <- calCcov(formula, predT, predE, Train.data, Validation.data, selFun=selFun)
      return(list(predT = predT, predE = predE, res = res,
                  Rsp = Rsp, testn = testn))
    }else{
      return(list(predT = predT, predE = predE, res = res,
                  Rsp = Rsp))
    }
  })
}

# Function to test using linear model
TEST_LM <- function (formula) 
{
  testModel <- function(Train.data, Validation.data, selFun = dcPre(), calCov=TRUE) {
    modT <- stats::lm(formula, Train.data)
    predT <- stats::predict(modT, newdata = Train.data, 
                            type = "response", se.fit = TRUE)$fit
    predE <- stats::predict(modT, newdata = Validation.data, 
                            type = "response", se.fit = TRUE)$fit
    res <- stats::resid(modT)
    Rsp <- as.character(formula)[2]
    if(calCov){
      testn <- calCcov(formula, predT, predE, Train.data, Validation.data, selFun=selFun)
      return(list(predT = predT, predE = predE, res = res,
                  Rsp = Rsp, testn = testn))
    }else{
      return(list(predT = predT, predE = predE, res = res,
                  Rsp = Rsp))
    }
  }
  return(testModel)
}

# Function to test using generalized linear model
TEST_GLM <- function (formula, link="logit") 
{
  testModel <- function(Train.data, Validation.data, selFun = dcPre(), calCov=TRUE) {
    modT <- stats::glm(formula, Train.data, family = binomial(link = link))
    predT <- stats::predict(modT, newdata = Train.data, 
                            type = "response", se.fit = TRUE)$fit
    predE <- stats::predict(modT, newdata = Validation.data, 
                            type = "response", se.fit = TRUE)$fit
    Rsp <- as.character(formula)[2]
    res <- Train.data[,Rsp] - predT #stats::resid(modT)
    if(calCov){
      testn <- calCcov(formula, predT, predE, Train.data, Validation.data, selFun=selFun)
      return(list(predT = predT, predE = predE, res = res,
                  Rsp = Rsp, testn = testn))
    }else{
      return(list(predT = predT, predE = predE, res = res,
                  Rsp = Rsp))
    }
  }
  return(testModel)
}

# Function to test using scad
TEST_SCAD <- function(formula, alpha = 1, penalty = "SCAD", binary=FALSE){
  # return a function of train data and test data
  return(function(Train.data, Validation.data, selFun = dcPre(), calCov=TRUE){
    # obtain the response name
    Rsp <- as.character(formula)[2]
    # regressor data 
    XmatT <- model.matrix(formula,  Train.data)[,-1] 
    XmatE <- model.matrix(formula,  Validation.data)[,-1] 
    if(!binary){
      # select lambda by cross validation
      lasso_cvlamT <- ncvreg :: cv.ncvreg(XmatT, Train.data[, Rsp], alpha = alpha, penalty=penalty)$lambda.min
      # fit the model after selecting the lambda
      lassoModT <- ncvreg :: ncvreg(XmatT, Train.data[, Rsp], alpha = alpha, penalty = penalty, lambda =lasso_cvlamT)
    }else{
      # select lambda by cross validation
      lasso_cvlamT <- ncvreg :: cv.ncvreg(XmatT, Train.data[, Rsp], family="binomial", alpha = alpha, penalty=penalty)$lambda.min
      # fit the model after selecting the lambda
      lassoModT <- ncvreg :: ncvreg(XmatT, Train.data[, Rsp], family="binomial", alpha = alpha, penalty = penalty, lambda =lasso_cvlamT)
    }
    #predict on the test set
    predE <- stats :: predict(lassoModT, XmatE, type="response")
    #predict on the training set
    predT <- stats :: predict(lassoModT, XmatT, type="response")
    # calculate the residual
    res <-   (Train.data[, Rsp] -  predT)
    if(calCov){
      testn <- calCcov(formula, predT, predE, Train.data, Validation.data, selFun=selFun)
      return(list(predT = predT, predE = predE, res = res,
                  Rsp = Rsp, testn = testn))
    }else{
      return(list(predT = predT, predE = predE, res = res,
                  Rsp = Rsp))
    }
  })
}

# Function to test using neural network
TEST_FNN <- function(formula, Train.data, Validation.data, verbose = 0,
                     units=80, epochs = 10, batch_size = 5, activation="sigmoid", binary = FALSE){
  # return a function of train data and test data
  return(function(Train.data, Validation.data, selFun = dcPre(), calCov=TRUE){
    # obtain the response name
    Rsp <- as.character(formula)[2]
    # regressor data 
    XmatT <- model.matrix(formula,  Train.data)[,-which(colnames(Train.data)==Rsp)] 
    XmatE <- model.matrix(formula,  Validation.data)[,-which(colnames(Train.data)==Rsp)] 
    
    # Determine the activation function for the output layer
    output_activation <- if (binary) "sigmoid" else "linear"
    loss_function <- if (binary) "binary_crossentropy" else "mse"
    
    # fit neural network with 1 hidden layer
    if(length(units) == 1){
      network <- keras :: keras_model_sequential() %>%
        layer_dense(units = units, activation = activation, input_shape = ncol(XmatT)) %>%
        layer_dense(units = 1, activation = output_activation)
    }else{
      network <- keras :: keras_model_sequential()
      for (i in 1:length(units)) {
        if (i == 1){
          network <- network %>% 
            layer_dense(units = units[i], activation = activation, input_shape = ncol(XmatT))
        }else{
          network <- network %>%
            layer_dense(units = units[i], activation = activation)
        }
      }
      network <- network %>% layer_dense(units = 1, activation = output_activation)
    }
    # compile the model
    network %>% keras :: compile(
      optimizer = "rmsprop",
      loss = loss_function,
      metrics = c(loss_function)
    )
    # fit the model
    network %>% keras :: fit(XmatT, Train.data[, Rsp],  epochs = epochs, batch_size = batch_size, verbose = verbose)
    # predict on the test set
    predE <- network %>% keras :: predict_on_batch(XmatE)
    # predict on the training set
    predT <- network %>% keras :: predict_on_batch(XmatT)
    # calculate the Pearson residual
    resT <-   (Train.data[, Rsp] -  predT)
    resE <- (Validation.data[, Rsp] - predE)
    if(calCov){
      testn <- calCcov(formula, predT, predE, Train.data, Validation.data, selFun=selFun)
      return(list(predT = predT, predE = predE, res = resT,
                  Rsp = Rsp, testn = testn))
    }else{
      return(list(predT = predT, predE = predE, res = resT,
                  Rsp = Rsp))
    }
  })
}

# Function to test using random forest
TEST_RF <- function(formula, ntree = 500, mtry = NULL, maxnodes = NULL, binary = FALSE){
  # return a function of train data and test data
  testModel <- function(Train.data, Validation.data, selFun = dcPre(), calCov=TRUE){
    if (is.null(mtry)){
      mtry <- max(floor( length( dim(stats::model.matrix(formula ,Train.data))[2]-1)/3),1)
    }
    # obtain the response name
    Rsp <- as.character(formula)[2]
    RspDat <- Train.data[,Rsp]
    RspDatTest <- Validation.data[,Rsp]
    
    # Convert the response variable to a factor if binary is TRUE
    if (binary) {
      Train.data[, Rsp] <- as.factor(Train.data[, Rsp])
      Validation.data[, Rsp] <- as.factor(Validation.data[, Rsp])
    }
    
    names(Train.data) <- make.names(names(Train.data))
    names(Validation.data) <- make.names(names(Validation.data))

    resRf <- randomForest :: randomForest(formula, data = Train.data, ntree = ntree,  maxnodes = maxnodes, mtry = mtry, importance=FALSE)
    # # obtain random forest prediction on the training set
    # predT <-  as.numeric(stats :: predict(resRf, newdata = Train.data))
    # # obtain random forest prediction on the test set
    # predE <-  as.numeric(stats :: predict(resRf, newdata = Validation.data))
    
    # obtain random forest prediction on the training set
    predT <- stats::predict(resRf, newdata = Train.data, type = if (binary) "prob" else "response")
    # obtain random forest prediction on the test set
    predE <- stats::predict(resRf, newdata = Validation.data, type = if (binary) "prob" else "response")
    
    # If binary, extract the probability of the positive class
    if (binary) {
      predT <- predT[, 2]  # Assuming the second column corresponds to the positive class
      predE <- predE[, 2]
      Validation.data[,Rsp] <- RspDatTest
      Train.data[,Rsp] <- RspDat
    }
    
    res <-   (RspDat -  predT)
    if(calCov){
      testn <- calCcov(formula, predT, predE, Train.data, Validation.data, selFun=selFun)
      return(list(predT = predT, predE = predE, res = res,
                  Rsp = Rsp, testn = testn))
    }else{
      return(list(predT = predT, predE = predE, res = res,
                  Rsp = Rsp))
    }
  }
  return(testModel)
}

# Function to test using support vector machine
TEST_SVM <- function (formula, kernel="radial", cost=10, gamma=0.1, binary = FALSE) 
{
  testModel <- function(Train.data, Validation.data, selFun = dcPre(), calCov=TRUE) {
    # Obtain the response name
    Rsp <- as.character(formula)[2]
    RspDatTrain <- Train.data[, Rsp]
    RspDatTest <- Validation.data[, Rsp]
    
    # Convert the response variable to a factor if binary is TRUE
    if (binary) {
      Train.data[, Rsp] <- as.factor(Train.data[, Rsp])
      Validation.data[, Rsp] <- as.factor(Validation.data[, Rsp])
    }
    
    if (!binary) {
      modT <- e1071::svm(formula, Train.data, kernel=kernel, cost=cost, gamma=gamma)
      predT <- stats::predict(modT, newdata = Train.data)
      predE <- stats::predict(modT, newdata = Validation.data)
    }else{
      modT <- e1071::svm(formula, Train.data, kernel=kernel, cost=cost, gamma=gamma, probability=TRUE)
      predT <- attr(stats::predict(modT, Train.data, probability = TRUE), "probabilities")[,"1"]
      predE <- attr(stats::predict(modT, Validation.data, probability = TRUE), "probabilities")[, "1"]
    }
  
    res <- RspDatTrain - predT
    
    if(binary){
      Train.data[,Rsp] <- RspDatTrain
      Validation.data[,Rsp] <- RspDatTest
    }
    
    if(calCov){
      testn <- calCcov(formula, predT, predE, Train.data, Validation.data, selFun=selFun)
      return(list(predT = predT, predE = predE, res = res,
                  Rsp = Rsp, testn = testn))
    }else{
      return(list(predT = predT, predE = predE, res = res,
                  Rsp = Rsp))
    }
  }
  return(testModel)
}

# Function to test using support vector machine for BAGofT
TEST_SVM_BAGofT <- function (formula, kernel="linear") 
{
  testModel <- function(Train.data, Validation.data) {
    Rsp <- as.character(formula)[2]
    y <- Train.data[,Rsp]
    Train.data[,Rsp] <- factor(Train.data[,Rsp])
    Validation.data[,Rsp] <- factor(Validation.data[,Rsp])
    
    modT <- e1071::svm(formula, Train.data, kernel=kernel, probability = TRUE)
    predT <- attr(stats::predict(modT, Train.data, probability = TRUE), "probabilities")[,"1"]
    predE <- attr(stats::predict(modT, Validation.data, probability = TRUE), "probabilities")[, "1"]
    res <- y - predT
    # mod <- e1071::svm(formula=y~., data=dat, probability=TRUE) #, kernel="linear", type="C-classification")
    # tmp <- attr(stats::predict(mod, dat, probability = TRUE), "probabilities")
    return(list(predT = predT, predE = predE, res = res, Rsp = Rsp))
  }
  return(testModel)
}

# Function to test using XGBoost
TEST_XGBOOST <- function(formula, params = list(), nrounds = 25, binary = FALSE){
  # return a function of train data and test data
  testModel <- function(Train.data, Validation.data, selFun = dcPre(), calCov=TRUE){
    # obtain the response name
    Rsp <- as.character(formula)[2]
    # regressor data
    XmatT <- stats::model.matrix(formula,  Train.data)[,-1]
    XmatE <- stats::model.matrix(formula,  Validation.data)[,-1]
    
    # Adjust parameters for binary classification
    if (binary) {
      params$objective <- "binary:logistic"
    }
    
    # fit xgboost
    xgModT <- xgboost :: xgboost(data = XmatT, label = Train.data[, Rsp], params = params, nrounds = nrounds, verbose = 0)
    # predict on the test set
    predE <- stats :: predict(xgModT, XmatE)
    # predict on the training set
    predT <- stats :: predict(xgModT, XmatT)
    # calculate the residual
    res <-   Train.data[, Rsp] -  predT
    if(calCov){
      testn <- calCcov(formula, predT, predE, Train.data, Validation.data, selFun=selFun)
      return(list(predT = predT, predE = predE, res = res,
                  Rsp = Rsp, testn = testn))
    }else{
      return(list(predT = predT, predE = predE, res = res,
                  Rsp = Rsp))
    }
  }
  return(testModel)
}

# Functions to calculate several functions in our test statistic
nm <- function(n, m){
  nm <- 1
  for (i in n:(n-m+1)) {
    nm <- nm*i
  }
  return(nm)
}
Cnm <- function(n,m){
  combinat::nCm(n,m)
}
psi <- function(x1,x2,x3){
  return((x1<x3)-(x2<x3))
}
cn <- function(n){
  return(((1-1/n)^2+1/n^2)^2)
}
K1 <- function(x1, x2) {
  # attention here: element-wise max
  return(x1^2+x2^2-2*pmax(x1, x2)+2/3)
}

# Calculate the T_{n,p} following Li, Xu, Zhou and Zhu (2023, JASA)
getTnp <- function(dat, Rsp){
  n <- nrow(as.matrix(dat))
  p <- ncol(as.matrix(dat))-1
  Xdat <- dat[,setdiff(colnames(dat) ,Rsp)]
  Ydat <- dat[,Rsp]
  Ybar <- mean(Ydat)
  if(p > 1){orderX <- apply(Xdat, 2, order)
  }else orderX <- order(Xdat)
  Ydot <- matrix(Ydat[orderX]-Ybar, ncol = p)
  j.ind <- 2:n
  if (p > 1) {
    p1 <- (n-2)*(n-3)*sum(apply(Ydot, 2, cumsum)[-n,]^2)
    p2 <- 2*sum((n*j.ind-2*n-2*j.ind+2)*rowSums(apply(Ydot, 2, cumsum)[-n,]*Ydot[-1,]))
    p3 <- -sum(rowSums(apply(Ydot^2, 2, cumsum)[-n,])*(n^2-2*n*j.ind-n+4*j.ind-4))
    p4 <- -n*(n^2-3*n+8)/3*sum(Ydot^2)
    p5 <- 2*sum(rowSums(Ydot^2)*(0:(n-1))^2)
    Tnp <- (p1+p2+p3+p4+p5)/nm(n,5)
    return(Tnp)
  }else{
    p1 <- (n-2)*(n-3)*sum(cumsum(Ydot)[-n]^2)
    p2 <- 2*sum((n*j.ind-2*n-2*j.ind+2)*cumsum(Ydot)[-n]*Ydot[-1])
    p3 <- -sum(cumsum(Ydot^2)[-n]*(n^2-2*n*j.ind-n+4*j.ind-4))
    p4 <- -n*(n^2-3*n+8)/3*sum(Ydot^2)
    p5 <- 2*sum((Ydot^2)*(0:(n-1))^2)
    Tnp <- (p1+p2+p3+p4+p5)/nm(n,5)
    return(Tnp)
  }
}

# Calculate the S_{n,p}
getSnp <- function(dat, Rsp){
  Xdat <- dat[,setdiff(colnames(dat) ,Rsp)]
  Ydat <- dat[,Rsp]
  n <- nrow(as.matrix(Xdat))
  p <- ncol(as.matrix(Xdat))
  
  if (p > 1){
    p1 <- 1/(4*cn(n)*(n-1)*n)
    Yd <- array(Ydat-mean(Ydat)) 
    K0 <- Yd %*% t(Yd) 
    diag(K0) <- 0
    K0[lower.tri(K0)] <- 0
    cdfs <- apply(Xdat, 2, rank)/n
    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        K0[i,j] <- K0[i,j]^2 * sum(K1(cdfs[i,], cdfs[j,]))^2
      }
    }
    Snp <- sqrt(2*p1*sum(K0))
    return(Snp)
  }else{
    p1 <- 1/(4*cn(n)*(n-1)*n)
    Yd <- array(Ydat-mean(Ydat)) 
    K0 <- Yd %*% t(Yd) 
    diag(K0) <- 0
    K0[lower.tri(K0)] <- 0
    cdfs <- rank(Xdat)/n
    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        K0[i,j] <- K0[i,j]^2 * sum(K1(cdfs[i], cdfs[j]))^2
      }
    }
    Snp <- sqrt(2*p1*sum(K0))
    return(Snp)
  }
  
}

# Function for preselection by distance correlation
dcPre <- function(npreSel = 5){
  return(function(datRf, parVar){
    # count the number of variables to partition
    nParV <- if(!identical(parVar, ".")){length(parVar)}else{ncol(datRf) - 1}
    if (nParV > npreSel){
      preSelected <- TRUE
      datRf_temp <- datRf
      datRf_temp$res <- NULL
      # calculate the distance correlation
      dcRes <- t(dcov :: mdcor(datRf$res, datRf_temp))
      rownames(dcRes) <- colnames(datRf_temp)
      # select Kmax partition variables with the largest variable importance
      parVarNew <- colnames(datRf_temp)[order(-as.numeric(dcRes))[c(1: npreSel)]]
      return(list(preSelected = preSelected, parVarNew = parVarNew, VI = dcRes))
    }else{
      preSelected = FALSE
      parVarNew <- parVar
      return(list(preSelected = preSelected, parVarNew = parVarNew))
    }
  })
}

# The adaptive Neyman test proposed by Fan and Huang, Goodness-of-fit tests for parametric regression models, 2001
AN.test <- function(resi, alpha=0.05){
  n <- length(resi)
  resi_star <- NULL
  index <- 1:n
  for (i in 1:(n %/% 2)) {
    resi_star[2*i-1] <- sqrt(2/n)* sum(resi*cos(2*pi*index*i/n))
    resi_star[2*i] <- sqrt(2/n)* sum(resi*sin(2*pi*index*i/n))
  }
  if(abs(n%%2-1)<1e-5){
    resi_star[n] <- sqrt(2/n)* sum(resi)
  }
  sig <- sd(resi)
  T.seq <- cumsum(resi_star^2-sig^2)
  T.AN_ <- max(T.seq/sqrt(2*index*sig^4))
  T.AN <- sqrt(2*log(log(n)))*T.AN_-(2*log(log(n))+0.5*log(log(log(n)))-0.5*log(4*pi))
  return(T.AN < -log(-log(1-alpha)))
}

# data generation for models 2-4
data_gen <- function(n, p, model){
  # model 4 for correlated features, models 2-3 for independent features
  if (model==4) {
    Sig <- matrix(rep(0, p^2), ncol = p)
    for (i in 1:p) {
      for (j in 1:p) {
        Sig[i,j] <- .5^(abs(i-j))
      }
    }
    Xdat <- MASS::mvrnorm(n, mu=rep(0,p), Sigma=Sig)
  }else{
    Xdat <- matrix(rnorm((n*p)), ncol = p)
  }
  colnames(Xdat) <- paste("x", c(1:p), sep = "")
  betaVec <- rnorm(5, 1, .1)
  if (model==2) {
    y <- betaVec[1]*Xdat[,1]+betaVec[2]*Xdat[,2]+betaVec[3]*Xdat[,3]+betaVec[4]*Xdat[,4]+betaVec[5]*Xdat[,5]+rnorm(n)
  }else if(model==3){
    y <- 3*(abs(betaVec[1]*Xdat[,1])<1)-3*(abs(betaVec[1]*Xdat[,1])>1)+(betaVec[2]*Xdat[,2])+betaVec[3]*Xdat[,3]+betaVec[4]*Xdat[,4]+betaVec[5]*Xdat[,5]+rnorm(n)
  }else if(model==4){
    y <- betaVec[1]*Xdat[,1]^2+betaVec[2]*Xdat[,2]+betaVec[3]*Xdat[,3]+betaVec[4]*Xdat[,4]+betaVec[5]*Xdat[,5]+rnorm(n)
  }
  dat <- data.frame(y = y, Xdat)
  return(dat)
}

# data generation for model 5
data_gen_binary <- function(n, p, alpha, model=5, q=0){
  Xdat <- matrix(rnorm((n*p)), ncol = p)
  colnames(Xdat) <- paste("x", c(1:p), sep = "")
  betaVec <- rnorm(p, 0, .1)
  tmp <- betaVec[1]*Xdat[,1] + betaVec[2]*Xdat[,2] + betaVec[3]*Xdat[,3] + rnorm(n, sd=.1) + (alpha*Xdat[,5]^2) +  1/(abs(alpha*Xdat[,4])+3)
  y <- as.numeric(tmp>quantile(tmp, probs = .5))
  dat <- data.frame(y = y, Xdat)
  return(list(dat=dat, xdat=Xdat, ydat=y))
}

# Algorithm 2 of the paper 
# Javanmard, A. & Mehrabi, M. GRASP: a goodness-of-fit test for classification learning. Journal of the Royal Statistical Society Series B: Statistical Methodology 86, 215–245 (2024).
GRASP <- function(dat, ratio, L=10, alpha=.05){
  n <- dim(dat)[1]
  p <- dim(dat)[2]-1
  ind <- sample(1:n, n*ratio)
  Train.data <- dat[ind,]
  Validation.data <- dat[-ind,]
  nt <- length(ind)
  ne <- n-nt
  modT <- stats::glm(formula=y~., Train.data, family = binomial(link = "logit"))
  predT <- stats::predict(modT, newdata = Train.data, type = "response", se.fit = TRUE)$fit
  predE <- stats::predict(modT, newdata = Validation.data, type = "response", se.fit = TRUE)$fit
  w <- rep(0, length(predE))
  y1 <- which(Validation.data$y==1)
  y0 <- which(Validation.data$y==0)
  w[y0] <- runif(length(y0), predE[y0], 1)
  w[y1] <- runif(length(y1), 0, predE[y1])
  label <- round(w*L)
  test_stat <- 0
  for (i in 1:L) {
    test_stat <- test_stat + (sum(label==i)-ne/L)^2*L/ne
  }
  rej <- (test_stat>=qchisq(1-alpha,L-1))
  return(1-rej)
}

# GRASP test for the real data MNIST
GRASP_test <- function(testModel, data, prop,formula,  L=10, alpha=.05){
  n <- dim(data)[1]
  p <- dim(data)[2]-1
  ind <- sample(1:n, n*prop)
  Train.data <- data[ind,]
  Validation.data <- data[-ind,]
  res <- testModel(Train.data = Train.data, Validation.data = Validation.data, calCov = FALSE)
  
  predT <- res$predT
  predE <- res$predE
  
  Rsp <- as.character(formula)[2]
  
  nt <- length(ind)
  ne <- n-nt
  w <- rep(0, length(predE))
  y1 <- which(Validation.data[,Rsp]==1)
  y0 <- which(Validation.data[,Rsp]==0)
  w[y0] <- runif(length(y0), predE[y0], 1)
  w[y1] <- runif(length(y1), 0, predE[y1])
  label <- round(w*L)
  test_stat <- 0
  for (i in 1:L) {
    test_stat <- test_stat + (sum(label==i)-ne/L)^2*L/ne
  }
  pvalue <- pchisq(test_stat, L-1, lower.tail = FALSE)
  return(pvalue)
}
