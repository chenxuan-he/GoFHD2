# code for the motivating example in introduction section
library(doSNOW)
library(parallel)
library(optparse)

# include parser to accept parameters
option_list <- list(
  make_option("--sd", type = "double", default = 1, help = "Standard deviation (irreducible error)."),
  make_option("--seed", type = "integer", default = 0, help = "Random seed."),
  make_option("--max_cores", type = "integer", default = 25, help = "Random seed."),
  make_option("--n_sim", type = "integer", default = 100, help = "Random seed.")
)
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)
sd <- opt$sd
seed <- opt$seed
max_cores <- opt$max_cores
n_sim <- opt$n_sim

numCores = detectCores()
# number of cpus used for parallel computing
cl <- makeCluster(min(numCores, max_cores))

# four types of dimension of x
# to save space, in the main article, we only present the results when p=100, 500, 1000
ps = c(100, 200, 500, 1000)
# start parallel computing
registerDoSNOW(cl)

for (p in ps){
  result = foreach(ii=1:n_sim, .combine = "rbind") %dopar% {
  set.seed(ii+seed)
  # generate a simple dataset and fit with multiple procedures
  # generate high-dimensional x 
  n <- 100
  Xdat <- matrix(rnorm((n*p)), ncol = p)
  # y is generated from noise independent of x
  y <- rnorm(n, sd=sd)
  dat <- data.frame(y = y, Xdat)
  # split data into training and test data
  tr_ratio <- 0.8
  index_tr <- sample(1:n, tr_ratio*n)
  index_est <- base::setdiff(1:n, index_tr)
  dat_tr <- dat[index_tr,]
  dat_est <- dat[index_est,]
  X_tr <- as.matrix(dat_tr[,-1])
  X_est <- as.matrix(dat_est[,-1])
  
  ### use mean as prediction
  mse_est_mean <- mean((dat_est[,1]-mean(dat_est[,1]))^2)
  mse_tr_mean <- mean((dat_tr[,1]-mean(dat_tr[,1]))^2)

  ### lasso
  alpha <- 1
  mod_lambda <- glmnet :: cv.glmnet(X_tr, dat_tr[,1], alpha = alpha)$lambda.min
  mod <- glmnet :: glmnet(X_tr, dat_tr[,1], alpha = alpha, lambda = mod_lambda)
  pred_est <- stats :: predict(mod, X_est)
  pred_tr <- stats :: predict(mod, X_tr)
  mse_est_lasso <- mean((pred_est-dat_est[,1])^2)
  mse_tr_lasso <- mean((pred_tr-dat_tr[,1])^2)
  
  ### scad
  alpha <- 1
  mod_lambda <- ncvreg :: cv.ncvreg(X_tr, dat_tr[,1], alpha = alpha, penalty = "SCAD")$lambda.min
  mod <- ncvreg :: ncvreg(X_tr, dat_tr[,1], alpha = alpha, lambda = mod_lambda, penalty="SCAD")
  pred_est <- stats :: predict(mod, X_est)
  pred_tr <- stats :: predict(mod, X_tr)
  mse_est_scad <- mean((pred_est-dat_est[,1])^2)
  mse_tr_scad <- mean((pred_tr-dat_tr[,1])^2)
  
  ### support vector machine
  mod <- e1071::svm(y~., data=dat_tr)
  pred_est <- stats :: predict(mod, X_est)
  pred_tr <- stats :: predict(mod, X_tr)
  mse_est_svm <- mean((pred_est-dat_est[,1])^2)
  mse_tr_svm <- mean((pred_tr-dat_tr[,1])^2)
  
  ## random forest
  mod <- randomForest::randomForest(y~., data=dat_tr, ntree=50)
  pred_est <- stats :: predict(mod, X_est)
  pred_tr <- stats :: predict(mod, X_tr)
  mse_est_rf <- mean((pred_est-dat_est[,1])^2)
  mse_tr_rf <- mean((pred_tr-dat_tr[,1])^2)
  
  ### xgboost
  mod <- xgboost::xgboost(data = X_tr, label = dat_tr[, 1], nrounds = 25, verbose = 0)
  pred_est <- stats :: predict(mod, X_est)
  pred_tr <- stats :: predict(mod, X_tr)
  mse_est_xgboost <- mean((pred_est-dat_est[,1])^2)
  mse_tr_xgboost <- mean((pred_tr-dat_tr[,1])^2)
  
  ### neural network
  library(dplyr)
  library(keras)
  # fit neural network with 1 hidden layer
  network <- keras :: keras_model_sequential() %>%
    layer_dense(units = 16, activation = "relu", input_shape = c(ncol(X_tr))) %>%
    layer_dense(units = 1, activation = NULL)
  network %>% keras :: compile(
    # optimizer = "rmsprop",
    optimizer = "adam",
    loss = 'mse',
    metrics = c("mse")
  )
  network %>% keras :: fit(X_tr, dat_tr[,1],  epochs = 200)
  pred_est <- network %>% keras :: predict_on_batch(X_est)
  pred_tr <- network %>% keras :: predict_on_batch(X_tr)
  mse_est_nn <- mean((pred_est-dat_est[,1])^2)
  mse_tr_nn <- mean((pred_tr-dat_tr[,1])^2)

  return(c(mse_tr_mean, mse_est_mean, mse_tr_lasso, mse_est_lasso, mse_tr_scad, mse_est_scad, mse_tr_svm, mse_est_svm, mse_tr_rf, mse_est_rf, mse_tr_xgboost, mse_est_xgboost,  mse_tr_nn, mse_est_nn))
  }
  # save the file to local file
  save(result, file = paste0("results/motivatingExample_sd=", sd, "_p=", p, ".Rdata"))
}

# stop the parallel computing
stopCluster(cl)