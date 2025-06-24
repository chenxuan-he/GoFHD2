# clean the work space and library working functions
rm(list = ls())
wd = getwd()
source(paste0(wd, "/functions.R"))

# packages for parallel computing and parsing auguments
library(optparse)
library(parallel)
library(doSNOW)

# accept parameters
option_list <- list(
  make_option("--n", type = "integer", default = 500, help = "Sample size, set as 500."),
  make_option("--p", type = "integer", default = 1000, help = "Dimension of x, set as 500/1000."),
  make_option("--nsim", type = "integer", default = 100, help = "Number of simulations."),
  make_option("--nsplits", type = "integer", default = 10, help = "Number of repeated splitting."),
  make_option("--ratio", type = "double", default = .5, help = "Splitting ratios for training and test, here we choose .5, .75, and .9."),
  make_option("--cores", type = "integer", default = 20, help = "Cores used for parallel computing"),
  make_option("--seed", type = "integer", default = 0, help = "Seed to be set."),
  make_option("--model", type = "integer", default = 2, help = "Types of models, including 2,3,4 as in the article."),
  make_option("--procedure", type = "character", default = "fnn", help = "Types of regressing procedures, including lasso, scad, svr, xgboost, rf, and fnn.")
)
# Parse the command line arguments
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)
# Print the arguments
n = opt$n
p = opt$p
nsim = opt$nsim
nsplits = opt$nsplits
ratio = opt$ratio
cores = opt$cores
seed = opt$seed
model = opt$model
procedure = opt$procedure

numCores = detectCores()
cl <- makeCluster(min(numCores, cores))
registerDoSNOW(cl)

result = foreach(ii=1:nsim, 
                 .combine = "rbind",
                 .packages = c("dplyr", "keras")) %dopar% 
  {
    set.seed(seed + ii)
    # generate data for different models
    dat <- data_gen(n,p,model)
    # define the model to be test based on the procedure provided
    if(procedure=="lasso"){
      testModel <- TEST_LASSO(formula = y ~ .)
    }else if(procedure=="scad"){
      testModel <- TEST_SCAD(formula = y ~ .)
    }else if(procedure=="svr"){
      testModel <- TEST_SVM(formula = y ~ .)
    }else if(procedure=="xgboost"){
      testModel <- TEST_XGBOOST(formula = y ~ .)
    }else if(procedure=="rf"){
      testModel <- TEST_RF(formula = y ~ .)
    }else if(procedure=="fnn"){
      testModel <- TEST_FNN(formula = y ~ .)
    }
    # perform test
    testRes <- ccovTest(testModel = testModel, nsplits = nsplits, ratio = ratio, data = dat)
    # return the test result
    return(testRes$CauchyP)
  }
stopCluster(cl)
save.image(file = paste0("results/model", model, "_", procedure, "_n", n, "_p", p, "_r", as.integer(ratio*100),".Rdata"))

