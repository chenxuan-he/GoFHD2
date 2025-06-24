# clean the work space and library working functions
rm(list = ls())
wd = getwd()
source(paste0(wd, "/functions.R"))

# packages for parallel computing and parsing auguments
library(optparse)
library(dplyr)
library(parallel)
library(doSNOW)
library(BAGofT)

# accept parameters
option_list <- list(
  make_option("--n", type = "integer", default = 500, help = "Sample size."),
  make_option("--p", type = "integer", default = 100, help = "Dimension of x."),
  make_option("--nsim", type = "integer", default = 500, help = "Number of simulations."),
  make_option("--nsplits", type = "integer", default = 1, help = "Number of repeated splitting."),
  make_option("--ratio", type = "double", default = .95, help = "Splitting ratios for training and test."),
  make_option("--alpha", type = "double", default = 1, help = "Different alpha for simulation."),
  make_option("--cores", type = "integer", default = 100, help = "Cores used for parallel computing"),
  make_option("--correlated", type = "logical", default = FALSE, help = "Whether or not the exposures are correlated."),
  make_option("--seed", type = "integer", default = 0, help = "Seed to be set."),
  make_option("--model", type = "integer", default = 5, help = "Types of models, here is 5.")
)
# Parse the command line arguments
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)
n = opt$n
p = opt$p
nsim = opt$nsim
nsplits = opt$nsplits
ratio = opt$ratio
alpha = opt$alpha
cores = opt$cores
correlated = opt$correlated
seed = opt$seed
model = opt$model

numCores = detectCores()
cl <- makeCluster(min(numCores, cores))
registerDoSNOW(cl)

result = foreach(ii=1:nsim,
                 .combine = "rbind",
                 .packages = c("dplyr")) %dopar%
  {
    set.seed(ii+seed)
    dat <- data_gen_binary(n, p, model=model, alpha=alpha)
    data <- dat$dat
    xdat <- dat$xdat
    ydat <- dat$ydat
    # our test
    testModel <- TEST_LM(formula = y~.)
    testRes <- ccovTest(testModel = testModel, nsplits = nsplits, ratio = ratio, data = data)
    # BAGofT test
    testModel <- TEST_SVM_BAGofT(formula = y~., kernel="linear")
    bag <- BAGofT::BAGofT(testModel=testModel, data=data, ne=n*(1-ratio), nsplits = nsplits, nsim=0)
    # GRASP test
    grasp <- GRASP(dat=data, ratio=ratio, L=10, alpha=.05)
    return(c(testRes$CauchyP, grasp, bag$pmean))
  }

stopCluster(cl)
print(alpha)
colMeans(result<0.05)