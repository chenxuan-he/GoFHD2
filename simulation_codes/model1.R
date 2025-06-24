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
  make_option("--n", type = "integer", default = 500, help = "Sample size."),
  make_option("--p", type = "integer", default = 100, help = "Dimension of x."),
  make_option("--nsim", type = "integer", default = 500, help = "Number of simulations."),
  make_option("--nsplits", type = "integer", default = 1, help = "Number of repeated splitting."),
  make_option("--ratio", type = "double", default = .95, help = "Splitting ratios for training and test."),
  make_option("--alpha", type = "double", default = .0, help = "Different alpha for simulation."),
  make_option("--cores", type = "integer", default = 20, help = "Cores used for parallel computing"),
  make_option("--correlated", type = "logical", default = FALSE, help = "Whether or not the exposures are correlated."),
  make_option("--seed", type = "integer", default = 0, help = "Seed to be set.")
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
alpha = opt$alpha
cores = opt$cores
correlated = opt$correlated
seed = opt$seed

numCores = detectCores()
cl <- makeCluster(min(numCores, cores))
registerDoSNOW(cl)

result = foreach(ii=1:nsim, 
                 .combine = "rbind",
                 .packages = c("dplyr")) %dopar% 
  {
    set.seed(ii+seed)
    # generate covariates data
    mu <- rep(0, p)
    Sigma <- diag(1, p)
    if (correlated){
      for (i in 1:p){
        for (j in 1:p){
          Sigma[i,j]=.2^(abs(i-j))
        }
      }
    }
    xdat <- MASS::mvrnorm(n, mu=mu, Sigma=Sigma)
    # generate error term
    eps <- rnorm(n, sd=.1)
    beta1 <- rnorm(1, mean=1, sd=1) 
    beta2 <- rnorm(1, mean=1, sd=1) 
    beta3 <- 0
    ydat <- beta1*xdat[,1] + beta2*xdat[,2] + beta3*xdat[,3] + exp(alpha*xdat[,4] + alpha*xdat[,5]^2 + alpha*abs(xdat[,6])) + eps
    # generate the dataset
    dat <- data.frame(y = ydat, x = xdat)
    colnames(dat)[2:(p+1)] <- paste0("x", 1:p)
    # GRPres <- GRPtests::GRPtest(X=as.matrix(xdat), y=ydat, fam = "gaussian", penalize = TRUE)
    RPres <- RPtests::RPtest(x=as.matrix(xdat), y=ydat)
    testModel <- TEST_SCAD(formula = y ~ .)    
    testModel <- TEST_LM(formula = y ~ .)    
    testRes <- ccovTest(testModel = testModel,
                    nsplits = nsplits, ratio = ratio,
                    data = dat)
    pca <- princomp(dat[,2:(p+1)])
    ANres <- AN.test(lm(data=dat, y~x1+x2+x3)$residual[order(pca$scores[,1])])

    return(c(testRes$CauchyP, RPres, ANres))
  }

stopCluster(cl)
print(alpha)
colMeans(result<0.05)
