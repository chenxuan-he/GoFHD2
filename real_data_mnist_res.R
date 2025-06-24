# read and store the prediction accuracy
all_results <- matrix(nrow=6, ncol=6)
i_ <- 0
nrep <- 100
for (ratio in c(0.5, 0.75, 0.9)) {
  i_ <- i_+1
  j <- 0
  for (procedure in 1:6){ #  c("lasso", "scad", svr", "rf", "xgboost", "fnn")
    j <- j+1
    load(paste0("results/real_data_mnist_accuracy_",ratio,"_", procedure, ".RData"))
    Ae1 <- matrix(ncol = 1, nrow = nrep)
    Ar1 <- matrix(ncol = 1, nrow = nrep)
    for (i in 1:nrep) {
      Ae1[i, ] <- results[[i]]$accuracy_e
      Ar1[i, ] <- results[[i]]$accuracy_r
    }
    all_results[i_, j] <- colMeans(Ae1)
    all_results[i_+3, j] <- colMeans(Ar1)
  }
}
write.csv(all_results,"real_data_mnist.csv")
