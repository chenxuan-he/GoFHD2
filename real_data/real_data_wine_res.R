# read and store the MSE
all_results <- matrix(nrow=6, ncol=12)
i_ <- 0
for (ratio in c(0.5, 0.75, 0.9)) {
  i_ <- i_+1
  j=0
  for (procedure in c("lasso", "scad", "svr", "rf", "xgboost", "fnn")){
    j <- j+1
    load(paste0("results/real_data_wine_",ratio,"_", procedure, ".Rdata"))
    all_results[i_, ((j-1)*2+1):(j*2)] <- colMeans(MSEr1)
    all_results[i_+3, ((j-1)*2+1):(j*2)] <- colMeans(MSEe1)
  }
}
write.csv(all_results,"real_data_wine.csv")
