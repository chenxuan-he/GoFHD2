# plot models 2-4
library(ggplot2)
for (model in 2:4) {
  for (p in c(500, 1000)) {
    newdat <- data.frame(method = character(), `splitting ratio` = character(), `p-value` = numeric(), stringsAsFactors = TRUE)
    for (procedure in c("LASSO","SCAD","SVR","RF","XGBoost","FNN")) {
      n=500
      nsim=100
      tmpDat <- data.frame(method=rep(procedure, nsim*3), `splitting ratio`=rep("", nsim*3), `p-value` =rep(0,nsim*3))
      for (ii in 1:3) {
        ratios = c(50,75,90)
        ratio <- ratios[ii]
        index <- ((ii-1)*nsim+1):(ii*nsim)
        tmpDat$splitting.ratio[index] <- rep(paste0(ratio,"%"), nsim)
        loc <- paste0("results/model",model,"_", tolower(procedure),"_n",n,"_p",p,"_r",ratio,".Rdata")
        load(loc)
        tmpDat$p.value[index] <- result
      }
      newdat <- rbind(tmpDat,newdat)
    }
    newdat$splitting.ratio <- factor(newdat$splitting.ratio, levels = c("50%", "75%", "90%"))
    newdat$method <- factor(newdat$method, levels = c("LASSO","SCAD","SVR","RF","XGBoost","FNN"))
    bxp <- ggplot(data=newdat) +
      geom_boxplot(aes(y = p.value, x = splitting.ratio)) +
      geom_hline(aes(yintercept=0.05), color=I("red"),linetype = 5, alpha=0.5)+
      facet_wrap(~(method), ncol = 3)+
      ylim(0,1)+
      theme_bw() +
      ylab(NULL) + xlab(NULL) +
      theme_bw()
      # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    bxp
    ggsave(bxp, filename = paste0("plots/","model",model,"_p",p,".pdf"), units = "cm", width=16, height=8)
  }
}
