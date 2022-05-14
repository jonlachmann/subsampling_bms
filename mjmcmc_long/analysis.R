source("mjmcmc_long/rmse_funs.R")

load("mjmcmc_long/10K_dyn.RData")
load(file="mjmcmc_long/full_10Kg.Rdata")

full_10Kg_renorm <- GMJMCMC:::marginal.probs.renorm(full_10Kg)

stat_100K_MC <- rmse_conv(full_100Kg_renorm, result, 100,F)
#save(stat_100K_MC, file="stat_100K_MC.RData")
dyn_10K_RM <- rmse_conv(full_10Kg_renorm, result, 100,T, T)
#save(dyn_10K_RM, file="dyn_10K_RM.RData")
load("mjmcmc_long/dyn_100K_MC.RData")
load("mjmcmc_long/dyn_100K_RM.RData")
load("mjmcmc_long/stat_100K_MC.RData")
load("mjmcmc_long/stat_100K_RM.RData")
load("mjmcmc_long/stat_10K_MC.RData")
load("mjmcmc_long/stat_10K_RM.RData")
load("mjmcmc_long/dyn_10K_MC.RData")
load("mjmcmc_long/dyn_10K_RM.RData")

main <- "RMSE of marginal posterior using MJMCMC with S-IRLS-SGD"
sub10K <- "Gaussian data with 10,000 observations"
sub100K <- "Gaussian data with 100,000 observations"
xlab <- "MJMCMC iterations x10,000"
ylab <- "RMSE"

plot(stat_100K_RM, type="l", ylim=c(0.145, 0.22), main=main, xlab=xlab, ylab=ylab, bty="n")
lines(stat_100K_MC, col="black", lty="dashed")
mtext(side = 3, line = 0.25, at = 1, adj = 0, sub100K)
legend(70, 0.22, legend=c("RM", "MC"),
       col=c("black", "black"),
       lty=c("solid", "dashed", "solid", "dashed"), bty="n")

lines(dyn_100K_RM, col="red")
lines(dyn_100K_MC, col="red", lty="dashed")

plot(stat_10K_RM, type="l", ylim=c(0.08, 0.16), main=main, xlab=xlab, ylab=ylab, bty="n")
lines(stat_10K_MC, col="black", lty="dashed")
lines(dyn_10K_RM, col="red")
lines(dyn_10K_MC, col="red", lty="dashed")
mtext(side = 3, line = 0.25, at = 1, adj = 0, sub10K)
legend(70, 0.16, legend=c("RM dynamic", "MC dynamic", "RM static", "MC static"),
       col=c("black", "black", "red", "red"),
       lty=c("solid", "dashed", "solid", "dashed"), bty="n")

renorm_1M_10K <- GMJMCMC:::marginal.probs.renorm(result$models)
mcmc_1M_10K <- GMJMCMC:::marginal.probs(result$models)

barplot(rbind(full_10Kg_renorm, renorm_1M_10K, mcmc_1M_10K), beside=T)

models_sort <- models[rev(order(models[,17])),]

diff <- matrix(NA, nrow(models_sort), 2)
for (i in seq_len(nrow(models_sort))) {
  idx <- GMJMCMC:::vec_in_mat(true_models[,2:16], models_sort[i,2:16])
  diff[i,] <- c(true_models[idx,17], true_models[idx,17] - models_sort[i,17])
}

plot(diff[1:300,2], type="l")
plot(diff)
