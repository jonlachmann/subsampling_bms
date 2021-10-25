# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-10-25

load("testdir/test.Rdata")

testmat <- matrix(unlist(test), ncol=length(unlist(test[[1]])), byrow=T)
testmat <- testmat[order(testmat[,17]),]

top1024 <- testmat[31745:32768,]

top1024mods <- numeric(0)
for (i in 1:1024) {
  top1024mods <- c(top1024mods, packBits(c(as.logical(top1024[i, 2:16]), rep(F, 17)), type = "integer"))
}

model_partitions <- align_models(top1024mods)
n_obs <- 10000
name <- "test2"
directory <- "testdir"

run_multisim(x_10K, y_10K, logistic.loglik.bic, model_partitions, n_obs, name, directory)

true_renorm <- GMJMCMC:::marginal.probs.renorm(test)
top_renorm <- GMJMCMC:::marginal.probs.renorm(test2)

barplot(true_renorm)
barplot(top_renorm)

top_renorm - true_renorm
