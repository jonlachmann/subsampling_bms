# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-10-25

load("testdir/test.Rdata")

testmat <- matrix(unlist(test), ncol=length(unlist(test[[1]])), byrow=T)
testmat <- testmat[order(testmat[,17]),]

top1024 <- testmat[31745:32768,]
top128 <- testmat[32641:32768,]

top1024mods <- numeric(0)
for (i in 1:1024) {
  top1024mods <- c(top1024mods, packBits(c(as.logical(top1024[i, 2:16]), rep(F, 17)), type = "integer"))
}

top128mods <- numeric(0)
for (i in 1:128) {
  top128mods <- c(top128mods, packBits(c(as.logical(top128[i, 2:16]), rep(F, 17)), type = "integer"))
}

save(top1024mods, file="top1024.RData")
save(top128mods, file="top128.RData")

model_partitions <- align_models(top1024mods)
n_obs <- 10000
name <- "test2"
directory <- "testdir"

run_multisim(x_10K, y_10K, logistic.loglik.bic, model_partitions, n_obs, name, directory)

true_renorm <- GMJMCMC:::marginal.probs.renorm(test)
top_renorm <- GMJMCMC:::marginal.probs.renorm(top1024)
top_renorm128 <- GMJMCMC:::marginal.probs.renorm(top128)
top_renorm_sgd <- GMJMCMC:::marginal.probs.renorm(test_sgd)
sgd1000 <- GMJMCMC:::marginal.probs.renorm(sgd_1000)
sgd1000_2 <- GMJMCMC:::marginal.probs.renorm(sgd_1000_2)
sgd1000_3 <- GMJMCMC:::marginal.probs.renorm(sgd_1000_3)

barplot(true_renorm)
barplot(top_renorm)
barplot(top_renorm128)
barplot(top_renorm_sgd)
barplot(sgd1000)
barplot(sgd1000_2)
barplot(sgd1000_3)

top_renorm - true_renorm

load("testdir/test_sgd.Rdata")
load("testdir/sgd_1000.Rdata")
load("testdir/sgd_1000_2.Rdata")
load("testdir/sgd_1000_3.Rdata")
load("testdir/top1024.Rdata")
load("testdir/top128.Rdata")


load("testdir/top128dev.Rdata")
load("testdir/top128dev_sgd.Rdata")
load("testdir/top128dev_irls.Rdata")
load("testdir/top128dev_sgd20K.Rdata")
load("testdir/top128dev_sgd10K.Rdata")

top128dev_mat <- matrix(unlist(top128dev), ncol=length(unlist(top128dev[[1]])), byrow=T)
top128dev_sgd_mat <- matrix(unlist(top128dev_sgd), ncol=length(unlist(top128dev_sgd[[1]])), byrow=T)
top128dev_sgd_mat <- matrix(unlist(top128dev_sgd20K), ncol=length(unlist(top128dev_sgd20K[[1]])), byrow=T)
top128dev_sgd10K_mat <- matrix(unlist(top128dev_sgd10K), ncol=length(unlist(top128dev_sgd10K[[1]])), byrow=T)
top128dev_irls_mat <- matrix(unlist(top128dev_irls), ncol=length(unlist(top128dev_irls[[1]])), byrow=T)

plot(top128dev_mat[,17], type="l", ylim=c(min(top128dev_mat[,17]), max(top128dev_sgd10K_mat[,17])))
lines(top128dev_sgd10K_mat[,17], col="red")
lines(top128dev_irls_mat[,17], col="green")

boxplot(top128dev_sgd10K_mat[,1])

boxdata <- cbind(top128dev_mat[,17] - top128dev_sgd_mat[,17],
top128dev_mat[,17] - top128dev_irls_mat[,17],
top128dev_mat[,17] - top128dev_sgd10K_mat[,17])

boxplot(boxdata)

as.logical(intToBits(292))

