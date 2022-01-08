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

load("testdir/top128dev_sgd.Rdata")


plot(top128dev_mat[,17], type="l", ylim=c(min(top128dev_mat[,17]), max(top128dev_sgd1K_mat[,17])))
lines(top128dev_sgd1K_mat[,17], col="red")
lines(top128dev_irls_mat[,17], col="green")

boxplot(top128dev_sgd1K_mat[,1])



modelvector <- as.logical(c(T,intToBits(292)[1:15]))

truemod <- glm.fit(as.matrix(x_10K[,modelvector]), y_10K, family=binomial())

library(profvis)

profvis({mod <- irls.sgd(as.matrix(million_x[,modelvector]), million_y_l, binomial(),
            irls.control=list(maxit=0),
            sgd.control=list(subs=0.01, maxit=2000, alpha=0.2, decay=0.99995, histfreq=10), save_hist = T)})

par(mfrow=c(2,2),oma = c(0,0,2,0), mar=c(4,5.1,2,2.1))
for (i in 1:4) {
  multiplot(cbind(mod$sgd_hist[,i], truemod$coefficients[i]),
            frame.plot=F, cex.lab=1.3,
            ylab=bquote(beta[.(i-1)]), xlab="Iteration")
}



top128dev_sgd_mat <- matrix(unlist(top128dev_sgd), ncol=length(unlist(top128dev_sgd[[1]])), byrow=T)
top128dev_sgd_mat <- matrix(unlist(top128dev_sgd20K), ncol=length(unlist(top128dev_sgd20K[[1]])), byrow=T)
top128dev_sgd10K_mat <- matrix(unlist(top128dev_sgd10K), ncol=length(unlist(top128dev_sgd10K[[1]])), byrow=T)
top128dev_sgd1K_mat <- matrix(unlist(top128dev_sgd1K), ncol=length(unlist(top128dev_sgd1K[[1]])), byrow=T)



# Useful code begins here

# Load a lot of stuff
load("sgd_exp1/top128dev_glm.Rdata")
top128dev_glm_mat <- matrix(unlist(top128dev_glm), ncol=length(unlist(top128dev_glm[[1]])), byrow=T)
load("sgd_exp1/top128dev_sgd500.Rdata")
top128dev_sgd500_mat <- matrix(unlist(top128dev_sgd500), ncol=length(unlist(top128dev_sgd500[[1]])), byrow=T)
load("sgd_exp1/top128dev_sgd1K.Rdata")
top128dev_sgd1K_mat <- matrix(unlist(top128dev_sgd1K), ncol=length(unlist(top128dev_sgd1K[[1]])), byrow=T)
load("sgd_exp1/top128dev_sgd5K.Rdata")
top128dev_sgd5K_mat <- matrix(unlist(top128dev_sgd5K), ncol=length(unlist(top128dev_sgd5K[[1]])), byrow=T)
load("sgd_exp1/top128dev_sgd10K.Rdata")
top128dev_sgd10K_mat <- matrix(unlist(top128dev_sgd10K), ncol=length(unlist(top128dev_sgd10K[[1]])), byrow=T)
load("sgd_exp1/top128dev_sgd20K.Rdata")
top128dev_sgd20K_mat <- matrix(unlist(top128dev_sgd20K), ncol=length(unlist(top128dev_sgd20K[[1]])), byrow=T)
load("sgd_exp1/top128dev_irls.Rdata")
top128dev_irls_mat <- matrix(unlist(top128dev_irls), ncol=length(unlist(top128dev_irls[[1]])), byrow=T)

# Create a box plot of the deviance of the deviance
par(mfrow=c(1,1),oma = c(0,0,2,0), mar=c(4,5.1,2,2.1))
boxdata <- cbind(top128dev_glm_mat[,17] - top128dev_sgd500_mat[,17],
top128dev_glm_mat[,17] - top128dev_sgd1K_mat[,17],
top128dev_glm_mat[,17] - top128dev_sgd5K_mat[,17],
top128dev_glm_mat[,17] - top128dev_sgd10K_mat[,17],
top128dev_glm_mat[,17] - top128dev_sgd20K_mat[,17],
top128dev_glm_mat[,17] - top128dev_irls_mat[,17])

colnames(boxdata) <- c("SGD 500", "SGD 1K", "SGD 5K", "SGD 10K", "SGD 20K", "S-IRLS")

boxplot(boxdata)

timedata <- cbind( #top128dev_glm_mat[,1],
top128dev_sgd500_mat[,1],
top128dev_sgd1K_mat[,1],
top128dev_sgd5K_mat[,1],
top128dev_sgd10K_mat[,1],
top128dev_sgd20K_mat[,1],
top128dev_irls_mat[,1])

colnames(timedata) <- c("SGD 500", "SGD 1K", "SGD 5K", "SGD 10K", "SGD 20K", "S-IRLS")

boxplot(timedata)

load("sgd_exp2/top128dev_1M_glm.Rdata")
top128dev_1M_glm_mat <- matrix(unlist(top128dev_1M_glm), ncol=length(unlist(top128dev_1M_glm[[1]])), byrow=T)
load("sgd_exp2/top128dev_1M_sgd500.Rdata")
top128dev_1M_sgd500_mat <- matrix(unlist(top128dev_1M_sgd500), ncol=length(unlist(top128dev_1M_sgd500[[1]])), byrow=T)
load("sgd_exp2/top128dev_1M_sgd1K.Rdata")
top128dev_1M_sgd1K_mat <- matrix(unlist(top128dev_1M_sgd1K), ncol=length(unlist(top128dev_1M_sgd1K[[1]])), byrow=T)
load("sgd_exp2/top128dev_1M_sgd5K.Rdata")
top128dev_1M_sgd5K_mat <- matrix(unlist(top128dev_1M_sgd5K), ncol=length(unlist(top128dev_1M_sgd5K[[1]])), byrow=T)
load("sgd_exp2/top128dev_1M_sgd10K.Rdata")
top128dev_1M_sgd10K_mat <- matrix(unlist(top128dev_1M_sgd10K), ncol=length(unlist(top128dev_1M_sgd10K[[1]])), byrow=T)
load("sgd_exp2/top128dev_1M_sgd20K.Rdata")
top128dev_1M_sgd20K_mat <- matrix(unlist(top128dev_1M_sgd20K), ncol=length(unlist(top128dev_1M_sgd20K[[1]])), byrow=T)
load("sgd_exp2/top128dev_1M_irlssgd.Rdata")
top128dev_1M_irlssgd_mat <- matrix(unlist(top128dev_1M_irlssgd), ncol=length(unlist(top128dev_1M_irlssgd[[1]])), byrow=T)



timedata <- cbind(top128dev_1M_glm_mat[,1],
                  top128dev_1M_sgd500_mat[,1],
                  top128dev_1M_sgd1K_mat[,1],
                  top128dev_1M_sgd5K_mat[,1],
                  top128dev_1M_sgd10K_mat[,1],
                  top128dev_1M_sgd20K_mat[,1],
                  top128dev_1M_irlssgd_mat[,1])

precdata <- cbind(top128dev_1M_sgd500_mat[,17] - top128dev_1M_glm_mat[,17],
                  top128dev_1M_sgd1K_mat[,17] - top128dev_1M_glm_mat[,17],
                  top128dev_1M_sgd5K_mat[,17] - top128dev_1M_glm_mat[,17],
                  top128dev_1M_sgd10K_mat[,17] - top128dev_1M_glm_mat[,17],
                  top128dev_1M_sgd20K_mat[,17] - top128dev_1M_glm_mat[,17],
                  top128dev_1M_irlssgd_mat[,17] - top128dev_1M_glm_mat[,17])

timedata <- timedata / 1000000

colnames(timedata) <- c("IRLS", "BSGD 500", "BSGD 1K", "BSGD 5K", "BSGD 10K", "BSGD 20K", "S-IRLS-SGD")
colnames(precdata) <- c("BSGD 500", "BSGD 1K", "BSGD 5K", "BSGD 10K", "BSGD 20K", "S-IRLS-SGD")
par(mfrow=c(1,1),oma = c(0,0,2,0), mar=c(6.2,5.1,2,2.1))
boxplot(precdata, main="Deviance error for different optimization methods", xaxt="n", frame=F, ylab="Deviance error")
axis(1, at=1:6, labels=colnames(precdata), las=2)
# Save as 800x600 @ 90DPI

boxplot(timedata, main="Computational time for different optimization methods", xaxt="n", frame=F, ylab="Computational time (ms)")
axis(1, at=1:7, labels=colnames(timedata), las=2)