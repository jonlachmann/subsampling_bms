# Load the full enumeration results
load("sgd_exp2/full_1M_irls.Rdata")

# Convert to a matrix and sort after deviance
full_matrix <- matrix(unlist(full_1M_irls), ncol=length(unlist(full_1M_irls[[1]])), byrow=T)
full_matrix <- full_matrix[order(full_matrix[,17]),]

# Select the top 1024 models and convert them to a format that can be read when calculating them
top1024 <- full_matrix[31745:32768,]
top1024mods <- numeric(0)
for (i in 1:1024) {
  top1024mods <- c(top1024mods, packBits(c(as.logical(top1024[i, 2:16]), rep(F, 17)), type = "integer"))
}
save(top1024mods, file="sgd_exp2/top1024.RData")

# Load all the data from the runs
load("sgd_exp2/top1024dev_1M_irls.Rdata")
load("sgd_exp2/top1024dev_1M_irlssgd.Rdata")
load("sgd_exp2/top1024dev_1M_sgd500.Rdata")
load("sgd_exp2/top1024dev_1M_sgd1K.Rdata")
load("sgd_exp2/top1024dev_1M_sgd5K.Rdata")
load("sgd_exp2/top1024dev_1M_sgd10K.Rdata")
load("sgd_exp2/top1024dev_1M_sgd20K.Rdata")
top1024dev_1M_irls_mat <- matrix(unlist(top1024dev_1M_irls), ncol=length(unlist(top1024dev_1M_irls[[1]])), byrow=T)
top1024dev_1M_irlssgd_mat <- matrix(unlist(top1024dev_1M_irlssgd), ncol=length(unlist(top1024dev_1M_irlssgd[[1]])), byrow=T)
top1024dev_1M_sgd500_mat <- matrix(unlist(top1024dev_1M_sgd500), ncol=length(unlist(top1024dev_1M_sgd500[[1]])), byrow=T)
top1024dev_1M_sgd1K_mat <- matrix(unlist(top1024dev_1M_sgd1K), ncol=length(unlist(top1024dev_1M_sgd1K[[1]])), byrow=T)
top1024dev_1M_sgd5K_mat <- matrix(unlist(top1024dev_1M_sgd5K), ncol=length(unlist(top1024dev_1M_sgd5K[[1]])), byrow=T)
top1024dev_1M_sgd10K_mat <- matrix(unlist(top1024dev_1M_sgd10K), ncol=length(unlist(top1024dev_1M_sgd10K[[1]])), byrow=T)
top1024dev_1M_sgd20K_mat <- matrix(unlist(top1024dev_1M_sgd20K), ncol=length(unlist(top1024dev_1M_sgd20K[[1]])), byrow=T)

timedata <- cbind(top1024dev_1M_irls_mat[,1],
                  top1024dev_1M_sgd500_mat[,1],
                  top1024dev_1M_sgd1K_mat[,1],
                  top1024dev_1M_sgd5K_mat[,1],
                  top1024dev_1M_sgd10K_mat[,1],
                  top1024dev_1M_sgd20K_mat[,1],
                  top1024dev_1M_irlssgd_mat[,1])

precdata <- cbind(top1024dev_1M_sgd500_mat[,17] - top1024dev_1M_irls_mat[,17],
                  top1024dev_1M_sgd1K_mat[,17] - top1024dev_1M_irls_mat[,17],
                  top1024dev_1M_sgd5K_mat[,17] - top1024dev_1M_irls_mat[,17],
                  top1024dev_1M_sgd10K_mat[,17] - top1024dev_1M_irls_mat[,17],
                  top1024dev_1M_sgd20K_mat[,17] - top1024dev_1M_irls_mat[,17],
                  top1024dev_1M_irlssgd_mat[,17] - top1024dev_1M_irls_mat[,17])

timedata <- timedata / 1000000

colnames(timedata) <- c("IRLS", "BSGD 500", "BSGD 1K", "BSGD 5K", "BSGD 10K", "BSGD 20K", "S-IRLS-SGD")
colnames(precdata) <- c("BSGD 500", "BSGD 1K", "BSGD 5K", "BSGD 10K", "BSGD 20K", "S-IRLS-SGD")
par(mfrow=c(1,1),oma = c(0,0,2,0), mar=c(6.2,5.1,2,2.1))
boxplot(precdata, main="Deviance error for different optimization methods", xaxt="n", frame=F, ylab="Deviance error")
axis(1, at=1:6, labels=colnames(precdata), las=2)
# Save as 800x600 @ 90DPI

boxplot(timedata, main="Computational time for different optimization methods", xaxt="n", frame=F, ylab="Computational time (ms)")
axis(1, at=1:7, labels=colnames(timedata), las=2)


boxplot(precdata[,3:6], main="Deviance error for different optimization methods", xaxt="n", frame=F, ylab="Deviance error")
axis(1, at=1:4, labels=colnames(precdata)[3:6], las=2)

boxplot(log(precdata[,1:6]), main="Deviance error for different optimization methods", xaxt="n", frame=F, ylab="Deviance error")
axis(1, at=1:6, labels=colnames(precdata)[1:6], las=2)