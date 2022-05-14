library(parallel)
library(microbenchmark)
library(fastglm)

setwd("sgemm_product")
sgemm <- cbind(1, read.csv("sgemm_product.csv"))
sgemm$run_mean <- rowMeans(sgemm[,16:19])
sgemm <- as.matrix(sgemm)

source("../functions.R")
source("../runner_functions.R")
source("../sgd_experiment/likelihoods.R")
model_partitions <- align_models(1:(2^14))
n_obs <- 241600
name <- "full_sgemm3"
directory <- "truth"

run_multisim(sgemm[,(1:15)], sgemm[,20], linear.g.prior.loglik, model_partitions, n_obs, name, directory, params=list(g=14^2))

load("truth/full_sgemm.Rdata")

full_sgemm_renorm2 <- GMJMCMC:::marginal.probs.renorm(full_sgemm)
