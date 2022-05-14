
source("../sgd_experiment/likelihoods.R")
source("../functions.R")
source("../runner_functions.R")
library(parallel)
library(fastglm)
library(microbenchmark)

load(file="data_x.RData")
load(file="data_y_10K.RData")

model_partitions <- align_models(1:32)
n_obs <- 10000
name <- "full_10Kg_new"
directory <- "truth"

data_use <- cbind(data_y_10K, data_x[1:n_obs,])


run_multisim(data_use[,-1], data_use[,1], linear.g.prior.loglik, model_partitions, n_obs, name, directory, params=list(g=100/sqrt(n_obs/100)))

load("truth/full_10Kg_new.Rdata")