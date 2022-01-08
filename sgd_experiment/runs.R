# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-12-09

source("logis_sim_data.R")
source("likelihoods.R")
source("functions.R")
source("runner_functions.R")
library(parallel)
library(microbenchmark)
library(irls.sgd)

model_partitions <- align_models(top1024mods)
n_obs <- 1000000
directory <- "sgd_exp2"

name <- "top1024dev_1M_sgd500"
run_multisim(mill_x_g, mill_y_l1M, logistic.loglik.bic.sgd, model_partitions, n_obs, name, directory, 500)

name <- "top1024dev_1M_sgd1K"
run_multisim(mill_x_g, mill_y_l1M, logistic.loglik.bic.sgd, model_partitions, n_obs, name, directory, 1000)

name <- "top1024dev_1M_sgd5K"
run_multisim(mill_x_g, mill_y_l1M, logistic.loglik.bic.sgd, model_partitions, n_obs, name, directory, 5000)

name <- "top1024dev_1M_sgd10K"
run_multisim(mill_x_g, mill_y_l1M, logistic.loglik.bic.sgd, model_partitions, n_obs, name, directory, 10000)

name <- "top1024dev_1M_sgd20K"
run_multisim(mill_x_g, mill_y_l1M, logistic.loglik.bic.sgd, model_partitions, n_obs, name, directory, 20000)

name <- "top1024dev_1M_irlssgd"
run_multisim(mill_x_g, mill_y_l1M, logistic.loglik.bic.irlssgd, model_partitions, n_obs, name, directory)

name <- "top1024dev_1M_irls"
run_multisim(mill_x_g, mill_y_l1M, logistic.loglik.bic, model_partitions, n_obs, name, directory)