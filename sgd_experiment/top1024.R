# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-11-03

library(parallel)
library(microbenchmark)
library(irls.sgd)
source("functions.R")
source("runner_functions.R")

load("sgd_experiment/data/data.RData")
load("top128.RData")

logistic.loglik.bic.sgd <- function (y, x, model) {
  mod <- irls.sgd(as.matrix(x[,model]), y, binomial(),
            irls.control=list(maxit=0),
            sgd.control=list(subs=0.001, maxit=20000, alpha=0.2, decay=0.99995, histfreq=10))
  #ret <- -(mod$deviance/2) - (0.5 * mod$rank * log(length(y)))
  ret <- mod$deviance
  return(ret)
}

logistic.loglik.bic.irlssgd <- function (y, x, model) {
  mod <- irls.sgd(as.matrix(x[,model]), y, binomial(),
            irls.control=list(subs=0.001, maxit=75, tol=1e-7, cooling = c(3,0.9,0.95), expl = c(3,1.5,1)),
            sgd.control=list(subs=0.001, maxit=500, alpha=0.05, decay=0.99, histfreq=10))
  #ret <- -(mod$deviance/2) - (0.5 * mod$rank * log(length(y)))
  ret <- mod$deviance
  return(ret)
}

logistic.loglik.bic <- function (y, x, model) {
  mod <- glm.fit(as.matrix(x[,model]), y, family=binomial())
  #ret <- -(mod$deviance/2) - (0.5 * mod$rank * log(length(y)))
  ret <- mod$deviance
  return(ret)
}

model_partitions <- align_models(1:32768)
n_obs <- 10000
n_obs <- 1000000
name <- "top128dev_1M_irlssgd"
name <- "full_1M_irls"
directory <- "sgd_exp2"

# run_multisim(x_10K, y_10K, logistic.loglik.bic, model_partitions, n_obs, name, directory)

run_multisim(mill_x_g, mill_y_l1M, logistic.loglik.bic, model_partitions, n_obs, name, directory)


setwd("../thesis_supplementary")
source("logis_sim_data.R")
setwd("../subsampling_bms")