# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-11-03

library(parallel)
library(irls.sgd)
source("functions.R")
source("runner_functions.R")

load("sgd_experiment/data/data.RData")
load("top1024.RData")

logistic.loglik.bic.sgd <- function (y, x, model) {
  mod <- irls.sgd(as.matrix(x[,model]), y, binomial(),
            irls.control=list(maxit=0),
            sgd.control=list(subs=0.01, maxit=10000, alpha=0.05, decay=0.9999999, histfreq=10))
  #ret <- -(mod$deviance/2) - (0.5 * mod$rank * log(length(y)))
  ret <- mod$deviance
  return(ret)
}

logistic.loglik.bic.irlssgd <- function (y, x, model) {
  mod <- irls.sgd(as.matrix(x[,model]), y, binomial(),
            irls.control=list(subs=0.01, maxit=75, tol=1e-7, cooling = c(3,0.9,0.95), expl = c(3,1.5,1)),
            sgd.control=list(subs=0.01, maxit=500, alpha=0.05, decay=0.99, histfreq=10))
  #ret <- -(mod$deviance/2) - (0.5 * mod$rank * log(length(y)))
  ret <- mod$deviance
  return(ret)
}

model_partitions <- align_models(top128mods)
n_obs <- 10000
name <- "top128dev_sgd10K"
directory <- "testdir"

run_multisim(x_10K, y_10K, logistic.loglik.bic.sgd, model_partitions, n_obs, name, directory)