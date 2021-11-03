# Title     : TODO
# Objective : TODO
# Created by: jonlachmann
# Created on: 2021-10-25

library(parallel)

load("sgd_experiment/data/data.RData")

full_model_count <- 32768

logistic.loglik.bic <- function (y, x, model, complex, params) {
  suppressWarnings({mod <- glm.fit(as.matrix(x[,model]), y, family=binomial())})
  #ret <- -(mod$deviance/2) - (0.5 * mod$rank * log(length(y)))
  ret <- mod$deviance
  return(ret)
}

model_partitions <- align_models(1:full_model_count)
n_obs <- 10000
name <- "test"
directory <- "testdir"

run_multisim(x_10K, y_10K, logistic.loglik.bic, model_partitions, n_obs, name, directory)