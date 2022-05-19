library(stochQN)
library(fastglm)
library(irls.sgd)
library(microbenchmark)
library(parallel)

setwd("sqn_experiment")

library(Rcpp)
sourceCpp("logistic.cpp")

source("~/PycharmProjects/subsampling_bms/functions.R")
source("~/PycharmProjects/subsampling_bms/runner_functions.R")

setwd("~/PycharmProjects/thesis_supplementary")
source("logis_sim_data.R")
mill_y_l1M <- as.numeric(mill_y_l1M)

setwd("~/PycharmProjects/subsampling_bms/sqn_experiment")

sqn.fit <- function (x, y, batch_size, iter) {
  model <- stochastic.logistic.regression(dim=ncol(x)-1, lambda=0, optimizer="SQN")

  model$stochQN$grad_fun <- logistic_grad_cpp
  model$stochQN$hess_vec_fun <- logistic_hess_vec_cpp
  model$stochQN$pred_fun <- logistic_pred_cpp

  for (i in seq_len(iter)) {
    batch <- sample(1000000,
      size = batch_size, replace=TRUE)
    partial_fit_logistic(model, x[batch ,-1], y[batch])

  }
  deviance <- irls.sgd::get_deviance(model$coef, x, y, family=binomial())
  return(deviance)
}



load("~/PycharmProjects/subsampling_bms/top1024.RData")

model_partitions <- align_models(top1024mods)
n_obs <- 1000000
directory <- "sqn_exp"

name <- "top1024dev_1M_sqn"
run_multisim(mill_x_g, mill_y_l1M, logistic.loglik.bic.sqn, model_partitions, n_obs, name, directory)

load("sqn_exp/top1024dev_1M_sqn.Rdata")
top1024dev_1M_sqn_mat <- matrix(unlist(top1024dev_1M_sqn), ncol=length(unlist(top1024dev_1M_sqn[[1]])), byrow=T)


library(profvis)
microbenchmark(glmm <- fastglm(mill_x_g, mill_y_l1M, family=binomial()), times=1)
microbenchmark(irlsres <- irls.sgd(mill_x_g, mill_y_l1M, binomial(), irls.control = list(subs = 0.0001, maxit = 50, tol = 1e-07, cooling = c(3, 0.9, 0.95), expl = c(3, 1.5, 1)), sgd.control = list(subs = 0.0001, alpha = 0.001, maxit=300), save_hist = F), times=5)
microbenchmark(devs <- sqn.fit(mill_x_g, mill_y_l1M, 100, 100), times=1)




logistic_grad_new <- function(coefs, X, y, weights = NULL, lambda = 1e-5) {
  pred <- 1 / (1 + exp(-(X %*% coefs)))
  grad <- colMeans(X * as.numeric(pred - y))
  return(as.numeric(grad))
}

logistic_Hess_vec_new <- function(coefs, vec, X, y, weights = NULL, lambda = 1e-5) {
  pred <- 1 / (1 + exp(-as.numeric(X %*% coefs)))
  diag <- pred * (1 - pred)
  Hp <- (t(X) * diag) %*% (X %*% vec)
  Hp <- Hp / NROW(X)
  return(as.numeric(Hp))
}